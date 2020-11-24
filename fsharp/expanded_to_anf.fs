module rec Roboot.ExpandedToANF
open Roboot.AST
open Roboot.Common
open Roboot.ExpandedAST
open Roboot.ANF

type Env = Map<LocalId, AVarName>

// this is quadratic, we would ideally use (ANode -> ANode) thunks
let insertJumpAtEnd joinId (node : ANode) =
    match node with
    | Let (varName, value, body) -> Let (varName, value, insertJumpAtEnd joinId body)
    | PatternMatch (value, patternShape, ifMatch, ifFails) ->
        PatternMatch (value, patternShape, insertJumpAtEnd joinId ifMatch, insertJumpAtEnd joinId ifFails)
    | Return varName ->
        JumpToJoin(joinId, [varName])
    | JumpToJoin(joinName, vars) ->
        // all join bodies that don't end with jump are transformed to jump to joinId
        JumpToJoin(joinName, vars)
    | LetJoin({ name = name; args = args; jumpBody = jumpBody; body = body }) ->
        LetJoin({ name = name; args = args; jumpBody = insertJumpAtEnd joinId jumpBody; body = insertJumpAtEnd joinId body })

let makeLet (result : ANode) (var : AVarName) (value : ANode) : ANode =
    let joinId = AJoinName (newUniqId ())
    LetJoin({
        name = joinId;
        args = [ var ];
        jumpBody = result;
        body = insertJumpAtEnd joinId value })

let withAllocVar (f : ((ANode -> AVarName) -> ANode)) =
    let mutable vars = []
    let allocVar value =
        let name = AVarName (newUniqId ())
        vars <- (name, value) :: vars
        name

    let result = f allocVar
    List.fold (fun accum (var, value) -> makeLet accum var value) result vars

let atomicToANF env (x : AtomicExpr<ExpandedAST>) : ANode =
    withAllocVar (fun allocVar ->
        let a : AtomicExpr<AVarName> = atomicExprMap x (fun child -> allocVar (expandedToANF env child))
        returnExpr (Atomic a))

let patternToANF env (jumpIfFail : AJoinName) (body : Env -> ANode) (pattern : MatchPattern) (value : AVarName) =
    match pattern with
    | MatchPattern.Var localId ->
        let newEnv = (env |> Map.add localId value) in body newEnv
    | MatchPattern.Record (typeId, attrs) ->
        let vars = attrs |> List.map (fun (key, _) -> (key, AVarName (newUniqId ())))
        let aux ((_, varName), (_, subpattern)) nextBody = fun env ->
            patternToANF env jumpIfFail nextBody subpattern varName
        let allSubpatterns = (List.foldBack aux (List.zip vars attrs) body) env
        let patternShape = APatternShape.Record (typeId, vars)
        PatternMatch(value, patternShape, allSubpatterns, (JumpToJoin(jumpIfFail, [])))
    | MatchPattern.MatchMap (varName, mapExpr, subpattern) ->
        let newEnv = (env |> Map.add varName value)
        withAllocVar (fun allocVar ->
            let newValue = allocVar (expandedToANF newEnv mapExpr)
            patternToANF env jumpIfFail body subpattern newValue)

let expandedToANF env (x : ExpandedAST) =
    match x with
    | ExpandedAST.Atomic a -> atomicToANF env a
    | ExpandedAST.Lambda (arg, body) ->
        let name = AVarName (newUniqId ())
        let body = expandedToANF (env |> Map.add arg name) body
        returnExpr (Lambda(name, body))
    | ExpandedAST.LocalId id ->
        (match Map.tryFind id env with
        | None -> failwithf "no var named %s" (id.name)
        | Some var -> Return var)
    | ExpandedAST.Match (value, cases) ->
        withAllocVar (fun allocVar ->
            let value = allocVar (expandedToANF env value)
            let aux { MatchCase.pattern = pattern; body = body } nextTry =
                let jumpIfFail = AJoinName (newUniqId ())
                LetJoin {
                    name = jumpIfFail;
                    args = [];
                    jumpBody = nextTry;
                    body = patternToANF env jumpIfFail (fun env -> expandedToANF env body) pattern value
                }

            List.foldBack aux cases (returnExpr (Atomic RaiseUnexpectedVariant)))
