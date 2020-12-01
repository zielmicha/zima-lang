module rec Roboot.ExpandedToANF
open Roboot.AST
open Roboot.Common
open Roboot.ExpandedAST
open Roboot.ANF

type Env = {
    vars: Map<LocalId, AVarName>
    loops: Map<LocalId, AJoinName>
}

let addVar k v env = { env with vars = env.vars |> Map.add k v }

let atomicToANF env (x : AtomicExpr<ExpandedAST>) : ANode =
    withAllocVar (fun allocVar ->
        let a : AtomicExpr<AVarName> = atomicExprMap x (fun child -> allocVar (expandedToANF env child))
        returnExpr (Atomic a))

let patternToANF env (jumpIfFail : AJoinName) (body : Env -> ANode) (pattern : MatchPattern) (value : AVarName) =
    match pattern with
    | MatchPattern.Var localId ->
        let newEnv = (env |> addVar localId value) in body newEnv
    | MatchPattern.Record (typeId, attrs) ->
        let vars = attrs |> List.map (fun (key, _) -> (key, AVarName (newUniqId ())))
        let aux ((_, varName), (_, subpattern)) nextBody = fun env ->
            patternToANF env jumpIfFail nextBody subpattern varName
        let allSubpatterns = (List.foldBack aux (List.zip vars attrs) body) env
        let patternShape = APatternShape.Record (typeId, vars)
        PatternMatch(value, patternShape, allSubpatterns, (JumpToJoin(jumpIfFail, [])))
    | MatchPattern.MatchMap (varName, mapExpr, subpattern) ->
        let newEnv = (env |> addVar varName value)
        withAllocVar (fun allocVar ->
            let newValue = allocVar (expandedToANF newEnv mapExpr)
            patternToANF env jumpIfFail body subpattern newValue)

let expandedToANF env (x : ExpandedAST) =
    match x with
    | ExpandedAST.Atomic a -> atomicToANF env a
    | ExpandedAST.Lambda (arg, body) ->
        let name = AVarName (newUniqId ())
        let body = expandedToANF (env |> addVar arg name) body
        returnExpr (Lambda(name, body))
    | ExpandedAST.LocalId id ->
        (match Map.tryFind id env.vars with
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
    | ExpandedAST.Loop (name, body) ->
        let startOfLoop = AJoinName (newUniqId ())
        let env = { env with loops = env.loops |> Map.add name startOfLoop }
        LetJoin {
            name = startOfLoop;
            args = [];
            jumpBody = expandedToANF env body;
            body = JumpToJoin (startOfLoop, []) }
    | ExpandedAST.LoopContinue name ->
        JumpToJoin ((env.loops |> Map.find name), [])
