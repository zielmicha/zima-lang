module rec Roboot.ANF
open Roboot.AST
open Roboot.Common
open Roboot.ExpandedAST

[<Struct>]
type AVarName = AVarName of UniqId

[<Struct>]
type AJoinName = AJoinName of UniqId

type ANode =
    | Let of varName: AVarName * value : AExpr * body : ANode
    // | TailCall of AExpr
    | PatternMatch of value : AVarName * patternShape: APatternShape * ifMatches : ANode * ifFails : ANode
    | Return of AVarName
    | JumpToJoin of AJoinName * vars : list<AVarName>
    | LetJoin of AJoin

type AExpr =
    | Atomic of AtomicExpr<AVarName>
    | Lambda of AVarName * ANode
    | LiftedLambda of context:AVarName * contextArg:AVarName * arg:AVarName * ANode

type AJoin = {
    name : AJoinName;
    args : list<AVarName>;
    jumpBody : ANode;
    body : ANode
}

type APatternShape =
    // TODO: add support for optional keys
    | Record of GlobalId * list<SymbolKey * AVarName>
    | IsTrue of IsTrueType

type IsTrueType =
    | Integer of IntegerSize
    | Ref
    | Bool

let returnExpr x =
    let varName = AVarName (newUniqId ())
    Let(varName, x, Return varName)

let patternShapeVars = function
    | Record (_, l) -> l |> List.map (fun (_, name) -> name)
    | IsTrue _ -> []

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

let anfLet x (f : AVarName -> ANode) =
    let varName = AVarName (newUniqId ())
    Let(varName, x, f varName)

let withAllocVar (f : ((ANode -> AVarName) -> ANode)) =
    let mutable vars = []
    let allocVar value =
        let name = AVarName (newUniqId ())
        vars <- (name, value) :: vars
        name

    let result = f allocVar
    List.fold (fun accum (var, value) -> makeLet accum var value) result vars
