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
