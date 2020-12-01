module rec Roboot.ExpandedAST
open Roboot.AST

type IntegerBinaryOp =
    | Add
    | Add_ovf
    | Add_ovf_un
    | And
    | Div
    | Div_un
    | Eq
    | Gt
    | Gt_un
    | Lt
    | Lt_un
    | Mul
    | Mul_ovf
    | Mul_ovf_un
    | Or
    | Rem
    | Rem_un
    | Shl
    | Shr_un
    | Sub
    | Sub_ovf
    | Xor

type IntegerSize =
    | I32 | I64 | U32 | U64

let isSigned x = match x with | I32 | I64 -> true | U32 | U64 -> false

type IntegerUnaryOp =
    | Convert of IntegerSize
    | Convert_ovf of IntegerSize
    | Not

type AtomicExpr<'a> =
    | CallGlobal of GlobalId * GlobalId * 'a
    | CallLocal of 'a * 'a
    | Id of 'a
    | MakeRecord of GlobalId * list<SymbolKey * 'a>
    | RaiseUnexpectedVariant
    | IntLiteral of int64
    | StringLiteral of string
    | IntegerBinaryOp of IntegerBinaryOp * IntegerSize * 'a * 'a
    | IntegerUnaryOp of IntegerUnaryOp * IntegerSize * 'a
    | ClrNew of objType:System.Type * argTypes:System.Type list * argValues:'a list
    | ClrCall of objType:System.Type * name:string * argTypes:System.Type list * returnType:System.Type * virt:bool * argValues:'a list
    | ClrGetField of objType:System.Type * name:string * fieldType:System.Type * obj:'a

let atomicExprMap (x : AtomicExpr<'a>) (f : 'a -> 'b) : AtomicExpr<'b>  =
    match x with
    | CallGlobal (id, bodyId, a) -> CallGlobal (id, bodyId, f a)
    | CallLocal (func, a) -> CallLocal (f func, f a)
    | Id a -> Id (f a)
    | MakeRecord (id, attrs) ->
        MakeRecord (id, List.map (fun (key, item) -> (key, f item)) attrs)
    | RaiseUnexpectedVariant ->
        RaiseUnexpectedVariant
    | IntLiteral x -> IntLiteral x
    | StringLiteral x -> StringLiteral x
    | IntegerBinaryOp (op, size, a, b) -> IntegerBinaryOp (op, size, f a, f b)
    | IntegerUnaryOp (op, size, a) -> IntegerUnaryOp (op, size, f a)
    | ClrNew (objType, argTypes, args) -> ClrNew (objType, argTypes, List.map f args)
    | ClrCall (objType, name, argTypes, returnType, virt, args) -> ClrCall (objType, name, argTypes, returnType, virt, List.map f args)
    | ClrGetField (objType, name, fieldType, obj) -> ClrGetField (objType, name, fieldType, f obj)

type ExpandedAST =
    | Atomic of AtomicExpr<ExpandedAST>
    | Lambda of LocalId * ExpandedAST
    | Match of ExpandedAST * list<MatchCase>
    | LocalId of LocalId
    | Loop of name:LocalId * body:ExpandedAST
    | LoopContinue of name:LocalId

type MatchCase = {
    pattern : MatchPattern;
    body: ExpandedAST
}

type MatchPattern =
    | Record of GlobalId * list<SymbolKey * MatchPattern>
    | MatchMap of varName:LocalId * mapExpr:ExpandedAST * MatchPattern
    | Var of LocalId
