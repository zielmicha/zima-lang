module rec Roboot.Types
open Roboot.AST

// Transient-Type

type TType =
    ()

// TypeExpr

type TypeExprName = TypeExprName of UniqId

type CompositeType = {
    fields: Map<SymbolKey, TypeExpr>
    isOpen: bool
}

type UnionType = {
    subtypeOf: list<TypeExprName>
    supertypeOf: list<TypeExprName>
    cases: Map<GlobalId, CompositeType>
}

type TypeExpr =
    | ForAll of TypeExprName * body:TypeExpr
    | TypeLet of TypeExprName * typeBody:TypeExrp * body:TypeExpr
    | UseTypeName of TypeExprName
    | AbstractTypeInst of GlobalId * Map<SymbolKey, TypeExpr>
    | UnionType of UnionType

type Variance =
    { usedAsArgument: bool; usedAsReturn: bool }

// GlobalId -> Map<SymbolKey, Variance>
