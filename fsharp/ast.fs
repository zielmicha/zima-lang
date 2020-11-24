module rec Roboot.AST
open Roboot.Runtime

type GlobalId = { id : int64 }

type SymbolKey =
    | Positional of int
    | GlobalId of GlobalId

type LocalId = { name: string }

type Composite = {
    attrs: Map<SymbolKey, AST>
    nodeId: int64
}

type AST = AST of Roboot.Runtime.RObject

let symbolKeyToInt (k : SymbolKey) =
    match k with
    | SymbolKey.GlobalId id -> id.id
    | SymbolKey.Positional p -> int64(p)

let dynobj (id : GlobalId) props =
    let props = List.sortBy fst props
    let fieldNames = props |> List.map (fun (id, _) -> symbolKeyToInt id) |> Array.ofList
    let fieldValues = props |> List.map (fun (_, v) -> (v :> obj)) |> Array.ofList
    Roboot.Runtime.DynObject(id.id, fieldNames, fieldValues)

module Ids =
    let global_id = { GlobalId.id = 7123121521254532152L }
    let positional = { GlobalId.id = 3183205737920190813L }
    let local_symbol = { GlobalId.id = 4748023960157525165L }
    let literal_str = { GlobalId.id = 973200770135020416L }
    let literal_int = { GlobalId.id = 7865579237671921627L }
    let composite = { GlobalId.id = 8782996361887212223L }

    let vec = { GlobalId.id = 6674409882201890986L }
    let attrs = { GlobalId.id = 1379931292032253541L }
    let node_id = { GlobalId.id = 2508468743753677121L }
    let t = { GlobalId.id = 2880350387728098053L }

let hasId (r : RObject) (id : GlobalId) =
    r.RType = id.id

let vecToList (r : RObject) =
    // we should probably call vec_to_array defined in the universe
    if hasId r Ids.vec then
        let a : obj array = downcast r.GetField(0L)
        Array.toList a
    else
        failwith "value is not a vector"

let tToPair (r : obj) =
    let r : RObject = downcast r
    if hasId r Ids.t then
        (r.GetField(0L), r.GetField(1L))
    else
        failwith "value is not a pair"

let symbolKey (key : obj) =
    let key = AST((downcast key) : RObject)
    match key with
        | GlobalId id -> SymbolKey.GlobalId id
        | Positional id -> SymbolKey.Positional id
        | _ -> failwith "not a SymbolKey"

let (|Composite|_|) (AST ast) =
    if hasId ast Ids.composite then
        let nodeId: int64 = downcast ast.GetField(Ids.node_id.id)
        let attrs: RObject = downcast ast.GetField(Ids.attrs.id)
        let pairToAttr (key, value) = (symbolKey key, AST ((downcast (value : obj)) : RObject))
        let attrs = Map.ofList (List.map pairToAttr (List.map tToPair (vecToList attrs)))
        Some ({ Composite.attrs = attrs; nodeId = nodeId })
    else
        None

let compositeToAst (c : Composite) =
    failwith "fail!"

let localIdToAst (l : LocalId) =
    failwith "fail!"

let stringLiteralToAst (s : string) =
    failwith "fail!"

let intLiteralToAst (s : int64) =
    failwith "fail!"

let globalIdToAst (s : GlobalId) =
    failwith "fail!"

let (|GlobalId|_|) (AST ast) =
    if hasId ast Ids.global_id then
        Some({ GlobalId.id = downcast ast.GetField(0L) })
    else
        None

let (|Positional|_|) (AST ast) : option<int> =
    if hasId ast Ids.positional then
        Some(downcast ast.GetField(0L))
    else
        None
