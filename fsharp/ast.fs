module rec Roboot.AST
open Roboot.Runtime
open Roboot.Common

type GlobalId = { id : int64 }

type BodyId = GlobalId * GlobalId

type SymbolKey =
    | Positional of int
    | Named of GlobalId

type LocalId = { name: string }

type Composite = {
    attrs: Map<SymbolKey, AST>
    nodeId: int64
}

type AST = AST of Roboot.Runtime.RObject

let genLocalId () =
    let (UniqId n) = newUniqId ()
    { LocalId.name = sprintf "__tmp_%d" n }

// TODO: use negative integers for positional?
let symbolKeyToInt (k : SymbolKey) =
    match k with
    | SymbolKey.Named id -> id.id
    | SymbolKey.Positional p -> int64(p)

let symbolKeyFromInt (key : int64) =
    if key < 100L then
        Positional(int(key))
    else
        Named { GlobalId.id = key}

let dynobj (id : GlobalId) props =
    let props = List.sortBy fst props
    let fieldNames = props |> List.map (fun (id, _) -> symbolKeyToInt id) |> Array.ofList
    let fieldValues = props |> List.map (fun (_, v) -> (v :> obj)) |> Array.ofList
    Roboot.Runtime.DynObject(id.id, fieldNames, fieldValues)

module Ids =
    let global_id = { GlobalId.id = 7123121521254532152L }
    let positional = { GlobalId.id = 3183205737920190813L }
    let local_id = { GlobalId.id = 4748023960157525165L }
    let literal_str = { GlobalId.id = 973200770135020416L }
    let literal_int = { GlobalId.id = 7865579237671921627L }
    let composite = { GlobalId.id = 8782996361887212223L }

    let vec = { GlobalId.id = 6674409882201890986L }
    let attrs = { GlobalId.id = 1379931292032253541L }
    let node_id = { GlobalId.id = 2508468743753677121L }
    let t = { GlobalId.id = 2880350387728098053L }

    let args = { GlobalId.id = 8303767780132907469L }
    let match_ = { GlobalId.id = 5145279716794819568L }
    let do_ = { GlobalId.id = 6909637523823766449L }

    let true_ = { GlobalId.id = 8518181807273299351L }
    let equal = { GlobalId.id = 7377059081048953208L }
    let if_ = { GlobalId.id = 5324235003218455448L }
    let loop_ = { GlobalId.id = 5383523003576508314L }
    let loop_continue = { GlobalId.id = 6006716333535668702L }
    let unit = { GlobalId.id = 7094982312640608431L }
    let fun_ = { GlobalId.id = 8443262434344113238L }

let unknownNodeId = 0L

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
        | GlobalId id -> SymbolKey.Named id
        | Positional id -> SymbolKey.Positional id
        | _ -> failwith "not a SymbolKey"

let (|Composite|_|) (AST ast) =
    if hasId ast Ids.composite then
        let nodeId: int64 = downcast ast.GetField(Ids.node_id.id)
        let attrs: RObject = downcast ast.GetField(Ids.attrs.id)
        let pairToAttr (key : obj, value) = (symbolKeyFromInt (downcast key), AST ((downcast (value : obj)) : RObject))
        let attrs = vecToList attrs |> List.map tToPair |> List.map pairToAttr |> Map.ofList
        Some ({ Composite.attrs = attrs; nodeId = nodeId })
    else
        None

let makeRPair a b =
    dynobj Ids.t [Positional 0, a :> obj; Positional 1, b :> obj]

let compositeOfList nodeId (l : list<AST>) =
    { Composite.nodeId = nodeId; attrs = List.indexed l |> List.map (fun (i, v) -> (Positional i, v)) |> Map.ofList }

let compositeToAst (c : Composite) =
    let node_id = 0L
    let attrs = Map.toList c.attrs |> List.map (fun (key, AST value) -> makeRPair (symbolKeyToInt key :> obj) value) |> List.toArray
    let attrs = dynobj Ids.vec [Positional 0, attrs]
    AST (dynobj Ids.composite [Named Ids.attrs, attrs :> obj;
                               Named Ids.node_id, node_id :> obj])

let localIdToAst (l : LocalId) =
    AST (dynobj Ids.local_id [Positional 0, l.name])

let literalStringToAst (s : string) =
    AST (dynobj Ids.literal_str [Positional 0, s])

let literalIntToAst (i : int64) =
    AST (dynobj Ids.literal_int [Positional 0, i :> obj])

let globalIdToAst (s : GlobalId) =
    AST (dynobj Ids.global_id [Positional 0, s.id :> obj])

let (|GlobalId|_|) (AST ast) =
    if hasId ast Ids.global_id then
        Some({ GlobalId.id = downcast ast.GetField(0L) })
    else
        None

let (|LocalId|_|) (AST ast) =
    if hasId ast Ids.local_id then
        Some({ LocalId.name = downcast ast.GetField(0L) })
    else
        None

let (|LiteralInt|_|) (AST ast) : option<int64> =
    if hasId ast Ids.literal_int then
        Some (downcast ast.GetField(0L))
    else
        None

let (|LiteralString|_|) (AST ast) : option<string> =
    if hasId ast Ids.literal_str then
        Some (downcast ast.GetField(0L))
    else
        None

let (|Positional|_|) (AST ast) : option<int> =
    if hasId ast Ids.positional then
        Some(downcast ast.GetField(0L))
    else
        None

let globalIdToString idToString (id : GlobalId) =
    match idToString id with
    | Some s -> s
    | None -> sprintf "id.%d" id.id

let extractPositional attrs =
    let rec aux (i, attrs) =
        match Map.tryFind (Positional i) attrs with
        | Some value ->
            Some (value, (i + 1, Map.remove (Positional i) attrs))
        | None ->
            None

    let ((_, attrs), positionalArgs) = unfoldWithState aux (0, attrs)
    attrs, positionalArgs

let (|OnlyPositional|_|) { Composite.attrs = attrs; nodeId = _ } : option<list<AST>> =
    let rest, positional = extractPositional attrs
    if Map.count rest = 0 then
        Some positional
    else
        None

let symbolKeyToString idToString (k : SymbolKey) =
    match k with
    | SymbolKey.Named g -> globalIdToString idToString g
    | SymbolKey.Positional i -> sprintf "positional.%d" i

let splitHead (c : Composite) =
    match Map.tryFind (Positional 0) c.attrs with
    | Some head ->
        let aux (k, v) =
            match k with
            | SymbolKey.Positional 0 -> None
            | SymbolKey.Positional n -> Some (Positional (n-1), v)
            | SymbolKey.Named i -> Some (Named i, v)
        let attrs = c.attrs |> Map.toList |> List.choose aux |> Map.ofList
        Some (head, { Composite.nodeId = c.nodeId; attrs = attrs })
    | None -> None

let astToString idToString (ast : AST) =
    match ast with
    | Composite { attrs = attrs; nodeId = _ } ->
        let (attrs, positional) = extractPositional attrs
        let items =
            List.map (astToString idToString) positional
            @
            (Map.toList attrs |> List.map (fun (k, v) -> sprintf "%s:%s" (symbolKeyToString idToString k) (astToString idToString v)) )

        sprintf "(%s)" (String.concat " " items)
    | LocalId name ->
        sprintf ".%s" name.name
    | GlobalId id ->
        globalIdToString idToString id
    | LiteralInt i ->
        sprintf "%d" i
    | LiteralString s ->
        sprintf "%A" s
    | _ ->
        ast.ToString()

module B =
    let id = globalIdToAst
    let c l = compositeOfList unknownNodeId l |> compositeToAst
    let l = localIdToAst
