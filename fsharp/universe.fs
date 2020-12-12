module rec Roboot.Universe

#nowarn "40"

open Roboot.AST
open Roboot.Common
open Roboot.ContextUtil

type Universe = Map<GlobalId, Map<GlobalId, AST>>

module UniverseIds =
    let current_raw_universe = { GlobalId.id = 6052639924500450093L }
    let absolute_name = { GlobalId.id = 7118174679217591314L }

let currentRaw () =
    getContextField<Universe> (UniverseIds.current_raw_universe)

let findNode nodeId =
    // TODO: run toplevel macros
    currentRaw () |> Map.tryFind nodeId

let withUniverse u f =
    withContextField UniverseIds.current_raw_universe u (fun () -> withEmptyCache f)

let find ((nodeId, bodyId): BodyId) =
    findNode nodeId
    |> Option.bind (Map.tryFind bodyId)

let getNameToIdMap () =
    currentRaw ()
    |> Map.toList
    |> List.choose (fun (id, body) ->
        body
        |> Map.tryFind UniverseIds.absolute_name
        |> Option.bind (fun name ->
            match name with
            | LiteralString name -> Some(name, id)
            | _ -> None))
    |> Map.ofList

let nameToId: unit -> Map<string, GlobalId> =
    let key =
        UnivMap.makeKey<Map<string, GlobalId>> ()

    fun () -> getCached key getNameToIdMap
