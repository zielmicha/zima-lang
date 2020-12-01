module rec Roboot.Universe
#nowarn "40"
open Roboot.AST
open Roboot.Common
open Roboot.ContextUtil

type Universe = Map<GlobalId, Map<GlobalId, AST>>

module Ids =
    let current_raw_universe = { GlobalId.id = 6052639924500450093L }
    let absolute_name = { GlobalId.id = 7118174679217591314L }

let currentRaw () = getContextField<Universe>(Ids.current_raw_universe)

let findNode nodeId =
    // TODO: run toplevel macros
    currentRaw () |> Map.tryFind nodeId

let find ((nodeId, bodyId) : BodyId) =
    findNode nodeId |> Option.bind (Map.tryFind bodyId)

let getNameToIdMap () =
    currentRaw () |> Map.toList |> List.choose (
        fun (id, body) ->
            body |> Map.tryFind Ids.absolute_name |> Option.bind (fun name -> match name with LiteralString name -> Some (name, id) | _ -> None)
        ) |> Map.ofList

let nameToId =
    let key = UnivMap.makeKey<Map<string, GlobalId>> () in
    fun () -> getCached key getNameToIdMap
