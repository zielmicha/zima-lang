module Roboot.Common

[<Struct>]
type UniqId = UniqId of int

let currentUniqId = ref 0

let newUniqId () =
    // TODO: use atomic
    lock currentUniqId (
        fun () ->
          currentUniqId := !currentUniqId + 1
          UniqId(!currentUniqId))

let unfoldWithState generator state =
    let rec aux generator state res =
        match generator state with
        | None -> (state, List.rev res)
        | Some (item, newState) -> aux generator newState (item :: res)

    aux generator state []

type LazyMapState<'v> =
    | Computing
    | Value of 'v

type LazyMap<'k, 'v> when 'k : comparison = {
    f : ('k -> 'v)
    data: ref<Map<'k, LazyMapState<'v>>>
}

module LazyMap =
    let create f =
        { LazyMap.f = f ; data = ref Map.empty }

    let find k m =
        match Map.tryFind k !m.data with
        | Some Computing ->
            failwithf "recursive dependency - value for %A is already being computed" k
        | Some (Value v) ->
            v
        | None ->
            m.data := Map.add k (Computing) !m.data
            let res = m.f k
            m.data := Map.add k (Value res) !m.data
            res

type UnivMap = {
    data: Map<UniqId, obj>
}

module UnivMap =
    type Key<'T> = { id: UniqId }

    let makeKey<'T> () : Key<'T> = { id = newUniqId () }

    let empty = { data = Map.empty }

    let tryFind (k : Key<'T>) (m : UnivMap) : option<'T> =
        Map.tryFind k.id m.data |> Option.map (fun v -> ((downcast v) : 'T))

    let set (k : Key<'T>) (v : 'T) (m : UnivMap) =
        { UnivMap.data = Map.add k.id (v :> obj) m.data }
