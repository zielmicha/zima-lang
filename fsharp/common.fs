module Roboot.Common

[<Struct>]
type UniqId = UniqId of int

let currentUniqId = ref 0
//let idMutex = new System.Threading.Mutex()

let newUniqId () =
    // not thread safe
    lock currentUniqId (
        fun () ->
          currentUniqId := !currentUniqId + 1
          UniqId(!currentUniqId))

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

    let find m k =
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
