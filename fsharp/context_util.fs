module rec Roboot.ContextUtil
open Roboot.AST
open Roboot.Common

let getContextField<'a> (id : GlobalId) : 'a =
    downcast (Roboot.Runtime.Context.Get ()).GetField(id.id)

let withContextField (id : GlobalId) value f =
    let ctx = Roboot.Runtime.Context.Get ()
    let newCtx = ctx.WithField(id.id, value :> obj)
    try
        Roboot.Runtime.Context.Set newCtx
        f ()
    finally
        Roboot.Runtime.Context.Set ctx

module Ids =
    let cache_id = { GlobalId.id = 5177125993998732314L }

let withEmptyCache f =
    withContextField Ids.cache_id (ref UnivMap.empty) f

let getCached<'T> (k : UnivMap.Key<'T>) (defaultF : unit -> 'T) : 'T =
    let m = getContextField<ref<UnivMap>>(Ids.cache_id)
    match UnivMap.tryFind k !m with
    | Some v ->
        v
    | None ->
        let r = defaultF ()
        m := UnivMap.set k r !m
        r

module LazyMapInContext =
    type Descriptor<'k, 'v> when 'k : comparison = {
        f : ('k -> 'v)
        cacheKey: UnivMap.Key<LazyMap<'k, 'v>>
    }

    let find k d =
        let m = getCached d.cacheKey (fun () -> LazyMap.create d.f)
        LazyMap.find k m
