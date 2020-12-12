module rec Roboot.ContextUtil

open Roboot.AST
open Roboot.Common

let getContextField<'a> (id: GlobalId): 'a =
    let ctx = Roboot.Runtime.Context.Get()

    if ctx = null then
        failwithf "context is empty and has no field %A" id
    else
        let r = ctx.GetField(id.id)

        if r = null then failwithf "context has no field %A" id else downcast r

let tryGetContextField<'a> (id: GlobalId): option<'a> =
    let ctx = Roboot.Runtime.Context.Get()

    if ctx = null then
        None
    else
        let r = ctx.GetField(id.id)

        if r = null then None else Some(downcast r)

module Ids =
    let ctx_record = { GlobalId.id = 6178504627254640106L }
    let cache_id = { GlobalId.id = 5177125993998732314L }

let withContextField (id: GlobalId) value f =
    let ctx = Roboot.Runtime.Context.Get()

    let ctx =
        if ctx <> null
        then ctx
        else ((dynobj Ids.ctx_record []) :> Roboot.Runtime.RObject)

    let newCtx = ctx.WithField(id.id, value :> obj)

    try
        Roboot.Runtime.Context.Set(newCtx)
        f ()
    finally
        Roboot.Runtime.Context.Set(ctx)

let withEmptyCache f =
    withContextField Ids.cache_id (ref UnivMap.empty) f

let getCached<'T> (k: UnivMap.Key<'T>) (defaultF: unit -> 'T): 'T =
    let m =
        getContextField<ref<UnivMap>> (Ids.cache_id)

    match UnivMap.tryFind k !m with
    | Some v -> v
    | None ->
        let r = defaultF ()
        m := UnivMap.set k r !m
        r

module LazyMapInContext =
    type Descriptor<'k, 'v when 'k: comparison> =
        { f: ('k -> 'v)
          cacheKey: UnivMap.Key<LazyMap<'k, 'v>> }

    let find k d =
        let m =
            getCached d.cacheKey (fun () -> LazyMap.create d.f)

        LazyMap.find k m