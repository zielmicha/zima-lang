module rec Roboot.Toplevel

open Roboot.AST
open Roboot.ExpandedToANF
open Roboot.ANF
open Roboot.Macroexpand
open Roboot.ILCompiler
open Roboot.ContextUtil
open Roboot.Common
open System.Reflection.Emit
open System.Reflection

module Universe = Roboot.Universe

// non-dynamic assembly
let zimaGeneratedAssembly =
    let name = new AssemblyName("ZimaGenerated")
    AssemblyBuilder.DefineDynamicAssembly(name, AssemblyBuilderAccess.Run)

let zimaGeneratedModule =
    zimaGeneratedAssembly.DefineDynamicModule("ZimaGenerated")

// ----

let methodInfos =
    { LazyMapInContext.Descriptor.cacheKey =
          UnivMap.makeKey<LazyMap<BodyId, ILGenerator * MethodInfo * Lazy<MethodInfo>>> ()
      LazyMapInContext.Descriptor.f =
          fun (nodeId, bodyId) ->
              let m =
                  new DynamicMethod(sprintf "m_%d_%d" nodeId.id bodyId.id, typeof<obj>, [| typeof<obj> |])

              (m.GetILGenerator(), m :> MethodInfo, lazy (m :> MethodInfo)) }
// let containerTypeBuilder =
//     zimaGeneratedModule.DefineType(sprintf "zima_gen_%d_%d" nodeId.id bodyId.id, TypeAttributes.Public)

// let m =
//     containerTypeBuilder.DefineMethod
//         ("call",
//          MethodAttributes.Public
//          ||| MethodAttributes.Static,
//          typeof<obj>,
//          [| typeof<obj> |])

// let getFinalMethod =
//     lazy
//         (let t = containerTypeBuilder.CreateType()

//          match t.GetMethod("call") with
//          | null -> failwith "defined method not found in the type (?)"
//          | m -> m)

// (m.GetILGenerator(), m :> MethodInfo, getFinalMethod) }

let findSigByGlobalId bodyId =
    let (_ilGenerator, methodRef, _getFinalMethod) =
        methodInfos |> LazyMapInContext.find bodyId

    { FunctionClrSig.argType = AnyObject
      returnType = AnyObject
      methodRef = methodRef }

let funToMatch argsVarName ast =
    match ast with
    | Composite (OnlyPositional (args :: body)) ->
        let args =
            match args with
            | Composite c ->
                prepend c (globalIdToAst Ids.args)
                |> compositeToAst
            | _ -> args

        B.c [ B.id Ids.match_
              B.l argsVarName
              B.c [ args; B.c (B.id Ids.do_ :: body) ] ]
    | _ -> failwithf "invalid [fun] syntax %A" ast

let rec expandFromUniverse ast =
    // todo: attempt expanding user macros
    expandBuiltin expandFromUniverse ast

let toplevelExpandedAst bodyId =
    let argsVarName = genLocalId ()

    let ast =
        match Universe.find bodyId with
        | Some x -> x
        | None -> failwithf "%A not found" bodyId

    let ast = funToMatch argsVarName ast
    let expandedAst = expandFromUniverse ast

    argsVarName, expandedAst

let toplevelAnf bodyId =
    let argsVarName, expandedAst = toplevelExpandedAst bodyId
    let argsANFName = AVarName(newUniqId ())

    let env =
        { Roboot.ExpandedToANF.Env.vars = Map.ofList [ argsVarName, argsANFName ]
          loops = Map.empty }

    argsANFName, expandedToANF env expandedAst

let emitHelperMethod (name, returnType, argTypes) =
    let m =
        new DynamicMethod(name, returnType, argTypes |> Array.ofList)

    (m.GetILGenerator(), m :> MethodInfo)

type ObjFunc = delegate of obj -> obj

let callCompiledToplevel bodyId: (obj -> obj) =
    let (_ilGenerator, _methodRef, getFinalMethod) =
        methodInfos |> LazyMapInContext.find bodyId

    let d: ObjFunc =
        downcast (getFinalMethod.Force())
            .CreateDelegate(typeof<ObjFunc>)

    fun arg -> d.Invoke(arg)

let compileToplevelMethod bodyId =
    let argsANFName, anf = toplevelAnf bodyId

    let (ilGenerator, methodInfo, _) =
        methodInfos |> LazyMapInContext.find bodyId

    let mutable deps = Set.empty

    let request =
        { CompilationRequest.body = ([ argsANFName ], anf)
          argType = AnyObject
          returnType = AnyObject
          emitHelperMethod = emitHelperMethod
          mainMethod = (ilGenerator, methodInfo)
          findSigByGlobalId =
              fun depId ->
                  deps <- deps.Add(depId)
                  findSigByGlobalId depId }

    Roboot.ILCompiler.compile request
    deps

let compileToplevelMethodRecursively bodyId =
    let pending = new ResizeArray<BodyId>([ bodyId ])
    let mutable compiled = Set.empty

    while pending.Count <> 0 do
        let bodyId = pending.[pending.Count - 1]
        pending.RemoveAt(pending.Count - 1)

        if not (compiled.Contains(bodyId)) then
            compiled <- compiled.Add(bodyId)
            pending.AddRange(compileToplevelMethod bodyId)