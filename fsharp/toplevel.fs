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

let methodInfos = {
    LazyMapInContext.Descriptor.cacheKey = UnivMap.makeKey<LazyMap<BodyId, ILGenerator * MethodInfo>> ();
    LazyMapInContext.Descriptor.f = fun (nodeId, bodyId) ->
        let m = new DynamicMethod(sprintf "m_%d_%d" nodeId.id bodyId.id, typeof<obj>, [| typeof<obj> |])
        (m.GetILGenerator(), m :> MethodInfo)
}

let findSigByGlobalId bodyId =
    let (_ilGenerator, methodRef) = methodInfos |> LazyMapInContext.find bodyId
    { FunctionClrSig.argType = AnyObject; returnType = AnyObject; methodRef = methodRef }

let funToMatch argsVarName ast =
    match ast with
    | Composite (OnlyPositional (args :: body)) ->
        B.c [ B.id Ids.match_; argsVarName; B.c [ args; B.c (B.id Ids.do_ :: body) ] ]
    | _ ->
        failwithf "invalid [fun] syntax %A" ast

let rec expandFromUniverse ast =
    // todo: attempt expanding user macros
    expandBuiltin expandFromUniverse ast

let toplevelExpandedAst bodyId =
    let argsVarName = genLocalId ()
    let ast = match Universe.find bodyId with Some x -> x | None -> failwithf "%A not found" bodyId
    let expandedAst = expandFromUniverse ast

    argsVarName, expandedAst

let toplevelAnf bodyId =
    let argsVarName, expandedAst = toplevelExpandedAst bodyId
    let argsANFName = AVarName (newUniqId ())
    let env = { Roboot.ExpandedToANF.Env.vars = Map.ofList [argsVarName, argsANFName]; loops = Map.empty }
    argsANFName, expandedToANF env expandedAst

let emitHelperMethod (name, returnType, argTypes) =
    let m = new DynamicMethod(name, returnType, argTypes |> Array.ofList)
    (m.GetILGenerator(), m :> MethodInfo)

type ObjFunc = delegate of obj -> obj

let callCompiledToplevel bodyId : (obj -> obj) =
    let (_ilGenerator, methodRef) = methodInfos |> LazyMapInContext.find bodyId
    let d : ObjFunc = downcast methodRef.CreateDelegate(typeof<ObjFunc>)
    fun arg -> d.Invoke(arg)

let compileToplevelMethod bodyId =
    let argsANFName, anf = toplevelAnf bodyId
    let mainMethod = methodInfos |> LazyMapInContext.find bodyId
    let request = {
        CompilationRequest.body = ([argsANFName], anf)
        argType = AnyObject
        returnType = AnyObject
        emitHelperMethod = emitHelperMethod
        mainMethod = mainMethod
        findSigByGlobalId = findSigByGlobalId
    }
    Roboot.ILCompiler.compile request
