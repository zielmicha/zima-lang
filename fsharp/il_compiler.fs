module rec Roboot.ILCompiler
open Roboot.AST
open Roboot.ExpandedAST
open Roboot.ANF
open System
open System.Reflection.Emit
open System.Reflection

type ClrType =
    | AnyObject
    | Native of Type

type FunctionClrSig = {
    methodRef: MethodInfo
    argType: ClrType
    returnType: ClrType
}

type CompilationRequest = {
    body: (AVarName list * ANode)
    argType : ClrType
    returnType : ClrType
    emitMethod : (string -> ILGenerator * MethodInfo)
    findSigByGlobalId : (GlobalId * GlobalId -> FunctionClrSig)
}

type ClrVar = {
    clrType : ClrType
    ref : LocalBuilder
}

type ClrJoin = {
    label : Label
    args : list<ClrVar>
}

type Env = {
    vars : Map<AVarName, ClrVar>
    labels : Map<AJoinName, ClrJoin>
    ilGenerator : ILGenerator
    emitMethod : (string -> ILGenerator * MethodInfo)
    findSigByGlobalId : (GlobalId * GlobalId -> FunctionClrSig)
}

let toClrType = function
    | ClrType.AnyObject -> typeof<obj>
    | ClrType.Native t -> t

let allocLocal env (name : AVarName) typ =
    let localBuilder = env.ilGenerator.DeclareLocal(toClrType typ)
    let clrVar = { clrType = typ; ref = localBuilder }
    clrVar, { env with Env.vars = Map.add name clrVar env.vars }

let loadVar env (name : AVarName) =
    env.ilGenerator.Emit(OpCodes.Ldloc, (Map.find name env.vars).ref)

let loadVarConverted env (name : AVarName) (targetType : ClrType) =
    let v = (Map.find name env.vars)
    env.ilGenerator.Emit(OpCodes.Ldloc, v.ref)
    match v.clrType, targetType with
    | AnyObject, AnyObject -> ()
    | Native t1, Native t2 ->
        // TODO-soon: we should probably check if [t1] is subclass of [t2] and allow conversion then
        if t1 <> t2 then
            failwithf "attempting to convert %A to %A" t1 t2
    | AnyObject, Native t ->
        if t.IsValueType then
            env.ilGenerator.Emit(OpCodes.Unbox_Any, t)
        else
            env.ilGenerator.Emit(OpCodes.Castclass, t)
    | Native t, AnyObject ->
        if t.IsValueType then
            env.ilGenerator.Emit(OpCodes.Box, t)

let compileAtomicExpr env (expr : AtomicExpr<AVarName>) =
    match expr with
    | AtomicExpr.CallGlobal (id, bodyId, arg) ->
        let clrSig = env.findSigByGlobalId(id, bodyId)
        loadVarConverted env arg clrSig.argType
        env.ilGenerator.EmitCall(OpCodes.Call, clrSig.methodRef, null)
        clrSig.returnType
    | AtomicExpr.CallLocal (f, arg) ->
        loadVarConverted env f AnyObject
        env.ilGenerator.Emit(OpCodes.Castclass, typeof<System.Func<obj, obj>>)
        loadVarConverted env arg AnyObject
        env.ilGenerator.EmitCall(OpCodes.Callvirt, typeof<System.Func<obj, obj>>.GetMethod("Invoke"), null)
        AnyObject
    | AtomicExpr.Id x ->
        loadVarConverted env x AnyObject
        AnyObject
    | AtomicExpr.MakeRecord (recordId, values) ->
        let values = List.sort values

        env.ilGenerator.Emit(OpCodes.Ldc_I8, recordId.id)

        env.ilGenerator.Emit(OpCodes.Ldc_I4, values.Length)
        env.ilGenerator.Emit(OpCodes.Newarr, typeof<int64>)

        List.indexed values |> List.iter (
            fun (i, (symbolKey, _)) ->
            env.ilGenerator.Emit(OpCodes.Dup)
            env.ilGenerator.Emit(OpCodes.Ldc_I4, i)
            env.ilGenerator.Emit(OpCodes.Ldc_I8, symbolKeyToInt symbolKey)
            env.ilGenerator.Emit(OpCodes.Stelem_I8))

        env.ilGenerator.Emit(OpCodes.Ldc_I4, values.Length)
        env.ilGenerator.Emit(OpCodes.Newarr, typeof<obj>)

        List.indexed values |> List.iter (
            fun (i, (_, v)) ->
            env.ilGenerator.Emit(OpCodes.Dup)
            env.ilGenerator.Emit(OpCodes.Ldc_I4, i)
            loadVarConverted env v AnyObject
            env.ilGenerator.Emit(OpCodes.Stelem_Ref))

        let ctrs = typeof<Roboot.Runtime.DynObject>.GetConstructors()
        env.ilGenerator.Emit(OpCodes.Newobj, ctrs.[0])
        AnyObject
    | AtomicExpr.IntLiteral value ->
        env.ilGenerator.Emit(OpCodes.Ldc_I8, value)
        env.ilGenerator.Emit(OpCodes.Box, typeof<int64>)
        AnyObject
    | AtomicExpr.StringLiteral value ->
        env.ilGenerator.Emit(OpCodes.Ldstr, value)
        AnyObject
    | AtomicExpr.RaiseUnexpectedVariant ->
        env.ilGenerator.EmitCall(OpCodes.Call, typeof<Roboot.Runtime.Util>.GetMethod("RaiseUnexpectedVariant"), null)
        AnyObject
    | AtomicExpr.IntegerUnaryOp (op, size, a) ->
        loadVarConverted env a AnyObject
        env.ilGenerator.Emit(OpCodes.Unbox, integerType size)
        let opcode =
            match op with
            | Convert I32 -> OpCodes.Conv_I4
            | Convert I64 -> OpCodes.Conv_I8
            | Convert U32 -> OpCodes.Conv_U4
            | Convert U64 -> OpCodes.Conv_U8
            | Convert_ovf I32 -> if isSigned size then OpCodes.Conv_Ovf_I4 else OpCodes.Conv_Ovf_I4_Un
            | Convert_ovf I64 -> if isSigned size then OpCodes.Conv_Ovf_I8 else OpCodes.Conv_Ovf_I8_Un
            | Convert_ovf U32 -> if isSigned size then OpCodes.Conv_Ovf_U4 else OpCodes.Conv_Ovf_U4_Un
            | Convert_ovf U64 -> if isSigned size then OpCodes.Conv_Ovf_U8 else OpCodes.Conv_Ovf_U8_Un
            | Not -> OpCodes.Not
        env.ilGenerator.Emit(opcode)
        let dstSize =
            match op with
            | Convert s -> s
            | Convert_ovf s -> s
            | Not -> size
        env.ilGenerator.Emit(OpCodes.Box, integerType dstSize)
        AnyObject
    | AtomicExpr.IntegerBinaryOp (op, size, a, b) ->
        loadVarConverted env a (Native (integerType size))
        loadVarConverted env b (Native (integerType size))

        let opcode =
            match op with
            | Add -> OpCodes.Add
            | Add_ovf -> OpCodes.Add_Ovf
            | Add_ovf_un -> OpCodes.Add_Ovf_Un
            | And -> OpCodes.And
            | Div -> OpCodes.Div
            | Div_un -> OpCodes.Div_Un
            | Eq -> OpCodes.Ceq
            | Gt -> OpCodes.Cgt
            | Gt_un -> OpCodes.Cgt_Un
            | Lt -> OpCodes.Clt
            | Lt_un -> OpCodes.Clt_Un
            | Mul -> OpCodes.Mul
            | Mul_ovf -> OpCodes.Mul_Ovf
            | Mul_ovf_un -> OpCodes.Mul_Ovf_Un
            | Or -> OpCodes.Or
            | Rem -> OpCodes.Rem
            | Rem_un -> OpCodes.Rem_Un
            | Shl -> OpCodes.Shl
            | Shr_un -> OpCodes.Shr_Un
            | Sub -> OpCodes.Sub
            | Sub_ovf -> OpCodes.Sub_Ovf
            | Xor -> OpCodes.Xor

        env.ilGenerator.Emit(opcode)
        Native (integerType size)
    | AtomicExpr.ClrCall (objType, name, argTypes, returnType, virt, args) ->
         let methodInfo = objType.GetMethod(name, (Array.ofList argTypes))
         List.zip argTypes args |> List.iter (fun (t, arg) -> loadVarConverted env arg (Native t))
         env.ilGenerator.EmitCall((if virt then OpCodes.Callvirt else OpCodes.Call), methodInfo, null)
         Native returnType

let integerType (s : IntegerSize) : Type =
    match s with
    | I32 -> typeof<System.Int32>
    | I64 -> typeof<System.Int64>
    | U32 -> typeof<System.UInt32>
    | U64 -> typeof<System.UInt64>

let compileExpr env (expr : AExpr) =
    match expr with
    | AExpr.Atomic expr ->
        compileAtomicExpr env expr
    | Lambda (_, _) -> failwith "il_compiler encountered unlifted lambda"
    | LiftedLambda (context, contextArg, arg, body) ->
        let req = { CompilationRequest.body = ([contextArg; arg], body);
                    argType = AnyObject; returnType = AnyObject;
                    emitMethod = env.emitMethod;
                    findSigByGlobalId = env.findSigByGlobalId }
        let (lambdaMethod : MethodInfo) = compileWithName "lambda" req
        loadVar env context
        env.ilGenerator.Emit(OpCodes.Ldftn, lambdaMethod)
        env.ilGenerator.Emit(OpCodes.Newobj, typeof<System.Func<obj, obj>>)
        AnyObject

let copyVar env (src : AVarName) (dst : ClrVar) =
    loadVarConverted env src dst.clrType
    env.ilGenerator.Emit(OpCodes.Stloc, dst.ref)

let compilePatternMatch env value (patternId, patternFields) ifMatches ifFails =
    loadVarConverted env value AnyObject
    env.ilGenerator.Emit(OpCodes.Castclass, typeof<Roboot.Runtime.RObject>)

    let patternFields = List.sort patternFields

    let origEnv = env
    let ifFailsLabel = env.ilGenerator.DefineLabel()
    let ifFailsLabelPop = env.ilGenerator.DefineLabel()

    env.ilGenerator.Emit(OpCodes.Dup)
    // TODO: maybe having a single virtual call that returns tuple of rtype, fieldnames and fieldvalues would be faster?
    env.ilGenerator.EmitCall(OpCodes.Callvirt, typeof<Roboot.Runtime.RObject>.GetMethod("get_RType"), null)
    env.ilGenerator.Emit(OpCodes.Ldc_I8, (patternId.id : int64))
    env.ilGenerator.Emit(OpCodes.Bne_Un, ifFailsLabel)

    env.ilGenerator.Emit(OpCodes.Dup)
    env.ilGenerator.EmitCall(OpCodes.Callvirt, typeof<Roboot.Runtime.RObject>.GetMethod("get_FieldNames"), null)

    env.ilGenerator.Emit(OpCodes.Dup)
    env.ilGenerator.Emit(OpCodes.Ldlen)
    env.ilGenerator.Emit(OpCodes.Ldc_I4, List.length patternFields)
    env.ilGenerator.Emit(OpCodes.Bne_Un, ifFailsLabelPop)

    List.indexed patternFields |> List.iter (
        fun (i, (fieldId, _)) ->
        env.ilGenerator.Emit(OpCodes.Dup)
        env.ilGenerator.Emit(OpCodes.Ldc_I4, i)
        env.ilGenerator.Emit(OpCodes.Ldelem_I8)
        env.ilGenerator.Emit(OpCodes.Ldc_I8, symbolKeyToInt fieldId)
        env.ilGenerator.Emit(OpCodes.Bne_Un, ifFailsLabelPop))

    env.ilGenerator.Emit(OpCodes.Pop) // field array

    env.ilGenerator.EmitCall(OpCodes.Callvirt, typeof<Roboot.Runtime.RObject>.GetMethod("get_FieldValues"), null)

    let f (env : Env) (i, (_, targetName)) =
        env.ilGenerator.Emit(OpCodes.Dup)
        env.ilGenerator.Emit(OpCodes.Ldc_I4, (i : int))
        env.ilGenerator.Emit(OpCodes.Ldelem_Ref)

        let (var, env) = allocLocal env targetName AnyObject
        env.ilGenerator.Emit(OpCodes.Stloc, var.ref)
        env

    let env = List.fold f env (List.indexed patternFields)
    env.ilGenerator.Emit(OpCodes.Pop) // value array

    compileNode env ifMatches

    env.ilGenerator.MarkLabel(ifFailsLabelPop)
    env.ilGenerator.Emit(OpCodes.Pop)

    env.ilGenerator.MarkLabel(ifFailsLabel)
    env.ilGenerator.Emit(OpCodes.Pop)

    compileNode origEnv ifFails

let compileIfTrue env value isTrueType ifMatches ifFails =
    let ifFailsLabel = env.ilGenerator.DefineLabel()

    let t = match isTrueType with
            | Integer s -> Native (integerType s)
            | Ref -> AnyObject
            | Bool -> Native (typeof<bool>)

    loadVarConverted env value t

    env.ilGenerator.Emit(OpCodes.Brfalse, ifFailsLabel)

    compileNode env ifMatches
    env.ilGenerator.MarkLabel(ifFailsLabel)
    compileNode env ifFails

let compileNode env (body : ANode) =
    match body with
    | ANode.Let (varName, value, nextBody) ->
        let clrType = compileExpr env value
        let (var, env) = allocLocal env varName clrType
        env.ilGenerator.Emit(OpCodes.Stloc, var.ref)
        compileNode env nextBody
    | ANode.LetJoin { AJoin.name = name; args = args; jumpBody = jumpBody; body = body } ->
        let mutable envForJump = env
        let argVars = List.map (fun arg -> let (var, newEnv) = allocLocal env arg AnyObject in envForJump <- newEnv; var) args
        let label = env.ilGenerator.DefineLabel()
        let clrJoin = { ClrJoin.label = label; args = argVars }
        let envForBody = { env with labels = Map.add name clrJoin env.labels }

        compileNode envForBody body
        env.ilGenerator.MarkLabel(label)
        compileNode envForJump jumpBody
    | ANode.JumpToJoin (name, vars) ->
        let { ClrJoin.label = label; args = args } = Map.find name env.labels
        assert (List.length vars = List.length args)
        List.zip vars args |> List.iter (fun (src, dst) -> copyVar env src dst)
        env.ilGenerator.Emit(OpCodes.Br, label)
    | ANode.PatternMatch (value, patternShape, ifMatches, ifFails) ->
        match patternShape with
        | ANF.Record (id, fields) ->
            compilePatternMatch env value (id, fields) ifMatches ifFails
        | ANF.IsTrue t ->
            compileIfTrue env value t ifMatches ifFails
    | ANode.Return varName ->
        loadVarConverted env varName AnyObject
        env.ilGenerator.Emit(OpCodes.Ret)

let compileWithName name { CompilationRequest.body = (args, body);
                            argType = argType;
                            returnType = returnType;
                            emitMethod = emitMethod;
                            findSigByGlobalId = findSigByGlobalId } =
    let (ilGenerator, methodInfo) = emitMethod name
    let env = { Env.vars = Map.empty;
                labels = Map.empty;
                ilGenerator = ilGenerator;
                emitMethod = emitMethod;
                findSigByGlobalId = findSigByGlobalId }

    let loadArg env (i, argName) =
        let (argVar, env) = allocLocal env argName argType
        env.ilGenerator.Emit(OpCodes.Ldarg, (i : int))
        env.ilGenerator.Emit(OpCodes.Stloc, argVar.ref)
        env

    let env = List.fold loadArg env (List.indexed args)
    compileNode env body
    methodInfo

let compile req = compileWithName "main" req
