module Roboot.ILCompiler
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
   emitHelperMethod : (string * Type * list<Type>) -> (ILGenerator * MethodInfo)
   mainMethod : (ILGenerator * MethodInfo)
   findSigByGlobalId : (GlobalId * GlobalId -> FunctionClrSig)
}

val compile : CompilationRequest -> unit
