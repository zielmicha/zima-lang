module Roboot.Tests.ILCompilerTests

open ExpectTest
open Roboot.ANF
open Roboot.AST
open Roboot.ExpandedAST
open Roboot.ExpandedToANF
open Roboot.SimplifyANF
open Roboot.Common
open Roboot.ILCompiler
open System.Reflection.Emit
open System.Reflection

type ObjFunc = delegate of obj -> obj

let compileAndRun (argName : AVarName) (body : ANode) value =
    let emitMethod (name, returnType, argTypes) =
        let m = new DynamicMethod(name, returnType, argTypes |> Array.ofList)
        (m.GetILGenerator(), m :> MethodInfo)

    let (ilGenerator, mainMethodInfo) = emitMethod ("main", typeof<obj>, [typeof<obj>])
    let req = {
        CompilationRequest.body = ([argName], body);
        argType = AnyObject;
        returnType = AnyObject;
        emitHelperMethod = emitMethod;
        mainMethod = (ilGenerator, mainMethodInfo)
        findSigByGlobalId = fun s -> failwith "unexpected findSigByGlobalId"
    }

    Roboot.ILCompiler.compile req
    let d : ObjFunc = downcast mainMethodInfo.CreateDelegate(typeof<ObjFunc>)
    d.Invoke(value)

let f =
 expectTest (
    fun () ->
    let argVar = (AVarName (newUniqId ()))

    let (body : ANode) = returnExpr (AExpr.Atomic (IntLiteral 5L))
    printf "%A\n" (compileAndRun argVar body ("test" :> obj))
    T.expect @"5L"

    let (body : ANode) = Return argVar
    printf "%A\n" (compileAndRun argVar body ("test" :> obj))
    T.expect @"""test"""

    let (body : ANode) = returnExpr (AExpr.Atomic (MakeRecord ({GlobalId.id=123L}, [Positional 1, argVar; Positional 0, argVar])))
    printf "%A\n" (compileAndRun argVar body ("test" :> obj))
    T.expect @"123{0=test, 1=test}"

    let p0 = (AVarName (newUniqId ()))
    let p1 = (AVarName (newUniqId ()))

    let shape = APatternShape.Record ({GlobalId.id=123L}, [Positional 0, p0; Positional 1, p1])
    let ifMatches = Return p0
    let ifFails = returnExpr (AExpr.Atomic (StringLiteral "not matched"))
    let (body : ANode) = (ANode.PatternMatch (argVar, shape, ifMatches, ifFails))
    printf "%A\n" (compileAndRun argVar body (dynobj {GlobalId.id=123L} [ Positional 0, "foo" ; Positional 1, "bar" ]))
    T.expect @"""foo"""

    printf "%A\n" (compileAndRun argVar body (dynobj {GlobalId.id=123L} [ Positional 0, "bar" ]))
    T.expect @"""not matched"""

    printf "%A\n" (compileAndRun argVar body (dynobj {GlobalId.id=123L} [ Positional 0, "foo" ; Positional 2, "bar" ]))
    T.expect @"""not matched"""

    printf "%A\n" (compileAndRun argVar body (dynobj {GlobalId.id=124L} [ Positional 0, "bar" ; Positional 1, "bar" ]))
    T.expect @"""not matched"""

    let shape = APatternShape.Record ({GlobalId.id=123L}, [Positional 0, p0; Positional 1, p1])
    let ifMatches = returnExpr (AExpr.Atomic (IntegerBinaryOp (Add, I32, p0, p1)))
    let ifFails = returnExpr (AExpr.Atomic (IntLiteral 66L))
    let (body : ANode) = (ANode.PatternMatch (argVar, shape, ifMatches, ifFails))
    printf "%A\n" (compileAndRun argVar body (dynobj {GlobalId.id=123L} [ Positional 0, (1 :> obj) ; Positional 1, (10 :> obj) ]))
    T.expect @"11"

    let shape = APatternShape.IsTrue (Integer I64)
    let ifMatches = returnExpr (AExpr.Atomic (IntLiteral 99L))
    let ifFails = returnExpr (AExpr.Atomic (IntLiteral 66L))
    let (body : ANode) = (ANode.PatternMatch (argVar, shape, ifMatches, ifFails))
    printf "%A\n" (compileAndRun argVar body 0L)
    T.expect @"66L"
    printf "%A\n" (compileAndRun argVar body 1L)
    T.expect @"99L"
)
