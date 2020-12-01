module rec Roboot.Tests.LambdaLiftingTests
open ExpectTest
open Roboot.Common
open Roboot.AST
open Roboot.ExpandedAST
open Roboot.ANF
open Roboot.LambdaLifting

let f =
 expectTest (
    fun () ->

    let v1 = AVarName (newUniqId ())
    let v2 = AVarName (newUniqId ())
    let x = returnExpr (Lambda (v1, returnExpr (Atomic (IntegerBinaryOp (Add, I32, v1, v2)))))

    printf "%A\n" x
    T.expect @"Let
  (AVarName (UniqId 25),
   Lambda
     (AVarName (UniqId 22),
      Let
        (AVarName (UniqId 24),
         Atomic
           (IntegerBinaryOp
              (Add, I32, AVarName (UniqId 22), AVarName (UniqId 23))),
         Return (AVarName (UniqId 24)))), Return (AVarName (UniqId 25)))"

    printf "%A\n" (liftLambdas x |> Roboot.SimplifyANF.simplifyJoins)
    T.expect @"Let
  (AVarName (UniqId 31),
   Atomic
     (MakeRecord
        ({ id = 6063950190264940280L }, [(Positional 0, AVarName (UniqId 23))])),
   Let
     (AVarName (UniqId 32),
      LiftedLambda
        (AVarName (UniqId 31), AVarName (UniqId 29), AVarName (UniqId 22),
         PatternMatch
           (AVarName (UniqId 29),
            Record
              ({ id = 6063950190264940280L },
               [(Positional 0, AVarName (UniqId 23))]),
            Let
              (AVarName (UniqId 24),
               Atomic
                 (IntegerBinaryOp
                    (Add, I32, AVarName (UniqId 22), AVarName (UniqId 23))),
               Return (AVarName (UniqId 24))),
            Let
              (AVarName (UniqId 30), Atomic RaiseUnexpectedVariant,
               Return (AVarName (UniqId 30))))),
      Let
        (AVarName (UniqId 33), Atomic (Id (AVarName (UniqId 32))),
         Let
           (AVarName (UniqId 25), Atomic (Id (AVarName (UniqId 33))),
            Return (AVarName (UniqId 25))))))"

    )
