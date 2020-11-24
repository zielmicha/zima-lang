module Roboot.Tests.ExpandedToANF

open ExpectTest
open Roboot.ANF
open Roboot.AST
open Roboot.ExpandedAST
open Roboot.ExpandedToANF
open Roboot.SimplifyANF
open Roboot.Common

let f =
 expectTest (
    fun () ->
        let env = Map.ofList [{LocalId.name="foo"}, AVarName (newUniqId ())]
        let a1 = expandedToANF env (Atomic (IntLiteral 5L))

        printf "%A\n" a1
        T.expect @"Let (AVarName (UniqId 2), Atomic (IntLiteral 5L), Return (AVarName (UniqId 2)))"

        let a2 = expandedToANF env (Atomic (MakeRecord ({GlobalId.id=123L}, [Positional 0, Atomic (IntLiteral 5L)])))
        printf "%A\n" a2
        T.expect @"LetJoin
  { name = AJoinName (UniqId 6)
    args = [AVarName (UniqId 4)]
    jumpBody =
              Let
                (AVarName (UniqId 5),
                 Atomic
                   (MakeRecord
                      ({ id = 123L }, [(Positional 0, AVarName (UniqId 4))])),
                 Return (AVarName (UniqId 5)))
    body =
          Let
            (AVarName (UniqId 3), Atomic (IntLiteral 5L),
             JumpToJoin (AJoinName (UniqId 6), [AVarName (UniqId 3)])) }"

        let a3 = expandedToANF env (Match (LocalId {LocalId.name="foo"}, [ {MatchCase.pattern = Record ({GlobalId.id = 123L}, [Positional 0, Var {LocalId.name = "bar"}]); body = LocalId {LocalId.name = "bar"}} ] ))
        printf "%A\n" a3
        T.expect @"LetJoin
  { name = AJoinName (UniqId 11)
    args = [AVarName (UniqId 7)]
    jumpBody =
              LetJoin
                { name = AJoinName (UniqId 9)
                  args = []
                  jumpBody =
                            Let
                              (AVarName (UniqId 8),
                               Atomic RaiseUnexpectedVariant,
                               Return (AVarName (UniqId 8)))
                  body =
                        PatternMatch
                          (AVarName (UniqId 7),
                           Record
                             ({ id = 123L },
                              [(Positional 0, AVarName (UniqId 10))]),
                           Return (AVarName (UniqId 10)),
                           JumpToJoin (AJoinName (UniqId 9), [])) }
    body = JumpToJoin (AJoinName (UniqId 11), [AVarName (UniqId 1)]) }"

        printf "%A\n" (simplifyJoins a3)
        T.expect @"Let
  (AVarName (UniqId 7), Atomic (Id (AVarName (UniqId 1))),
   PatternMatch
     (AVarName (UniqId 7),
      Record ({ id = 123L }, [(Positional 0, AVarName (UniqId 10))]),
      Return (AVarName (UniqId 10)),
      Let
        (AVarName (UniqId 8), Atomic RaiseUnexpectedVariant,
         Return (AVarName (UniqId 8)))))"
    )
