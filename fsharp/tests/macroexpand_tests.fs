module Roboot.Tests.MacroexpandTests

open ExpectTest
open Roboot.AST
open Roboot.ExpandedAST
open Roboot.Macroexpand
open Roboot.Universe
open Roboot.MakeAST

let f =
 expectTest (
    fun () ->
    let names = [
     "integer_op", { GlobalId.id = 4828890982335511950L };
     "int64", { GlobalId.id = 8471708282667706810L };
     "add", { GlobalId.id = 8687139141449869645L };
     "match", Ids.match_
     "foo", { GlobalId.id = 8200069868577233510L };
    ]

    let resolveName = let m = Map.ofList names in (fun k -> match Map.tryFind k m with Some g -> g | None -> failwithf "name not found %A" k)
    let rec expandBuiltinRecur ast = expandBuiltin expandBuiltinRecur ast

    let simpleAdd = makeOneASTFromString resolveName "(integer_op add int64 1 2)"

    expandBuiltinRecur simpleAdd |> printf "%A\n"
    T.expect @"Atomic
  (IntegerBinaryOp (Add, I64, Atomic (IntLiteral 1L), Atomic (IntLiteral 2L)))"

    let simpleMatch = makeOneASTFromString resolveName "(match 1 (.x 2))"
    expandBuiltinRecur simpleMatch |> printf "%A\n"
    T.expect @"Match (Atomic (IntLiteral 1L), [{ pattern = Var { name = ""x"" }
                                  body = Atomic (IntLiteral 2L) }])"

    let simpleMatch = makeOneASTFromString resolveName "(foo 1 2)"
    expandBuiltinRecur simpleMatch |> printf "%A\n"
    T.expect @"Atomic
  (CallGlobal
     ({ id = 8200069868577233510L }, { id = 8443262434344113238L },
      Atomic
        (MakeRecord
           ({ id = 8303767780132907469L },
            [(Positional 0, Atomic (IntLiteral 1L));
             (Positional 1, Atomic (IntLiteral 2L))]))))"
 )
