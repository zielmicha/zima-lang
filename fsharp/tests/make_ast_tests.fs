module Roboot.Tests.MakeAstTests

open ExpectTest
open Roboot.Tokenizer
open Roboot.AST
open Roboot.MakeAST

let f =
 expectTest (
    fun () ->

    let names = [
     "foo", { GlobalId.id = 123L };
     "bar", { GlobalId.id = 124L };
    ]

    let resolveName = let m = Map.ofList names in (fun k -> match Map.tryFind k m with Some g -> g | None -> failwithf "name not found %A" k)
    let idToString = let m = Map.ofList (List.map (fun (k, v) -> (v, k)) names) in (fun k -> Map.tryFind k m |> Option.map (fun name -> "std." + name))

    let printAst ast = astToString idToString ast |> printf "%A\n"
    let printAsts asts = asts |> List.iter printAst

    printAst (localIdToAst { LocalId.name = "foo" })
    printAst (literalIntToAst 123L)
    printAst (literalStringToAst "foobar")
    printAst (globalIdToAst { GlobalId.id = 123L })
    printAst (globalIdToAst { GlobalId.id = 66L })

    T.expect @""".foo""
""123""
""""foobar""""
""std.foo""
""id.66"""

    printAsts (makeASTFromString resolveName "(foo bar)")
    T.expect @"""(std.foo std.bar)"""

    printAsts (makeASTFromString resolveName "(foo (bar 1 2) foo: 3 bar: ())")
    T.expect @"""(std.foo (std.bar 1 2) std.foo:3 std.bar:())"""

    printAsts (makeASTFromString resolveName "(bar:1 foo: 2)")
    T.expect @"""(std.foo:2 std.bar:1)"""

    printAsts (makeASTFromString resolveName "(foo \"hello world\" .foo)")
    T.expect @"""(std.foo ""hello world"" .foo)"""
    )
