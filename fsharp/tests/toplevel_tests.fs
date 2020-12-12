module rec Roboot.Tests.ToplevelTests

open ExpectTest
open Roboot.AST
open Roboot.Toplevel
open Roboot.Universe
open Roboot.SimpleTextProgram

let data = @"
open std

!!!8443262434344113238
std.fun

!!!5147849020646879983
test.f
(fun (.a) .a)
"

let f =
    expectTest (fun () ->
        let universe = makeUniverse [ splitBlocks data ]

        withUniverse universe (fun () ->
            let id =
                ({ GlobalId.id = 5147849020646879983L }, Ids.fun_)

            printf "%A\n" (toplevelExpandedAst id)

            T.expect @"({ name = ""__tmp_36"" },
 Match
   (LocalId { name = ""__tmp_36"" },
    [{ pattern =
                Record
                  ({ id = 8303767780132907469L },
                   [(Positional 0, Var { name = ""a"" })])
       body = LocalId { name = ""a"" } }]))"

            compileToplevelMethodRecursively id
            let f = callCompiledToplevel id

            let arg =
                dynobj Ids.args [ Positional 0, 5 :> obj ]

            printf "%A\n" (f arg)
            T.expect @"5"))