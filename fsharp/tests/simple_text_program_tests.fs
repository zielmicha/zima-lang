module rec Roboot.Tests.SimpleTextProgramTests

open ExpectTest
open Roboot.AST
open Roboot.SimpleTextProgram

let example1 = @"
open my

!!!6382825602061754973
my.foo
(bar 5 my.bar: 10)

!!!9009097841047913115
my.bar
"

let f =
    expectTest (fun () ->
        printf "%A\n" (splitBlocks example1)

        let idToString (id: GlobalId) =
            match id.id with
            | 6382825602061754973L -> Some "foo"
            | 9009097841047913115L -> Some "bar"
            | _ -> None

        T.expect @"{ header = { directives = [[""open""; ""my""]] }
  blocks =
          [{ id = { id = 6382825602061754973L }
             name = ""my.foo""
             data = ""(bar 5 my.bar: 10)"" }; { id = { id = 9009097841047913115L }
                                              name = ""my.bar""
                                              data = """" }] }"


        printf "%A\n" (parseAsts [ splitBlocks example1 ])

        T.expect @"[({ id = 6382825602061754973L }, ""my.foo"",
  [AST (id.9009097841047913115 5 id.9009097841047913115:10)]);
 ({ id = 9009097841047913115L }, ""my.bar"", [])]"

        printf "%A\n"
            (makeUniverse [ splitBlocks example1 ]
             |> Map.map (fun k m -> m |> Map.map (fun k m -> astToString idToString m)))

        T.expect @"map
  [({ id = 6382825602061754973L },
    map
      [({ id = 7118174679217591314L }, """"my.foo"""");
       ({ id = 9009097841047913115L }, ""(5 bar:10)"")]);
   ({ id = 9009097841047913115L },
    map [({ id = 7118174679217591314L }, """"my.bar"""")])]")