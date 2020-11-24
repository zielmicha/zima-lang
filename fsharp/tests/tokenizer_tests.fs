module Roboot.Tests.TokenizerTests

open ExpectTest
open Roboot.Tokenizer

let printTokens (text : string) =
    tokenize text |> List.iter (fun {Token.s = s; start = _; id = _} -> printf "%s\n" s)

let printTree (text : string) =
    tokenize text |> makeTrees |> printf "%A"

let f =
 expectTest (
    fun () ->
    printTokens "(foo bar)"
    T.expect @"(
foo
bar
)"

    printTokens "(foo \"foo\" bar xoo:123 a:b(5) \"123"
    T.expect @"(
foo
""foo""
bar
xoo:
123
a:
b
(
5
)
""123"

    printTree "()"
    T.expect @"[List (""("", [Token { s = "")""
                     start = Some 1
                     id = Some 2 }])]"

    printTree "(foo bar (1 2))"
    T.expect @"[List
   (""("",
    [Token { s = ""foo""
             start = Some 1
             id = Some 2 }; Token { s = ""bar""
                                    start = Some 5
                                    id = Some 4 };
     List
       (""("",
        [Token { s = ""1""
                 start = Some 10
                 id = Some 7 }; Token { s = ""2""
                                        start = Some 12
                                        id = Some 9 }; Token { s = "")""
                                                               start = Some 13
                                                               id = Some 10 }]);
     Token { s = "")""
             start = Some 14
             id = Some 11 }])]"
    )
