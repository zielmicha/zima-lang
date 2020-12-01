module rec Roboot.Tests.ToplevelTests
open ExpectTest
open Roboot.AST
open Roboot.Toplevel

let f =
 expectTest (
    fun () ->

    printf "foo\n"
    T.expect @"foo"

    )
