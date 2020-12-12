module Roboot.MakeAST

open Roboot.Tokenizer
open Roboot.AST

val makeASTFromString : (string -> GlobalId) -> string -> AST list
val makeOneASTFromString : (string -> GlobalId) -> string -> AST
