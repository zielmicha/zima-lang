module rec Roboot.Universe
open Roboot.AST

type Universe = Map<GlobalId, Map<GlobalId, AST>>
