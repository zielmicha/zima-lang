
module Roboot.SimplifyANF
open Roboot.ExpandedAST
open Roboot.ANF

let mapIncr m k =
    match Map.tryFind k m with
    | Some x -> Map.add k (x + 1) m
    | None -> Map.add k 1 m

/// Remove unused joins and inline these used only once
let simplifyJoins v =
    let visited = ref Set.empty
    let numberOfUses = ref Map.empty

    let rec countUses env node =
        match (node : ANode) with
        | ANF.Let (_, _, body) -> countUses env body
        | PatternMatch (_, _, ifMatches, ifFails) -> countUses env ifMatches; countUses env ifFails
        | Return _ -> ()
        | JumpToJoin (joinName, _) ->
            numberOfUses := mapIncr !numberOfUses joinName
            if not (Set.contains joinName !visited) then
                visited := Set.add joinName !visited
                countUses env (Map.find joinName env)
        | LetJoin { AJoin.name = name; jumpBody = jumpBody; body = body; args = _ } ->
            let env = Map.add name jumpBody env
            countUses env body

    countUses Map.empty v

    let rec simplifyAux env node =
        match (node : ANode) with
        | Let (varName, value, body) ->
            Let (varName, value, simplifyAux env body)
        | PatternMatch (value, patternShape, ifMatches, ifFails) ->
            PatternMatch (value, patternShape, simplifyAux env ifMatches, simplifyAux env ifFails)
        | Return varName ->
            Return varName
        | JumpToJoin (joinName, args) ->
            if Map.find joinName !numberOfUses = 1 then
                let (targetArgs, targetBody) = Map.find joinName env
                assert (List.length targetArgs = List.length args)
                List.fold (fun acc (targetName, srcName) -> Let (targetName, Atomic (AtomicExpr.Id srcName), acc)) targetBody (List.zip targetArgs args)
            else
                JumpToJoin (joinName, args)
        | LetJoin { AJoin.name = name; jumpBody = jumpBody; body = body; args = args } ->
            if Map.find name !numberOfUses = 0 then
                simplifyAux env body
            else
                let jumpBody = simplifyAux env jumpBody
                let env = Map.add name (args, jumpBody) env

                if Map.find name !numberOfUses > 1 then
                    LetJoin { AJoin.name = name; jumpBody = jumpBody; body = simplifyAux env body; args = args }
                else
                    simplifyAux env body


    simplifyAux Map.empty v
