module rec Roboot.LambdaLifting
open Roboot.Common
open Roboot.AST
open Roboot.ExpandedAST
open Roboot.ANF

let lambdaContextId = { GlobalId.id = 6063950190264940280L }

let liftLambdasExpr (expr : AExpr) : (ANode * Set<AVarName>) =
    match expr with
    | LiftedLambda (_, _, _, _) ->
        failwithf "didn't expect lambda that was already lifted %A" expr
    | Lambda (argName, body) ->
        let (liftedBody, freeVars) = liftLambdasAux body
        let freeVars = freeVars.Remove(argName) |> Set.toList
        let contextArg = AVarName (newUniqId ())
        let shape : list<SymbolKey * AVarName> = freeVars |> List.indexed |> List.map (fun (i, v) -> (Positional i, v))
        let context = Atomic (MakeRecord (lambdaContextId, shape))
        let pattern = APatternShape.Record (lambdaContextId, shape)
        let newBody = PatternMatch (contextArg, pattern, body, returnExpr (Atomic RaiseUnexpectedVariant))
        anfLet context (fun context -> returnExpr (LiftedLambda (context, contextArg, argName, newBody))), Set.ofList freeVars
    | Atomic a ->
        let mutable freeVars = Set.empty
        let aux v =
            freeVars <- freeVars.Add(v)
            v
        atomicExprMap a aux |> ignore
        (returnExpr (Atomic a)), freeVars

// returns [node] with lambdas lifted and set of free variables
let rec liftLambdasAux (node : ANode) : (ANode * Set<AVarName>) =
    match node with
    | Let (varName, value, body) ->
        let (newValue, vars1) = liftLambdasExpr value
        let (newBody, vars2) = liftLambdasAux body
        let vars = vars1 + vars2.Remove(varName)
        withAllocVar (fun allocVar ->
            Let (varName, Atomic (Id (allocVar newValue)), newBody)), vars
    | PatternMatch (value, patternShape, ifMatches, ifFails) ->
        let (newIfMatches, vars1) = liftLambdasAux ifMatches
        let (newIfFails, vars2) = liftLambdasAux ifFails
        let vars = vars2 + (vars1 - (patternShapeVars patternShape |> Set.ofList))
        PatternMatch (value, patternShape, newIfMatches, newIfFails), vars
    | Return varName ->
        Return varName, Set.ofList [varName]
    | JumpToJoin (joinName, vars) ->
        JumpToJoin (joinName, vars), Set.ofList vars
    | LetJoin { AJoin.name = name; args = args; jumpBody = jumpBody; body = body } ->
        let (newJumpBody, vars1) = liftLambdasAux jumpBody
        let (newBody, vars2) = liftLambdasAux body
        let vars = (vars1 - Set.ofList args) + vars2
        LetJoin { AJoin.name = name; args = args; jumpBody = newJumpBody; body = newBody }, vars

let liftLambdas (node : ANode) =
    let node, freeVars = liftLambdasAux node
    // if Set.count freeVars <> 0 then
    //    failwithf "node contains free variables (%A, %A)" node freeVars
    node
