module rec Roboot.Macroexpand
open Roboot.AST
open Roboot.ExpandedAST
open Roboot.ContextUtil
open Roboot.Universe

let expandBuiltinComposite recur head rest =
    match Map.tryFind head Roboot.Builtins.builtins with
    | Some f ->
        f recur rest
    | None ->
        match Map.tryFind head otherBuiltins with
        | Some f ->
            f recur rest
        | None ->
            Roboot.Builtins.expandDefaultCall recur head rest

let expandBuiltin recur (ast : AST) =
    match ast with
    | AST.Composite c ->
        match splitHead c with
        | Some (head, rest) ->
            match head with
            | GlobalId headId ->
                expandBuiltinComposite recur headId rest
            | _ ->
                failwithf "when compiling composite, first argument needs to be a global id (%A)" ast
        | None ->
            failwithf "cannot compile empty composite (%A)" ast
    | AST.LocalId name ->
        ExpandedAST.LocalId name
    | AST.GlobalId id ->
        failwithf "global IDs are not compliable on their own (%A)" ast
    | AST.LiteralInt i ->
        Atomic (IntLiteral i)
    | AST.LiteralString s ->
        Atomic (StringLiteral s)
    | _ ->
        failwithf "unknown AST type (%A)" ast

let ignoreVar = { LocalId.name = "_" }

let wrapInDo (l : list<AST>) =
    compositeOfList unknownNodeId ([Ids.do_ |> globalIdToAst] @ l) |> compositeToAst

let expandDoLet recur args rest =
    match args with
    | OnlyPositional [name; value] ->
        let n = compositeOfList args.nodeId [
            Ids.match_ |> globalIdToAst;
            value;
            compositeOfList args.nodeId [
                compositeOfList args.nodeId [ name; wrapInDo rest ] |> compositeToAst
            ] |> compositeToAst ]
        recur (compositeToAst n)
    // | OnlyPositional [name, type, value]
    | _ ->
        failwithf "invalid let syntax (%A)" args

let builtinDoMacros : ((Map<GlobalId, (AST -> ExpandedAST) -> Composite -> AST list -> ExpandedAST>)) = ([
    { GlobalId.id = 7418612383998351871L }, expandDoLet
] |> Map.ofList)

let expandDoStmt recur stmt rest =
    match stmt with
    | AST.Composite c ->
        match splitHead c with
        | Some (GlobalId head, args) ->
            match builtinDoMacros |> Map.tryFind head with
            | Some f -> Some (f recur args rest)
            | None -> None
        | _ -> None
    | _ -> None

let expandDo recur (stmts : list<AST>) =
    match stmts with
    | [] -> Atomic (MakeRecord (Ids.unit, []))
    | [stmt] ->
        recur stmt
    | stmt :: rest ->
        match expandDoStmt recur stmt rest with
        | Some r -> r
        | None ->
            let rest = expandDo recur rest
            ExpandedAST.Match (recur stmt, [{MatchCase.pattern = Var ignoreVar; body = rest}])

let expandDoAst recur ast =
    match ast with
    | OnlyPositional stmts ->
        expandDo recur stmts
    | _ -> failwithf "do expects only positional arguments %A" ast

let makeEqualityPattern recur value =
    let varName = genLocalId ()
    MatchMap (varName,
              compositeOfList unknownNodeId [ globalIdToAst Ids.equal; localIdToAst varName; value ] |> compositeToAst |> recur,
              compositeOfList unknownNodeId [ globalIdToAst Ids.true_ ] |> compositeToAst |> expandPattern recur)

let expandPattern recur pattern =
    // TODO: support pattern-macros
    match pattern with
    | AST.Composite c ->
        match splitHead c with
        | Some (head, rest) ->
            match head with
            | GlobalId headId ->
                let attrs = (rest.attrs |> Map.toList |> List.map (fun (key, value) -> (key, expandPattern recur value)))
                MatchPattern.Record (headId, attrs)
            | _ ->
                failwithf "when matching against composite, first item should be a global id (%A)" pattern
        | None ->
            failwithf "cannot match against empty composite (%A)" pattern
    | AST.LocalId name ->
        MatchPattern.Var name
    | AST.GlobalId id ->
        failwithf "global IDs are not supported in patterns on their own (%A)" pattern
    | AST.LiteralInt _
    | AST.LiteralString _ ->
        makeEqualityPattern recur pattern
    | _ ->
        failwithf "unknown AST type (%A)" pattern

let expandCase recur ast =
    match ast with
    | Composite (OnlyPositional (pattern :: stmts)) ->
        { MatchCase.pattern = expandPattern recur pattern; body = expandDo recur stmts }
    | _ -> failwithf "invalid case syntax %A" ast

let expandMatch recur ast =
    match ast with
    | OnlyPositional l ->
        match l with
        | value :: cases ->
            Match (recur value, cases |> List.map (expandCase recur))
        | _ ->
            failwithf "match expects at least two arguments %A" ast
    | _ -> failwithf "match expects only positional arguments %A" ast

let otherBuiltins : ((Map<GlobalId, (AST -> ExpandedAST) -> Composite -> ExpandedAST>)) = ([
    Ids.do_, expandDoAst
    Ids.match_, expandMatch
] |> Map.ofList)
