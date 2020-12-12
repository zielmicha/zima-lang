module rec Roboot.MakeAST

open Roboot.Tokenizer
open Roboot.AST

let syntaxError (token: Token) msg = failwith msg

let makeAttrs resolveName (tokens: list<TokenTree>): list<SymbolKey * TokenTree> =
    let rec aux (positionalIndex, tokens) =
        match tokens with
        | Token t :: rest when t.s.EndsWith(":") ->
            match rest with
            | [] -> syntaxError t "named argument name is last value of a composite"
            | value :: rest ->
                let keyId =
                    resolveName (t.s.Substring(0, t.s.Length - 1))

                Some((Named keyId, value), (positionalIndex, rest))
        | token :: rest ->
            match token with
            | Token t when t.s.StartsWith(":") ->
                let keyId = resolveName (t.s.Substring(1))

                let localIdToken =
                    Token { t with s = "." + t.s.Substring(1) }

                Some((Named keyId, localIdToken), (positionalIndex + 1, rest))
            | _ -> Some((Positional positionalIndex, token), (positionalIndex + 1, rest))
        | [] -> None

    List.unfold aux (0, tokens)

let wrapForSpecialOpeningBracket openingBracket node =
    match openingBracket with
    | "(" -> node
    | "`(" -> failwith "todo"
    | "~(" -> failwith "todo"
    | _ -> failwith "invalid bracket"

let makeComposite resolveName openingBracket children =
    let children =
        children
        |> List.filter (function
            | TokenTree.List _ -> true
            | Token t -> t.s <> ")")

    let attrs =
        makeAttrs resolveName children
        |> List.map (fun (k, child) -> (k, makeAST resolveName child))
    // TODO: nodeId?
    // TODO: nice error message for duplicate keys
    compositeToAst
        { Composite.nodeId = 0L
          attrs = Map.ofList attrs }

let unquoteString (s: string) =
    if s.Length < 2 then failwithf "invalid string %A" s
    // TODO: unescape characters
    s.Substring(1, s.Length - 2)

let makeAST resolveName (token: TokenTree): AST =
    match token with
    | TokenTree.List (openingBracket, children) -> makeComposite resolveName openingBracket children
    | Token t ->
        if t.s.StartsWith(".") then
            localIdToAst { LocalId.name = t.s.Substring(1) }
        elif t.s.StartsWith("\"") then
            literalStringToAst (unquoteString t.s)
        else
            let mutable i = -42L
            if System.Int64.TryParse(t.s, &i) then literalIntToAst i else globalIdToAst (resolveName t.s)

let makeASTFromString resolveName text: AST list =
    let tokens = tokenize text
    let trees = makeTrees tokens
    trees |> List.map (makeAST resolveName)

let makeOneASTFromString resolveName text: AST =
    match makeASTFromString resolveName text with
    | [ x ] -> x
    | _ -> failwithf "expected exactly one AST in %A" text