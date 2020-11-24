module rec Roboot.Tokenizer
open System.Text.RegularExpressions

let ident_re = @"[a-zA-Z0-9_.-]+"

let regexes = [
    @"\(";
    @"\)";
    @"`\(";
    @"~\(";
    "\"(([^\\\\\"]|\\\\.)*)(\"|$)";
    @":" + ident_re;
    @"`" + ident_re;
    @"$" + ident_re;
    @"~" + ident_re;
    ident_re + @":";
    ident_re;
    @"[ \t\n\r]+";
]

let fullRegex = (regexes |> List.map (fun r -> sprintf "\\G(%s)" r) |> String.concat "|")
let tokenRegex = new Regex(fullRegex, RegexOptions.Compiled ||| RegexOptions.Singleline)

type Token = {
    s: string
    start: int option
    id: int option
}


let tokenize (text : string) =
    let mutable pos = 0
    let tokens = new System.Collections.Generic.List<Token>()
    let mutable id = 1

    while pos < text.Length do
        let m = tokenRegex.Match(text, pos)

        let endpos =
            if m.Success then
                assert (m.Index = pos)
                assert (m.Length > 0)
                pos + m.Length
            else
                // we need to tokenize no matter what - treat unsupported characters as single tokens
                pos + 1

        let s = text.Substring(pos, endpos - pos)

        if s.Trim() <> "" then
            tokens.Add({Token.s = s; start = Some pos; id = Some id})

        id <- id + 1
        pos <- endpos

    tokens |> List.ofSeq

type TokenTree =
    | List of openingBracket:string * children:TokenTree list
    | Token of Token

let makeTreeAux tokens =
    let currentLevel = new System.Collections.Generic.List<TokenTree>()

    let rec iterTokens tokens =
        match tokens with
        | [] ->
            // should we insert synthetic closing parens here?
            // currentLevel.Add(Token { s = ")"; start = None; id = None })
            tokens
        | token :: rest ->
            match token.s with
            | ")" ->
                currentLevel.Add(Token token)
                rest
            | "(" | "`(" | "~(" ->
                let children, rest = makeTreeAux rest
                currentLevel.Add(TokenTree.List (token.s, children))
                iterTokens rest
            | _ ->
                currentLevel.Add(Token token)
                iterTokens rest

    let rest = iterTokens tokens
    (List.ofSeq currentLevel), rest

let makeTrees tokens =
    let aux tokens =
        match tokens with
        | [] -> None
        | _ ->
            let children, rest = makeTreeAux tokens
            Some (children, rest)

    // repeatedly parse trees to ignore all stray ')' tokens
    List.unfold aux tokens |> List.concat
