module rec Roboot.SimpleTextProgram

open Roboot.AST
open Roboot.MakeAST
open Roboot.Tokenizer
open Roboot.Universe
open System

type Header = { directives: list<list<string>> }

type Block =
    { id: GlobalId
      name: string
      data: string }

type SimpleTextProgram = { header: Header; blocks: list<Block> }

let trimAndRemoveEmpty x =
    x
    |> List.map (fun (s: string) -> s.Trim())
    |> List.filter (fun s -> (s <> ""))

let parseHeader (data: string) =
    let lines =
        data.Split('\n', StringSplitOptions.None)
        |> List.ofArray
        |> trimAndRemoveEmpty

    { Header.directives =
          lines
          |> List.map (fun (l: string) -> l.Split(' ') |> List.ofArray |> trimAndRemoveEmpty) }

let parseBlock (data: string) =
    let split = data.Split("\n")

    match Array.toList split with
    | id :: name :: rest ->
        let id =
            { GlobalId.id =
                  try
                      Int64.Parse(id)
                  with _ex -> failwithf "invalid block id %A" id }

        let rest = rest |> String.concat "\n"

        { Block.id = id
          name = name
          data = rest }
    | _ -> failwithf "block too short %A" data

let splitBlocks (data: string) =
    let blocks =
        data.Split("\n!!!", StringSplitOptions.None)
        |> List.ofArray
        |> trimAndRemoveEmpty

    eprintf "blocks %A\n" blocks

    match blocks with
    | headerText :: blocks ->
        let blocks = blocks |> List.map parseBlock
        let header = parseHeader headerText

        { SimpleTextProgram.header = header
          blocks = blocks }
    | [] ->
        { SimpleTextProgram.header = { directives = [] }
          blocks = [] }

let nameOnlyUniverse (programs: list<SimpleTextProgram>) =

    programs
    |> List.map (fun program -> program.blocks)
    |> List.concat
    |> List.map (fun block -> (block.id, Map.ofList [ UniverseIds.absolute_name, literalStringToAst block.name ]))
    |> Map.ofList

let resolveAbsoluteName name = Map.tryFind name (nameToId ())

let resolveName openNamespaces name =
    let names =
        (name
         :: (openNamespaces
             |> List.map (fun ns -> ns + "." + name)))

    match names
          |> List.choose (fun name -> resolveAbsoluteName name) with
    | id :: _ -> id
    | [] -> failwithf "failed to resolve %A in namespaces %A" name openNamespaces

let parseAsts (programs: list<SimpleTextProgram>) =
    let nameUniverse = nameOnlyUniverse programs

    withUniverse nameUniverse (fun () ->
        programs
        |> List.map (fun program ->
            let openNamespaces =
                (program.header.directives
                 |> List.choose (function
                     | [ "open"; ns ] -> Some ns
                     | _ -> None))

            program.blocks
            |> List.map (fun block ->
                let asts =
                    makeASTFromString (resolveName openNamespaces) block.data

                block.id, block.name, asts))
        |> List.concat)

let extractBodyId ast =
    match ast with
    | Composite c ->
        match splitHead c with
        | Some (GlobalId id, rest) -> id, compositeToAst rest
        | _ -> failwithf "for toplevel values, first argument needs to be a global id (%A)" ast
    | _ -> failwithf "toplevel values need to be composites, not %A" ast


let makeUniverse (programs: list<SimpleTextProgram>) =
    parseAsts programs
    |> List.map (fun (id, name, asts) ->
        id,
        (Map.ofList
            ([ UniverseIds.absolute_name, literalStringToAst name ]
             @ (asts |> List.map extractBodyId))))
    |> Map.ofList