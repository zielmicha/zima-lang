module Program

open System
open Roboot.AST
open Roboot.Toplevel
open Roboot.Universe
open Roboot.SimpleTextProgram

let loadFilesToUniverse files =
    let contents =
        files
        |> List.map (fun fn -> IO.File.ReadAllText(fn))
        |> List.map (fun content -> splitBlocks content)

    makeUniverse contents

let mainId =
    { GlobalId.id = 8821604172334482281L }, Ids.fun_

let run files =
    let universe = loadFilesToUniverse files

    withUniverse universe (fun () ->

        compileToplevelMethodRecursively mainId
        (callCompiledToplevel mainId) (dynobj Ids.args []))

let compile out files =
    let universe = loadFilesToUniverse files

    withUniverse universe (fun () ->
        let generator = new Lokad.ILPack.AssemblyGenerator()

        compileToplevelMethodRecursively mainId
        callCompiledToplevel mainId |> ignore

        generator.GenerateAssembly(zimaGeneratedAssembly, out))

[<EntryPoint>]
let main argv =
    match Array.toList argv with
    | "run" :: files ->
        run files
        0
    | "compile" :: out :: files ->
        compile out files
        0
    | _ -> failwithf "unsupported command %A" argv