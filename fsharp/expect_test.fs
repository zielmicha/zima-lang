module ExpectTest

open System.Diagnostics
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

let quoteShell x =
    let quoted = x |> String.collect (fun c -> if c = '\'' then "'\"'\"'" else string c)
    "'" + quoted + "'"

let private stdout = ref None
let private corrected: ref<Map<string * int, string>> = ref Map.empty

let private containsStringClose (s: string) =
    s.Replace("\"\"", "").Contains("\"")

let private afterStringClose (s: string) =
    let loc = s.Replace("\"\"", "aa").IndexOf("\"")
    s.Substring(loc)

let private correctFile file corrections =
    let corrections = List.sort corrections |> ref
    let lines = System.IO.File.ReadAllLines file |> List.ofArray |> List.mapi (fun i x -> (i + 1, x)) |> ref
    let result = ref []

    //fprintfn stderr "correctFile: %s, %O\n" file !corrections

    while not (List.isEmpty !lines) do
        let lineNumber, line = List.head !lines
        lines := List.tail !lines
        if List.isEmpty !corrections || fst (List.head !corrections) <> lineNumber then
            result := line :: !result
        else
            let _, targetData = List.head !corrections
            corrections := List.tail !corrections
            let startStr = "T.expect @\""
            if not (line.Trim().StartsWith(startStr)) then
                failwith ("invalid T.expect line - " + line)
            let startRest = line.Trim().Substring(startStr.Length)
            let trailing = ref ""
            if containsStringClose startRest then
                trailing := afterStringClose startRest
            else
                while not (List.isEmpty !lines) && not (let _, line = List.head !lines in containsStringClose line) do
                    lines := List.tail !lines

                if List.isEmpty !lines then
                    failwith "unterminated T.expect string"

                let _, line = List.head !lines
                trailing := afterStringClose line
                lines := List.tail !lines

            let initialWhitespace = line.Substring(0, line.IndexOf(startStr))
            let targetData = (targetData : string).Trim().Replace("\"", "\"\"")
            result := (initialWhitespace + startStr + targetData + !trailing) :: !result

    let resultStr = String.concat "\n" (List.rev !result) + "\n"
    System.IO.File.WriteAllText(file + ".corrected", resultStr)
    fprintfn stderr "corrected: %s\n" file
    let args = ProcessStartInfo("patdiff")
    // TODO: doesn't work with spaces
    args.Arguments <- file + " " + file + ".corrected"
    ignore (Process.Start args)

let finish () =
    let files = Map.toSeq !corrected |> Seq.map (fun ((fn, linenum), data) -> (fn, (linenum, data))) |> Seq.groupBy fst
    // fprintf stderr "%O\n" !corrected
    for (filename, corrections) in files do
        correctFile filename (corrections |> Seq.map snd |> List.ofSeq)

let private initExpectTest () =
    let out = new System.IO.StringWriter()
    stdout := Some out
    System.Console.SetOut(out)

let private currentString () =
    match !stdout with
        | None -> failwith "expect test not initialized"
        | Some s -> s.ToString()

let private normalize (s: string) = s.Trim()

let failedExpectation result expected path line =
    let key = (path, line)
    if (Map.containsKey key !corrected) && (Map.find key !corrected <> result) then
        fprintf stderr "expectation yielded multiple results"
    corrected := !corrected |> Map.add key result

let expectTest f () =
    fprintfn stderr "running expect test"
    initExpectTest ()
    try
        f ()
    with
    | ex ->
      printf "<Exception>\n"
      fprintf stderr "Expect test resulted in an exception:\n"
      fprintf stderr "%O\n" ex

[<Sealed>]
type T() =
    static member expect(expected: string,
                         [<CallerFilePath; Optional; DefaultParameterValue("")>] path: string,
                         [<CallerLineNumber; Optional; DefaultParameterValue(0)>] line: int) =
        let result = currentString ()
        if normalize result <> normalize expected then
            failedExpectation result expected path line
        initExpectTest ()
