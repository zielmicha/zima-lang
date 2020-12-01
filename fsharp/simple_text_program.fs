module rec Roboot.SimpleTextProgram

type Header = {
    directives: list<list<string>>
}

type Block = {
    id: GlobalId
    data: string
}

type SimpleTextProgram = {
    header: Header
    blocks: list<Block>
}

let parseHeader (data : string) =
    let lines = data.Split('\n', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries) |> List.ofArray
    { Header.directives = lines |> List.map (fun (l : string) -> l.Split(' ', StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)) }

let parseBlock (data : string) =
    let pos = data.IndexOf('\n')
    if pos == -1 then failwithf "empty block %A" data

    let id = Int64.Parse(data.Substring(0, pos))
    let rest = data.Substring(pos)
    { Block.id = id; data = rest }

let splitBlocks (data : string) =
    let blocks = data.Split("\n!!!", StringSplitOptions.None) |> List.ofArray parseBlock
    let header = parseHeader (blocks.(0))
    { SimpleTextProgram.header = header; blocks
