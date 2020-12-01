module rec Roboot.Builtins
#nowarn "58"

open Roboot.ExpandedAST
open Roboot.AST

let integerBinaryOps = ([
    IntegerBinaryOp.Add, { GlobalId.id = 8687139141449869645L };
    IntegerBinaryOp.Add_ovf, { GlobalId.id = 6265646249296500781L };
    IntegerBinaryOp.Add_ovf_un, { GlobalId.id = 6827639586345834897L };
    IntegerBinaryOp.And, { GlobalId.id = 8705286050777354476L };
    IntegerBinaryOp.Div, { GlobalId.id = 5618335103903433419L };
    IntegerBinaryOp.Div_un, { GlobalId.id = 9154027216755091828L };
    IntegerBinaryOp.Eq, { GlobalId.id = 6349950489050485728L };
    IntegerBinaryOp.Gt, { GlobalId.id = 7188816003197141461L };
    IntegerBinaryOp.Gt_un, { GlobalId.id = 7535522072621967003L };
    IntegerBinaryOp.Lt, { GlobalId.id = 8690624497727397903L };
    IntegerBinaryOp.Lt_un, { GlobalId.id = 6436439928902135774L };
    IntegerBinaryOp.Mul, { GlobalId.id = 6619288643789458946L };
    IntegerBinaryOp.Mul_ovf, { GlobalId.id = 8271782702849628166L };
    IntegerBinaryOp.Mul_ovf_un, { GlobalId.id = 5689944807723357891L };
    IntegerBinaryOp.Or, { GlobalId.id = 8913386557431646220L };
    IntegerBinaryOp.Rem, { GlobalId.id = 8988295406793695224L };
    IntegerBinaryOp.Rem_un, { GlobalId.id = 6798262582204179775L };
    IntegerBinaryOp.Shl, { GlobalId.id = 8758977060366776004L };
    IntegerBinaryOp.Shr_un, { GlobalId.id = 5736540685374516898L };
    IntegerBinaryOp.Sub, { GlobalId.id = 9099529457660703302L };
    IntegerBinaryOp.Sub_ovf, { GlobalId.id = 7153691739105988676L };
    IntegerBinaryOp.Xor, { GlobalId.id = 5850943493260172943L }
]|> List.map (fun (op, id) -> (id, op)) |> Map.ofList)

let integerSizes = ([
    IntegerSize.I32, { GlobalId.id = 6045426955361681942L };
    IntegerSize.I64, { GlobalId.id = 8471708282667706810L };
    IntegerSize.U32, { GlobalId.id = 4997300403487477942L };
    IntegerSize.U64, { GlobalId.id = 8971983968226176129L };
] |> List.map (fun (op, id) -> (id, op)) |> Map.ofList)

let expandIntegerOp recur args =
    match args with
    | OnlyPositional [GlobalId op; GlobalId size; a; b] ->
        let op = Map.find op integerBinaryOps
        let size = Map.find size integerSizes
        Atomic (IntegerBinaryOp (op, size, recur a, recur b))
    | _ -> failwithf "invalid arguments (%A)" args

let buildRecord recur kind (args : Composite) =
    Atomic (MakeRecord (kind, (args.attrs |> Map.toList |> List.map (fun (key, value) -> (key, recur value)))))

let buildArgs recur args =
    buildRecord recur Ids.args args

let expandCallLocal recur args =
    match splitHead args with
    | Some (head, args) ->
        Atomic (CallLocal (recur head, buildArgs recur args))
    | None -> failwithf "call expects at least one argument (%A)" args

let expandDefaultCall recur headId (args : Composite) =
    Atomic (CallGlobal (headId, Ids.fun_, buildArgs recur args))

let expandRecord recur args =
    match splitHead args with
    | Some (head, args) ->
        match head with
        | GlobalId id ->
            buildRecord recur id args
        | _ -> failwithf "first argument of record should be a global ID (%A)" args
    | None -> failwithf "record expects at least one argument (%A)" args

let expandWhile (recur : (AST -> ExpandedAST)) (args : Composite) =
    match args with
    | OnlyPositional [cond; body] ->
        let name = genLocalId ()
        (B.c [
            B.id Ids.loop_;
            B.l name;
            B.c [
                B.id Ids.if_;
                cond;
                B.c [
                    B.id Ids.do_;
                    body;
                    B.c [ B.id Ids.loop_continue; B.l name ]
                ];
                B.id Ids.unit
            ]
        ]) |> recur
    | _ -> failwithf "invalid while arguments (%A)" args

let expandUnit recur args =
    Atomic (MakeRecord (Ids.unit, []))

let builtins : (Map<GlobalId, (AST -> ExpandedAST) -> Composite -> ExpandedAST>) = ([
    { GlobalId.id = 4828890982335511950L }, expandIntegerOp
    { GlobalId.id = 8752846209867014520L }, expandCallLocal
    { GlobalId.id = 5171142284694163194L }, expandRecord
    { GlobalId.id = 4689525846744222724L }, expandWhile
    Ids.unit, expandUnit
] |> Map.ofList)
