module Sanuki

open FSharpPlus

module Ast =
  type Literal<'label> =
    | This
    | Null
    | IntLiteral of int
    | StringLiteral of string
    | FloatLiteral of float
    | Label of 'label
    static member Map (x, f) =
      match x with
      | This -> This | Null -> Null
      | IntLiteral i -> IntLiteral i
      | StringLiteral s -> StringLiteral s
      | FloatLiteral f -> FloatLiteral f
      | Label l -> Label (f l)

  type With<'T, 'U> =
    { item: 'T; info: 'U }
    override x.ToString() = x.item.ToString()

  module With =
    let inline itemof x = x.item
    let inline infoof x = x.info
    let inline bind f x : With<_, _> = f x.item
    let inline map f x = { item = f x.item; info = x.info }
    let inline mapInfo f x = { item = x.item; info = f x.info }
    let inline sameInfoOf orig x = { item = x; info = orig.info }
    let inline info i x = { item = x; info = i }
    let inline noinfo x = { item = x; info = Unchecked.defaultof<_> }
   
  let (|With|) x = With (x.item, x.info)
  let (|Item|) x = Item x.item
  let (|Info|) x = Info x.info

  type Expr<'Label> =
    | Literal of Literal<'Label>
    | Var of name:string
  type ExprWithInfo<'Label, 'Info> = With<Expr<'Label>, 'Info>

  type Stmt<'Label, 'Info> =
    | DefineVar of ty:string * var:string * isPublic:bool * Literal<'Label>
    | DefineLabel of name:string * isPublic:bool
    | Call of funcName:string * args:ExprWithInfo<'Label, 'Info> list
    | Assign of var:string * ExprWithInfo<'Label, 'Info>
    | Goto of label:string
    | GotoIf of cond:ExprWithInfo<'Label, 'Info> * label:string
    | GotoIndirect of ExprWithInfo<'Label, 'Info>
    | Exit
  and StmtWithInfo<'Label, 'Info> = With<Stmt<'Label, 'Info>, 'Info>

  type ParsedProgram = StmtWithInfo<string, FParsec.Position> list

module Parser =
  open FParsec
  open Ast

  module Convert =
    let inline private foldi folder state xs =
      Seq.fold (fun (i, state) x -> (i + 1, folder i state x)) (0, state) xs |> snd
    let inline hexsToInt (hexs: #seq<char>) =
      let len = Seq.length hexs - 1
      hexs |> foldi (fun i sum x ->
        let n =
          let n = int x - int '0'
          if n < 10 then n
          else if n < 23 then n - 7
          else n - 44
        sum + n * pown 16 (len - i)) 0
    let inline digitsToInt (digits: #seq<char>) =
      let len = Seq.length digits - 1
      digits |> foldi (fun i sum x ->
        sum + (int x - int '0') * pown 10 (len - i)) 0

  module Utils =
    let inline ws x = x .>> spaces
    let inline ws1 x = x .>> spaces1
    let inline syn x = skipString x .>> spaces
    let inline syn1 x = skipString x .>> spaces1
    let inline pdict (d: seq<_*_>) =
      d |> Seq.map (fun (k, v) -> stringReturn k v) |> choice
    let inline pdictL (d: seq<_*_>) descr =
      d |> Seq.map (fun (k, v) -> stringReturn k v) |> choiceL <| descr
    let inline escapedString (escapedChars: seq<char>) =
      let escapedChars = escapedChars |> System.Collections.Generic.HashSet<char>
      let controls =
        pdictL [
          "\\b", '\b'; "\\t", '\t'; "\\n", '\n';
          "\\v", '\u000B'; "\\f", '\u000C'; "\\r", '\r'; "\\\\", '\\'
        ] "control characters"
      let unicode16bit =
        syn "\\u" >>? parray 4 hex |>> (Convert.hexsToInt >> char)
      let unicode32bit =
        syn "\\U" >>? parray 8 hex |>> (Convert.hexsToInt >> char)
      let customEscapedChars =
        let d = escapedChars |> Seq.map (fun c -> sprintf "\\%c" c, c)
        pdict d
      let escape = choice [controls; unicode16bit; unicode32bit; customEscapedChars]
      let nonEscape = satisfy (function '\\'|'\b'|'\t'|'\n'|'\u000B'|'\u000C' -> false
                                      | c -> escapedChars.Contains c |> not)
      let character = nonEscape <|> escape
      many character |>> (List.toArray >> System.String)
    let inline withPos (p: Parser<'a, _>) : Parser<With<'a, Position>, _> =
      fun stream ->
        let pos = stream.Position
        let reply = p stream
        if reply.Status = Ok then Reply(reply.Result |> With.info pos)
        else Reply (Error, reply.Error)
  open Utils

  let reserved =
    Set.ofList [
      "let"; "label"; "pub"; "call"; "set"; "exit"
      "goto"; "goto_if"; "goto_indirect"
      "this"; "null"
    ]
  let inline excludeReserved (i: Parser<string, unit>) : Parser<string, unit> =
    fun stream ->
      let state = stream.State
      let reply = i stream
      if reply.Status = Ok && not (reserved |> Set.contains reply.Result) then reply
      else stream.BacktrackTo state; Reply (Error, expected "non-reserved identifier")
  let pVarName = identifier (IdentifierOptions()) |> excludeReserved
  let pLabelName = identifier (IdentifierOptions(isAsciiIdStart = fun c -> c = '_' || isAsciiLetter c))

  let pIntLiteral = pint32 |>> IntLiteral
  let pFloatLiteral = pfloat |>> FloatLiteral
  let pStringLiteral = between (skipString "\"") (skipString "\"") (escapedString "\"") |>> StringLiteral
  let pThisLiteral = stringReturn "this" This
  let pNullLiteral = stringReturn "null" Null
  let pLabel = skipChar '@' >>. pLabelName
  let pLabelLiteral = pLabel |>> Label
  let pLiteral =
    pIntLiteral <|> pFloatLiteral <|> pStringLiteral
                <|> pThisLiteral  <|> pNullLiteral <|> pLabelLiteral
  let pLiteralExpr = pLiteral |>> Literal |> withPos
  let pVariableExpr = pVarName |>> Var |> withPos
  let pExpr = pLiteralExpr <|> pVariableExpr

  let pDefVarStmt =
    pipe3
      (syn "let" >>. (opt (syn "pub")) .>>. ws pVarName)
      (syn ":" >>. ws pVarName)
      (syn "=" >>. ws pLiteral)
      (fun (b, n) ty v -> DefineVar(ty, n, b.IsSome, v))
    |> withPos
  let pDefLabelStmt =
    syn "label" >>. (opt (syn "pub")) .>>. ws pLabel
    |>> (fun (b, l) -> DefineLabel (l, b.IsSome))
    |> withPos
  let pExternName = pVarName .>> skipChar '.' .>>. pLabelName |>> fun (s, t) -> sprintf "%s.%s" s t
  let pCallStmt =
    syn "call" >>. ws pExternName .>>. sepEndBy pExpr spaces1 |>> Call |> withPos
  let pAssignStmt =
    syn "set" >>. ws pVarName .>>. ws pExpr |>> Assign |> withPos
  let pGotoStmts =
    skipString "goto" >>. choice [
      syn "_if" >>. ws pExpr .>>. ws pLabel |>> GotoIf
      syn "_indirect" >>. ws pExpr |>> GotoIndirect
      spaces >>. ws pLabel |>> Goto
    ] |> withPos
  let pExitStmt = syn "exit" >>% Exit |> withPos

  let pStmt =
    pDefVarStmt <|> pDefLabelStmt <|> pCallStmt <|> pAssignStmt
                <|> pGotoStmts <|> pExitStmt

  let pProgram : Parser<ParsedProgram, _> = spaces >>. manyTill pStmt eof
  
  let parseString fileName str = runParserOnString pProgram () fileName str
  let parseFile fileName = runParserOnFile pProgram () fileName System.Text.Encoding.UTF8

let src = """
let returnAddr:SystemUInt32=0

label @func
  call Debug.Log "\"Hello, World!\n\""
  goto_indirect returnAddr

labelpub @_start
  let pub x : SystemBool = null
  goto_if x @next
  set returnAddr @next
  goto @func
  label @next
  exit

"""

let test() =
  src |> Parser.parseString "test" |> printfn "%A"

module Compiler =
  type [<Measure>] addr

  type VarTable<'Label> = Map<string, int<addr> * string * bool * Ast.Literal<'Label>>
  type LabelTable = Map<string, int<addr> * bool>
  
  [<RequireQualifiedAccess>]
  type Op =
    | Nop
    | Push of int<addr>
    | Pop
    | Jump of int<addr>
    | JumpIf of int<addr>
    | JumpIndirect of int<addr>
    | Extern of string
    | Copy

  type Assembly = VarTable<int<addr>> * Op list

  type AbstractOp =
    | Nop
    | Push of var:string
    | Pop
    | Label of name:string * isPublic:bool
    | Jump of label:string
    | JumpIf of label:string
    | JumpIndirect of var:string
    | Extern of string
    | Copy
    | Exit

  module ParsedProgram =
    open Ast

    // type Stmt<'Label, 'Info> =
    //   | DefineVar of ty:string * var:string * isPublic:bool * Literal<'Label>
    //   | DefineLabel of name:string * isPublic:bool
    //   | Call of funcName:string * args:ExprWithInfo<'Label, 'Info> list
    //   | Assign of var:string * ExprWithInfo<'Label, 'Info>
    //   | Goto of label:string
    //   | GotoIf of cond:ExprWithInfo<'Label, 'Info> * label:string
    //   | GotoIndirect of ExprWithInfo<'Label, 'Info>
    //   | Exit
    let toAbstractOp (p: ParsedProgram) : VarTable<string> * AbstractOp list =
      let gensym =
        let c = ref 0<addr>
        fun () ->
          let v = !c
          c := v + 1<addr>
          v
     
      let checkLiteralType ty l =
        match ty, l with
        | "SystemString", StringLiteral _
        | "SystemUInt32", Label _
        | ("SystemInt32" | "SystemUInt32" | "SystemSingle" | "SystemDouble"), IntLiteral _
        | ("SystemSingle" | "SystemDouble"), FloatLiteral _
        | _, (This | Null) -> true
        | _ -> false

      let requestLiteralVar =
        let mutable mapping : Map<string * Literal<_>, int> = Map.empty
        // TODO
        ()

      let rec go (vt: VarTable<string>) acc = function
        | [] -> vt, acc
        | With (x, pos) :: rest ->
          let vt, ops =
            match x with
            | DefineVar (ty, var, p, l) ->
              vt |> Map.add var (gensym(), ty, p, l), []
            | DefineLabel (n, p) -> vt, [AbstractOp.Label (n, p)]
            | Assign (v, l) ->
              match l with
              | Item (Var u) -> vt, [AbstractOp.Push u; AbstractOp.Push v; AbstractOp.Copy]
            | Goto l -> vt, [AbstractOp.Jump l]
            | Exit -> vt, [AbstractOp.Exit]
          go vt (acc @ ops) rest
      go Map.empty [] p

  module AbstractOp =
    let length (op: AbstractOp) =
      match op with
      | Label _ -> 0<addr>
      | Nop | Pop | Copy -> 1<addr>
      | Push _ | Jump _ | JumpIf _| JumpIndirect _ | Extern _ | Exit -> 5<addr>

    let getLabelAddress label ops =
      let rec go len ops =
        match ops with
        | Label (l, _) :: _ when l = label -> len
        | op :: ops -> go (len + length op) ops
        | [] -> failwith "label '%s' does not exist"
      go 0<addr> ops

    let createLabelTable ops : LabelTable =
      ops |> List.choose (function Label (l, p) -> Some (l, p) | _ -> None)
          |> List.groupBy id
          |> List.map (function ((l, p), [_]) -> l, (getLabelAddress l ops, p)
                              | ((l, _), _) -> failwithf "label '%s' is defined twice" l)
          |> Map.ofList

    let replaceLabelsInVarTable (labelTable: LabelTable) (varTable: VarTable<string>) : VarTable<int<addr>> =
      varTable |> Map.map (fun _ (addr, ty, pub, l) ->
        (addr, ty, pub, l |> map (fun x -> labelTable |> Map.find x |> fst)))

    let toOp (varTable: VarTable<int<addr>>) ops : Op list =
      let labelTable = createLabelTable ops
      let rec f = function
        | [] -> []
        | Nop :: xs -> Op.Nop :: f xs
        | Push var :: xs -> Op.Push (varTable |> Map.find var |> item1) :: f xs
        | Pop :: xs -> Op.Pop :: f xs
        | Label _ :: xs -> f xs
        | Jump label :: xs -> Op.Jump (labelTable |> Map.find label |> fst) :: f xs
        | JumpIf label :: xs -> Op.JumpIf (labelTable |> Map.find label |> fst) :: f xs
        | JumpIndirect var :: xs -> Op.JumpIndirect (varTable |> Map.find var |> item1) :: f xs
        | Extern s :: xs -> Op.Extern s :: f xs
        | Copy :: xs -> Op.Copy :: f xs
        | Exit :: xs -> Op.Jump 0xFFFFFF<addr> :: f xs
      f ops
