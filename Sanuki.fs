module Sanuki

open FSharpPlus

module Ast =
  type Literal<'label> =
    | This
    | Null
    | IntLiteral of int
    | StringLiteral of string
    | FloatLiteral of float
    | Label of string

  type Expr<'Label, 'info> =
    | Literal of Literal<'Label> * 'info
    | Var of name:string * 'info

  module Expr =
    let info = function Literal (_, i) | Var (_, i) -> i
    let withInfo i = function
      | Literal (x, _) -> Literal (x, i)
      | Var (x, _) -> Var (x, i)  

  type Stmt<'Label, 'Expr> =
    | DefineVar of ty:string * var:string * isPublic:bool * Literal<'Label>
    | DefineLabel of name:string * isPublic:bool
    | Call of funcName:string * args:'Expr list
    | Assign of var:string * 'Expr
    | Goto of label:string
    | GotoIf of cond:'Expr * label:string
    | GotoIndirect of 'Expr
    | Exit

  type ParsedProgram = Stmt<string, Expr<string, FParsec.Position>> list

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
    let inline syn x = skipString x .>> spaces
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
    let inline withPos (f: Position -> 'a -> 'b) (p: Parser<'a, _>) : Parser<'b, _> =
      pipe2 getPosition p f
  open Utils

  let reserved =
    Set.ofList [
      "let"; "label"; "pub"; "call"; "set"; "exit"
      "goto"; "goto_if"; "goto_indirect"
      "this"; "null"
    ]

  let pIntLiteral = pint32 |>> IntLiteral
  let pFloatLiteral = pfloat |>> FloatLiteral
  let pStringLiteral = between (skipString "\"") (skipString "\"") (escapedString "\"") |>> StringLiteral
  let pThisLiteral = stringReturn "this" This
  let pNullLiteral = stringReturn "null" Null
  let pLiteral : Parser<Literal<string>, _> = pIntLiteral <|> pFloatLiteral <|> pStringLiteral <|> pThisLiteral <|> pNullLiteral
  let pLiteralExpr : Parser<_, unit> = pLiteral |> withPos (fun pos x -> Literal (x, pos))
  // TODO

module Compiler =
  type [<Measure>] addr

  type VarTable<'Label> = Map<string, int<addr> * string * string * bool * Ast.Literal<'Label>>
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

  module AbstractOp =
    let length (op: AbstractOp) =
      match op with
      | Label _ -> 0<addr>
      | Nop | Pop | Copy -> 1<addr>
      | Push _ | Jump _ | JumpIf _| JumpIndirect _ | Extern _ -> 5<addr>

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
      f ops
