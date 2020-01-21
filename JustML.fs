module JustML
open UdonBase

[<StructuralEquality; StructuralComparison>]
type Range = { StartPos: Parsec.Position; EndPos: Parsec.Position }

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

type IFV<'a when 'a:comparison> =
  abstract member FV: unit -> Set<'a>
let inline fv (a: #IFV<'v>) = a.FV()

type list2<'a> = { item0:'a; item1:'a; itemN:'a list }

module List2 =
  let length xs = List.length xs.itemN + 2
  let create x1 x2 xn = { item0 = x1; item1 = x2; itemN = xn }
  let toList xs = xs.item0 :: xs.item1 :: xs.itemN
  let map f xs = { item0 = f xs.item0; item1 = f xs.item1; itemN = List.map f xs.itemN }
  let map2 f xs ys =
    { item0 = f xs.item0 ys.item0; item1 = f xs.item1 ys.item1; itemN = List.map2 f xs.itemN ys.itemN }

type MonoType =
  | TyBasic of string
  | TyPtr of MonoType
  | TyFun of arg:MonoType * ret:MonoType
  | TyPlaceholder of TypePlaceholder ref
  | TyUnit
  | TyTuple of MonoType list2
  | TyArray of MonoType
with
  interface IFV<string> with
    member this.FV() =
      let rec go = function
        | TyPlaceholder x -> fv (!x)
        | TyArray t | TyPtr t -> go t
        | TyFun (arg, ret) -> Set.unionMany [ yield go arg; yield go ret ]
        | TyUnit -> Set.empty
        | TyTuple ts -> ts |> List2.toList |> Seq.map go |> Set.unionMany
        | TyBasic _ -> Set.empty
      go this
and TypePlaceholder = TPVar of string | TPValue of MonoType
with
  interface IFV<string> with
    member this.FV() =
      match this with TPVar s -> set [s] | TPValue t -> fv t

module MonoType =
  let private alphabets = [|'a'..'z'|]
  let private genUniq ng =
    let a = ng % 26 in
    let p = ng / 26 in
    (new string [| for _ = 0 to p do yield alphabets.[a] |], ng + 1)
  let rec parseUdonType (udonType: string) =
    match udonType with
    | "SystemVoid" -> TyUnit
    | s when s.EndsWith "Array" ->
      s.Substring(0, s.Length-5) |> parseUdonType |> TyArray
    | s when s.EndsWith "Ref" ->
      s.Substring(0, s.Length-3) |> parseUdonType |> TyPtr
    | "T" -> TyPlaceholder (ref (TPVar "T"))
    | _ -> TyBasic udonType
  let genTypeVar =
    let mutable i = 0
    fun () ->
      let s, i' = genUniq i
      i <- i'
      TyPlaceholder (ref (TPVar ("_" + s)))
  let concat xs =
    match xs with
    | [] -> TyUnit
    | x :: [] -> x
    | x1 :: x2 :: xs -> TyTuple (List2.create x1 x2 xs)
  let rec foldFun builder args ret =
    match args with [] -> ret | a :: rest -> builder (a, ret |> foldFun builder rest)
  let rec normalize = function
    | TyPlaceholder x as t -> match !x with TPVar _ -> t | TPValue t -> normalize t
    | TyPtr t -> TyPtr (normalize t)
    | TyArray t -> TyArray (normalize t)
    | TyBasic s -> TyBasic s
    | TyFun (arg, ret) -> TyFun (normalize arg, normalize ret)
    | TyUnit -> TyUnit
    | TyTuple xs -> TyTuple (xs |> List2.map normalize)
  let rec pp = function
    | TyBasic s -> s
    | TyPtr t -> "ptr(" + pp t + ")"
    | TyArray t -> "array(" + pp t + ")"
    | TyUnit -> "unit"
    | TyTuple xs -> "(" + (xs |> List2.toList |> Seq.map pp |> String.concat ", ") + ")"
    | TyFun (arg, ret) -> pp arg + " => " + pp ret
    | TyPlaceholder x -> match !x with TPValue t -> pp t | TPVar s -> "'" + s
  let instantiate (mapping: Map<string, MonoType>) x =
    let rec go = function
      | TyPlaceholder v as t ->
        match !v with
        | TPValue x -> go x
        | TPVar s ->
          match mapping |> Map.tryFind s with
          | Some t -> t | None -> t
      | TyPtr t -> TyPtr (go t)
      | TyArray t -> TyArray (go t)
      | TyBasic s -> TyBasic s
      | TyFun (arg, ret) -> TyFun (go arg, go ret)
      | TyUnit -> TyUnit
      | TyTuple xs -> TyTuple (xs |> List2.map go)
    go x
  let instantiate1 tyvar ty x =
    let rec go = function
      | TyPlaceholder v as t ->
        match !v with
        | TPValue x -> go x
        | TPVar s -> if tyvar = s then ty else t
      | TyPtr t -> TyPtr (go t)
      | TyArray t -> TyArray (go t)
      | TyBasic s -> TyBasic s
      | TyFun (arg, ret) -> TyFun (go arg, go ret)
      | TyUnit -> TyUnit
      | TyTuple xs -> TyTuple (xs |> List2.map go)
    go x
  let union ty1 ty2 =
    let rec union ty1 ty2 =
      match ty1, ty2 with
      | _ when ty1 = ty2 -> ty1
      | TyPtr x, TyPtr y -> TyPtr (union x y)
      | TyArray x, TyArray y -> TyArray (union x y)
      | TyFun (arg1, ret1), TyFun (arg2, ret2) ->
        TyFun (union arg1 arg2, union ret1 ret2)
      | TyTuple xs, TyTuple ys when List2.length xs = List2.length ys ->
        TyTuple (List2.map2 union xs ys)
      | _, _ -> genTypeVar ()
    union (normalize ty1) (normalize ty2)

type Bit = B8 | B16 | B32 | B64
module Bit =
  let intType = function
    | B8 -> TyBasic "SystemSByte"
    | B16 -> TyBasic "SystemInt16"
    | B32 -> TyBasic "SystemInt32"
    | B64 -> TyBasic "SystemInt64"
  let uintType = function 
    | B8 -> TyBasic "SystemByte"
    | B16 -> TyBasic "SystemUInt16"
    | B32 -> TyBasic "SystemUInt32"
    | B64 -> TyBasic "SystemUInt64"
  let floatType = function
    | B32 -> TyBasic "SystemSingle"
    | B64 -> TyBasic "SystemDouble"
    | _ -> failwith "unsupported"

type ExternTrait =
  | TrOp1 of {| op:string; arg:MonoType; ty:MonoType |}
  | TrOp2 of {| op:string; lhs:MonoType; rhs:MonoType; ty:MonoType |}
  | TrStaticMember of {| moduleName:string; memberName:string; arg: MonoType; ty:MonoType |}
  | TrInstanceMember of {| instanceType:MonoType; memberName:string; arg: MonoType; ty:MonoType |}
  | TrItemGet of {| instanceType:MonoType; indexType:MonoType; returnType:MonoType |}
  | TrItemSet of {| instanceType:MonoType; indexType:MonoType; valueType:MonoType |}
  | TrCast of {| fromType:MonoType; destType:MonoType |}
with
  interface IFV<string> with
    member this.FV() =
      match this with
      | TrOp1 x -> Set.union (fv x.arg) (fv x.ty)
      | TrOp2 x -> Set.unionMany [fv x.lhs; fv x.rhs; fv x.ty]
      | TrStaticMember x -> Set.union (fv x.arg) (fv x.ty)
      | TrInstanceMember x -> Set.unionMany [fv x.instanceType; fv x.arg; fv x.ty]
      | TrItemGet x -> Set.unionMany [fv x.instanceType; fv x.indexType; fv x.returnType]
      | TrItemSet x -> Set.unionMany [fv x.instanceType; fv x.indexType; fv x.valueType]
      | TrCast x -> Set.union (fv x.fromType) (fv x.destType)
module ExternTrait =
  let toMonoType et =
    match et with
    | TrOp1 x -> TyFun (x.arg, x.ty)
    | TrOp2 x -> TyFun (MonoType.concat [x.lhs; x.rhs], x.ty)
    | TrStaticMember x -> TyFun (x.arg, x.ty)
    | TrInstanceMember x -> TyFun (x.instanceType, TyFun (x.arg, x.ty))
    | TrItemGet x -> TyFun (x.instanceType, TyFun (x.indexType, x.returnType))
    | TrItemSet x -> TyFun (x.instanceType, TyFun (MonoType.concat [x.indexType; x.valueType], TyUnit))
    | TrCast x -> TyFun (x.fromType, x.destType)
  let pp = function
    | TrOp1 x -> sprintf "(%s): %s => %s" x.op (MonoType.pp x.arg) (MonoType.pp x.ty)
    | TrOp2 x -> sprintf "(%s): (%s, %s) => %s" x.op (MonoType.pp x.lhs) (MonoType.pp x.rhs) (MonoType.pp x.ty)
    | TrStaticMember x -> sprintf "%s.%s: %s" x.moduleName x.memberName (MonoType.pp x.ty)
    | TrInstanceMember x -> sprintf "(this: %s).%s: %s" (MonoType.pp x.instanceType) x.memberName (MonoType.pp x.ty)
    | TrItemGet x ->
      sprintf "get (this: %s)[%s]: %s"
              (MonoType.pp x.instanceType) (MonoType.pp x.indexType) (MonoType.pp x.returnType)
    | TrItemSet x ->
      sprintf "set (this: %s)[%s]: %s"
              (MonoType.pp x.instanceType) (MonoType.pp x.indexType) (MonoType.pp x.valueType)
    | TrCast x -> sprintf "%s :> %s" (MonoType.pp x.fromType) (MonoType.pp x.destType)
  let mapType f = function
    | TrOp1 x -> TrOp1 {| x with arg = f x.arg; ty = f x.ty |}
    | TrOp2 x -> TrOp2 {| x with lhs = f x.lhs; rhs = f x.rhs; ty = f x.ty |}
    | TrStaticMember x -> TrStaticMember {| x with arg = f x.arg; ty = f x.ty |}
    | TrInstanceMember x -> TrInstanceMember {| x with instanceType = f x.instanceType; arg = f x.arg; ty = f x.ty |}
    | TrItemGet x -> TrItemGet {| instanceType = f x.instanceType; indexType = f x.indexType; returnType = f x.returnType |}
    | TrItemSet x -> TrItemSet {| instanceType = f x.instanceType; indexType = f x.indexType; valueType = f x.valueType |}
    | TrCast x -> TrCast {| fromType = f x.fromType; destType = f x.destType |}

type ExternWitness = {
  Info: ExternInfo<MonoType>
  TypeBindings: Map<string, MonoType>
}

module ExternWitness =
  let create (info: ExternInfo<_>) (tr: ExternTrait) : ExternWitness =
    // TODO:
    failwith "todo"

type PolyType = { TyVars: Set<string>; Type:MonoType }
with
  interface IFV<string> with
    member this.FV() =
      Set.difference (fv this.Type) this.TyVars
module PolyType =
  let inline ofMonoType t = { TyVars = Set.empty; Type = t }
  let descheme x =
    let vars =
      x.TyVars |> Seq.map (fun x -> x, MonoType.genTypeVar()) |> Map.ofSeq
    x.Type |> MonoType.instantiate vars
let (|MonoType|PolyType|) (x: PolyType) =
  if x.TyVars |> Set.isEmpty then MonoType x.Type
  else PolyType (x.TyVars, x.Type)

module ExternInfo =
  let toPolyType (ei: ExternInfo<MonoType>) : PolyType option =
    let tf isInstance args ret =
      let body =
        TyFun (
          MonoType.concat (List.ofArray args),
          match ret with None -> TyUnit | Some t -> t
        )
      if isInstance then TyFun (MonoType.parseUdonType ei.Namespace, body)
      else body
    match ei.Type with
    | StaticFunc   (args, ret) -> tf false args ret |> PolyType.ofMonoType |> Some
    | InstanceFunc (args, ret) -> tf true args ret |> PolyType.ofMonoType |> Some
    | StaticGenericFunc (prms, args, ret) ->
      { TyVars = Set.ofList prms; Type = tf false args ret } |> Some
    | InstanceGenericFunc (prms, args, ret) ->
      { TyVars = Set.ofList prms; Type = tf true args ret } |> Some
    | Constructor (args, ty) -> tf false args (Some ty) |> PolyType.ofMonoType |> Some
    | Unknown _ -> None

[<StructuralEquality; StructuralComparison>]
type JLiteral<'ty> =
  | LUnit
  | LInt of int64 * Bit
  | LUInt of uint64 * Bit
  | LFloat of float * Bit
  | LBool of bool
  | LString of string
  | LChar of char
  | LNull of 'ty
module JLiteral =
  let mapType f = function
    | LNull t -> LNull (f t)
    | LUnit -> LUnit
    | LInt (x, b) -> LInt (x, b)
    | LUInt (x, b) -> LUInt (x, b)
    | LFloat (x, b) -> LFloat (x, b)
    | LBool b -> LBool b
    | LString s -> LString s
    | LChar c -> LChar c
  let getType = function
    | LUnit -> TyUnit
    | LInt (_, b) -> Bit.intType b
    | LUInt (_, b) -> Bit.uintType b
    | LFloat (_, b) -> Bit.floatType b
    | LBool _ -> TyBasic "SystemBoolean"
    | LString _ -> TyBasic "SystemString"
    | LChar _ -> TyBasic "SystemChar"
    | LNull t -> t
  let canEmbed = function
    | LInt (_, B32) | LUInt (_, B32) | LFloat _ | LNull _ -> true
    | LString s ->
      s |> Seq.forall (fun c -> int c < 128 && not (System.Char.IsControl c) && c <> '"')
    | _ -> false

type Id = { name:string; index:int }
let Id (name, index) = { name = name; index = index }
let (|Id|) x = x.name, x.index

type Term<'info, 'external, 'annotation> =
  | TmLiteral of JLiteral<'annotation>
  | TmBoundVar of Id
  | TmBoundVarSet of {| name:Id; value:TermWithInfo<'info, 'external, 'annotation> |}
  | TmPtr of Id
  | TmCall of {| func:TermWithInfo<'info, 'external, 'annotation>; arg:TermWithInfo<'info, 'external, 'annotation> |}
  | TmLet of {| bindings: {| name:Id; isMut: bool; annotation:'annotation |} list; value:TermWithInfo<'info, 'external, 'annotation>; body:TermWithInfo<'info, 'external, 'annotation> |}
  | TmArray of TermWithInfo<'info, 'external, 'annotation> list
  | TmTuple of TermWithInfo<'info, 'external, 'annotation> list2
  | TmDo of {| lhs:TermWithInfo<'info, 'external, 'annotation>; rhs:TermWithInfo<'info, 'external, 'annotation> |}
  | TmLambda of {| args: {| name: Id; annotation:'annotation |} list; body: TermWithInfo<'info, 'external, 'annotation> |}
  | TmIf of {| cond:TermWithInfo<'info,'external,'annotation>; trueBranch:TermWithInfo<'info,'external,'annotation>; falseBranch:TermWithInfo<'info,'external,'annotation> option |}
  | TmFor of {| name:Id; startVal:TermWithInfo<'info,'external,'annotation>; endVal:TermWithInfo<'info,'external,'annotation>; down:bool; body:TermWithInfo<'info,'external,'annotation> |}
  | TmWhile of {| cond:TermWithInfo<'info,'external,'annotation>; body:TermWithInfo<'info,'external,'annotation> |}
  | TmExternal of 'external ref
  | TmUnsafe of UnsafeTerm<'info,'external,'annotation>
and UnsafeTerm<'info,'external,'annotation> =
  | UnsafeCast   of {| value:TermWithInfo<'info,'external,'annotation>; ty:MonoType |}
  | UnsafeExtern of {| fullName:string; args:TermWithInfo<'info,'external,'annotation> list |}
and TermWithInfo<'info, 'external, 'annotation> = With<Term<'info, 'external, 'annotation>, 'info>

module Term =
  let rec isValue = function
    | TmLiteral _ | TmBoundVar _ | TmLambda _ -> true
    | TmTuple xs -> xs |> List2.toList |> List.forall (With.itemof >> isValue)
    | TmArray xs -> xs |> List.forall (With.itemof >> isValue)
    | _ -> false
  let isValue' (t: TermWithInfo<_,_,_>) = t.item |> isValue

type ResolvedExternal<'info, 'annotation> = {
  Witness: ExternWitness
  Args: TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation> list
}

and UnresolvedExternal<'info, 'annotation> =
  /// static
  | UEStaticMember of {| moduleName:string; memberName:string; arg: TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation> |}
  /// instance, x.Foo
  | UEInstanceMember of {| instance:TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation>; name:string; arg: TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation> |}
  /// static, Type.op_Foo
  | UEOp1 of {| op:string; arg:TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation> |}
  | UEOp2 of {| lhs:TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation>; op:string; rhs:TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation> |}
  /// instance, x.get_Item / array, Get
  | UEItemGet of {| instance:TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation>; index:TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation> |}
  /// instance, x.set_Item / array, Set
  | UEItemSet of {| instance:TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation>; index:TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation>; value:TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation> |}
  /// Convert.ToFoo / op_Implicit
  | UECast of {| value:TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation>; ty:MonoType |}
  | UEResolved of ResolvedExternal<'info, 'annotation>

module UnresolvedExternal =
  let resolve (tr: ExternTrait) (info: ExternInfo<_>) (ur: UnresolvedExternal<_, _>) : ResolvedExternal<_, _> =
    let witness = ExternWitness.create info tr
    // TODO:
    failwith "todo"

type UntypedTermWI<'info>  = TermWithInfo<'info, UnresolvedExternal<'info, MonoType option>, MonoType option>
type TypedTermWI<'info>    = TermWithInfo<'info, UnresolvedExternal<'info, MonoType>, MonoType>
type EracedTermWI<'info>   = TermWithInfo<'info, unit, MonoType>

type SyncType =
  | NotSynced
  | Itself of method:string
  | Property of {| property:string; method:string |}

type TopLevel<'info, 'external, 'annotation, 'quantifier> =
  | TopVariable of {| name:string; isMut:bool; isExported:bool; syncType:SyncType; annotation:'annotation; value:TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation> |}
  | TopFunction of {| name:string; args: {| name:string; annotation:'annotation |} list; quantifier:'quantifier; body:TermWithInfo<'info, UnresolvedExternal<'info, 'annotation>, 'annotation> |}
  | TopEvent of {| name:string; body:TermWithInfo<'info, 'external, 'annotation> |}
type TopLevelWithInfo<'info, 'external, 'annotation, 'quantifier> =
  With<TopLevel<'info, 'external, 'annotation, 'quantifier>, 'info>

type Program<'info, 'external, 'annotation, 'quantifier> =
  TopLevelWithInfo<'info, 'external, 'annotation, 'quantifier> list
type UntypedProgram<'info> = Program<'info, UnresolvedExternal<'info, MonoType option>, MonoType option, unit>
type TypedProgram<'info> = Program<'info, UnresolvedExternal<'info, MonoType>, MonoType, Set<string>>
type EracedProgram<'info> = Program<'info, unit, MonoType, Set<string>>

type DeBruijnCtx<'key, 'value when 'key:comparison> = {
  Bindings: Map<'key, 'value>
  CurrentLevel: int
}
module DeBruijnCtx =
  let empty = { Bindings = Map.empty; CurrentLevel = 0 }
  let succ ctx = { ctx with CurrentLevel = ctx.CurrentLevel + 1 }
  let addToCurrentLevel (f: int -> 'k * 'v) ctx =
    let k, v = f ctx.CurrentLevel
    if ctx.Bindings |> Map.containsKey k then failwith "duplicate key"
    (ctx.CurrentLevel, k, v), { ctx with Bindings = ctx.Bindings |> Map.add k v }
  let add (f: int -> 'k * 'v) ctx =
    let k, v = f ctx.CurrentLevel
    if ctx.Bindings |> Map.containsKey k then failwith "duplicate key"
    (ctx.CurrentLevel, k, v), { ctx with Bindings = ctx.Bindings |> Map.add k v; CurrentLevel = ctx.CurrentLevel + 1 }
  let addManyToCurrentLevel xs (f: int -> 'a -> 'k * 'v) ctx =
    xs |> List.mapFold (fun state x -> 
      addToCurrentLevel (fun i -> f i x) state
    ) ctx
  let addMany xs (f: int -> 'a -> 'k * 'v) ctx =
    xs |> List.mapFold (fun state x -> 
      add (fun i -> f i x) state
    ) ctx
  let find key ctx = ctx.Bindings |> Map.find key
  let tryFind key ctx = ctx.Bindings |> Map.tryFind key
  let map f ctx = { Bindings = ctx.Bindings |> Map.map (fun _ -> f); CurrentLevel = ctx.CurrentLevel }
 
module Parser =
  type ParsedTerm =
    | PLiteral of JLiteral<MonoType option>
    | PVar of name:string
    /// x = t
    | PVarSet of {| name:string; value:ParsedTermWithInfo |}
    /// &x
    | PPtr of name:string
    /// f x y..
    | PCall of {| func:ParsedTermWithInfo; args:ParsedTermWithInfo list |}
    /// let x = t; u
    | PLet of {| bindings: {| name:string; isMut:bool; annotation: MonoType option |} list; value:ParsedTermWithInfo; body:ParsedTermWithInfo |}
    /// (t1,t2,t3)
    | PTuple of ParsedTermWithInfo list
    /// [t1,t2,t3]
    | PArray of ParsedTermWithInfo list
    /// t; u
    | PDo of {| lhs:ParsedTermWithInfo; rhs:ParsedTermWithInfo |}
    /// (..) => u
    | PLambda of {| args: {| name:string; annotation: MonoType option |} list; body:ParsedTermWithInfo |}
    /// +t
    | POp1 of {| op:string; arg:ParsedTermWithInfo |}
    /// t + u
    | POp2 of {| lhs:ParsedTermWithInfo; op:string; rhs:ParsedTermWithInfo |}
    /// new Type t
    | PNew of {| name:string; arg:ParsedTermWithInfo |}
    /// x.F t
    | PMemberCall of {| instance:ParsedTermWithInfo; name:string; arg:ParsedTermWithInfo |}
    /// x#A
    | PPropertyGet of {| instance:ParsedTermWithInfo; name:string |}
    /// x#A = t
    | PPropertySet of {| instance:ParsedTermWithInfo; name:string; value:ParsedTermWithInfo |}
    /// x[i]
    | PItemGet of {| instance:ParsedTermWithInfo; index:ParsedTermWithInfo |}
    /// x[i] = t
    | PItemSet of {| instance:ParsedTermWithInfo; index:ParsedTermWithInfo; value:ParsedTermWithInfo |}
    /// if (t) { u1 } else { u2 }
    | PIf of {| cond:ParsedTermWithInfo; trueBranch:ParsedTermWithInfo; falseBranch:ParsedTermWithInfo option |}
    /// for (x in t1 to t2) { u }
    | PFor of {| name:string; startVal:ParsedTermWithInfo; endVal:ParsedTermWithInfo; down:bool; body:ParsedTermWithInfo |}
    /// while (cond) { t }
    | PWhile of {| cond:ParsedTermWithInfo; body:ParsedTermWithInfo |}
    /// x :> t
    | PSafeCast of {| value:ParsedTermWithInfo; ty:MonoType |}
    /// x :?> t
    | PUnsafeCast of {| value:ParsedTermWithInfo; ty:MonoType |}
    /// extern UnityEngineDebug.__Log__SystemObject__SystemVoid(..)
    | PUnsafeExtern of {| fullName:string; args:ParsedTermWithInfo list |}
  and ParsedTermWithInfo = With<ParsedTerm, Range>

  type ParsedTopLevel =
    /// let x = t;
    | PTopLet of {| name:string; isMut: bool; syncType:SyncType; isExported:bool; annotation: MonoType option; value:ParsedTermWithInfo|}
    /// event _start = { t };
    | PEvent of {| name:string; body:ParsedTermWithInfo |}

  type ParsedProgram = With<ParsedTopLevel, Range> list

  type ParseErrorType =
    | DuplicateVar of string
    | UnknownVar of string
    | NotMutable of string
  exception ParseError of ParseErrorType * Range
  let inline internal err info t = ParseError (t, info) |> raise

  let rec getFirstDuplicate xs =
    let rec go s = function
      | [] -> None
      | x :: xs ->
        if s |> Set.contains x then Some x else go (s |> Set.add x) xs
    go Set.empty xs

  let internal conv (ctx: DeBruijnCtx<string, Id * bool>) (staticModules:Set<string>) (pt: ParsedTermWithInfo) : UntypedTermWI<Range> =
    let rec getMemberExternal ctx tm name arg =
      match tm.item with
      | PVar v when staticModules |> Set.contains v ->
        UEStaticMember {| moduleName = v; memberName = name; arg = conv ctx arg |}
      | _ ->
        UEInstanceMember {| instance = conv ctx tm; name = name; arg = conv ctx arg |}
    and conv ctx (With (x, info)) =
      With.info info <|
        match x with
        | PLiteral l -> TmLiteral l
        | PVar v ->
          match ctx |> DeBruijnCtx.tryFind v with
          | Some (id, _) -> TmBoundVar id
          | None -> UnknownVar v |> err info
        | PVarSet x ->
          match ctx |> DeBruijnCtx.tryFind x.name with
          | Some (id, true) -> TmBoundVarSet {| name = id; value = conv ctx x.value|}
          | Some (_, false) -> NotMutable x.name |> err info
          | None -> UnknownVar x.name |> err info
        | PPtr name ->
          match ctx |> DeBruijnCtx.tryFind name with
          | Some (id, true) -> TmPtr id
          | Some (_, false) -> NotMutable name |> err info
          | None -> UnknownVar name |> err info
        | PCall c ->
          let rec go f = function
            | [] -> failwith "impossible"
            | [x] -> TmCall {| func = f; arg = conv ctx x|}
            | x :: xs ->
              go (TmCall {| func = f; arg = conv ctx x |} |> With.info info) xs
          go (conv ctx c.func) c.args
        | PLet x ->
          match x.bindings |> List.map (fun x -> x.name) |> getFirstDuplicate with
          | Some v -> DuplicateVar v |> err info
          | None -> ()
          let ids, ctx' =
            ctx |> DeBruijnCtx.addManyToCurrentLevel x.bindings (fun i b -> b.name, ({name = b.name; index = i}, b.isMut))
          let ctx' = ctx' |> DeBruijnCtx.succ
          TmLet
            {| bindings = x.bindings |> List.map2 (fun (_, _, (id, _)) x -> {| x with name = id |}) ids
               value = conv ctx x.value
               body = conv ctx' x.body |}
        | PTuple xs ->
          match xs with
          | [] -> TmLiteral LUnit
          | x :: [] -> conv ctx x |> With.itemof
          | x1 :: x2 :: xs ->
            TmTuple (List2.create (conv ctx x1) (conv ctx x2) (List.map (conv ctx) xs))
        | PArray xs -> xs |> List.map (conv ctx) |> TmArray
        | PDo x -> TmDo {| lhs = conv ctx x.lhs; rhs = conv ctx x.rhs |}
        | PLambda x ->
          match x.args |> List.map (fun x -> x.name) |> getFirstDuplicate with
          | Some v -> DuplicateVar v |> err info
          | None -> ()
          let ids, ctx' =
            ctx |> DeBruijnCtx.addManyToCurrentLevel x.args (fun i b -> b.name, ({ name = b.name; index = i }, false))
          let ctx' = ctx' |> DeBruijnCtx.succ
          TmLambda
            {| args = x.args |> List.map2 (fun (_, _, (id, _)) x -> {| x with name = id |}) ids
               body = x.body |> conv ctx' |}
        | POp1 x -> UEOp1 {| op = x.op; arg = conv ctx x.arg |} |> ref |> TmExternal
        | POp2 x -> UEOp2 {| lhs = conv ctx x.lhs; op = x.op; rhs = conv ctx x.rhs |} |> ref |> TmExternal
        | PNew x ->
          UEStaticMember {| moduleName = x.name; memberName = "ctor"; arg = conv ctx x.arg |}
          |> ref |> TmExternal
        | PMemberCall x ->
          getMemberExternal ctx x.instance x.name x.arg |> ref |> TmExternal
        | PPropertyGet x ->
          getMemberExternal ctx x.instance (sprintf "get_%s" x.name) (PLiteral LUnit |> With.info info) |> ref |> TmExternal
        | PPropertySet x ->
          getMemberExternal ctx x.instance (sprintf "set_%s" x.name) x.value |> ref |> TmExternal
        | PItemGet x -> UEItemGet {| instance = x.instance |> conv ctx; index = x.index |> conv ctx |} |> ref |> TmExternal
        | PItemSet x -> UEItemSet {| instance = x.instance |> conv ctx; index = x.index |> conv ctx; value = x.value |> conv ctx |} |> ref |> TmExternal
        | PIf x -> TmIf {| cond = x.cond |> conv ctx; trueBranch = x.trueBranch |> conv ctx; falseBranch = x.falseBranch |> Option.map (conv ctx) |}
        | PFor x ->
          let (_, _, (id, _)), ctx' = ctx |> DeBruijnCtx.add (fun i -> x.name, ({ name = x.name; index = i}, false))
          TmFor {| x with name = id; body = x.body |> conv ctx'; startVal = x.startVal |> conv ctx; endVal = x.endVal |> conv ctx |}
        | PWhile x -> TmWhile {| cond = x.cond |> conv ctx; body = x.body |> conv ctx |}
        | PSafeCast x -> UECast {| x with value = x.value |> conv ctx |} |> ref |> TmExternal
        | PUnsafeCast x -> TmUnsafe <| UnsafeCast {| x with value = x.value |> conv ctx |}
        | PUnsafeExtern x -> TmUnsafe <| UnsafeExtern {| x with args = x.args |> List.map (conv ctx) |}
    conv ctx pt

  let toUntyped (staticModules: Set<string>) (pp: ParsedProgram) : UntypedProgram<Range> =
    pp |> List.mapFold (fun ctx p ->
      match p.item with
      | PTopLet x ->
        TopVariable {| x with value = x.value |> conv ctx staticModules |} |> With.sameInfoOf p,
        ctx |> DeBruijnCtx.addToCurrentLevel (fun i -> x.name, (Id (x.name, i), x.isMut)) |> snd
      | PEvent x -> TopEvent {| x with body = x.body |> conv ctx staticModules |} |> With.sameInfoOf p, ctx
    ) DeBruijnCtx.empty |> fst

module Typer =
  type TyperErrorType =
    | UnifyFailed of MonoType * MonoType
    | UnresolvableTraitCall of ExternTrait
    | UnknownVar of string
    | InvalidMutable of string 
    | Message of string
  exception TyperError of TyperErrorType * obj
  exception TyperErrors of (TyperErrorType * obj) list

  type Context = Map<Id, PolyType * bool>
  type ExternContext = {
    unaryOps: Map<string, ExternInfo<MonoType> list>
    binOps:   Map<string, ExternInfo<MonoType> list>
    staticMembers:   Map<string * string, ExternInfo<MonoType> list>
    instanceMembers: Map<string, ExternInfo<MonoType> list>
    getItems: ExternInfo<MonoType> list
    setItems: ExternInfo<MonoType> list
    casts:    ExternInfo<MonoType> list
  }

  type CEq<'info> = {| lhs: MonoType; rhs: MonoType; info:'info |}
  type CTrait<'info> = {| tr:ExternTrait; info:'info; ext: UnresolvedExternal<'info, MonoType> ref |}
  type Constraint<'info> =
    | CEq of CEq<'info>
    | CTrait of CTrait<'info>
  module Constraint =
    let mapType f = function
      | CEq e -> CEq {| e with lhs = f e.lhs; rhs = f e.rhs |}
      | CTrait t -> CTrait {| t with tr = t.tr |> ExternTrait.mapType f |}
    let mapInfo f = function
      | CEq e -> CEq {| e with info = f e.info |}
      | CTrait t -> CTrait {| t with info = f t.info |}
    let info = function CEq e -> e.info | CTrait t -> t.info
  let inline ceq info (x, y) = CEq {| lhs=x; rhs=y; info=info |}
  let inline ctrait info t ext = CTrait {| tr = t; info = info; ext = ext |}
  type Constraints<'info> = Constraint<'info> list
  module Constraints =
    let reduce xs =
      xs |> List.distinctBy (function CEq x -> Choice1Of2 (x.lhs, x.rhs) | CTrait x -> Choice2Of2 x.tr)

  let generalize (ctx: Context) ty =
    let fvs = ctx |> Map.toSeq |> Seq.map (snd >> fst >> fv) |> Set.unionMany
    let tvars =
      Set.difference (fv ty) fvs
    { TyVars = tvars; Type = ty }
  let generalizeAll ty = { TyVars = fv ty; Type = ty }

  let inline private err info t = TyperError(t, box info) |> raise
  let inline private errs xs = TyperErrors (xs |> List.map (fun (x, y) -> x, box y)) |> raise

  let unify (eCtx: ExternContext) (cs: Constraints<_>) =
    let rec f overwrite (cs: Constraints<_>) (ts: CTrait<_> list) = function
      | CEq e :: rest ->
        match e.lhs, e.rhs with
        | TyPlaceholder p, t | t, TyPlaceholder p ->
          match !p with
          | TPValue s -> f overwrite cs ts (CEq {| e with lhs = s; rhs = t |} :: rest)
          | TPVar v ->
            if fv t |> Set.contains v then
              UnifyFailed (TyPlaceholder p, t) |> err e.info
            else
              if overwrite then
                p := TPValue t; f overwrite cs ts rest
              else
                let ts =
                  ts |> List.map (fun x ->
                    {| x with tr = x.tr |> ExternTrait.mapType (MonoType.instantiate1 v t)|})
                let rest =
                  rest |> List.map (Constraint.mapType (MonoType.instantiate1 v t))
                f overwrite (ceq e.info (TyPlaceholder p, t) :: cs) ts rest
        | TyPtr s, TyPtr t | TyArray s, TyArray t ->
          f overwrite cs ts (CEq {| e with lhs = s; rhs = t |} :: rest)
        | TyTuple xs, TyTuple ys when List2.length xs = List2.length ys ->
          let cs =
            List2.map2 (fun x y -> CEq {| e with lhs = x; rhs = y |}) xs ys
            |> List2.toList
          cs @ rest |> f overwrite cs ts
        | TyFun (arg1, ret1), TyFun (arg2, ret2) ->
          CEq {| e with lhs = arg1; rhs = arg2 |} :: CEq {| e with lhs = ret1; rhs = ret2 |} :: rest |> f overwrite cs ts
        | s, t -> if s = t then f overwrite cs ts rest else UnifyFailed (s, t) |> err e.info
      | CTrait t :: rest -> f overwrite cs (t :: ts) rest
      | [] -> cs, g [] ts
    and f' b cs ts xs =
      try f b cs ts xs |> Some with _ -> None
    and g rs = function 
      | [] ->
        let rec go prev = function
          | [] -> prev 
          | (tr: CTrait<_>, overloads) :: rs ->
            let overloads' =
              overloads
              |> List.choose (function
                | (_, mt) as o ->
                  let cs = [ceq tr.info (tr.tr |> ExternTrait.toMonoType, mt)]
                  match f' false [] [] cs with 
                  | None -> None
                  | Some (cs, ts) -> Some (o, cs, ts)
              )
            match overloads' with
            | [] -> UnresolvableTraitCall tr.tr |> err tr.info
            | [(o, _), cs, ts] ->
              let r = UnresolvedExternal.resolve tr.tr o !tr.ext
              assert (f true [] [] cs = ([], []))
              tr.ext := UEResolved r
              go [] (ts @ prev @ rs)
            | xs ->
              go ((tr, xs |> List.map (fun (x, _, _) -> x)) :: prev) rs
        go [] rs
      | t :: ts ->
        let overloads =
          let os =
            match t.tr with
            | TrOp1 x -> eCtx.unaryOps |> Map.tryFind x.op |> Option.defaultValue []
            | TrOp2 x -> eCtx.binOps |> Map.tryFind x.op |> Option.defaultValue []
            | TrStaticMember x -> eCtx.staticMembers |> Map.tryFind (x.moduleName, x.memberName) |> Option.defaultValue []
            | TrInstanceMember x -> eCtx.instanceMembers |> Map.tryFind x.memberName |> Option.defaultValue []
            | TrItemGet _ -> eCtx.getItems | TrItemSet _ -> eCtx.setItems | TrCast _ -> eCtx.casts
          os |> List.choose (fun o ->
            match o |> ExternInfo.toPolyType with
            | Some t ->
              let mt = t |> PolyType.descheme
              Some (o, mt) | None -> None
          )
        let len = List.length overloads
        if len = 0 then UnresolvableTraitCall t.tr |> err t.info
        else if len = 1 then
          let _, mt = List.head overloads
          assert (f true [] [] [ceq t.info (mt, ExternTrait.toMonoType t.tr)] = ([], []))
          g rs ts
        else g ((t, overloads) :: rs) ts
    let cs, ts = f true [] [] cs
    assert (cs = [])
    ts |> List.map fst

  let rec recon (ctx: Context) (eCtx: ExternContext) (tm: UntypedTermWI<_>) : TypedTermWI<'a> * MonoType * Constraints<'a> =
    let tm', ty, cs =
      match tm.item with
      | TmLiteral l ->
        match l with
        | LUnit -> TmLiteral LUnit, TyUnit, []
        | LNull None ->
          let t = MonoType.genTypeVar()
          TmLiteral (LNull t), t, []
        | LNull (Some t) -> TmLiteral (LNull t), t, []
        | LString s ->
          let t = TyBasic "SystemString"
          TmLiteral (LString s), t, []
        | LBool b ->
          let t = TyBasic "SystemBoolean"
          TmLiteral (LBool b), t, []
        | LChar c ->
          let t = TyBasic "SystemChar"
          TmLiteral (LChar c), t, []
        | LInt (i, b) ->
          let t = Bit.intType b
          TmLiteral (LInt (i, b)), t, []
        | LUInt (i, b) ->
          let t = Bit.uintType b
          TmLiteral (LUInt (i, b)), t, []
        | LFloat (f, b) ->
          let t = Bit.floatType b
          TmLiteral (LFloat (f, b)), t, []
      | TmBoundVar id ->
        match ctx |> Map.tryFind id with
        | Some (pt, _) ->
          let t = PolyType.descheme pt
          TmBoundVar id, t, []
        | None -> UnknownVar id.name |> err tm.info
      | TmBoundVarSet x ->
        match ctx |> Map.tryFind x.name with
        | Some (MonoType t, true) ->
          let value', vt, cs = recon ctx eCtx x.value
          TmBoundVarSet {| x with value = value' |}, TyUnit,
          ceq tm.info (t, vt) :: cs
        | Some _ -> InvalidMutable x.name.name |> err tm.info
        | None -> UnknownVar x.name.name |> err tm.info
      | TmPtr id ->
        match ctx |> Map.tryFind id with
        | Some (MonoType t, true) ->
          let t = TyPtr t
          TmPtr id, t, []
        | Some _ -> InvalidMutable id.name |> err tm.info
        | None -> UnknownVar id.name |> err tm.info
      | TmArray xs ->
        let ty = MonoType.genTypeVar()
        let tms, vts, cs = multiRecon ctx eCtx xs
        let cs' =
          vts |> List.map (fun vt -> ceq tm.info (ty, vt))
        TmArray tms, TyArray ty, cs' @ cs
      | TmTuple xs ->
        let tms, vts, cs = multiRecon ctx eCtx (List2.toList xs)
        let l2 = function t1 :: t2 :: tn -> List2.create t1 t2 tn | _ -> failwith "impossible"
        let ty = TyTuple (l2 vts)
        TmTuple (l2 tms), ty, cs
      | TmLet x ->
        let vtm, vt, cs = recon ctx eCtx x.value
        let args, cs' =
          x.bindings |> List.mapFold (fun state x ->
            let t = MonoType.genTypeVar ()
            let state =
              match x.annotation with
              | Some u -> ceq tm.info (t, u) :: state | None -> state
            (x.name, (t, x.isMut)), state
          ) []
        let argTy = args |> List.map (snd >> fst) |> MonoType.concat
        let trs = unify eCtx (ceq tm.info (argTy, vt) :: cs' @ cs)
        let ctx' =
          if List.isEmpty trs && Term.isValue' vtm then
            args |> Map.ofList |> Map.map (fun _ (t, b) -> generalize ctx t, b)
          else
            args |> Map.ofList |> Map.map (fun _ (t, b) -> PolyType.ofMonoType t, b)
        let btm, bt, cs = recon ctx' eCtx x.body
        TmLet
          {| x with
              bindings = args |> List.map (fun (id, (t, isMut)) -> {| name = id; annotation = t; isMut = isMut |})
              value = vtm; body = btm
          |} 
        , bt, cs @ List.map CTrait trs
      | TmDo x ->
        let ltm, _, lcs = recon ctx eCtx x.lhs
        let rtm, rt, rcs = recon ctx eCtx x.rhs
        TmDo {| lhs = ltm; rhs = rtm |}, rt, lcs @ rcs
      | TmCall x ->
        let argTy, retTy = MonoType.genTypeVar(), MonoType.genTypeVar()
        let funTy = TyFun (argTy, retTy)
        let ftm, ft, fcs = recon ctx eCtx x.func
        let vtm, vt, vcs = recon ctx eCtx x.arg
        TmCall {| func = ftm; arg = vtm |},
        retTy,
        ceq tm.info (funTy, ft) :: ceq tm.info (argTy, vt) :: fcs @ vcs
      | TmFor x ->
        let intTy = TyBasic "SystemInt32"
        let stm, st, scs = recon ctx eCtx x.startVal
        let etm, et, ecs = recon ctx eCtx x.endVal
        let ctx' = ctx |> Map.add x.name (PolyType.ofMonoType intTy, false)
        let btm, bt, bcs = recon ctx' eCtx x.body
        TmFor {| x with startVal = stm; endVal = etm; body = btm |},
        TyUnit,
        ceq tm.info (st, intTy) :: ceq tm.info (et, intTy)
          :: ceq tm.info (bt, TyUnit) :: scs @ ecs @ bcs
      | TmWhile x ->
        let boolTy = TyBasic "SystemBoolean"
        let ctm, ct, ccs = recon ctx eCtx x.cond
        let btm, bt, bcs = recon ctx eCtx x.body
        TmWhile {| x with cond = ctm; body = btm |},
        TyUnit,
        ceq tm.info (ct, boolTy) :: ceq tm.info (bt, TyUnit) :: ccs @ bcs
      | TmIf x ->
        let boolTy = TyBasic "SystemBoolean"
        let ctm, ct, ccs = recon ctx eCtx x.cond
        let ttm, tt, tcs = recon ctx eCtx x.trueBranch
        match x.falseBranch with
        | Some fb ->
          let rt = MonoType.genTypeVar()
          let ftm, ft, fcs = recon ctx eCtx fb
          TmIf {| x with cond = ctm; trueBranch = ttm; falseBranch = Some ftm |},
          rt,
          ceq tm.info (ct, boolTy) :: ceq tm.info (rt, tt)
            :: ceq tm.info (rt, ft) :: ccs @ tcs @ fcs
        | None ->
          TmIf {| x with cond = ctm; trueBranch = ttm; falseBranch = None |},
          TyUnit,
          ceq tm.info (ct, boolTy) :: ceq tm.info (tt, TyUnit) :: ccs @ tcs
      | TmExternal e ->
        match !e with
        | UEStaticMember x ->
          let atm, at, cs = recon ctx eCtx x.arg
          let vt = MonoType.genTypeVar ()
          let ext = UEStaticMember {| x with arg = atm |} |> ref
          let tr = TrStaticMember {| x with arg = at; ty = vt |}
          TmExternal ext, vt, ctrait tm.info tr ext :: cs
        | UEInstanceMember x ->
          let atm, at, acs = recon ctx eCtx x.arg
          let itm, it, ics = recon ctx eCtx x.instance
          let vt = MonoType.genTypeVar ()
          let ext = UEInstanceMember {| x with arg = atm; instance = itm |} |> ref
          let tr = TrInstanceMember {| arg = at; instanceType = it; ty = vt; memberName = x.name |}
          TmExternal ext, vt, ctrait tm.info tr ext :: acs @ ics
        | UEOp1 x ->
          let atm, at, cs = recon ctx eCtx x.arg
          let rt = MonoType.genTypeVar ()
          let ext = UEOp1 {| x with arg = atm |} |> ref
          let tr = TrOp1 {| op = x.op; arg = at; ty = rt |}
          TmExternal ext, rt, ctrait tm.info tr ext :: cs
        | UEOp2 x ->
          let ltm, lt, lcs = recon ctx eCtx x.lhs
          let rtm, rt, rcs = recon ctx eCtx x.rhs
          let vt = MonoType.genTypeVar ()
          let ext = UEOp2 {| x with lhs = ltm; rhs = rtm |} |> ref
          let tr = TrOp2 {| op = x.op; lhs = lt; rhs = rt; ty = vt |}
          TmExternal ext, vt, ctrait tm.info tr ext :: lcs @ rcs
        | UEItemGet x ->
          let ttm, tt, tcs = recon ctx eCtx x.instance
          let itm, it, ics = recon ctx eCtx x.index
          let vt = MonoType.genTypeVar ()
          let ext = UEItemGet {| instance = ttm; index = itm |} |> ref
          let tr = TrItemGet {| instanceType = tt; indexType = it; returnType = vt |}
          TmExternal ext, vt, ctrait tm.info tr ext :: tcs @ ics
        | UEItemSet x ->
          let ttm, tt, tcs = recon ctx eCtx x.instance
          let itm, it, ics = recon ctx eCtx x.index
          let atm, at, acs = recon ctx eCtx x.value
          let vt = MonoType.genTypeVar ()
          let ext = UEItemSet {| instance = ttm; index = itm; value = atm |} |> ref
          let tr = TrItemSet {| instanceType = tt; indexType = it; valueType = at |}
          TmExternal ext, vt, ctrait tm.info tr ext :: tcs @ ics @ acs
        | UECast x ->
          let atm, at, acs = recon ctx eCtx x.value
          let ext = UECast {| x with value = atm |} |> ref
          let tr = TrCast {| fromType = at; destType = x.ty |} 
          TmExternal ext, x.ty, ctrait tm.info tr ext :: acs
        | UEResolved _ -> failwith "undefined"
      | _ -> failwith ""
    tm' |> With.sameInfoOf tm, ty, cs
  and multiRecon ctx eCtx tms =
    tms |> List.map (recon ctx eCtx)
        |> List.foldBack (fun (t, vt, cs) (ts, vts, css) -> t :: ts, vt :: vts, cs @ css) <| ([],[],[])