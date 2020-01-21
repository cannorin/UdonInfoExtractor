module UdonBase

type ExternType<'a> =
  | StaticFunc of args:'a[] * ret:'a option
  | StaticGenericFunc of typrm:string list * args:'a[] * ret:'a option
  | InstanceFunc of args:'a[] * ret:'a option
  | InstanceGenericFunc of typrm:string list * args:'a[] * ret:'a option
  | Constructor of args:'a[] * ty:'a
  | Unknown of arity:int * argret:'a[][]

module ExternType =
  let map f = function
    | StaticFunc (args, ret) -> StaticFunc (args |> Array.map f, ret |> Option.map f)
    | InstanceFunc (args, ret) -> InstanceFunc (args |> Array.map f, ret |> Option.map f)
    | StaticGenericFunc (typrm, args, ret) -> StaticGenericFunc (typrm, args |> Array.map f, ret |> Option.map f)
    | InstanceGenericFunc (typrm, args, ret) -> InstanceGenericFunc (typrm, args |> Array.map f, ret |> Option.map f)
    | Constructor (args, ty) -> Constructor (args |> Array.map f, f ty)
    | Unknown (arity, argret) -> Unknown (arity, argret |> Array.map (Array.map f))
  let parseSignature (name: string) (argret: string[][]) (arity: int) =
    let StaticVoidRetArgFunc = StaticFunc (Array.empty, None)
    let inline StaticVoidRetFunc xs = StaticFunc (xs, None)
    let inline StaticVoidArgFunc x  = StaticFunc (Array.empty, Some x)
    let InstanceVoidRetArgFunc = InstanceFunc (Array.empty, None)
    let inline InstanceVoidRetFunc xs = InstanceFunc (xs, None)
    let inline InstanceVoidArgFunc x  = InstanceFunc (Array.empty, Some x)

    match name, argret with
    | "ctor", [| xs; [|ret|] |] when xs.Length + 1 = arity -> Constructor (xs, ret)
    | _, [|[|"SystemVoid"|]|] ->
      if arity = 0 then StaticVoidRetArgFunc
      else if arity = 1 then InstanceVoidRetArgFunc
      else Unknown (arity, argret)
    | _, [|[|("T" | "TArray") as ret|]|] ->
      if arity = 2 then StaticGenericFunc(["T"], [||], Some ret)
      else if arity = 3 then InstanceGenericFunc(["T"], [||], Some ret)
      else Unknown (arity, argret)
    | _, [|[|ret|]|] ->
      if arity = 1 then StaticVoidArgFunc ret
      else if arity = 2 then InstanceVoidArgFunc ret
      else Unknown (arity, argret)
    | _, [| args; [|"SystemVoid"|] |] ->
      if arity = args.Length then StaticVoidRetFunc args
      else if arity = args.Length + 1 then InstanceVoidRetFunc args
      else Unknown (arity, argret)
    | _, [| args; [| ("T" | "TArray") as ret |] |] ->
      if arity = args.Length + 2 then StaticGenericFunc (["T"], args, Some ret)
      else if arity = args.Length + 3 then InstanceGenericFunc (["T"], args, Some ret)
      else Unknown (arity, argret)
    | _, [| args; [|ret|] |] ->
      if arity = args.Length + 1 then StaticFunc (args, Some ret)
      else if arity = args.Length + 2 then InstanceFunc (args, Some ret)
      else Unknown (arity, argret)
    | _ -> Unknown (arity, argret)

type ExternInfo<'a> = { Namespace: string; Name:string; Type: ExternType<'a>; Signature: string }
module ExternInfo =
  let map f info =
    { Namespace = info.Namespace; Name = info.Name; Signature = info.Signature; Type = ExternType.map f info.Type }