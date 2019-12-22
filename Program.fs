open System
open System.Reflection
open VRC.Udon
open FSharpPlus
open FSharp.Scanf

let myWrapper =
  let print =
    Common.Delegates.UdonExternDelegate(fun heap stack ->
      stack.[0] |> heap.GetHeapVariable<obj> |> printfn "My.Print> %A"
    )
  { new Common.Interfaces.IUdonWrapperModule with
      member __.Name = "My"
      member __.GetExternFunctionParameterCount(signature) =
        match signature with
        | "Print" -> 1
        | _ -> invalidArg "signature" (sprintf "no impl for sig %s" signature)
      member __.GetExternFunctionDelegate(signature) =
        match signature with
        | "Print" -> print
        | _ -> invalidArg "signature" (sprintf "no impl for sig %s" signature)
  }

let src = """
.data_start
  bar: %SystemString, "Hello, \nWorld!\u0061"
.data_end

.code_start
  .export _start
  _start:
  JUMP, 30u
  PUSH, bar
  EXTERN, "My.Print"
  PUSH, bar
  EXTERN, "My.Print"
  JUMP, 0xFFFFFF
  print:
  PUSH, bar
  EXTERN, "My.Print"
  JUMP, 0xFFFFFF
.code_end
"""

let asm = UAssembly.Assembler.UAssemblyAssembler()
let program = asm.Assemble src
let dwf = Wrapper.UdonDefaultWrapperFactory()
dwf.RegisterWrapperModule myWrapper
let uvf = VM.UdonVMFactory(dwf)

let runVM () =
  let vm = uvf.ConstructUdonVM()
  if vm.LoadProgram program then
    vm.Interpret() |> ignore
  else
    printfn "fail"

runVM()

type UdonType<'t> =
  | Void
  | BasicType of 't
  | Array of UdonType<'t>
  | Ref of UdonType<'t>

let rec parseTypeName (s: string) =
  match s with
  | "SystemVoid" -> Void
  | Sscanf "%sArray" s | Sscanf "%s[]" s -> Array (parseTypeName s)
  | Sscanf "%sRef" s -> Ref (parseTypeName s)
  | s -> BasicType s

let getTypeStringsFromResolver (typeResolver: #UAssembly.Interfaces.IUAssemblyTypeResolver) =
  let ty = typeResolver.GetType()
  let types =
    monad.strict {
      let! f = ty.GetField("_types", BindingFlags.Static ||| BindingFlags.NonPublic) |> Option.ofObj
      let dict = f.GetValue() :?> System.Collections.Generic.Dictionary<string, Type>
      return dict.Keys :> _ seq
    }
  defaultArg types Seq.empty

let getTypeStrings () =
  seq {
    yield! UAssembly.Assembler.SystemTypeResolver() |> getTypeStringsFromResolver
    yield! EditorBindings.UdonTypeResolver() |> getTypeStringsFromResolver
    yield! EditorBindings.VRCSDK2TypeResolver() |> getTypeStringsFromResolver
    yield! EditorBindings.UnityEngineTypeResolver() |> getTypeStringsFromResolver
  } |> Seq.map parseTypeName |> Set.ofSeq

type ExternType<'a> =
  | StaticFunc of args:'a[] * ret:'a
  | StaticVoidRetFunc of arg:'a[]
  | StaticVoidArgFunc of ret:'a
  | StaticVoidRetArgFunc
  | InstanceFunc of args:'a[] * ret:'a
  | InstanceVoidRetFunc of arg:'a[]
  | InstanceVoidArgFunc of ty:'a
  | InstanceVoidRetArgFunc
  | Constructor of args:'a[] * ty:'a
  | Unknown of arity:int * argret:'a[][]

let parseSignature (name: string) (argret: string[][]) (arity: int) =
  match name, argret with
  | "ctor", [| xs; [|ret|] |] when xs.Length + 1 = arity -> Constructor (xs, ret)
  | _, [|[|"SystemVoid"|]|] ->
    if arity = 0 then StaticVoidRetArgFunc
    else if arity = 1 then InstanceVoidRetArgFunc
    else Unknown (arity, argret)
  | _, [|[|ret|]|] ->
    if arity = 1 then StaticVoidArgFunc ret
    else if arity = 2 then InstanceVoidArgFunc ret
    else Unknown (arity, argret)
  | _, [| args; [|"SystemVoid"|] |] ->
    if arity = args.Length then StaticVoidRetFunc args
    else if arity = args.Length + 1 then InstanceVoidRetFunc args
    else Unknown (arity, argret)
  | _, [| args; [|ret|] |] ->
    if arity = args.Length + 1 then StaticFunc (args, ret)
    else if arity = args.Length + 2 then InstanceFunc (args, ret)
    else Unknown (arity, argret)
  | _ -> Unknown (arity, argret)

let enumerateExterns () =
  let asm = (typeof<Wrapper.UdonWrapper>).Assembly
  let wrapperTy = typeof<Common.Interfaces.IUdonWrapperModule>
  let name = wrapperTy.GetProperty("Name")
  let types =
    asm.GetTypes()
    |> Seq.filter (fun t -> t.Namespace = "VRC.Udon.Wrapper.Modules" && t.Name.StartsWith "Extern")
    |> Seq.choose (fun t ->
      let ctor = t.GetConstructor([||]) |> Option.ofObj
      let pc =
        t.GetField("_parameterCounts", BindingFlags.NonPublic ||| BindingFlags.Static)
        |> Option.ofObj
      Option.map2 (fun x y -> x, y) pc ctor
    )
    |> Seq.map (fun (pc, ctor) ->
      let instance = ctor.Invoke([||])
      let name = name.GetValue(instance) :?> string
      let dict = pc.GetValue() :?> System.Collections.Generic.Dictionary<string, int>
      dict |> Seq.map (function KeyValue(k, v) -> name,k,v)
      )
    |> Seq.concat
    |> Seq.map (fun (name, signature, argcount) ->
      let fn, xs =
        match signature |> String.split ["__"] |> Seq.filter ((<>) "") |> Seq.toList with
        | [] -> failwith "impossible"
        | x :: xs ->
          x, 
          xs |> Seq.map (fun x -> x.Split '_')
             |> Seq.toArray
      name,fn,xs,signature,argcount
      )

  for moduleName, funcName, xs, orig, arity in types do
    let ty = parseSignature funcName xs arity
    match ty with
    | Unknown _ -> () // printfn "%s.%s: Unknown, arity = %i, orig = %s.%s" moduleName funcName arity moduleName orig
    | _ -> () // printfn "%s.%s :: %A, orig = %s.%s" moduleName funcName ty moduleName orig
  types |> Seq.length |> printfn "%i"

// enumerateExterns()
