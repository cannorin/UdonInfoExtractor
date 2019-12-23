module Utils

open System
open System.Reflection
open VRC.Udon
open FSharpPlus
open FSharp.Scanf

module VM =
  open System.Collections.Generic

  let facadeModule =
    let print =
      Common.Delegates.UdonExternDelegate(fun heap stack ->
        stack.[0] |> heap.GetHeapVariable<obj> |> printfn "Debug.Log> %A"
      )
    { new Common.Interfaces.IUdonWrapperModule with
        member __.Name = "Debug"
        member __.GetExternFunctionParameterCount(signature) =
          match signature with
          | "Log" -> 1
          | _ -> invalidArg "signature" (sprintf "no impl for sig %s" signature)
        member __.GetExternFunctionDelegate(signature) =
          match signature with
          | "Log" -> print
          | _ -> invalidArg "signature" (sprintf "no impl for sig %s" signature)
    }

  let inline private tArrRef<'t> tyName =
    [ 
      tyName, typeof<'t>
      sprintf "%sArray" tyName, typeof<'t[]>
      sprintf "%sRef" tyName, typeof<'t>
    ]

  type FacadeResolver() =
    inherit UAssembly.Assembler.BaseTypeResolver()
    let types =
      [
        yield "SystemArray", typeof<System.Array>
        yield "SystemBase64FormattingOptions", typeof<Base64FormattingOptions>
        yield! tArrRef<bool> "SystemBoolean"
        yield! tArrRef<byte> "SystemByte"
        yield! tArrRef<char> "SystemChar"
        yield "SystemCharEnumerator", typeof<CharEnumerator>
        yield "SystemCollectionsGenericIEnumerableSystemString", typeof<IEnumerable<string>>
        yield "SystemCollectionsGenericListSystemInt32", typeof<List<int>>
        yield "SystemCollectionsGenericListSystemSingle", typeof<List<float32>>
        yield "SystemCollectionsGenericListSystemString", typeof<List<string>>
        yield "SystemCollectionsIEnumerator", typeof<Collections.IEnumerator>
        yield "SystemCollectionsObjectModelReadOnlyCollectionSystemTimeZoneInfo", typeof<Collections.ObjectModel.ReadOnlyCollection<TimeZoneInfo>>
        yield "SystemConvert", typeof<System.Convert>
        yield! tArrRef<DateTime> "SystemDateTime"
        yield "SystemDateTimeKind", typeof<DateTimeKind>
        yield "SystemDateTimeOffset", typeof<DateTimeOffset>
        yield "SystemDayOfWeek", typeof<DayOfWeek>
        yield "SystemDecimal", typeof<decimal>
        yield! tArrRef<float> "SystemDouble"
        yield "SystemException", typeof<Exception>
        yield "SystemGlobalizationCompareOptions", typeof<Globalization.CompareOptions>
        yield "SystemGlobalizationCultureInfo", typeof<Globalization.CultureInfo>
        yield "SystemGlobalizationDateTimeStyles", typeof<Globalization.DateTimeStyles>
        yield "SystemGlobalizationNumberStyles", typeof<Globalization.NumberStyles>
        yield "SystemGlobalizationUnicodeCategry", typeof<Globalization.UnicodeCategory>
        yield "SystemGuid", typeof<Guid>
        yield "SystemIFormatProvider", typeof<IFormatProvider>
        yield! tArrRef<int16> "SystemInt16"
        yield! tArrRef<int> "SystemInt32"
        yield! tArrRef<int64> "SystemInt64"
        yield! tArrRef<Object> "SystemObject"
        yield "SystemRuntimeInteropServicesStructLayoutAttribute", typeof<Runtime.InteropServices.StructLayoutAttribute>
        yield "SystemRuntimeTypeHandle", typeof<RuntimeTypeHandle>
        yield! tArrRef<SByte> "SystemSByte"
        yield! tArrRef<Single> "SystemSingle"
        yield! tArrRef<string> "SystemString"
        yield "SystemStringCompaison", typeof<StringComparison>
        yield "SystemStringSplitOptions", typeof<StringSplitOptions>
        yield "SystemTextNormalizationForm", typeof<Text.NormalizationForm>
        yield! tArrRef<TimeSpan> "SystemTimeSpan"
        yield! tArrRef<TimeZoneInfo> "SystemTimeZoneInfo"
        yield "SystemTimeZoneInfoAdjustmentRuleArray", typeof<TimeZoneInfo.AdjustmentRule[]>
        yield! tArrRef<Type> "SystemType"
        yield "SystemTypeCode", typeof<TypeCode>
        yield! tArrRef<uint16> "SystemUInt16"
        yield! tArrRef<uint32> "SystemUInt32"
        yield! tArrRef<uint64> "SystemUInt64"
      ] |> Seq.map KeyValuePair.Create |> Dictionary
    override this.Types = types
  
  let assembler = UAssembly.Assembler.UAssemblyAssembler(null, FacadeResolver())
  let factory =
    let dwf = Wrapper.UdonDefaultWrapperFactory()
    dwf.RegisterWrapperModule facadeModule
    VM.UdonVMFactory(dwf)

  let test () =
    let src = """
    .data_start
      baz: %SystemUInt32, 0
      foo: %SystemInt32, 42
      bar: %SystemString, "Hello, \nWorld!\u0061"
      piyo: %SystemBoolean, true
    .data_end

    .code_start
      .export _start
      _start:
      JUMP, 30u
      PUSH, bar
      EXTERN, "Debug.Log"
      PUSH, bar
      EXTERN, "Debug.Log"
      JUMP, 0xFFFFFF
      print:
      PUSH, bar
      EXTERN, "Debug.Log"
      JUMP, 0xFFFFFF
    .code_end
    """
    let program = assembler.Assemble src
    program.SymbolTable.GetSymbols()
    |> Seq.map (program.SymbolTable.GetAddressFromSymbol)
    |> Seq.iter (printfn "%i")
    let vm = factory.ConstructUdonVM()
    if vm.LoadProgram program then
      vm.Interpret() |> ignore
    else
      printfn "fail"

type UdonType<'t> =
  | Void
  | TypeParam of string
  | BasicType of 't
  | Array of UdonType<'t>
  | Ref of UdonType<'t>

module UdonType =
  let rec parse (s: string) =
    match s with
    | "SystemVoid" -> Void
    | "T" -> TypeParam "T"
    | Sscanf "%sArray" s | Sscanf "%s[]" s -> Array (parse s)
    | Sscanf "%sRef" s -> Ref (parse s)
    | s -> BasicType s

  let getAllFromResolver (typeResolver: #UAssembly.Interfaces.IUAssemblyTypeResolver) =
    let ty = typeResolver.GetType()
    let types =
      monad.strict {
        let! f = ty.GetField("_types", BindingFlags.Static ||| BindingFlags.NonPublic) |> Option.ofObj
        let dict = f.GetValue() :?> System.Collections.Generic.Dictionary<string, Type>
        return dict.Keys :> _ seq |> Seq.map parse
      }
    defaultArg types Seq.empty

  let getAllSupported () =
    seq {
      yield! UAssembly.Assembler.SystemTypeResolver() |> getAllFromResolver
      yield! EditorBindings.UdonTypeResolver() |> getAllFromResolver
      yield! EditorBindings.VRCSDK2TypeResolver() |> getAllFromResolver
      yield! EditorBindings.UnityEngineTypeResolver() |> getAllFromResolver
    } |> Set.ofSeq

type ExternType<'a> =
  | StaticFunc of args:'a[] * ret:'a option
  | StaticGenericFunc of typrm:string list * args:'a[] * ret:'a option
  | InstanceFunc of args:'a[] * ret:'a option
  | InstanceGenericFunc of typrm:string list * args:'a[] * ret:'a option
  | Constructor of args:'a[] * ty:'a
  | Unknown of arity:int * argret:'a[][]

module ExternType =
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

  let enumerateDefined () =
    let asm = (typeof<Wrapper.UdonWrapper>).Assembly
    let wrapperTy = typeof<Common.Interfaces.IUdonWrapperModule>
    let name = wrapperTy.GetProperty("Name")
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
      name,fn,parseSignature fn xs argcount, argcount,signature
      )

let doEnumerateExterns () =
  let types = ExternType.enumerateDefined ()

  for moduleName, funcName, ty, arity, orig in types do
    match ty with
    | Unknown _ -> printfn "%s.%s: Unknown, arity = %i, orig = %s.%s" moduleName funcName arity moduleName orig
    | _ -> printfn "%s.%s :: %A, orig = %s.%s" moduleName funcName ty moduleName orig

