module Utils

open UdonBase
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
    # Address: 0x0
    foo: %SystemInt32, 2
    # Address: 0x1
    bar: %SystemInt32, 3
    # Address: 0x2
    baz: %SystemInt32, 4
    # Address: 0x3
    res: %SystemInt32, 0
.data_end
.code_start
    PUSH, 0x0
    PUSH, 0x1
    PUSH, 0x2
    PUSH, 0x3
    EXTERN, "SystemInt32.__op_Addition__SystemInt32_SystemInt32__SystemInt32"
    PUSH, 0x3
    PUSH, 0x3
    EXTERN, "SystemInt32.__op_Multiplication__SystemInt32_SystemInt32__SystemInt32"
    PUSH, 0x3
    EXTERN, "Debug.Log"
.code_end
    """
    let program = assembler.Assemble src
    let vm = factory.ConstructUdonVM()
    if vm.LoadProgram program then
      vm.Interpret() |> ignore
    else
      printfn "fail"

type UdonType<'t> =
  | Void
  | TypeParam of string
  | BasicType of 't
  | Ref of UdonType<'t>

module UdonType =
  let rec parse (s: string) =
    match s with
    | "SystemVoid" -> Void
    | "T" -> TypeParam "T"
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

module ExternType =
  open ExternType

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
      let dict = pc.GetValue() :?> Collections.Generic.Dictionary<string, int>
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

open Trie

let createExternMap () =
  ExternType.enumerateDefined ()
  |> Seq.groupBy (fun (m,f,_,_,_) -> m,f)
  |> Seq.map (fun ((m, f), xs) ->
    sprintf "%s.%s" m f,
    xs |> Seq.map (fun (_,_,ty,_,orig) -> { Namespace = m; Name = f; Type = ty; Signature = orig })
       |> Seq.toArray)
  |> StringTrie.ofSeq

open Thoth.Json.Net

let charCoder =
  Extra.empty
  |> Extra.withCustom
    (string >> Encode.string)
    (fun str obj ->
      Decode.string str obj
      |> Result.bind (fun s ->
        if s.Length = 1 then Ok s.[0]
        else Error (DecoderError("not a char", FailMessage "not a char"))))

let createExternMapJson () =
  Encode.Auto.toString(0, createExternMap () |> StringTrie.toArray, extra=charCoder, skipNullField=true)

