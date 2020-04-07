module Utils

open UdonBase
open System
open System.Reflection
open VRC.Udon
open FSharpPlus


[<AutoOpen>]
module Misc =
  let inline private ccl (fc: ConsoleColor) =
    Console.ForegroundColor <- fc;
    { new IDisposable with
        member x.Dispose() = Console.ResetColor() }

  let inline cprintf color format =
    Printf.kprintf (fun s -> use __ = ccl color in printf "%s" s) format

  let inline cprintfn color format =
    Printf.kprintf (fun s -> use __ = ccl color in printfn "%s" s) format

module UdonType =
  open VRC.Udon.Graph
  let getAllFromNodeRegistry () =
    let node = new UdonTypeNodeRegistry()
    node.GetNodeDefinitions()
    |> Seq.choose (fun d ->
      if not <| d.fullName.StartsWith "Type_" then
        None
      else Some (d.fullName.[5..], d.``type``))

  let getAllFromResolver (typeResolver: #UAssembly.Interfaces.IUAssemblyTypeResolver) =
    let ty = typeResolver.GetType()
    let types =
      monad.strict {
        let! f = ty.GetField("_types", BindingFlags.Static ||| BindingFlags.NonPublic) |> Option.ofObj
        let dict = f.GetValue() :?> System.Collections.Generic.Dictionary<string, Type>
        return dict |> Seq.map (function KeyValue (k, v) -> (k, v))
      }
    defaultArg types Seq.empty

  let getAllSupported () =
    seq {
      yield! UAssembly.Assembler.SystemTypeResolver() |> getAllFromResolver
      yield! EditorBindings.UdonTypeResolver() |> getAllFromResolver
      yield! EditorBindings.VRCSDK2TypeResolver() |> getAllFromResolver
      yield! EditorBindings.UnityEngineTypeResolver() |> getAllFromResolver
    } |> Seq.cache
  
  let mutable private revMaps = Map.empty

  let getRevMap (m: UdonTypeContext<string>) : Map<string, string> =
    match revMaps |> Map.tryFind m with
    | Some m -> m
    | None ->
      let revMap = m |> Map.toSeq |> Seq.map (fun (name, ty) -> ty.ActualType, name) |> Map.ofSeq
      revMaps <- revMaps |> Map.add m revMap
      revMap

  let createTyperMap (xs: seq<string * Type>) : UdonTypeContext<string> =
    let xs = xs |> Seq.filter (fst >> String.endsWith "Ref" >> not) |> Seq.distinctBy fst |> Seq.toArray
    let revMap = xs |> Seq.map (fun (name, ty) -> ty.FullName, name) |> Map.ofSeq
    let rec findNearestBaseType (ty: Type) =
      let bt = ty.BaseType
      if isNull bt then None
      else
        match revMap |> Map.tryFind bt.FullName with
        | None -> findNearestBaseType bt
        | Some t -> Some t
    let ctx =
      Map.ofSeq <| seq {
        for name, ty in xs do
          let atys =
            xs |> Seq.filter (fun (_, ty2) -> ty2.IsAssignableFrom ty)
               |> Seq.map fst |> Seq.toArray
          let prms =
            ty.GenericTypeArguments
            |> Seq.map (fun t -> t.FullName)
            |> Seq.choose (fun n -> revMap |> Map.tryFind n)
            |> Seq.toArray
          name,
          {
            Type = name; ActualType = ty.FullName
            IsAbstract = ty.IsAbstract; IsInterface = ty.IsInterface
            IsClass = ty.IsClass; IsValueType = ty.IsValueType
            IsEnum = ty.IsEnum
            IsPrimitive = ty.IsPrimitive; IsArray = ty.IsArray;
            IsGenericType = ty.IsGenericType
            BaseType = findNearestBaseType ty
            GenericTypeArguments = prms; AssignableTo = atys
          }
      }
    revMaps <- revMaps |> Map.add ctx revMap
    ctx

  let describe (ty: Type) (ctx: UdonTypeContext<string>) : bool * UdonTypeInfo<string> =
    let revMap = getRevMap ctx
    match revMap |> Map.tryFind ty.FullName with
    | Some ty -> true, ctx |> Map.find ty
    | None ->
      let rec findNearestBaseType (ty: Type) =
        let bt = ty.BaseType
        if isNull bt then None
        else
          match revMap |> Map.tryFind bt.FullName with
          | None -> findNearestBaseType bt
          | Some t -> Some t
      let prms =
        ty.GenericTypeArguments
        |> Seq.map (fun t -> t.FullName)
        |> Seq.choose (fun n -> revMap |> Map.tryFind n)
        |> Seq.toArray
      false, {
        Type = ty.Name; ActualType = ty.FullName
        IsAbstract = ty.IsAbstract; IsInterface = ty.IsInterface
        IsClass = ty.IsClass; IsValueType = ty.IsValueType
        IsEnum = ty.IsEnum
        IsPrimitive = ty.IsPrimitive; IsArray = ty.IsArray;
        IsGenericType = ty.IsGenericType
        BaseType = findNearestBaseType ty
        GenericTypeArguments = prms; AssignableTo = [||]
      }

module Extern =
  open ExternType

  let parse signature arity =
    let fn, xs =
      match signature |> String.split ["__"] |> Seq.filter ((<>) "") |> Seq.toList with
      | [] -> failwith "impossible"
      | x :: xs ->
        x, 
        xs |> Seq.map (fun x -> x.Split '_')
           |> Seq.toArray
    fn, parseSignature fn xs arity

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
      let fn, et = parse signature argcount
      name, fn, et, argcount, signature
      )

  let createExternMap () =
    enumerateDefined ()
    |> Seq.groupBy (fun (m,f,_,_,_) -> m,f)
    |> Seq.map (fun ((m, f), xs) ->
      sprintf "%s.%s" m f,
      xs |> Seq.map (fun (_,_,ty,_,orig) -> {| Namespace = m; Name = f; Type = ty; Signature = orig |})
         |> Seq.toArray)
    |> Seq.toArray

module GraphNode =
  open VRC.Udon.Graph

  let enumerateAllNodeDefinitions () : UdonNodeDefinition seq =
    let asm = (typeof<UdonNodeDefinition>).Assembly
    let baseTy = typeof<BaseNodeRegistry>
    asm.GetTypes()
    |> Seq.filter (fun t -> t.IsSubclassOf baseTy && not t.IsAbstract)
    |> Seq.choose (fun t -> t.GetConstructor([||]) |> Option.ofObj)
    |> Seq.collect (fun ctor ->
      let instance = ctor.Invoke [||] :?> BaseNodeRegistry
      instance.GetNodeDefinitions())
    |> Seq.distinctBy (fun d -> d.fullName)

  let (|StringSplit|) (sep: string seq) (s: string) =
    s.Split(Array.ofSeq sep, StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

  type bool3 = True | Maybe | False

  let getExternDefinition (tyCtx: UdonTypeContext<string>) (und: UdonNodeDefinition) : UdonExternDefinition<string> option =
    match und.fullName with
    | StringSplit ["."] [ ns; (StringSplit ["__"] (name :: _) & signature) ] ->
      let name =
        match und.name |> String.split [" "] |> Seq.tryFind (fun s -> s.StartsWith name && signature.StartsWith ("__" + s)) with
        | Some name -> name
        | _ -> name
      let types = signature.[name.Length+2..].Split("_", StringSplitOptions.RemoveEmptyEntries) |> Array.filter ((<>) "SystemVoid")
      let isInstance =
        if und.parameters.Count = 0 then False
        else
          let x = und.parameters.[0]
          match tyCtx |> Map.tryFind ns with
          | Some ty when ty.ActualType = x.``type``.FullName ->
            if x.name <> "instance" then Maybe else True
          | None when x.name = "instance" || String.isSubString (x.``type``.Name) ns -> Maybe
          | _ -> False
      let hasReturn =
        signature |> String.endsWith "SystemVoid" |> not &&
        und.parameters |> Seq.tryLast
                       |> Option.map (fun x -> x.parameterType = UdonNodeParameter.ParameterType.OUT)
                       |> Option.defaultValue false
      let paramIsTypeParam (x: UdonNodeParameter) = 
        x.name = "type" && x.``type`` = typeof<Type> && x.parameterType = UdonNodeParameter.ParameterType.IN
      let canBeGeneric =
        und.parameters |> Seq.exists paramIsTypeParam
      let canTrustSignature, isGeneric, isInstance =
        let isGeneric =
          canBeGeneric && types |> Array.exists (fun t -> t = "T" || t = "TArray")
        let expected =
          und.parameters.Count - (if isInstance = True then 1 else 0)
                               - (if isGeneric then 1 else 0)
        if expected = types.Length then true, isGeneric, isInstance = True
        else
          if isInstance = Maybe && expected - 1 = types.Length then true, isGeneric, true
          else false, isGeneric, isInstance = True
      let getNthParameter =
        let lookup ty =
          match UdonType.describe ty tyCtx with
          | true, t -> Ok t.Type
          | false, t -> Error t
        if canTrustSignature then
          fun i (ty: Type) ->
            let expected = types.[i]
            match tyCtx |> Map.tryFind expected with
            | None ->
              if expected = "T" || expected = "TArray" then Ok expected
              else lookup ty |> Result.mapError (fun t -> { t with Type = expected })
            | Some expectedT ->
              if ty.FullName = expectedT.ActualType then Ok expected
              else lookup ty |> Result.mapError (fun t -> { t with Type = expected })
        else
          let revMap = tyCtx |> UdonType.getRevMap
          fun _ (ty: Type) ->
            match revMap |> Map.tryFind ty.FullName with
            | Some s -> Ok s
            | None -> lookup ty 
      let argConv (xs: UdonNodeParameter list) : UdonExternParameterInfo<string> list =
        let xs, prmacc =
          if isInstance then
            xs.Tail, [{ Name = Some "instance"; Type = Instance ns }]
          else xs, []
        let rec go i : UdonNodeParameter list -> _ = function
          | [] -> []
          | ty :: ret :: [] when isGeneric && hasReturn && paramIsTypeParam ty ->
            { Name = Some "type"; Type = TypeParameter "T" } :: go i [ ret ]
          | ty :: [] when isGeneric && not hasReturn && paramIsTypeParam ty ->
            { Name = Some "type"; Type = TypeParameter "T" } :: []
          | x :: xs ->
            let kind s =
              match x.parameterType with
              | UdonNodeParameter.ParameterType.IN -> In s
              | UdonNodeParameter.ParameterType.IN_OUT -> InOut s
              | UdonNodeParameter.ParameterType.OUT -> Out s
              | _ -> failwith "impossible"
            let t = 
              match getNthParameter i x.``type`` with
              | Ok s -> kind s
              | Error t -> UsesUnknownType (kind t)
            { Name = Option.ofObj x.name; Type = t } :: go (i+1) xs
        prmacc @ go 0 xs
      let prms = argConv (und.parameters |> List.ofSeq) |> List.toArray
      let et, rett, typrms =
        let types =
          if canTrustSignature then types
          else
            prms |> Array.choose (fun prm ->
              match prm.Type with
              | In s | Out s | InOut s -> Some s
              | UsesUnknownType (In t | Out t | InOut t) ->
                Some t.Type
              | _ -> None
            )
        match name, isInstance, isGeneric, hasReturn with
        | "ctor", false, false, true ->
          Constructor (types.[0..types.Length-2], Array.last types),
          Some (Array.last types),
          [||]
        | _ ->
          let arg, ret =
            if hasReturn then types.[0..types.Length-2], Some (Array.last types)
            else types, None
          match isInstance, isGeneric with
          | true, true -> InstanceGenericFunc (["T"], arg, ret), ret, [|"T"|]
          | true, false -> InstanceFunc (arg, ret), ret, [||]
          | false, true -> StaticGenericFunc (["T"], arg, ret), ret, [|"T"|]
          | false, false -> StaticFunc (arg, ret), ret, [||]
      if canTrustSignature then
        let expected = Extern.parse signature und.parameters.Count |> snd
        match expected with
        | Unknown _ -> ()
        | _ ->
          if et <> expected && name |> String.startsWith "set_" |> not then
            cprintfn ConsoleColor.Red "sig mismatch for '%s.%s':\n  got: %A\nvs\n  expected: %A" ns signature et expected
            cprintfn ConsoleColor.Red "isInstance=%b, isGeneric=%b, hasReturn=%b" isInstance isGeneric hasReturn
            cprintfn ConsoleColor.Red "%A" prms
            printfn ""
          else ()
      else
        cprintfn ConsoleColor.Yellow "sig '%s.%s' with arity %i was considered untrustable" ns signature und.parameters.Count
        cprintfn ConsoleColor.Yellow "name='%s', isInstance=%b, isGeneric=%b, hasReturn=%b" name isInstance isGeneric hasReturn
        cprintfn ConsoleColor.Yellow "%A" prms
        printfn ""
      Some
        {| 
          Namespace = ns; Name = name; Signature = signature;
          Type = et; FullName = und.fullName; IsStatic = not isInstance;
          ReturnType = rett; TypeParameters = typrms; Parameters = prms
        |}
    | _ -> None

  let createExternMap tyCtx =
    enumerateAllNodeDefinitions ()
    |> Seq.choose (getExternDefinition tyCtx)
    |> Seq.toArray
    |> Array.groupBy (fun d -> sprintf "%s.%s" d.Namespace d.Name)