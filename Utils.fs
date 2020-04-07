module Utils

open UdonBase
open System
open System.Reflection
open VRC.Udon
open FSharpPlus

module UdonType =
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
    let revMap =
      match revMaps |> Map.tryFind ctx with
      | Some r -> r
      | None ->
        let r = ctx |> Map.toSeq |> Seq.map (fun (name, ty) -> ty.ActualType, name) |> Map.ofSeq
        revMaps <- revMaps |> Map.add ctx r
        r
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
      instance.GetNodeDefinitions()
    )

  let (|StringSplit|) (sep: string seq) (s: string) =
    s.Split(Array.ofSeq sep, StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

  let getExternDefinition (und: UdonNodeDefinition) (tyCtx: UdonTypeContext<string>) : UdonExternDefinition<string> option =
    match und.fullName with
    | StringSplit ["."] [ ns; (StringSplit ["__"] (name :: _) & signature) ] ->
      let name =
        match und.name |> String.split [" "] |> Seq.tryFind (fun s -> s.StartsWith name && signature.StartsWith s) with
        | Some name -> name
        | _ -> name
      let types = signature.[name.Length..].Split("_", StringSplitOptions.RemoveEmptyEntries)
      let isInstance =
        if und.parameters.Count = 0 then false
        else
          let x = und.parameters.[0]
          if x.name <> "instance" then false
          else
            match tyCtx |> Map.tryFind ns with
            | Some ty -> ty.ActualType = x.``type``.FullName
            | None -> false
      let hasReturn =
        und.parameters |> Seq.tryLast
                       |> Option.map (fun x -> x.parameterType = UdonNodeParameter.ParameterType.OUT)
                       |> Option.defaultValue false
      let canBeGeneric =
        und.parameters |> Seq.exists (fun x -> x.name = "type" && x.``type`` = typeof<Type> && x.parameterType = UdonNodeParameter.ParameterType.IN)
      let canTrustSignature, isGeneric =
        let isGeneric =
          canBeGeneric && types |> Array.contains "SystemType" |> not
        let expected =
          und.parameters.Count - (if isInstance then 1 else 0)
                               + (if hasReturn then 0 else 1)
                               - (if isGeneric then 1 else 0)
        expected = types.Length, isGeneric
      let et, rett, typrms, prms =
        if canTrustSignature then
          match name, isInstance, isGeneric, hasReturn with
          | "ctor", false, false, true ->
            Constructor (types.[0..types.Length-2], Array.last types),
            Some (Array.last types),
            [||],
            failwith "TODO"
          | _ ->
            let argret (xs: UdonNodeDefinition list) =
              let xs, prmacc =
                if isInstance then
                  xs.Tail, [{ Name = Some "instance"; Type = In ns }]
                else xs, []

              failwith "TODO"
            failwith "TODO"
        else failwith "TODO"
      Some
        {| 
          Namespace = ns; Name = name; Signature = signature;
          Type = et; FullName = und.fullName; IsStatic = not isInstance;
          ReturnType = rett; TypeParameters = typrms; Parameters = prms
        |}
    | _ -> None
