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

  let createTyperMap (xs: seq<string * Type>) : UdonTypeContext<string> =
    let xs = xs |> Seq.filter (fst >> String.endsWith "Ref" >> not) |> Seq.distinctBy fst
    let revMap = xs |> Seq.map (fun (name, ty) -> ty.FullName, name) |> Map.ofSeq
    let rec findNearestBaseType (ty: Type) =
      let bt = ty.BaseType
      if isNull bt then None
      else
        match revMap |> Map.tryFind bt.FullName with
        | None -> findNearestBaseType bt
        | Some t -> Some t
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

  let getExternDefinition (und: UdonNodeDefinition) (tyCtx: UdonTypeContext<string>) : UdonExternDefinition<string> option =
    match und.fullName.Split '.' with
    | [| ns; signature |] ->
      let name, et = Extern.parse signature und.parameters.Count
      let name =
        match und.name |> String.split [" "] |> Seq.tryFind (fun s -> s.StartsWith name && signature.Contains s) with
        | Some name -> name
        | _ -> name
      let et =
        match et with
        | Unknown _ -> failwith "TODO"
        | _ -> et
      failwith "TODO"
    | _ -> None
