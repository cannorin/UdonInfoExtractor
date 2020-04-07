module Program
open System
open System.IO
open Thoth.Json.Net
open UdonBase
open Utils

let charCoder =
  Extra.empty
  |> Extra.withCustom
    (string >> Encode.string)
    (fun str obj ->
      Decode.string str obj
      |> Result.bind (fun s ->
        if s.Length = 1 then Ok s.[0]
        else Error (DecoderError("not a char", FailMessage "not a char"))))

let vrcsdk3Version = "2020.04.03.13.19"
let udonsdkVersion = vrcsdk3Version

[<EntryPoint>]
let main argv =
  let types =
    let xs =
      Seq.append (UdonType.getAllSupported ()) (UdonType.getAllFromNodeRegistry ())
      |> Seq.distinctBy fst
    UdonType.createTyperMap xs

  // let externs = Extern.createExternMap ()
  let externs = GraphNode.createExternMap types

  let info = { Externs = externs; Types = types; VRCSDK3Version = vrcsdk3Version; UDONSDKVersion = udonsdkVersion }

  let encode x =
    Encode.Auto.toString(0, x, extra=charCoder, skipNullField=true)

  let externsJson = encode externs
  let typesJson = encode types
  let infoJson = encode info

  let targetDir =
    if argv.Length = 0 then Environment.CurrentDirectory
    else
      let dirName = Path.GetFullPath argv.[0]
      if Directory.Exists dirName then dirName
      else
        try
          Directory.CreateDirectory dirName |> ignore
          dirName
        with
         | _ -> failwithf "error: directory '%s' does not exist" argv.[0]
  
  let write fileName json =
    let path = Path.Combine(targetDir, fileName)
    File.WriteAllText (path, json)

  write "udon_externs.json" externsJson
  write "udon_types.json" typesJson
  write "udon_info.json" infoJson
  0
