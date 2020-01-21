module Program
open System
open Trie

// Utils.VM.test()

let json = Utils.createExternMapJson ()

json |> printfn "%s"

(*
printfn "json generated"

let t : Result<StringTrie<Utils.ExternInfo[]>, _> =
  Thoth.Json.Net.Decode.Auto.fromString(json, extra = Utils.charCoder)

match t with
| Ok _ -> printfn "json decoded"
| Error _ -> printfn "json decode failed"
*)

// Sanuki.test()

(*
let staticModules =
  Utils.ExternType.enumerateDefined()
  |> Seq.map (fun (n, _, _, _, _) -> n)
  |> Set.ofSeq

let src = """
this: UnityEngineTransform;
let mutable foo = 32;
sync foo with SomeMethod;

fun f1(x, y) = {
  UnityEngineDebug.Log (x :> SystemObject :> SystemObject);
  x + y
};

fun f2(xs: array(SystemString)) = {
  xs#Length = 42
};

fun f3(x, y) = {
  let a = x + y;
  let b = x - y;
  (a, b)
};

event _start = {
  let mutable bar = 0;
  bar = 2;
  let barP = &bar;
  for (i = 0 to 10) {
    let (a, b) = f3 (*bar, i * foo);
    f1(a, b);
  }
}
"""

match Pho.Parser.parseToUntyped staticModules src with
  | Ok prog -> printfn "%A" prog
  | Error e -> printfn "err: %A" e
*)