open VRC.Udon

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
  foo: %SystemInt32, null
  bar: %SystemString, null
.data_end

.code_start
  .export _start
  _start:
  PUSH, foo
  PUSH, bar
  EXTERN, "SystemConvert.__ToString__SystemInt32__SystemString"
  PUSH, bar
  EXTERN, "My.Print"
  JUMP, 0xFFFFFF
.code_end
"""

let asm = UAssembly.Assembler.UAssemblyAssembler()
let program = asm.Assemble src
let fooAddr = program.SymbolTable.GetAddressFromSymbol "foo"
let barAddr = program.SymbolTable.GetAddressFromSymbol "bar"

let dwf = Wrapper.UdonDefaultWrapperFactory()
dwf.RegisterWrapperModule myWrapper
let uvf = VM.UdonVMFactory(dwf)
let vm = uvf.ConstructUdonVM()

if vm.LoadProgram program then
  let heap = vm.InspectHeap()
  heap.SetHeapVariable<int>(fooAddr, 42)
  vm.Interpret() |> ignore
else
  printfn "fail"


