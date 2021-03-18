UdonInfoExtractor
=================

# ARCHIVED!

This tool has stopped working because of the internal changes of VRCSDK.

I archived this repository because this kind of tool now has to be run inside Unity(\*1), and updating this tool to do that would require a lot of work.

(\*1): VRC.Udon.EditorBindings.dll is now linked against Cinemachine, which is not available as a standalone DLL.

-------

This tool generates the following JSON files:

- https://github.com/cannorin/UdonInfoExtractor/releases/latest/download/udon_externs.json
  - Contains every extern function the UDON SDK provides.
- https://github.com/cannorin/UdonInfoExtractor/releases/latest/download/udon_types.json
  - Contains every type the UDON SDK supports.
- https://github.com/cannorin/UdonInfoExtractor/releases/latest/download/udon_info.json
  - Contains externs + types + version string of the SDKs.
  
These are intended to be consumed from compilers/tools/etc to help developing with UDON. Live example: https://7colou.red/UdonExternSearch/

# How to run

* Requires .NET Core 3.1 SDK. Download from [here](https://dotnet.microsoft.com/download/dotnet-core/3.1) if you don't have yet.
* You have to set the `UnityInstallRoot` property in `UdonTest.fsproj` and copy the `Asset` folder from a project with VRCSDK3 and UdonSDK installed.
* After copying those DLLs, it should run with `dotnet run`. It creates the 3 json files in the current directory.

# License

Apache 2. See LICENSE.txt.
