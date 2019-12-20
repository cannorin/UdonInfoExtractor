UDON Test

Attempting to write a compiler for Udon.

For those who is brave enough to try to run and modify this:

* Requires .NET Core 2.1 SDK. Download from [here](https://dotnet.microsoft.com/download/dotnet-core/2.1) if you don't have yet.
* You'll have to copy some DLLs from the Udon SDK. See `Udon/about.txt`.
* After copying those DLLs, it should run with `dotnet run`.
* `EXTERN`s calling unity functions internally will not work with this setup, since this does not run on Unity but on .NET Core.
* Udon DLLs seem to be pure self-contained .NET libraries; I was able to run this on Windows and also on Debian GNU/Linux.

