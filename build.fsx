#r "paket: groupref Build //"
#load ".fake/build.fsx/intellisense.fsx"
#nowarn "58"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO

Target.initEnvironment()

Target.create "Clean" (fun _ ->
  [ "bin"
    
    "src/FBitTorrent.BEncode/bin"
    "src/FBitTorrent.BEncode/obj"
    "src/FBitTorrent.Core/bin"
    "src/FBitTorrent.Core/obj"
    "src/FBitTorrent.Cli/bin"
    "src/FBitTorrent.Cli/obj"
    
    "tests/FBitTorrent.BEncode.Tests/bin"
    "tests/FBitTorrent.BEncode.Tests/obj"
    "tests/FBitTorrent.Core.Tests/bin"
    "tests/FBitTorrent.Core.Tests/obj" ]
  |> List.iter Shell.cleanDir)

Target.create "Build" (fun _ ->
  DotNet.exec id "build" "FBitTorrent.sln -c Release" |> ignore)

Target.create "Test" (fun _ ->
  DotNet.exec id "test" "tests/FBitTorrent.BEncode.Tests/" |> ignore
  DotNet.exec id "test" "tests/FBitTorrent.Core.Tests/"    |> ignore)

// Runtime Identifiers (https://docs.microsoft.com/en-us/dotnet/core/rid-catalog):
let rid () =
  Environment.environVarOrDefault "rid" (
    if   Environment.isWindows then "win-x64"
    elif Environment.isLinux   then "linux-x64"
    elif Environment.isMacOS   then "osx-x64"
    else failwith "Unsupported OS")
  
let publishParams rid =
  "--output bin/"  + rid + " " +
  "--runtime "     + rid + " " +
  "--configuration Release "   +
  "--self-contained true "     +
  "-p:PublishReadyToRun=true " +
  "-p:PublishTrimmed=true "    +
  "-p:PublishSingleFile=true " +
  "-p:IncludeNativeLibrariesForSelfExtract=true"

Target.create "Publish-Cli" (fun _ ->
  DotNet.exec id "publish" ("src/FBitTorrent.Cli/FBitTorrent.Cli.fsproj " + publishParams (rid ())) |> ignore)

Target.create "Cli" ignore

"Clean"
  ==> "Build"
  ==> "Test"
  ==> "Publish-Cli"
  ==> "Cli"

Target.create "All" ignore

"Clean"
  ==> "Build"
  ==> "Test"
  ==> "Publish-Cli"
  ==> "All"
  
Target.runOrDefaultWithArguments "All"