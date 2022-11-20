namespace FBitTorrent.BEncode.Tests

open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Loggers
open BenchmarkDotNet.Running
open FBitTorrent.BEncode
open Xunit
open Xunit.Abstractions

[<MemoryDiagnoser>]
type BEncodeIntegerBenchmarkTests() =
    
    [<Params("i-9223372036854775808e", "i9223372036854775807e")>]
    member val EncodedValue = "" with get, set
    
    [<Benchmark>]
    member __.DecodeValue () = BDecode.defaultFromString __.EncodedValue

[<MemoryDiagnoser>]
type BEncodeStringBenchmarkTests() =
    
    [<Params("0:", "10:aaaaaaaaaa")>]
    member val EncodedValue = "" with get, set
    
    [<Benchmark>]
    member __.DecodeValue () = BDecode.defaultFromString __.EncodedValue
    
[<MemoryDiagnoser>]
type BEncodeListBenchmarkTests() =
    
    [<Params("le", "l6:stringl7:stringsl8:stringedei23456eei12345ee")>]
    member val EncodedValue = "" with get, set
    
    [<Benchmark>]
    member __.DecodeValue () = BDecode.defaultFromString __.EncodedValue

[<MemoryDiagnoser>]
type BEncodeDictionaryBenchmarkTests() =
    
    [<Params("de", "d4:testd5:testsli12345ei12345ee2:tod3:tomi12345eeee")>]
    member val EncodedValue = "" with get, set
    
    [<Benchmark>]
    member __.DecodeValue () = BDecode.defaultFromString __.EncodedValue

type BEncodeBenchmarkTests(output: ITestOutputHelper) =
    
    member _.Run<'T> () =
        let logger = AccumulationLogger()
        let config = ManualConfig.Create(DefaultConfig.Instance)
                        .AddLogger(logger)
                        .WithOptions(ConfigOptions.DisableOptimizationsValidator)
        BenchmarkRunner.Run<'T>(config) |> ignore
        output.WriteLine(logger.GetLog());
    
    [<Fact(Skip = "Benchmark tests should be run explicitly")>]
    member __.``Benchmark test integer performance`` () = __.Run<BEncodeIntegerBenchmarkTests>()
    
    [<Fact(Skip = "Benchmark tests should be run explicitly")>]
    member __.``Benchmark test string performance`` () = __.Run<BEncodeStringBenchmarkTests>()
    
    [<Fact(Skip = "Benchmark tests should be run explicitly")>]
    member __.``Benchmark test list performance`` () = __.Run<BEncodeListBenchmarkTests>()
    
    [<Fact(Skip = "Benchmark tests should be run explicitly")>]
    member __.``Benchmark test dictionary performance`` () = __.Run<BEncodeDictionaryBenchmarkTests>()
        