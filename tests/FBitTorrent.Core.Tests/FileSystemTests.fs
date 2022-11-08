namespace FBitTorrent.Core.Tests

open System
open System.IO
open FBitTorrent.Core
open Xunit

module FileSystemTests =
    
    let [<Literal>] ValidFilePath = "./test.txt"
    let [<Literal>] ValidDirPath = "./test"
    
    let [<Literal>] InvalidFilePath = ""
    let [<Literal>] InvalidDirPath = ""
    
    let local = FileSystem.createLocal ()
    
    [<Fact>]
    let ``Test local file exists`` () =
        let stream = File.Create ValidFilePath
        stream.Close()
        Assert.True(local.FileExists ValidFilePath)
        File.Delete ValidFilePath
        
    [<Fact>]
    let ``Test local file doesn't exists`` () =
        Assert.False(local.FileExists ValidFilePath)
        File.Delete ValidFilePath
        
    [<Fact>]
    let ``Test local dir exists`` () =
        let _ = Directory.CreateDirectory ValidDirPath
        Assert.True(local.DirectoryExists ValidDirPath)
        Directory.Delete ValidDirPath
        
    [<Fact>]
    let ``Test local dir doesn't exists`` () =
        Assert.False(local.DirectoryExists ValidDirPath)
    
    [<Fact>]
    let ``Test local open success`` () =
        let stream = File.Create ValidFilePath
        stream.Close()
        let stream = local.Open ValidFilePath
        stream.Close()
        File.Delete ValidFilePath
    
    [<Fact>]
    let ``Test local file open failure`` () =
        Assert.ThrowsAny<Exception>(fun () -> local.Open InvalidFilePath |> ignore)
    
    [<Fact>]
    let ``Test local open or create success`` () =
        let stream = File.Create ValidFilePath
        stream.Close()
        let stream = local.OpenOrCreate ValidFilePath
        stream.Close()
        File.Delete ValidFilePath
        
    [<Fact>]
    let ``Test local file open or create failure`` () =
        Assert.ThrowsAny<Exception>(fun () -> local.OpenOrCreate InvalidFilePath |> ignore)
        
    [<Fact>]
    let ``Test local create dir success`` () =
        let _ = local.CreateDirectory ValidDirPath
        Directory.Delete ValidDirPath
        
    [<Fact>]
    let ``Test local create dir failure`` () =
        Assert.ThrowsAny<Exception>(fun () -> local.CreateDirectory InvalidDirPath)
