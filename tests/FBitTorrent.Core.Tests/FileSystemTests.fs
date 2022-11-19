namespace FBitTorrent.Core.Tests

open System
open System.IO
open FBitTorrent.Core
open Xunit

module FileSystemTests =
    
    let validFilePath = ".\\test.txt"
    let validDirPath = ".\\test"
    
    let invalidFilePath = ""
    let invalidDirPath = ""
    
    let local = FileSystem.createLocal ()
    
    [<Fact>]
    let ``Test local file exists`` () =
        let stream = File.Create validFilePath
        stream.Close()
        Assert.True(local.FileExists validFilePath)
        File.Delete validFilePath
        
    [<Fact>]
    let ``Test local file doesn't exists`` () =
        Assert.False(local.FileExists validFilePath)
        File.Delete validFilePath
        
    [<Fact>]
    let ``Test local dir exists`` () =
        let _ = Directory.CreateDirectory validDirPath
        Assert.True(local.DirectoryExists validDirPath)
        Directory.Delete validDirPath
        
    [<Fact>]
    let ``Test local dir doesn't exists`` () =
        Assert.False(local.DirectoryExists validDirPath)
    
    [<Fact>]
    let ``Test local open or create success`` () =
        let stream = File.Create validFilePath
        stream.Close()
        let stream = local.OpenOrCreate validFilePath
        stream.Close()
        File.Delete validFilePath
        
    [<Fact>]
    let ``Test local file open or create failure`` () =
        Assert.ThrowsAny<Exception>(fun () -> local.OpenOrCreate invalidFilePath |> ignore)
        
    [<Fact>]
    let ``Test local create dir success`` () =
        let _ = local.CreateDirectory validDirPath
        Directory.Delete validDirPath
        
    [<Fact>]
    let ``Test local create dir failure`` () =
        Assert.ThrowsAny<Exception>(fun () -> local.CreateDirectory invalidDirPath)
