namespace FBitTorrent.Core

open System.IO

type IFileSystem =
    abstract member FileExists: string -> bool
    abstract member DirectoryExists: string -> bool
    abstract member Open: string -> Stream
    abstract member OpenOrCreate: string -> Stream
    abstract member CreateDirectory: string -> unit

module FileSystem =
    let createLocal () =
        { new IFileSystem with
            member _.FileExists(path: string) = File.Exists(path)
            member _.DirectoryExists(path: string) = Directory.Exists(path)
            member _.Open(path: string) = File.Open(path, FileMode.Open) :> Stream
            member _.OpenOrCreate(path: string) = File.Open(path, FileMode.OpenOrCreate) :> Stream
            member _.CreateDirectory(path: string) = Directory.CreateDirectory(path) |> ignore }