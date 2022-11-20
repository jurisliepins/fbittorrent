namespace FBitTorrent.Core.Tests

open System.IO
open Xunit
open FBitTorrent.Core

module MetaInfoTests =
    open MetaInfo
    
    [<Fact>]
    let ``Test decode single file info and hash from bytes`` () =
        let metaInfo = fromBytes Constants.SingleFileMetaInfoBytes
        match metaInfo.Info with
        | SingleFileInfo info ->
            Assert.Equal<Hash>(Constants.SingleFileMetaInfoHash, (singleFileInfoHash info))
        | _ ->
            Assert.False(true, "Should have been single file info")

    [<Fact>]
    let ``Test decode single file info and hash from stream`` () =
        let metaInfo = fromStream (new MemoryStream(Constants.SingleFileMetaInfoBytes))
        match metaInfo.Info with
        | SingleFileInfo info ->
            Assert.Equal<Hash>(Constants.SingleFileMetaInfoHash, (singleFileInfoHash info))
        | _ ->
            Assert.False(true, "Should have been single file info")

    [<Fact>]
    let ``Test decode single file info and hash from string`` () =
        let metaInfo = fromString (DefaultEncoding.GetString(Constants.SingleFileMetaInfoBytes))
        match metaInfo.Info with
        | SingleFileInfo info ->
            Assert.Equal<Hash>(Constants.SingleFileMetaInfoHash, (singleFileInfoHash info))
        | _ ->
            Assert.False(true, "Should have been single file info")

    [<Fact>]
    let ``Test encode/decode single file UTF-8 info from string`` () =
        let info =
            match Constants.SingleFileMetaInfo.Info with
            | SingleFileInfo info ->
                { info with Name = "Название" } |> SingleFileInfo
            | MultiFileInfo _ -> failwith "Expected single-file info"
        let mi = 
            { Constants.SingleFileMetaInfo with
                Info      = info 
                Comment   = Some "Комментарий"
                CreatedBy = Some "Пользователь" }
        let mi = fromString (toString mi)
        match Constants.SingleFileMetaInfo.Info, mi.Info with
        |  SingleFileInfo exp, SingleFileInfo act ->
            Assert.Equal(exp.Length, act.Length)
            Assert.Equal(exp.MD5Sum, act.MD5Sum)
            Assert.Equal("Название", act.Name)
            Assert.Equal(exp.PieceLength, act.PieceLength)
            for p1, p2 in Array.zip act.Pieces exp.Pieces do
                Assert.Equal(p1, p2)
            Assert.Equal(exp.Private, act.Private)
            Assert.Equal(Constants.SingleFileMetaInfo.Announce, mi.Announce)
            Assert.Equal(Constants.SingleFileMetaInfo.AnnounceList, mi.AnnounceList)
            Assert.Equal("Комментарий", mi.Comment.Value)
            Assert.Equal("Пользователь", mi.CreatedBy.Value)
            Assert.Equal(Constants.SingleFileMetaInfo.CreationDate, mi.CreationDate)
            Assert.Equal(Constants.SingleFileMetaInfo.Encoding, mi.Encoding)
        | _ ->
            Assert.False(true, "Should have been single file info")
    
    [<Fact>]
    let ``Test decode multi file info and hash from bytes`` () =
        let metaInfo = fromBytes Constants.MultiFileMetaInfoBytes
        match metaInfo.Info with
        | MultiFileInfo info ->
            Assert.Equal<Hash>(Constants.MultiFileMetaInfoHash, (multiFileInfoHash info))
        | _ ->
            Assert.False(true, "Should have been multi file info")

    [<Fact>]
    let ``Test decode multi file info and hash from stream`` () =
        let metaInfo = fromStream (new MemoryStream(Constants.MultiFileMetaInfoBytes))
        match metaInfo.Info with
        | MultiFileInfo info ->
            Assert.Equal<Hash>(Constants.MultiFileMetaInfoHash, (multiFileInfoHash info))
        | _ ->
            Assert.False(true, "Should have been multi file info")

    [<Fact>]
    let ``Test decode multi file info and hash from string`` () =
        let metaInfo = fromString (DefaultEncoding.GetString(Constants.MultiFileMetaInfoBytes))
        match metaInfo.Info with
        | MultiFileInfo info ->
            Assert.Equal<Hash>(Constants.MultiFileMetaInfoHash, (multiFileInfoHash info))
        | _ ->
            Assert.False(true, "Should have been multi file info")
            
    [<Fact>]
    let ``Test encode/decode multi file UTF-8 info from string`` () =
        let info =
            match Constants.MultiFileMetaInfo.Info with
            | SingleFileInfo   _ -> failwith "Expected multi-file info"
            | MultiFileInfo info ->
                let mapFile (file: File) =
                    { Length = file.Length
                      MD5Sum = file.MD5Sum
                      Path   = file.Path |> List.mapi (fun idx _ -> $"%d{idx} - Путь") }
                { info with
                    Name  = "Название"
                    Files = info.Files |> List.map mapFile }
                |> MultiFileInfo
        let mi = 
            { Constants.MultiFileMetaInfo with
                Info      = info 
                Comment   = Some "Комментарий"
                CreatedBy = Some "Пользователь" }
        let mi = fromString (toString mi)
        match Constants.MultiFileMetaInfo.Info, mi.Info with
        |  MultiFileInfo exp, MultiFileInfo act ->
            Assert.Equal(exp.PieceLength, act.PieceLength)
            for p1, p2 in Array.zip act.Pieces exp.Pieces do
                Assert.Equal(p1, p2)
            Assert.Equal(exp.Private, act.Private)
            Assert.Equal("Название", act.Name)
            let mapFile (file: File) =
                file.Path
                |> List.iteri (fun idx path -> Assert.Equal($"%d{idx} - Путь", path))
            act.Files
            |> List.iter mapFile
            Assert.Equal(Constants.MultiFileMetaInfo.Announce, mi.Announce)
            Assert.Equal(Constants.MultiFileMetaInfo.AnnounceList, mi.AnnounceList)
            Assert.Equal("Комментарий", mi.Comment.Value)
            Assert.Equal("Пользователь", mi.CreatedBy.Value)
            Assert.Equal(Constants.MultiFileMetaInfo.CreationDate, mi.CreationDate)
            Assert.Equal(Constants.MultiFileMetaInfo.Encoding, mi.Encoding)
        | _ ->
            Assert.False(true, "Should have been multi file info")