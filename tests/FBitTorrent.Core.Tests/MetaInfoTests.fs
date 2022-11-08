namespace FBitTorrent.Core.Tests

open System.IO
open System.Text
open Xunit
open FBitTorrent.Core

module MetaInfoTests =
    open MetaInfo
    
    let assertDecodedSingleFileMetaInfo (mi: MetaInfo) =
        match mi.Info with
        | SingleFileInfo info ->
            Assert.Equal<Hash>(Constants.singleFileMetaInfoHash, (singleFileInfoHash info))
        | _ ->
            Assert.False(true, "Should have been single file info")
    
    let assertDecodedMultiFileMetaInfo (mi: MetaInfo) =
        match mi.Info with
        | MultiFileInfo info ->
            Assert.Equal<Hash>(Constants.multiFileMetaInfoHash, (multiFileInfoHash info))
        | _ ->
            Assert.False(true, "Should have been multi file info")
    
    [<Fact>]
    let ``Test decode single file info and hash from bytes`` () =
        let mi = fromBytes Constants.singleFileMetaInfoBytes
        assertDecodedSingleFileMetaInfo mi

    [<Fact>]
    let ``Test decode single file info and hash from stream`` () =
        let mi = fromStream (new MemoryStream(Constants.singleFileMetaInfoBytes))
        assertDecodedSingleFileMetaInfo mi

    [<Fact>]
    let ``Test decode single file info and hash from string`` () =
        let mi = fromString (Encoding.Latin1.GetString(Constants.singleFileMetaInfoBytes))
        assertDecodedSingleFileMetaInfo mi

    [<Fact>]
    let ``Test encode/decode single file UTF-8 info from string`` () =
        let info =
            match Constants.singleFileMetaInfo.Info with
            | SingleFileInfo info ->
                { info with Name = "Название" } |> SingleFileInfo
            | MultiFileInfo _ ->
                failwith "Expected single-file info"
        let mi = 
            { Constants.singleFileMetaInfo with
                Info      = info 
                Comment   = Some "Комментарий"
                CreatedBy = Some "Пользователь" }
        let mi =
            fromString (toString mi)
        match Constants.singleFileMetaInfo.Info, mi.Info with
        |  SingleFileInfo exp, SingleFileInfo act ->
            Assert.Equal(exp.Length, act.Length)
            Assert.Equal(exp.MD5Sum, act.MD5Sum)
            Assert.Equal("Название", act.Name)
            Assert.Equal(exp.PieceLength, act.PieceLength)
            for p1, p2 in Seq.zip act.Pieces exp.Pieces do
                Assert.Equal(p1, p2)
            Assert.Equal(exp.Private, act.Private)
            Assert.Equal(Constants.singleFileMetaInfo.Announce, mi.Announce)
            Assert.Equal(Constants.singleFileMetaInfo.AnnounceList, mi.AnnounceList)
            Assert.Equal("Комментарий", mi.Comment.Value)
            Assert.Equal("Пользователь", mi.CreatedBy.Value)
            Assert.Equal(Constants.singleFileMetaInfo.CreationDate, mi.CreationDate)
            Assert.Equal(Constants.singleFileMetaInfo.Encoding, mi.Encoding)
        | _ ->
            Assert.False(true, "Should have been single file info")
    
    [<Fact>]
    let ``Test decode multi file info and hash from bytes`` () =
        let mi = fromBytes Constants.multiFileMetaInfoBytes
        assertDecodedMultiFileMetaInfo mi

    [<Fact>]
    let ``Test decode multi file info and hash from stream`` () =
        let mi = fromStream (new MemoryStream(Constants.multiFileMetaInfoBytes))
        assertDecodedMultiFileMetaInfo mi

    [<Fact>]
    let ``Test decode multi file info and hash from string`` () =
        let mi = fromString (Encoding.Latin1.GetString(Constants.multiFileMetaInfoBytes))
        assertDecodedMultiFileMetaInfo mi
            
    [<Fact>]
    let ``Test encode/decode multi file UTF-8 info from string`` () =
        let info =
            match Constants.multiFileMetaInfo.Info with
            | SingleFileInfo _ ->
                failwith "Expected multi-file info"
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
            { Constants.multiFileMetaInfo with
                Info      = info 
                Comment   = Some "Комментарий"
                CreatedBy = Some "Пользователь" }
        let mi =
            fromString (toString mi)
        match Constants.multiFileMetaInfo.Info, mi.Info with
        |  MultiFileInfo exp, MultiFileInfo act ->
            Assert.Equal(exp.PieceLength, act.PieceLength)
            for p1, p2 in Seq.zip act.Pieces exp.Pieces do
                Assert.Equal(p1, p2)
            Assert.Equal(exp.Private, act.Private)
            Assert.Equal("Название", act.Name)
            let mapFile (file: File) =
                file.Path
                |> List.iteri (fun idx path -> Assert.Equal($"%d{idx} - Путь", path))
            act.Files
            |> List.iter mapFile
            Assert.Equal(Constants.multiFileMetaInfo.Announce, mi.Announce)
            Assert.Equal(Constants.multiFileMetaInfo.AnnounceList, mi.AnnounceList)
            Assert.Equal("Комментарий", mi.Comment.Value)
            Assert.Equal("Пользователь", mi.CreatedBy.Value)
            Assert.Equal(Constants.multiFileMetaInfo.CreationDate, mi.CreationDate)
            Assert.Equal(Constants.multiFileMetaInfo.Encoding, mi.Encoding)
        | _ ->
            Assert.False(true, "Should have been multi file info")