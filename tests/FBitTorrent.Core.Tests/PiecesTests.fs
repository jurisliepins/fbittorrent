namespace FBitTorrent.Core.Tests

open System
open System.Linq
open Akka.TestKit.Xunit2
open FBitTorrent.Core
open Xunit

type PiecesTests() = 
    inherit TestKit()
    
    let [<Literal>] OutputDir = "./"
    
    let createSingleFilePiecesState () =
        match Constants.singleFileMetaInfo.Info with
        | SingleFileInfo info -> 
            Pieces.createStateFromInfo OutputDir (info |> SingleFileInfo)
        | _ ->
            failwith "Should have loaded single file torrent"
        
    let createMultiFilePiecesState () =
        match Constants.multiFileMetaInfo.Info with
        | MultiFileInfo info -> 
            Pieces.createStateFromInfo OutputDir (info |> MultiFileInfo)
        | _ ->
            failwith "Should have loaded multi file torrent"
    
    [<Fact>]
    member _.``Test should create pieces state from single file info``() =
        let state = createSingleFilePiecesState ()
        Assert.Equal<Pieces.Status>(Pieces.Stopped, state.Status)
        Assert.Equal<int64>(0L, state.Downloaded)
        Assert.Equal<int64>(0L, state.Uploaded)
        Assert.Equal<int64>(Constants.singleFileInfo.Length, state.Left)
        Assert.Equal<int32>(Constants.singleFileInfo.PieceLength, state.PieceLength)
        Assert.Equal<Bitfield>(Bitfield(Constants.singleFileInfo.Pieces.Length), state.Bitfield)
        Assert.Equal<string>(OutputDir, state.OutputDir)
        Assert.Equal<string[]>([||], state.OutputSubDirs)
        Assert.Equal<int64>(Constants.singleFileInfo.Length, state.Pieces.Values.Sum(fun piece -> int64 piece.Length))
        for idx in 0..(state.Pieces.Count - 1) do
            match state.Pieces[idx] with
            | { Index  = index
                Hash   = hash
                Offset = offset
                Length = length
                Files  = files } ->
                Assert.Equal<int32>(idx, index)
                Assert.Equal<Hash>(Constants.singleFileInfo.Pieces[idx], hash)
                Assert.Equal<int64>(int64 Constants.singleFileInfo.PieceLength * int64 idx, offset)
                if idx < (state.Pieces.Count - 1) then
                    // All but last piece are expected to be of same length, which is the piece length. 
                    Assert.Equal<int32>(Constants.singleFileInfo.PieceLength, length)
                else
                    // Last piece can have different length unless file length evenly divides into piece length.
                    if Constants.singleFileInfo.Length % int64 Constants.singleFileInfo.PieceLength = 0L then
                        Assert.Equal(Constants.singleFileInfo.PieceLength, length)
                    else
                        Assert.Equal(Constants.singleFileInfo.Length % int64 Constants.singleFileInfo.PieceLength, length)
                Assert.Equal<int32>(1, files.Length)
                Assert.Equal<string>(Constants.SingleFileTorrentPath, files.First().Path)
                Assert.Equal<int64>(0L, files.First().Offset)
                Assert.Equal<int64>(Constants.singleFileInfo.Length, files.First().Length)
        Assert.Equal(0.0, state.DownRate.GetSpeed())
        Assert.Equal(0.0, state.UpRate.GetSpeed())
        
    [<Fact>]
    member _.``Test should create pieces state from multi file info``() =
        let state = createMultiFilePiecesState ()
        Assert.Equal<Pieces.Status>(Pieces.Stopped, state.Status)
        Assert.Equal<int64>(0L, state.Downloaded)
        Assert.Equal<int64>(0L, state.Uploaded)
        Assert.Equal<int64>(Constants.multiFileInfo.Files.Sum(fun file -> int64 file.Length), state.Left)
        Assert.Equal<int32>(Constants.multiFileInfo.PieceLength, state.PieceLength)
        Assert.Equal<Bitfield>(Bitfield(Constants.multiFileInfo.Pieces.Length), state.Bitfield)
        Assert.Equal<string>(OutputDir, state.OutputDir)
        Assert.Equal<string[]>([| Constants.multiFileInfo.Name |], state.OutputSubDirs)
        Assert.Equal<int64>(Constants.multiFileInfo.Files.Sum(fun file -> int64 file.Length), state.Pieces.Values.Sum(fun piece -> int64 piece.Length))
        // TODO: Check pieces and files correctness in a more generic way. Right now the values are hardcoded.
        match state.Pieces[0] with
        | { Index  = index
            Hash   = hash
            Offset = offset
            Length = length
            Files  = files } ->
            Assert.Equal<int32>(0, index)
            Assert.Equal<Hash>(Constants.multiFileInfo.Pieces[0], hash)
            Assert.Equal<int64>(0L, offset)
            Assert.Equal<int32>(Constants.multiFileInfo.PieceLength, length)
            Assert.Equal(4, files.Length)
            let paths = [| Constants.MultiFileTorrentPath1
                           Constants.MultiFileTorrentPath2
                           Constants.MultiFileTorrentPath3
                           Constants.MultiFileTorrentPath4 |]
            let offsets = [| 0L; 11682L; 19605L; 28356L |]
            let lengths = [| 11682L; 7923L; 8751L; 8180L |]
            for idx in 0..(files.Length - 1) do
                Assert.Equal(paths[idx], files[idx].Path)
                Assert.Equal(offsets[idx], files[idx].Offset)
                Assert.Equal(lengths[idx], files[idx].Length)
        match state.Pieces[1] with
        | { Index  = index
            Hash   = hash
            Offset = offset
            Length = length
            Files  = files } ->
            Assert.Equal<int32>(1, index)
            Assert.Equal<Hash>(Constants.multiFileInfo.Pieces[1], hash)
            Assert.Equal(int64 Constants.multiFileInfo.PieceLength * 1L, offset)
            Assert.Equal(Constants.multiFileInfo.Files.Sum(fun file -> int64 file.Length) % int64 Constants.multiFileInfo.PieceLength, length)
            Assert.Equal(1, files.Length)
            Assert.Equal(Constants.MultiFileTorrentPath4, files[0].Path)
            Assert.Equal(28356L, files[0].Offset)
            Assert.Equal(8180L, files[0].Length)
        Assert.Equal(0.0, state.DownRate.GetSpeed())
        Assert.Equal(0.0, state.UpRate.GetSpeed())
        
    [<Fact>]
    member _.``Test should byte buffer copy blocks success``() =
        let buffer = Pieces.ByteBuffer.create 100
        let copiedCount = Pieces.ByteBuffer.copy ((Array.create 50 0uy).Chunk(10).ToArray()) buffer
        Assert.Equal(50, copiedCount)
        
    [<Fact>]
    member _.``Test should byte buffer copy blocks failure``() =
        Assert.ThrowsAny<Exception>(fun () ->
            let buffer = Pieces.ByteBuffer.create 100
            let copiedCount = Pieces.ByteBuffer.copy ((Array.create 200 0uy).Chunk(10).ToArray()) buffer
            ())
        
    [<Fact>]
    member _.``Test should byte buffer try copy blocks success``() =
        let buffer = Pieces.ByteBuffer.create 100
        match Pieces.ByteBuffer.tryCopy ((Array.create 50 0uy).Chunk(10).ToArray()) buffer with
        | Error _ ->
            Assert.True(false, "Buffer copy should have succeed")
        | _ -> ()
        
    [<Fact>]
    member _.``Test should byte buffer try copy blocks failure``() =
        let buffer = Pieces.ByteBuffer.create 100
        match Pieces.ByteBuffer.tryCopy ((Array.create 200 0uy).Chunk(10).ToArray()) buffer with
        | Ok _ ->
            Assert.True(false, "Buffer copy should have failed")
        | _ -> ()