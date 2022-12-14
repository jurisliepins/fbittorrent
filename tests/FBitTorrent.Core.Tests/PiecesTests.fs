namespace FBitTorrent.Core.Tests

open System
open System.IO
open System.Linq
open Akka.FSharp
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

    let successFileSystem =
        { new IFileSystem with
            member _.FileExists(path: string) = true
            member _.DirectoryExists(path: string) = true
            member _.OpenOrCreate(path: string) = new MemoryStream() :> Stream
            member _.CreateDirectory(path: string) = () }
    
    let failureFileSystem =
        { new IFileSystem with
            member _.FileExists(path: string) = failwith "File system failed"
            member _.DirectoryExists(path: string) = failwith "File system failed"
            member _.OpenOrCreate(path: string) = failwith "File system failed"
            member _.CreateDirectory(path: string) = failwith "File system failed" }
    
    [<Fact>]
    member _.``Test should byte buffer copy blocks success``() =
        let buffer = ByteBuffer.create 100
        let copiedCount = ByteBuffer.copy ((Array.create 50 0uy).Chunk(10).ToArray()) buffer
        Assert.Equal(50, copiedCount)
        
    [<Fact>]
    member _.``Test should byte buffer copy blocks failure``() =
        Assert.ThrowsAny<Exception>(fun () ->
            let buffer = ByteBuffer.create 100
            let copiedCount = ByteBuffer.copy ((Array.create 200 0uy).Chunk(10).ToArray()) buffer
            ())
        
    [<Fact>]
    member _.``Test should byte buffer try copy blocks success``() =
        let buffer = ByteBuffer.create 100
        match ByteBuffer.tryCopy ((Array.create 50 0uy).Chunk(10).ToArray()) buffer with
        | Error _ ->
            Assert.True(false, "Buffer copy should have succeed")
        | _ -> ()
        
    [<Fact>]
    member _.``Test should byte buffer try copy blocks failure``() =
        let buffer = ByteBuffer.create 100
        match ByteBuffer.tryCopy ((Array.create 200 0uy).Chunk(10).ToArray()) buffer with
        | Ok _ ->
            Assert.True(false, "Buffer copy should have failed")
        | _ -> ()

    [<Fact>]
    member _.``Test should bitfields add``() =
        // TODO: Add!
        ()
        
    [<Fact>]
    member _.``Test should bitfields find first``() =
        // TODO: Add!
        ()
        
    [<Fact>]
    member _.``Test should bitfields find rarest``() =
        // TODO: Add!
        ()
          
    [<Fact>]
    member _.``Test should bitfields find``() =
        // TODO: Add!
        ()
            
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
    member _.``Test should file system write single file torrent success``() =
        // TODO: Add!
        ()
        
    [<Fact>]
    member _.``Test should file system write multi file torrent success``() =
        // TODO: Add!
        ()
        
    [<Fact>]
    member _.``Test should file system write torrent failure``() =
        // TODO: Add!
        ()
        
    [<Fact>]
    member _.``Test should file system write dir success``() =
        // TODO: Add!
        ()
        
    [<Fact>]
    member _.``Test should file system write dir failure``() =
        // TODO: Add!
        ()

    [<Fact>]
    member __.``Test should notify start status change``() =
        let initialState = createSingleFilePiecesState ()
        let initialState = { initialState with Status = Pieces.Status.Stopped }
        let notifiedRef = __.CreateTestProbe()
        let piecesRef = spawn __.Sys (Pieces.actorName ()) (Pieces.actorFn successFileSystem notifiedRef initialState)
        piecesRef.Tell(Pieces.Command.Start, notifiedRef)
        notifiedRef.ExpectMsg(Pieces.Notification.DirTreeWriteSuccess) |> ignore
        notifiedRef.ExpectMsg(Pieces.Notification.StateChanged { Status     = Pieces.Status.Started
                                                                 Downloaded = initialState.Downloaded
                                                                 Uploaded   = initialState.Uploaded
                                                                 Left       = initialState.Left
                                                                 DownSpeed  = 0.0
                                                                 UpSpeed    = 0.0 }) |> ignore
        
    [<Fact>]
    member __.``Test should notify stop status change``() =
        let initialState = createSingleFilePiecesState ()
        let initialState = { initialState with Status = Pieces.Status.Started }
        let notifiedRef = __.CreateTestProbe()
        let piecesRef = spawn __.Sys (Pieces.actorName ()) (Pieces.actorFn successFileSystem notifiedRef initialState)
        piecesRef.Tell(Pieces.Command.Stop, notifiedRef)
        notifiedRef.ExpectMsg(Pieces.Notification.StateChanged { Status     = Pieces.Status.Stopped
                                                                 Downloaded = initialState.Downloaded
                                                                 Uploaded   = initialState.Uploaded
                                                                 Left       = initialState.Left
                                                                 DownSpeed  = 0.0
                                                                 UpSpeed    = 0.0 }) |> ignore
        
    [<Fact>]
    member __.``Test should write directory tree success``() =
        let initialState = createSingleFilePiecesState ()
        let notifiedRef = __.CreateTestProbe()
        let piecesRef = spawn __.Sys (Pieces.actorName ()) (Pieces.actorFn successFileSystem notifiedRef initialState)
        piecesRef.Tell(Pieces.Command.Start, notifiedRef)
        notifiedRef.ExpectMsg<Pieces.Notification>(Pieces.Notification.DirTreeWriteSuccess) |> ignore
        notifiedRef.ExpectMsg(Pieces.Notification.StateChanged { Status     = Pieces.Status.Started
                                                                 Downloaded = initialState.Downloaded
                                                                 Uploaded   = initialState.Uploaded
                                                                 Left       = initialState.Left
                                                                 DownSpeed  = 0.0
                                                                 UpSpeed    = 0.0 }) |> ignore
        
    [<Fact>]
    member __.``Test should write directory tree failure``() =
        let initialState = createSingleFilePiecesState ()
        let notifiedRef = __.CreateTestProbe()
        let piecesRef = spawn __.Sys (Pieces.actorName ()) (Pieces.actorFn failureFileSystem notifiedRef initialState)
        piecesRef.Tell(Pieces.Command.Start, notifiedRef)
        notifiedRef.ExpectMsg<Pieces.Notification>(fun (notification: Pieces.Notification) ->
            match notification with
            | Pieces.Notification.DirTreeWriteFailure error ->
                error.Message.Equals("Failed to setup output directory tree")
            | _ -> false) |> ignore
        
    [<Fact>]
    member __.``Test should notify piece write success on leech success``() =
        let initialState = createSingleFilePiecesState ()
        let stream = new MemoryStream()
        let fileStream () = 
            { new MemoryStream() with
                member _.SetLength(count: int64) = ()
                member _.Seek(offset: int64, origin: SeekOrigin) = offset
                member _.Write(bytes: byte[], offset: int, count: int) = stream.Write(bytes, offset, count) }
        let fileSystem =
            { new IFileSystem with
                    member _.FileExists(path: string) = true
                    member _.DirectoryExists(path: string) = false
                    member _.OpenOrCreate(path: string) = fileStream ()
                    member _.CreateDirectory(path: string) = () }
        let notifiedRef = __.CreateTestProbe()
        let piecesRef = spawn __.Sys (Pieces.actorName ()) (Pieces.actorFn fileSystem notifiedRef initialState)
        let rec iterate (pieces: Pieces.Piece list) (state: Pieces.State) =
            match pieces with
            | [] -> ()
            | piece::pieces' ->
                let bitfield = Bitfield(initialState.Bitfield.Capacity)
                bitfield.Set(Array.create (initialState.Bitfield.ToArray().Length) 0uy)
                bitfield.Set(piece.Index, true)
                piecesRef.Tell(Pieces.Request.LeechPiece bitfield, notifiedRef)
                notifiedRef.ExpectMsg(Pieces.Response.LeechPiece ((piece.Index, piece.Length))) |> ignore
                piecesRef.Tell(Pieces.LeechSuccess (piece.Index, Constants.singleFilePieces[piece.Index]
                                                                 |> Array.chunkBySize Block.BlockLength), notifiedRef)
                notifiedRef.ExpectMsg(Pieces.PieceLeechSuccess piece.Index) |> ignore
                notifiedRef.ExpectMsg(Pieces.PieceWriteSuccess piece.Index) |> ignore
                let nextState =
                    { state with
                        Downloaded = state.Downloaded + int64 piece.Length
                        Left       = state.Left       - int64 piece.Length }
                state.Bitfield.Set(piece.Index, true)
                notifiedRef.ExpectMsg(Pieces.StateChanged (Pieces.NotificationState.Create(nextState))) |> ignore
                iterate pieces' nextState
        let pieces =
            initialState.Pieces.ToArray()
            |> Array.map (fun kv -> kv.Value)
            |> Array.toList
        iterate pieces initialState
        let exp = Constants.singleFilePieces |> Array.concat
        let act = stream.ToArray()
        Assert.Equal<byte[]>(exp, act)
        
    [<Fact>]
    member __.``Test should notify piece write failure on leech success``() =
        let initialState = createSingleFilePiecesState ()
        let fileStream () = 
            { new MemoryStream() with
                member _.SetLength(count: int64) = failwith "File stream failed"
                member _.Seek(offset: int64, origin: SeekOrigin) = failwith "File stream failed"
                member _.Write(bytes: byte[], offset: int, count: int) = failwith "File stream failed" }
        let fileSystem =
            { new IFileSystem with
                    member _.FileExists(path: string) = true
                    member _.DirectoryExists(path: string) = false
                    member _.OpenOrCreate(path: string) = fileStream ()
                    member _.CreateDirectory(path: string) = () }
        let notifiedRef = __.CreateTestProbe()
        let piecesRef = spawn __.Sys (Pieces.actorName ()) (Pieces.actorFn fileSystem notifiedRef initialState)
        let rec iterate (pieces: Pieces.Piece list) =
            match pieces with
            | [] -> ()
            | piece::pieces' ->
                let bitfield = Bitfield(initialState.Bitfield.Capacity)
                bitfield.Set(Array.create (initialState.Bitfield.ToArray().Length) 0uy)
                bitfield.Set(piece.Index, true)
                piecesRef.Tell(Pieces.Request.LeechPiece bitfield, notifiedRef)
                notifiedRef.ExpectMsg(Pieces.Response.LeechPiece ((piece.Index, piece.Length))) |> ignore
                piecesRef.Tell(Pieces.LeechSuccess (piece.Index, Constants.singleFilePieces[piece.Index]
                                                                 |> Array.chunkBySize Block.BlockLength), notifiedRef)
                notifiedRef.ExpectMsg<Pieces.Notification>(Pieces.PieceLeechSuccess piece.Index) |> ignore
                notifiedRef.ExpectMsg<Pieces.Notification>(fun (notification: Pieces.Notification) ->
                    match notification with
                    | Pieces.Notification.PieceWriteFailure (idx, error) -> error.Message.Equals($"Failed to write piece %d{idx}")
                    | _ -> false) |> ignore
                iterate pieces'
        let pieces =
            initialState.Pieces.ToArray()
            |> Array.map (fun kv -> kv.Value)
            |> Array.toList
        iterate pieces
        
    [<Fact>]
    member __.``Test should notify piece buffer copy failure on leech success``() =
        let initialState = createSingleFilePiecesState ()
        let initialState = { initialState with PieceLength = 0 }
        let fileStream () = 
            { new MemoryStream() with
                member _.SetLength(count: int64) = failwith "File stream failed"
                member _.Seek(offset: int64, origin: SeekOrigin) = failwith "File stream failed"
                member _.Write(bytes: byte[], offset: int, count: int) = failwith "File stream failed" }
        let fileSystem =
            { new IFileSystem with
                    member _.FileExists(path: string) = true
                    member _.DirectoryExists(path: string) = false
                    member _.OpenOrCreate(path: string) = fileStream ()
                    member _.CreateDirectory(path: string) = () }
        let notifiedRef = __.CreateTestProbe()
        let piecesRef = spawn __.Sys (Pieces.actorName ()) (Pieces.actorFn fileSystem notifiedRef initialState)
        let rec iterate (pieces: Pieces.Piece list) =
            match pieces with
            | [] -> ()
            | piece::pieces' ->
                let bitfield = Bitfield(initialState.Bitfield.Capacity)
                bitfield.Set(Array.create (initialState.Bitfield.ToArray().Length) 0uy)
                bitfield.Set(piece.Index, true)
                piecesRef.Tell(Pieces.Request.LeechPiece bitfield, notifiedRef)
                notifiedRef.ExpectMsg(Pieces.Response.LeechPiece ((piece.Index, piece.Length))) |> ignore
                piecesRef.Tell(Pieces.LeechSuccess (piece.Index, Constants.singleFilePieces[piece.Index]
                                                                 |> Array.chunkBySize Block.BlockLength), notifiedRef)
                notifiedRef.ExpectMsg<Pieces.Notification>(Pieces.PieceLeechSuccess piece.Index) |> ignore
                notifiedRef.ExpectMsg<Pieces.Notification>(fun (notification: Pieces.Notification) ->
                    match notification with
                    | Pieces.Notification.PieceWriteFailure (idx, error) -> error.Message.Equals($"Failed to copy received blocks into the buffer for piece %d{idx}")
                    | _ -> false) |> ignore
                iterate pieces'
        let pieces =
            initialState.Pieces.ToArray()
            |> Array.map (fun kv -> kv.Value)
            |> Array.toList
        iterate pieces
        
    [<Fact>]
    member __.``Test should notify on leech failure``() =
        let initialState = createSingleFilePiecesState ()
        let initialState = { initialState with PieceLength = 0 }
        let fileStream () = 
            { new MemoryStream() with
                member _.SetLength(count: int64) = failwith "File stream failed"
                member _.Seek(offset: int64, origin: SeekOrigin) = failwith "File stream failed"
                member _.Write(bytes: byte[], offset: int, count: int) = failwith "File stream failed" }
        let fileSystem =
            { new IFileSystem with
                    member _.FileExists(path: string) = true
                    member _.DirectoryExists(path: string) = false
                    member _.OpenOrCreate(path: string) = fileStream ()
                    member _.CreateDirectory(path: string) = () }
        let notifiedRef = __.CreateTestProbe()
        let piecesRef = spawn __.Sys (Pieces.actorName ()) (Pieces.actorFn fileSystem notifiedRef initialState)
        let rec iterate (pieces: Pieces.Piece list) =
            match pieces with
            | [] -> ()
            | piece::pieces' ->
                let bitfield = Bitfield(initialState.Bitfield.Capacity)
                bitfield.Set(Array.create (initialState.Bitfield.ToArray().Length) 0uy)
                bitfield.Set(piece.Index, true)
                piecesRef.Tell(Pieces.Request.LeechPiece bitfield, notifiedRef)
                notifiedRef.ExpectMsg(Pieces.Response.LeechPiece ((piece.Index, piece.Length))) |> ignore
                piecesRef.Tell(Pieces.LeechFailure (piece.Index, Exception("Leeching failed")), notifiedRef)
                notifiedRef.ExpectMsg<Pieces.Notification>(fun (notification: Pieces.Notification) ->
                    match notification with
                    | Pieces.Notification.PieceLeechFailure (_, error) -> error.Message.Equals("Leeching failed")
                    | _ -> false) |> ignore
                iterate pieces'
        let pieces =
            initialState.Pieces.ToArray()
            |> Array.map (fun kv -> kv.Value)
            |> Array.toList
        iterate pieces