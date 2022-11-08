namespace FBitTorrent.Core.Tests

// open System
// open System.Collections.Concurrent
// open System.Collections.Generic
// open System.IO
// open System.Net
// open System.Linq
// open Akka.Actor
// open Akka.FSharp
// open Akka.TestKit.Xunit2
// open FBitTorrent.Core
// open FBitTorrent.Core.Tests
// open Xunit
//
// /// Messages we've read from the remote peer.
// type private RPeerMessage =
//     | RNothing
//
//     | RBitfieldLength
//     | RBitfieldId
//     | RBitfield
//
//     | RUnChokeLength
//     | RUnChokeId
//
//     | RPieceLength
//     | RPieceId
//     | RPieceIndex
//     | RPieceBegin
//     | RPieceBlock
//
// /// Message we've written to the remote peer.
// type private WPeerMessage =
//     | WNothing
//     | WMessage of Message
//
// /// Stream that returns requested blocks.
// type private BasicConnectionStream(bitfield: Bitfield, pieces: byte[][]) =
//     inherit MemoryStream()
//     
//     let r = ConcurrentStack<RPeerMessage>([| RNothing |])
//     let w = ConcurrentStack<WPeerMessage>([| WNothing |])
//     
//     override val WriteTimeout = 0 with get, set
//     override val ReadTimeout = 0 with get, set
//     
//     member _.R with get() = List<_>(r.Reverse())
//     member _.W with get() = List<_>(w.Reverse()) 
//         
//     override _.Read(buffer: byte[], offset: int, count: int) =
//         match r.First(), w.First(), count with
//         | RNothing, _, 4 ->
//             Array.Copy((Message.toBytes (BitfieldMessage (bitfield.ToArray())))
//                            .Take(4)
//                            .ToArray(),
//                 buffer, count)
//             r.Push RBitfieldLength
//         | RBitfieldId, _, _ ->
//             Array.Copy((Message.toBytes (BitfieldMessage (bitfield.ToArray())))
//                            .Skip(4 + 1)
//                            .ToArray(),
//                 buffer, bitfield.ToArray().Length)
//             r.Push RBitfield
//         | RBitfield, WMessage InterestedMessage, 4 -> 
//             // Don't un-choke until we've received an interested message.
//             Array.Copy((Message.toBytes UnChokeMessage)
//                            .Take(4)
//                            .ToArray(),
//                 buffer, count)
//             r.Push RUnChokeLength
//         | RUnChokeId,  WMessage (RequestMessage (idx, beg, length)), 4
//         | RPieceBlock, WMessage (RequestMessage (idx, beg, length)), 4 ->
//             Array.Copy((Message.toBytes (PieceMessage (idx, beg, pieces[idx]
//                                                                       .Skip(beg)
//                                                                       .Take(length)
//                                                                       .ToArray())))
//                            .Take(4)
//                            .ToArray(),
//                 buffer, count)
//             r.Push RPieceLength
//         | RPieceId, WMessage (RequestMessage (idx, beg, length)), 4 ->
//             Array.Copy((Message.toBytes (PieceMessage (idx, beg, pieces[idx]
//                                                                       .Skip(beg)
//                                                                       .Take(length)
//                                                                       .ToArray())))
//                            .Skip(4 + 1)
//                            .ToArray(),
//                 buffer, count)
//             r.Push RPieceIndex
//         | RPieceIndex, WMessage (RequestMessage (idx, beg, length)), 4 ->
//             Array.Copy((Message.toBytes (PieceMessage (idx, beg, pieces[idx]
//                                                                       .Skip(beg)
//                                                                       .Take(length)
//                                                                       .ToArray())))
//                            .Skip(4 + 1 + 4)
//                            .ToArray(),
//                 buffer, count)
//             r.Push RPieceBegin
//         | RPieceBegin, WMessage (RequestMessage (idx, beg, length)), _ ->
//             Array.Copy((Message.toBytes (PieceMessage (idx, beg, pieces[idx]
//                                                                       .Skip(beg)
//                                                                       .Take(length)
//                                                                       .ToArray())))
//                            .Skip(4 + 1 + 4 + 4)
//                            .ToArray(),
//                 buffer, count)
//             r.Push RPieceBlock
//             w.TryPop() |> ignore
//         | _ ->
//             Array.Copy((Message.toBytes KeepAliveMessage)
//                            .Take(4)
//                            .ToArray(),
//                 buffer, count)
//         count
//     
//     override _.ReadByte() =
//         match r.First(), w.First() with
//         | RBitfieldLength, _ -> r.Push RBitfieldId; int (BitfieldType.ToByte())
//         | RUnChokeLength,  _ -> r.Push RUnChokeId;  int (UnChokeType.ToByte())
//         | RPieceLength,
//           WMessage (RequestMessage _) -> r.Push RPieceId; int (PieceType.ToByte())
//         | _ ->
//             failwith "Invalid state"
//     
//     override __.Flush() =
//         let bytes = __.ToArray()
//                       .Take(int __.Position)
//                       .ToArray()
//         __.Seek(0, SeekOrigin.Begin) |> ignore
//         __.SetLength(0)
//         w.Push (WMessage (Message.fromBytes bytes))
//
// /// Stream that fails on read.        
// type private ReadFailureConnectionStream() =
//     inherit MemoryStream()
//     
//     override val WriteTimeout = 0 with get, set
//     override val ReadTimeout = 0 with get, set
//     
//     override _.Read(buffer: byte[], offset: int, count: int) = failwith "Test read failure"
//
// /// Stream that fails on write.    
// type private WriteFailureConnectionStream() =
//     inherit MemoryStream()
//     
//     override val WriteTimeout = 0 with get, set
//     override val ReadTimeout = 0 with get, set
//     
//     override _.Read(buffer: byte[], offset: int, count: int) = failwith "Test write failure"
//
// /// Stream that responds to the initial connection but doesn't return requested blocks.
// type private NoBlocksConnectionStream(bitfield: Bitfield) =
//     inherit MemoryStream()
//     
//     let r = ConcurrentStack<RPeerMessage>([| RNothing |])
//     let w = ConcurrentStack<WPeerMessage>([| WNothing |])
//     
//     override val WriteTimeout = 0 with get, set
//     override val ReadTimeout = 0 with get, set
//     
//     member _.R with get() = List<_>(r.Reverse())
//     member _.W with get() = List<_>(w.Reverse()) 
//         
//     override _.Read(buffer: byte[], offset: int, count: int) =
//         match r.First(), w.First(), count with
//         | RNothing, _, 4 ->
//             Array.Copy((Message.toBytes (BitfieldMessage (bitfield.ToArray())))
//                            .Take(4)
//                            .ToArray(),
//                 buffer, count)
//             r.Push RBitfieldLength
//         | RBitfieldId, _, _ ->
//             Array.Copy((Message.toBytes (BitfieldMessage (bitfield.ToArray())))
//                            .Skip(4 + 1)
//                            .ToArray(),
//                 buffer, bitfield.ToArray().Length)
//             r.Push RBitfield
//         | RBitfield, WMessage InterestedMessage, 4 -> 
//             // Don't un-choke until we've received an interested message.
//             Array.Copy((Message.toBytes UnChokeMessage)
//                            .Take(4)
//                            .ToArray(),
//                 buffer, count)
//             r.Push RUnChokeLength
//         | RUnChokeId,  WMessage (RequestMessage _), 4
//         | RPieceBlock, WMessage (RequestMessage _), 4 -> ()
//         | RPieceId,    WMessage (RequestMessage _), 4 -> ()
//         | RPieceIndex, WMessage (RequestMessage _), 4 -> ()
//         | RPieceBegin, WMessage (RequestMessage _), _ -> ()
//         | _ ->
//             Array.Copy((Message.toBytes KeepAliveMessage)
//                            .Take(4)
//                            .ToArray(),
//                 buffer, count)
//         count
//     
//     override _.ReadByte() =
//         match r.First(), w.First() with
//         | RBitfieldLength, _ -> r.Push RBitfieldId; int (BitfieldType.ToByte())
//         | RUnChokeLength,  _ -> r.Push RUnChokeId;  int (UnChokeType.ToByte())
//         | _ ->
//             failwith "Invalid state"
//     
//     override __.Flush() =
//         let bytes = __.ToArray()
//                       .Take(int __.Position)
//                       .ToArray()
//         __.Seek(0, SeekOrigin.Begin) |> ignore
//         __.SetLength(0)
//         w.Push (WMessage (Message.fromBytes bytes))
//         
// /// Stream that writes 2 bitfields.
// type private DoubleBitfieldConnectionStream(first: Bitfield, second: Bitfield) =
//     inherit MemoryStream()
//     
//     let r = ConcurrentStack<RPeerMessage>([| RNothing |])
//     let w = ConcurrentStack<WPeerMessage>([| WNothing |])
//
//     override val WriteTimeout = 0 with get, set
//     override val ReadTimeout = 0 with get, set
//     
//     member _.R with get() = List<_>(r.Reverse())
//     member _.W with get() = List<_>(w.Reverse())
//     
//     override _.Read(buffer: byte[], offset: int, count: int) =
//         match r.First(), w.First(), count with
//         | RNothing, _, 4 when not (r.Contains RBitfield) ->
//             Array.Copy((Message.toBytes (BitfieldMessage (first.ToArray())))
//                            .Take(4)
//                            .ToArray(),
//                 buffer, count)
//             r.Push RBitfieldLength
//         | RBitfieldId, _, _ when not (r.Contains RBitfield) ->
//             Array.Copy((Message.toBytes (BitfieldMessage (first.ToArray())))
//                            .Skip(4 + 1)
//                            .ToArray(),
//                 buffer, first.ToArray().Length)
//             r.Push RBitfield
//         | RBitfield, _, 4 when (r.Contains RBitfield) -> 
//             Array.Copy((Message.toBytes (BitfieldMessage (second.ToArray())))
//                            .Take(4)
//                            .ToArray(),
//                 buffer, count)
//             r.Push RBitfieldLength
//         | RBitfieldId, _, _ when (r.Contains RBitfield) ->
//             Array.Copy((Message.toBytes (BitfieldMessage (second.ToArray())))
//                            .Skip(4 + 1)
//                            .ToArray(),
//                 buffer, second.ToArray().Length)
//             r.Push RBitfield
//         | _ ->
//             Array.Copy((Message.toBytes KeepAliveMessage)
//                            .Take(4)
//                            .ToArray(),
//                 buffer, count)
//         count
//     
//     override _.ReadByte() =
//         match r.First(), w.First() with
//         | RBitfieldLength, _ -> r.Push RBitfieldId; int (BitfieldType.ToByte())
//         | RUnChokeLength,  _ -> r.Push RUnChokeId;  int (UnChokeType.ToByte())
//         | _ ->
//             failwith "Invalid state"
//     
//     override __.Flush() =
//         let bytes = __.ToArray()
//                       .Take(int __.Position)
//                       .ToArray()
//         __.Seek(0, SeekOrigin.Begin) |> ignore
//         __.SetLength(0)
//         w.Push (WMessage (Message.fromBytes bytes))
//     
// type PeerTests() = 
//     inherit TestKit()
//
//     let mockPeerState bitfield =
//         Peer.createState bitfield
//     
//     let mockConnection (stream: Stream) =
//         let reader = new ConnectionReader(stream)
//         let writer = new ConnectionWriter(stream)
//         { new IConnection with
//             member _.Stream with get() = stream 
//             member _.Reader with get() = reader
//             member _.Writer with get() = writer
//             member _.RemoteEndpoint with get() = IPEndPoint.Parse("127.0.0.1:6570")
//             member _.LocalEndpoint  with get() = IPEndPoint.Parse("127.0.0.1:9570")
//             member _.Disconnect() = ()
//             member _.Dispose()    = () }
//     
//     let mockPeerFn notifiedRef piecesRef connection initialState mailbox =
//         Peer.actorFn Constants.backlog notifiedRef piecesRef connection initialState mailbox 
//     
//     [<Fact>]
//     member __. ``Test should download single file torrent`` () =
//         // Pieces that the remote peer has - remote peer has all the pieces.
//         let remoteBitfield = Bitfield(Constants.singleFilePieces.Length)
//         remoteBitfield.Set(Array.create Constants.singleFilePieces.Length 255uy)
//         // Pieces that we have - last piece (we need this so that we send a bitfield to the remote peer).
//         let localBitfield = Bitfield(Constants.singleFilePieces.Length)
//         localBitfield.Set(Constants.singleFilePieces.Length - 1, true)
//                 
//         use connectionStream = new BasicConnectionStream(remoteBitfield, Constants.singleFilePieces)
//
//         let notifiedRef = __.CreateTestProbe()
//         let piecesRef = __.CreateTestProbe()
//         let peerRef = spawn __.Sys "peer"
//                           (mockPeerFn
//                                notifiedRef
//                                piecesRef
//                                (mockConnection (connectionStream :> Stream))
//                                (mockPeerState (Bitfield(Constants.singleFilePieces.Length)))) 
//         
//         // Actor must request for the current bitfield state from pieces actor.
//         piecesRef.ExpectMsg(Pieces.SelfBitfield) |> ignore
//         peerRef.Tell(Pieces.Response.SelfBitfield localBitfield)
//         
//         // Actor should request for a pieces to leech once it gets un-choked by the peer.
//         // Checking only the first 2 pieces - this is enough to verify that the flow is right.
//         for idx in 0..1 do
//             piecesRef.ExpectMsg(Pieces.Request.LeechPiece remoteBitfield) |> ignore
//             peerRef.Tell(Pieces.Response.LeechPiece (idx, Constants.singleFilePieces[idx].Length))
//             piecesRef.ExpectMsg(Pieces.LeechSuccess (idx, Constants.singleFilePieces[idx]
//                                                           |> Array.chunkBySize Constants.Block.BlockLength)) |> ignore
//         
//         // Actor should have written the following messages to the remote peer - bitfield, interested,
//         // followed by a stream of piece requests, which are verified implicitly in the loop above (receiving LeechSuccess
//         // indicates that we've requested and received pieces successfully).   
//         match connectionStream.W[0] with
//         | WNothing ->
//             ()
//         | _ -> Assert.True(false)
//         match connectionStream.W[1] with
//         | WMessage message ->
//             Assert.Equal(KeepAliveMessage, message)
//         | _ -> Assert.True(false)
//         match connectionStream.W[2] with
//         | WMessage message ->
//             Assert.Equal(BitfieldMessage (localBitfield.ToArray()), message)
//         | _ -> Assert.True(false)
//         match connectionStream.W[3] with
//         | WMessage message ->
//             Assert.Equal(InterestedMessage, message)
//         | _ -> Assert.True(false)
//         
//         // Remote peer should have written - bitfield and un-choke, followed by a stream of piece requests.
//         match connectionStream.R[0] with
//         | RNothing ->
//             ()
//         | _ -> Assert.True(false)
//         match connectionStream.R[1], connectionStream.R[2], connectionStream.R[3] with
//         | RBitfieldLength,
//           RBitfieldId,
//           RBitfield ->()
//         | _ -> Assert.True(false)
//         match connectionStream.R[4], connectionStream.R[5] with
//         | RUnChokeLength,
//           RUnChokeId -> ()
//         | _ -> Assert.True(false)
//     
//     [<Fact>]
//     member _.``Test should create initial peer state with expected values`` () =
//         let bitfield = Bitfield(0)
//         let initialState = Peer.createState bitfield
//         Assert.Equal(bitfield, initialState.Bitfield)
//         Assert.Equal(false, initialState.Established)
//         Assert.Equal(true, initialState.SelfChoking)
//         Assert.Equal(false, initialState.SelfInterested)
//         Assert.Equal(true, initialState.PeerChoking)
//         Assert.Equal(false, initialState.PeerInterested)
//         Assert.Equal(0L, initialState.Downloaded)
//         Assert.Equal(0L, initialState.Uploaded)
//         
//     [<Fact>]
//     member __. ``Test should pipeline requests`` () =
//         let remoteBitfield = Bitfield(Constants.backlog * 2)
//         remoteBitfield.Set(Array.create (Constants.backlog * 2) 255uy)
//         let localBitfield = Bitfield(Constants.backlog * 2)
//         localBitfield.Set(Constants.backlog * 2 - 1, true)
//         
//         let connectionStream = new NoBlocksConnectionStream(remoteBitfield)
//         
//         let notifiedRef = __.CreateTestProbe()
//         let piecesRef = __.CreateTestProbe()
//         let peerRef = spawn __.Sys "peer"
//                           (mockPeerFn
//                                notifiedRef
//                                piecesRef
//                                (mockConnection (connectionStream :> Stream))
//                                (mockPeerState (Bitfield(Constants.backlog * 2))))
//         
//         piecesRef.ExpectMsg(Pieces.Request.SelfBitfield) |> ignore
//         peerRef.Tell(Pieces.Response.SelfBitfield localBitfield)
//
//         piecesRef.ExpectMsg(Pieces.Request.LeechPiece remoteBitfield) |> ignore
//         peerRef.Tell(Pieces.Response.LeechPiece (0, Constants.Block.BlockLength * Constants.backlog * 2))
//
//         // Should have a backlog of X number of piece requests sent to the remote peer.
//         __.AwaitConditionAsync (fun _ ->
//             connectionStream.W
//                            .Where(fun w ->
//                                match w with
//                                | WMessage message ->
//                                    match message with RequestMessage _ -> true | _ -> false
//                                | _ -> false)
//                            .Count()
//                            .Equals Constants.backlog)
//         |> Async.AwaitTask
//         |> Async.RunSynchronously
//         
//         // Each of the backlogged requests should have expected index, beginning offset and length.
//         for i, (idx, beg, length) in connectionStream.W
//             |> Seq.map (fun w ->
//                 match w with
//                 | WMessage message ->
//                     match message with RequestMessage (idx, beg, length) -> Some ((idx, beg, length)) | _ -> None
//                 | _ -> None)
//             |> Seq.filter Option.isSome
//             |> Seq.mapi (fun i request -> (i, Option.get request)) do
//             Assert.Equal (0, idx)
//             Assert.Equal (i * Constants.Block.BlockLength, beg)
//             Assert.Equal (Constants.Block.BlockLength, length)
//         
//     [<Fact>]
//     member __. ``Test should send bitfield when local bitfield not empty`` () =
//         let remoteBitfield = Bitfield 10
//         remoteBitfield.Set(Array.create 10 255uy)
//         let localBitfield = Bitfield 10
//         localBitfield.Set(10 - 1, true)
//         
//         let connectionStream = new NoBlocksConnectionStream(remoteBitfield)
//         
//         let notifiedRef = __.CreateTestProbe()
//         let piecesRef = __.CreateTestProbe()
//         let peerRef = spawn __.Sys "peer"
//                           (mockPeerFn
//                                notifiedRef
//                                piecesRef
//                                (mockConnection (connectionStream :> Stream))
//                                (mockPeerState (Bitfield(Constants.backlog * 2))))
//         
//         piecesRef.ExpectMsg(Pieces.Request.SelfBitfield) |> ignore
//         peerRef.Tell(Pieces.Response.SelfBitfield localBitfield)
//         
//         __.AwaitConditionAsync (fun _ ->
//             connectionStream.W
//                            .Where(fun w ->
//                                match w with
//                                | WMessage message ->
//                                    match message with InterestedMessage -> true | _ -> false
//                                | _ -> false)
//                            .Any())
//         |> Async.AwaitTask
//         |> Async.RunSynchronously
//         
//         Assert.True(connectionStream.W
//                            .Where(fun w ->
//                                match w with
//                                | WMessage message ->
//                                    match message with BitfieldMessage _ -> true | _ -> false
//                                | _ -> false)
//                            .Any())
//         
//     [<Fact>]
//     member __. ``Test should not send bitfield when local bitfield empty`` () =
//         let remoteBitfield = Bitfield(10)
//         remoteBitfield.Set(Array.create 10 255uy)
//         let localBitfield = Bitfield(10)
//         
//         let connectionStream = new NoBlocksConnectionStream(remoteBitfield)
//         
//         let notifiedRef = __.CreateTestProbe()
//         let piecesRef = __.CreateTestProbe()
//         let peerRef = spawn __.Sys "peer"
//                           (mockPeerFn
//                                notifiedRef
//                                piecesRef
//                                (mockConnection (connectionStream :> Stream))
//                                (mockPeerState (Bitfield(Constants.backlog * 2))))
//         
//         piecesRef.ExpectMsg(Pieces.Request.SelfBitfield) |> ignore
//         peerRef.Tell(Pieces.Response.SelfBitfield localBitfield)
//         
//         __.AwaitConditionAsync (fun _ ->
//             connectionStream.W
//                            .Where(fun w ->
//                                match w with
//                                | WMessage message ->
//                                    match message with InterestedMessage -> true | _ -> false
//                                | _ -> false)
//                            .Any())
//         |> Async.AwaitTask
//         |> Async.RunSynchronously
//         
//         Assert.False(connectionStream.W
//                            .Where(fun w ->
//                                match w with
//                                | WMessage message ->
//                                    match message with BitfieldMessage _ -> true | _ -> false
//                                | _ -> false)
//                            .Any())
//         
//     [<Fact>]
//     member __. ``Test should send interested when local bitfield not full`` () =
//         let remoteBitfield = Bitfield(10)
//         remoteBitfield.Set(Array.create 10 255uy)
//         let localBitfield = Bitfield(10)
//         localBitfield.Set(10 - 1, true)
//         
//         let connectionStream = new NoBlocksConnectionStream(remoteBitfield)
//         
//         let notifiedRef = __.CreateTestProbe()
//         let piecesRef = __.CreateTestProbe()
//         let peerRef = spawn __.Sys "peer"
//                           (mockPeerFn
//                                notifiedRef
//                                piecesRef
//                                (mockConnection (connectionStream :> Stream))
//                                (mockPeerState (Bitfield(Constants.backlog * 2))))
//         
//         piecesRef.ExpectMsg(Pieces.Request.SelfBitfield) |> ignore
//         peerRef.Tell(Pieces.Response.SelfBitfield localBitfield)
//         
//         __.AwaitConditionAsync (fun _ ->
//             connectionStream.W
//                            .Where(fun w ->
//                                match w with
//                                | WMessage message ->
//                                    match message with BitfieldMessage _ -> true | _ -> false
//                                | _ -> false)
//                            .Any())
//         |> Async.AwaitTask
//         |> Async.RunSynchronously
//         
//         Assert.True(connectionStream.W
//                            .Where(fun w ->
//                                match w with
//                                | WMessage message ->
//                                    match message with InterestedMessage -> true | _ -> false
//                                | _ -> false)
//                            .Any())
//         
//     [<Fact>]
//     member __. ``Test should not send interested when local bitfield full`` () =
//         let remoteBitfield = Bitfield(10)
//         remoteBitfield.Set(Array.create 10 255uy)
//         let localBitfield = Bitfield(10)
//         localBitfield.Set(Array.create 10 255uy)
//         
//         let connectionStream = new NoBlocksConnectionStream(remoteBitfield)
//         
//         let notifiedRef = __.CreateTestProbe()
//         let piecesRef = __.CreateTestProbe()
//         let peerRef = spawn __.Sys "peer"
//                           (mockPeerFn
//                                notifiedRef
//                                piecesRef
//                                (mockConnection (connectionStream :> Stream))
//                                (mockPeerState (Bitfield(Constants.backlog * 2))))
//         
//         
//         piecesRef.ExpectMsg(Pieces.Request.SelfBitfield) |> ignore
//         peerRef.Tell(Pieces.Response.SelfBitfield localBitfield)
//         
//         __.AwaitConditionAsync (fun _ ->
//             connectionStream.W
//                            .Where(fun w ->
//                                match w with
//                                | WMessage message ->
//                                    match message with BitfieldMessage _ -> true | _ -> false
//                                | _ -> false)
//                            .Any())
//         |> Async.AwaitTask
//         |> Async.RunSynchronously
//         
//         Assert.False(connectionStream.W
//                            .Where(fun w ->
//                                match w with
//                                | WMessage message ->
//                                    match message with InterestedMessage -> true | _ -> false
//                                | _ -> false)
//                            .Any())
//         
//     [<Fact>]
//     member __. ``Test should send cancel when pending piece leeched from another peer`` () =
//         let remoteBitfield = Bitfield(10)
//         remoteBitfield.Set(Array.create 10 255uy)
//         let localBitfield = Bitfield(10)
//         localBitfield.Set(10 - 1, true)
//                 
//         use connectionStream = new NoBlocksConnectionStream(remoteBitfield)
//         
//         let notifiedRef = __.CreateTestProbe()
//         let piecesRef = __.CreateTestProbe()
//         let peerRef = spawn __.Sys "peer"
//                           (mockPeerFn
//                                notifiedRef
//                                piecesRef
//                                (mockConnection (connectionStream :> Stream))
//                                (mockPeerState (Bitfield(10))))
//
//         piecesRef.ExpectMsg(Pieces.Request.SelfBitfield) |> ignore
//         peerRef.Tell(Pieces.Response.SelfBitfield localBitfield)
//         
//         piecesRef.ExpectMsg(Pieces.Request.LeechPiece remoteBitfield) |> ignore
//         peerRef.Tell(Pieces.Response.LeechPiece (0, Constants.Block.BlockLength * 2))
//         
//         __.AwaitConditionAsync (fun _ ->
//             connectionStream.W
//                            .Where(fun w ->
//                                match w with
//                                | WMessage message ->
//                                    match message with RequestMessage _ -> true | _ -> false
//                                | _ -> false)
//                            .Count()
//                            .Equals 2)
//         |> Async.AwaitTask
//         |> Async.RunSynchronously
//         
//         peerRef.Tell(Peer.PieceLeeched 0)
//         
//         __.AwaitConditionAsync (fun _ ->
//             connectionStream.W
//                            .Where(fun w ->
//                                match w with
//                                | WMessage message ->
//                                    match message with CancelMessage _ -> true | _ -> false
//                                | _ -> false)
//                            .Count()
//                            .Equals 2)
//         |> Async.AwaitTask
//         |> Async.RunSynchronously
//         
//         match connectionStream.W[4], connectionStream.W[6] with
//         | WMessage (RequestMessage (ridx, rbeg, rlength)),
//           WMessage (CancelMessage  (cidx, cbeg, clength)) ->
//             Assert.Equal(ridx, cidx)
//             Assert.Equal(rbeg, cbeg)
//             Assert.Equal(rlength, clength)
//         | _ ->
//             Assert.True(false, "Should have written request and cancel messages")
//             
//         match connectionStream.W[5], connectionStream.W[7] with
//         | WMessage (RequestMessage (ridx, rbeg, rlength)),
//           WMessage (CancelMessage  (cidx, cbeg, clength)) ->
//             Assert.Equal(ridx, cidx)
//             Assert.Equal(rbeg, cbeg)
//             Assert.Equal(rlength, clength)
//         | _ ->
//             Assert.True(false, "Should have written request and cancel messages")
//         
//     [<Fact>]
//     member __. ``Test should not send cancel when not pending piece leeched from another peer`` () =
//         let remoteBitfield = Bitfield(10)
//         remoteBitfield.Set(Array.create 10 255uy)
//         let localBitfield = Bitfield(10)
//         localBitfield.Set(10 - 1, true)
//                 
//         use connectionStream = new NoBlocksConnectionStream(remoteBitfield)
//         
//         let notifiedRef = __.CreateTestProbe()
//         let piecesRef = __.CreateTestProbe()
//         let peerRef = spawn __.Sys "peer"
//                           (mockPeerFn
//                                notifiedRef
//                                piecesRef
//                                (mockConnection (connectionStream :> Stream))
//                                (mockPeerState (Bitfield(10))))
//
//         piecesRef.ExpectMsg(Pieces.Request.SelfBitfield) |> ignore
//         peerRef.Tell(Pieces.Response.SelfBitfield localBitfield)
//         
//         piecesRef.ExpectMsg(Pieces.Request.LeechPiece remoteBitfield) |> ignore
//         peerRef.Tell(Pieces.Response.LeechPiece (0, Constants.Block.BlockLength * 2))
//         
//         __.AwaitConditionAsync (fun _ ->
//             connectionStream.W
//                            .Where(fun w ->
//                                match w with
//                                | WMessage message ->
//                                    match message with RequestMessage _ -> true | _ -> false
//                                | _ -> false)
//                            .Count()
//                            .Equals 2)
//         |> Async.AwaitTask
//         |> Async.RunSynchronously
//         
//         peerRef.Tell(Peer.PieceLeeched 1)
//         
//         match connectionStream.W[4], connectionStream.W[5] with
//         | WMessage (RequestMessage _), WMessage (RequestMessage _) -> ()
//         | _ ->
//             Assert.True(false, "Should have written 2 request messages")
//     
//     [<Fact>]
//     member __. ``Test should notify on reader failure`` () =
//         // let notifiedRef = __.CreateTestProbe()
//         // let piecesRef = __.CreateTestProbe()
//         // let _ = spawn __.Sys "peer"
//         //             (mockPeerFn
//         //                  notifiedRef
//         //                  piecesRef
//         //                  (mockConnection ((new ReadFailureConnectionStream()) :> Stream))
//         //                  (mockPeerState (Bitfield(0))))
//         // let state = mockPeerState (Bitfield(0))
//         // notifiedRef.ExpectMsg(Peer.StateChanged { state with Established = true }) |> ignore
//         // notifiedRef.ExpectMsg(Peer.Failed "Peer failed - Failed to read from connection - Test read failure") |> ignore
//         ()
//
//     [<Fact>]
//     member __. ``Test should notify on writer failure`` () =
//         // let notifiedRef = __.CreateTestProbe()
//         // let piecesRef = __.CreateTestProbe()
//         // let _ = spawn __.Sys "peer"
//         //             (mockPeerFn
//         //                  notifiedRef
//         //                  piecesRef
//         //                  (mockConnection ((new WriteFailureConnectionStream()) :> Stream))
//         //                  (mockPeerState (Bitfield(0))))
//         // let state = mockPeerState (Bitfield(0))
//         // notifiedRef.ExpectMsg(Peer.StateChanged { state with Established = true }) |> ignore
//         // notifiedRef.ExpectMsg(Peer.Failed "Peer failed - Failed to read from connection - Test write failure") |> ignore
//         ()
//         
//     [<Fact>]
//     member __. ``Test should terminate on stop`` () =
//         let remoteBitfield = Bitfield(10)
//         remoteBitfield.Set(Array.create 10 255uy)
//         let localBitfield = Bitfield(10)
//         localBitfield.Set(10 - 1, true)
//                 
//         use connectionStream = new NoBlocksConnectionStream(remoteBitfield)
//         
//         let notifiedRef = __.CreateTestProbe()
//         let piecesRef = __.CreateTestProbe()
//         let peerRef = spawn __.Sys "peer"
//                           (mockPeerFn
//                                notifiedRef
//                                piecesRef
//                                (mockConnection (connectionStream :> Stream))
//                                (mockPeerState (Bitfield(10))))
//         
//         piecesRef.ExpectMsg(Pieces.Request.SelfBitfield) |> ignore
//         peerRef.Tell(Pieces.Response.SelfBitfield localBitfield)
//         
//         piecesRef.ExpectMsg(Pieces.Request.LeechPiece remoteBitfield) |> ignore
//         peerRef.Tell(Pieces.Response.LeechPiece (0, Constants.Block.BlockLength * 2))
//         
//         __.Sys.Stop peerRef
//         
//         let terminatedRef = __.CreateTestProbe()
//         terminatedRef.Watch peerRef |> ignore
//         terminatedRef.ExpectTerminated peerRef
//         
//     [<Fact>]
//     member _.``Test should equal expected peer ID value`` () =
//         let exp =
//             [| 45uy; 70uy; 88uy; 48uy; 46uy; 48uy; 49uy; 45uy
//                0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy |]
//             |> Id
//         Assert.Equal<IEnumerable<byte>>(exp.ToArray().Take(8), Constants.Peer.Id.Take(8))
