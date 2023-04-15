namespace FBitTorrent.Core

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.IO
open System.Linq
open System.Security.Cryptography
open Akka.Actor
open Akka.FSharp
open FBitTorrent.Core

module Pieces =
    
    let [<Literal>] InitialPiecesRatio = 0.1
    
    type File =
        { Path:   string 
          Offset: int64
          Length: int64
          MD5Sum: string option }

    type Piece =
        { Index:  int
          Hash:   Hash
          Offset: int64
          Length: int
          Files:  ReadOnlyCollection<File> }
    
    let private computePieceOffset (index: int) (pieceLength: int) =
        int64 index * int64 pieceLength
    
    let private computePieceLength (index: int) (pieceLength: int) (length: int64) =
        let head = int64 index * int64 pieceLength
        let tail = int64 index * int64 pieceLength + int64 pieceLength
        // Length is the tail index - head index, unless tail is longer than the entire length of the file.
        if tail > length then int (length - head) else pieceLength 
    
    let private createFilesFromSingleFileInfo (sfi: SingleFileInfo) =
        let files =
             [| { Path   = sfi.Name
                  Offset = 0L
                  Length = sfi.Length
                  MD5Sum = sfi.MD5Sum } |]
             |> Array.AsReadOnly
        files
    
    let private createPiecesFromSingleFileInfo (sfi: SingleFileInfo) =
        sfi.Pieces
        |> Array.ofSeq
        |> Array.mapi (fun idx hash ->
            { Index  = idx
              Hash   = hash
              Offset = computePieceOffset idx sfi.PieceLength
              Length = computePieceLength idx sfi.PieceLength sfi.Length
              Files  = createFilesFromSingleFileInfo sfi })
        |> Array.AsReadOnly
    
    let private createFilesFromMultiFileInfo (mfi: MultiFileInfo) =
        let files, _ =
            mfi.Files
            |> Array.ofList
            |> Array.mapFold (fun offset file ->
                { Path   = Path.Combine(mfi.Name, Path.Combine(file.Path.ToArray()))
                  Offset = offset
                  Length = file.Length
                  MD5Sum = file.MD5Sum }, offset + file.Length) 0L
        files |> Array.AsReadOnly
    
    let private createPiecesFromMultiFileInfo (mfi: MultiFileInfo) =
        let files = createFilesFromMultiFileInfo mfi
        let length = files |> Seq.sumBy (fun file -> file.Length)
        mfi.Pieces
        |> Array.ofSeq
        |> Array.mapi (fun idx hash ->
            { Index  = idx
              Hash   = hash
              Offset = computePieceOffset idx mfi.PieceLength 
              Length = computePieceLength idx mfi.PieceLength length
              Files  = files
                       |> Array.ofSeq
                       |> Array.filter (fun file ->
                           // Keep only the files that this piece is contained in.
                           file.Offset + file.Length >= computePieceOffset idx mfi.PieceLength &&
                           file.Offset <= int64 (computePieceOffset idx mfi.PieceLength) +
                                          int64 (computePieceLength idx mfi.PieceLength length))
                       |> Array.AsReadOnly })
        |> Array.AsReadOnly
    
    let createFilesFromInfo (info: Info) =
        match info with
        | SingleFileInfo sfi -> createFilesFromSingleFileInfo sfi
        | MultiFileInfo  mfi -> createFilesFromMultiFileInfo mfi
    
    let createPiecesFromInfo (info: Info) =
        match info with
        | SingleFileInfo sfi -> createPiecesFromSingleFileInfo sfi
        | MultiFileInfo  mfi -> createPiecesFromMultiFileInfo mfi
    
    type State =
        { PendingPieces: Bitfield
          RunningPieces: Bitfield
          PieceSelector: BitfieldSelector
          Pieces:        ReadOnlyCollection<Piece>
          PeerBitfields: Dictionary<string, Bitfield> }
    
    let createState (bitfield: Bitfield) (pieces: ReadOnlyCollection<Piece>) =
        { PendingPieces = bitfield
                          |> Bitfield.createFromBitfield
                          |> Bitfield.notBits
          RunningPieces = Bitfield.create bitfield.Capacity
          PieceSelector = BitfieldSelector.create pieces.Count
          Pieces        = pieces
          PeerBitfields = Dictionary() }
    
    type Message =
        | PeerJoined            of PeerName: string * Bitfield: Bitfield
        | PeerLeft              of PeerName: string
        | PieceLeeched          of Id: int * Data: ByteBuffer
        | BitfieldBytesReceived of Bytes: byte[]
        | BitfieldBitReceived   of Bit: int
    
    type Request =
        | PieceToLeech
        
    type Response =
        | PieceToLeech of Id: int * Length: int
    
    type Notification =
        | PieceReceived of Id: int * Data: ByteBuffer
        | PieceFailed   of Id: int * Data: ByteBuffer * Error: Exception
        
    let actorName () = "pieces"
    
    let actorBody notifiedRef (initialState: State) (mailbox: Actor<obj>) =
        logDebug mailbox $"Initial state \n%A{initialState}"
        let rec receive (state: State) = actor {
            match! mailbox.Receive() with
            | :? Message as message ->
                return! handleMessage state message
            
            | :? Request as request ->
                return! handleRequest state request

            | message ->
                return! unhandled state message }

            and handleMessage (state: State) message =
                match message with
                | PeerJoined (peerName, bitfield) ->
                    if state.PeerBitfields.TryAdd(peerName, bitfield) then
                        logDebug mailbox $"Peer %s{peerName} bitfield added"
                    else
                        logError mailbox $"Peer %s{peerName} bitfield already exists"
                | PeerLeft peerName ->
                    match state.PeerBitfields.TryGetValue(peerName) with
                    | true, bitfield ->
                        state.PieceSelector |> BitfieldSelector.subtractBitfield bitfield
                        state.PeerBitfields.Remove(peerName) |> ignore
                    | _ ->
                        logError mailbox $"Peer %s{peerName} bitfield doesn't exist"
                | PieceLeeched (idx, data) ->
                    if Bitfield.getBit idx state.RunningPieces then
                        // Piece is running means that we're waiting for it to complete.
                        if state.Pieces[idx].Hash.Equals(SHA1.HashData(data.AsReadOnlySpan()) |> Hash) then
                            logDebug mailbox $"Leeched piece %d{idx} and hash is valid"
                            notifiedRef <! PieceReceived (idx, data)
                            state.RunningPieces |> Bitfield.setBit idx false
                        else
                            logDebug mailbox $"Leeched piece %d{idx} hash is invalid"
                            notifiedRef <! PieceFailed (idx, data, Exception($"Leeched piece %d{idx} hash is invalid"))
                    else
                        // Piece is currently not running means that multiple peers were leeching and one already successfully completed.
                        logDebug mailbox $"Leeched piece %d{idx} %A{data} but we already have it"
                | BitfieldBytesReceived bytes ->
                    match state.PeerBitfields.TryGetValue(mailbox.Context.Sender.Path.Name) with
                    | true, bitfield ->
                        bitfield |> Bitfield.setBytes bytes
                        state.PieceSelector |> BitfieldSelector.addBitfield bitfield
                    | _ ->
                        logError mailbox $"Failed to set bytes peer %s{mailbox.Context.Sender.Path.Name} bitfield doesn't exist"
                | BitfieldBitReceived idx ->
                    match state.PeerBitfields.TryGetValue(mailbox.Context.Sender.Path.Name) with
                    | true, bitfield ->
                        bitfield |> Bitfield.setBit idx true
                        state.PieceSelector |> BitfieldSelector.addBit idx
                    | _ ->
                        logError mailbox $"Failed to set bit peer %s{mailbox.Context.Sender.Path.Name} bitfield doesn't exist"
                receive state
            
            and handleRequest (state: State) request =
                match request with
                | Request.PieceToLeech ->
                    match state.PeerBitfields.TryGetValue(mailbox.Context.Sender.Path.Name) with
                    | true, bitfield -> 
                        logDebug mailbox $"Requested for piece to leech by %s{mailbox.Context.Sender.Path.Name}"
                        // First N percent of pieces we leech should not be rare pieces. When we're starting, we need to
                        // get up and running as soon as possible - picking rarest pieces can lead to us getting stuck
                        // and not having anything to seed to other peers. 
                        let pendingPieceSelector =
                            if float (state.PendingPieces.Capacity - state.PendingPieces.Count) / float state.PendingPieces.Capacity < InitialPiecesRatio then
                                BitfieldSelector.firstBit
                            else
                                BitfieldSelector.rarestBit
                        let runningPieceSelector = BitfieldSelector.firstBit
                        // Try to pick (rarest or first) piece that is in the pending pieces bitfield and in the peer bitfield
                        // (a piece that we want and one that the peer has).
                        match state.PieceSelector |> pendingPieceSelector state.PendingPieces bitfield with
                        | Some idx ->
                            logDebug mailbox $"Found pending piece %d{idx} for %s{mailbox.Context.Sender.Path.Name}"
                            mailbox.Context.Sender <! Response.PieceToLeech (state.Pieces[idx].Index, state.Pieces[idx].Length)
                            state.PendingPieces |> Bitfield.setBit idx false
                            state.RunningPieces |> Bitfield.setBit idx true
                        | None ->
                            // If no match was found then look in the running pieces bitfield. These are the pieces that
                            // have been given to other peers to download but have not completed yet or have failed to download.
                            //
                            // End-game:
                            //  The goal of this algorithm is to get unique pending pieces from peers first (no peer will download the
                            //  same piece), but once those have been exhausted - make idle peers swarm on problematic, running pieces.
                            match state.PieceSelector |> runningPieceSelector state.RunningPieces bitfield with
                            | Some idx ->
                                logDebug mailbox $"Found running piece %d{idx} for %s{mailbox.Context.Sender.Path.Name}"
                                mailbox.Context.Sender <! Response.PieceToLeech (state.Pieces[idx].Index, state.Pieces[idx].Length)
                            | _ ->
                                logDebug mailbox $"Found nothing for %s{mailbox.Context.Sender.Path.Name}"
                    | _ ->
                        logError mailbox $"Failed to find piece peer %s{mailbox.Context.Sender.Path.Name} bitfield doesn't exist"
                    receive state
                    
            and unhandled (state: State) message =
                mailbox.Unhandled(message)
                receive state
                    
        receive initialState
    
    let defaultActorBody notifiedRef initialState mailbox =
        actorBody notifiedRef initialState mailbox
        
    let spawn (actorFactory: IActorRefFactory) notifiedRef (initialState: State) =
        spawn actorFactory (actorName ()) (defaultActorBody notifiedRef initialState)
        
module PiecesExtensions =
    type IActorContext with
        member __.GetPieces() : IActorRef = __.Child(Pieces.actorName ())