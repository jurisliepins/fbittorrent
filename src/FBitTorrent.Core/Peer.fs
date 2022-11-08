namespace FBitTorrent.Core

open System
open System.Collections.Generic
open System.Net
open System.Linq
open Akka.FSharp
open Akka.Actor
open FBitTorrent.Core

module Peer =
    module Reader =
        [<Struct>]
        type Command = Read
        
        [<Struct>]
        type ReadResult =
            | Success of Result: Message
            | Failure of Error:  Exception
        
        let actorName () = "reader"
        
        let actorFn (connection: IMessageConnection) ref (mailbox: Actor<obj>) =
            let rec receive () = actor {
                match! mailbox.Receive() with
                | :? Command as command ->
                    return! handleCommand command
                    
                | message ->
                    mailbox.Unhandled(message)
                    return! receive () }
            
            and handleCommand command =
                match command with
                | Read -> 
                    Async.StartAsTask(async {
                       try
                           let! message = connection.AsyncReadMessage()
                           return Success message
                       with exn ->
                           return Failure (Exception("Failed to read from connection", exn)) }
                    ).PipeTo(ref) |> ignore
                    receive ()
            
            receive ()
    
    module Writer =
        [<Struct>]
        type Command = Write of Message
        
        [<Struct>]
        type WriteResult =
            | Success
            | Failure of Error: Exception
        
        let actorName () = "writer"
        
        let actorFn (connection: IMessageConnection) ref (mailbox: Actor<obj>) =
            let rec receive () = actor {
                match! mailbox.Receive() with    
                | :? Command as command ->
                    return! handleCommand command
                    
                | message ->
                    mailbox.Unhandled(message)
                    return! receive () }
            
            and handleCommand command =
                match command with
                | Write message ->
                    try
                        connection.WriteMessage(message)
                        ref <! Success
                    with exn ->
                        ref <! Failure (Exception("Failed to write to the connection", exn))
                    receive ()
            
            receive ()

    let [<Literal>] KeepAliveIntervalSec = 30.0
    
    let [<Literal>] MeasureRateIntervalSec = 1.0
    
    type State =
        { Bitfield:       Bitfield
          SelfChoking:    bool
          SelfInterested: bool
          PeerChoking:    bool
          PeerInterested: bool
          Downloaded:     int64
          Uploaded:       int64
          DownRate:       Rate
          UpRate:         Rate }

    let createState (pieceCount: int) =
        { Bitfield       = Bitfield.create pieceCount
          SelfChoking    = true
          SelfInterested = false
          PeerChoking    = true
          PeerInterested = false
          Downloaded     = 0L
          Uploaded       = 0L
          DownRate       = Rate.zero
          UpRate         = Rate.zero }
    
    type Command =
        | Read
        | Leech of Bitfield option
        | KeepAlive
        | MeasureRate
    
    type Message =
        | PieceLeeched of int
    
    type BitfieldChange =
        | BitfieldBytesChange of byte[]
        | BitfieldBitChange   of int
    
    type Notification =
        | BitfieldChanged of BitfieldChange
        | FlagsChanged    of bool * bool * bool * bool
        | BytesChanged    of int64 * int64
        | RateChanged     of Rate * Rate
        | Failed          of Exception
    
    let actorName (address: IPAddress) (port: int) = $"peer-%A{address}:%d{port}"
    
    let actorFn notifiedRef piecesRef (connection: IMessageConnection) (initialState: State) (mailbox: Actor<obj>) =
        logDebug mailbox $"Initial state \n%A{initialState}" 
        mailbox.Defer(connection.Dispose)
        let readerRef = spawn mailbox (Reader.actorName ()) (Reader.actorFn connection mailbox.Self)
        let writerRef = spawn mailbox (Writer.actorName ()) (Writer.actorFn connection mailbox.Self)
        let rec receive pipeline leechOpt (downMeter: RateMeter) (upMeter: RateMeter) (state: State) = actor {
            match! mailbox.Receive() with
            | :? Command as command ->
                return! handleCommand pipeline leechOpt downMeter upMeter state command
            
            | :? Message as message ->
                return! handleMessage pipeline leechOpt downMeter upMeter state message
            
            | :? Pieces.Response as response ->
                return! handlePiecesResponse pipeline leechOpt downMeter upMeter state response
            
            | :? Reader.ReadResult as result ->
                return! handleReadResult pipeline leechOpt downMeter upMeter state result
            
            | :? Writer.WriteResult as result ->
                return! handleWriteResult pipeline leechOpt downMeter upMeter state result
                        
            | message ->
                mailbox.Unhandled(message)
                return! receive pipeline leechOpt downMeter upMeter state }
        
        and handleCommand pipeline leechOpt downMeter upMeter (state: State) command =
            match command with
            | Read ->
                readerRef <! Reader.Read
                receive pipeline leechOpt downMeter upMeter state
            | Leech bitfieldOpt ->
                match bitfieldOpt with
                | Some bitfield ->
                    if not (Bitfield.isEmpty bitfield) then
                        writerRef <! Writer.Write (BitfieldMessage (bitfield.ToArray()))
                    if not (Bitfield.isFull bitfield) then
                        writerRef <! Writer.Write InterestedMessage
                        let nextState = { state with SelfInterested = true }
                        notifiedRef <! FlagsChanged (nextState.SelfChoking, nextState.SelfInterested, nextState.PeerChoking, nextState.PeerInterested)
                        receive pipeline leechOpt downMeter upMeter nextState
                    else
                        receive pipeline leechOpt downMeter upMeter state
                | None ->
                    match leechOpt, state with
                    | Some (idx, requests, responses), { PeerChoking = false } ->
                        match BlockRequests.pop requests with 
                        | Some block when not (BlockPipeline.isBacklogFilled requests responses pipeline)->
                            writerRef <! Writer.Write (RequestMessage (idx, block.Beginning, block.Length))
                            mailbox.Self <! Leech None
                            receive pipeline leechOpt downMeter upMeter state
                        | Some block ->
                            writerRef <! Writer.Write (RequestMessage (idx, block.Beginning, block.Length))
                            receive pipeline leechOpt downMeter upMeter state
                        | None ->
                            receive pipeline leechOpt downMeter upMeter state
                    | _ ->
                        receive pipeline leechOpt downMeter upMeter state
            | KeepAlive ->
                writerRef <! Writer.Write KeepAliveMessage
                mailbox.Context.System.Scheduler.ScheduleTellOnce(TimeSpan.FromSeconds(KeepAliveIntervalSec), mailbox.Self, KeepAlive)
                receive pipeline leechOpt downMeter upMeter state
            | MeasureRate ->
                downMeter |> RateMeter.update state.Downloaded
                upMeter |> RateMeter.update state.Uploaded
                let nextState =
                    { state with
                        DownRate = downMeter |> RateMeter.average
                        UpRate   = upMeter |> RateMeter.average }
                notifiedRef <! RateChanged (nextState.DownRate, nextState.UpRate)
                mailbox.Context.System.Scheduler.ScheduleTellOnce(TimeSpan.FromSeconds(MeasureRateIntervalSec), mailbox.Self, MeasureRate)
                receive pipeline leechOpt downMeter upMeter nextState
        
        and handleMessage pipeline leechOpt downMeter upMeter (state: State) message =
            match message with
            | PieceLeeched idx ->
                match leechOpt with
                | Some (index, requests, _) when idx = index ->
                    for request in requests do
                        match request with
                        | BlockRequest.Requested { Beginning = beg; Length = length } ->
                            writerRef <! Writer.Write (CancelMessage (idx, beg, length))
                        | _ -> ()
                    writerRef <! Writer.Write (HaveMessage idx)
                    receive pipeline None downMeter upMeter state
                | _ ->
                    writerRef <! Writer.Write (HaveMessage idx)
                    receive pipeline leechOpt downMeter upMeter state
        
        and handlePiecesResponse pipeline leechOpt downMeter upMeter (state: State) response =
            match response with
            | Pieces.Response.LeechPiece (idx, length) ->
                match leechOpt with
                | Some _ ->
                    receive pipeline leechOpt downMeter upMeter state
                | None ->
                    mailbox.Self <! Leech None
                    receive pipeline (Some (idx, BlockRequests.create length, BlockResponses.create length)) downMeter upMeter state

        and handleReadResult pipeline leechOpt downMeter upMeter (state: State) result =
            match result with
            | Reader.Success message ->
                match message with
                | KeepAliveMessage ->
                    readerRef <! Reader.Read
                    receive pipeline leechOpt downMeter upMeter state
                
                | ChokeMessage ->
                    let nextState = { state with PeerChoking = true }
                    notifiedRef <! FlagsChanged (nextState.SelfChoking, nextState.SelfInterested, nextState.PeerChoking, nextState.PeerInterested)
                    readerRef <! Reader.Read
                    receive pipeline leechOpt downMeter upMeter nextState
                | UnChokeMessage ->
                    let nextState = { state with PeerChoking = false }
                    notifiedRef <! FlagsChanged (nextState.SelfChoking, nextState.SelfInterested, nextState.PeerChoking, nextState.PeerInterested)
                    match leechOpt with
                    | Some (_, requests, responses) ->
                        // In this case we were in the process of leeching when we got choked. We reset the requests
                        // collection to make sure we don't miss any blocks we requested but never received and continue leeching. 
                        BlockRequests.reset requests responses
                        mailbox.Self <! Leech None
                    | None ->
                        // We were idle when we got un-choked (usually the first un-choke for this peer), so we need to
                        // request for a new piece to start leeching. 
                        piecesRef <! Pieces.Request.LeechPiece
                    readerRef <! Reader.Read
                    receive pipeline leechOpt downMeter upMeter nextState
                
                | InterestedMessage ->
                    let nextState = { state with PeerInterested = true }
                    notifiedRef <! FlagsChanged (nextState.SelfChoking, nextState.SelfInterested, nextState.PeerChoking, nextState.PeerInterested)
                    readerRef <! Reader.Read
                    receive pipeline leechOpt downMeter upMeter nextState
                | NotInterestedMessage ->
                    let nextState = { state with PeerInterested = false }
                    notifiedRef <! FlagsChanged (nextState.SelfChoking, nextState.SelfInterested, nextState.PeerChoking, nextState.PeerInterested)
                    readerRef <! Reader.Read
                    receive pipeline leechOpt downMeter upMeter nextState
                
                | HaveMessage idx ->
                    state.Bitfield |> Bitfield.setBit idx true
                    notifiedRef <! BitfieldChanged (BitfieldBitChange idx)
                    piecesRef <! Pieces.BitfieldBitReceived idx
                    // Some clients (Deluge for example) will send blank bitfields even when peer has the data and will instead send
                    // the rest as 'have' messages. This is called 'lazy bitfield' and is supposed to help with ISP filtering of the BitTorrent protocol.
                    // When we are in a state where we don't have an actively leeching piece and we are un-choked,
                    // that means we are getting a 'lazy bitfield' and need to ask for pieces as we receive 'have' messages.    
                    match leechOpt, state with
                    | None, { PeerChoking = false } ->
                        piecesRef <! Pieces.Request.LeechPiece
                    | _ -> ()
                    readerRef <! Reader.Read
                    receive pipeline leechOpt downMeter upMeter state
                
                | BitfieldMessage bytes ->
                    if Bitfield.isEmpty state.Bitfield then
                        state.Bitfield |> Bitfield.setBytes bytes
                        notifiedRef <! BitfieldChanged (BitfieldBytesChange bytes)
                        piecesRef <! Pieces.BitfieldBytesReceived bytes 
                    readerRef <! Reader.Read
                    receive pipeline leechOpt downMeter upMeter state
                
                | PieceMessage (idx, beg, block) ->
                    let nextState =
                        { state with Downloaded = state.Downloaded + int64 block.Length }
                    notifiedRef <! BytesChanged (nextState.Downloaded, nextState.Uploaded)
                    match leechOpt with
                    | Some (index, requests, responses) when idx = index ->
                        BlockResponses.push { Beginning = beg; Data = block } responses
                        if BlockResponses.isAllResponded responses then
                            piecesRef <! Pieces.PieceLeeched (idx, BlockResponses.toPiece responses)
                            piecesRef <! Pieces.Request.LeechPiece
                            readerRef <! Reader.Read
                            receive (BlockPipeline.update block.Length pipeline) None downMeter upMeter nextState
                        elif BlockRequests.isAllRequested requests then
                            readerRef <! Reader.Read
                            receive (BlockPipeline.update block.Length pipeline) leechOpt downMeter upMeter nextState
                        else
                            mailbox.Self <! Leech None
                            readerRef <! Reader.Read
                            receive (BlockPipeline.update block.Length pipeline) leechOpt downMeter upMeter nextState
                    | _ ->
                        readerRef <! Reader.Read
                        receive (BlockPipeline.update block.Length pipeline) leechOpt downMeter upMeter nextState
            
                | message ->
                    mailbox.Unhandled(message)
                    readerRef <! Reader.Read
                    receive pipeline leechOpt downMeter upMeter state
                    
            | Reader.Failure error ->
                notifiedRef <! Notification.Failed (Exception("Peer failed to read", error))
                receive pipeline leechOpt downMeter upMeter state
            
        and handleWriteResult pipeline leechOpt downMeter upMeter (state: State) result =
            match result with
            | Writer.Success     _ -> ()
            | Writer.Failure error -> notifiedRef <! Notification.Failed (Exception("Peer failed to write", error))
            receive pipeline leechOpt downMeter upMeter state
        
        receive (BlockPipeline.create ()) None (RateMeter.create ()) (RateMeter.create ()) initialState
            
    let defaultActorFn notifiedRef piecesRef connection initialState mailbox =
        actorFn notifiedRef piecesRef connection initialState mailbox

module PeerExtensions =
    type IActorContext with
        member __.GetPeer(address: IPAddress, port: int) : IActorRef = __.Child(Peer.actorName address port)
        member __.GetPeers() : IEnumerable<IActorRef> = __.GetChildren().Where(fun ref -> ref.Path.Name.StartsWith("peer"))