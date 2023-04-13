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
        type Command = Read
        
        type ReadResult =
            | Success of Message
            | Failure of Exception
        
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
                           mailbox.Self <! Read
                           return Success message
                       with exn ->
                           return Failure (Exception("Failed to read from connection", exn)) }
                    ).PipeTo(ref) |> ignore
                    receive ()
            
            receive ()
    
    module Writer =
        type Command = Write of Message
        
        type WriteResult =
            | Failure of Exception
        
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
                    with exn ->
                        ref <! Failure (Exception("Failed to write to the connection", exn))
                    receive ()
            
            receive ()

    let [<Literal>] KeepAliveIntervalSec = 30.0
    
    let [<Literal>] MeasureRateIntervalSec = 1.0
    
    type State =
        { SelfBitfield:   Bitfield
          SelfChoking:    bool
          SelfInterested: bool
          PeerBitfield:   Bitfield
          PeerChoking:    bool
          PeerInterested: bool
          Downloaded:     int64
          Uploaded:       int64
          DownRate:       Rate
          UpRate:         Rate }

    let createState (selfBitfield: Bitfield) (peerBitfield: Bitfield) =
        { SelfBitfield   = selfBitfield
          SelfChoking    = true
          SelfInterested = false
          PeerBitfield   = peerBitfield
          PeerChoking    = true
          PeerInterested = false
          Downloaded     = 0L
          Uploaded       = 0L
          DownRate       = Rate.zero
          UpRate         = Rate.zero }
    
    type private LeechType =
        | FirstLeech
        | NextLeech
    
    type private Action =
        | Read
        | Leech of LeechType
        | KeepAlive
        | MeasureRate
    
    type Message =
        | PieceLeeched of Id: int
    
    type BitfieldChange =
        | BitfieldBytesChange of Bytes: byte[]
        | BitfieldBitChange   of Bit: int
    
    type Notification =
        | BitfieldChanged of Change: BitfieldChange
        | FlagsChanged of
            SelfChoking:    bool *
            SelfInterested: bool *
            PeerChoking:    bool *
            PeerInterested: bool
        | BytesChanged    of Downloaded: int64 * Uploaded: int64
        | RateChanged     of Down: Rate * Up: Rate
        | Failed          of Error: Exception
    
    let actorName (address: IPAddress) (port: int) = $"peer-%A{address}:%d{port}"
    
    let actorFn notifiedRef piecesRef (connection: IMessageConnection) (initialState: State) (mailbox: Actor<obj>) =
        logDebug mailbox $"Initial state \n%A{initialState}" 
        mailbox.Defer(connection.Dispose)
        let readerRef = spawn mailbox (Reader.actorName ()) (Reader.actorFn connection mailbox.Self)
        let writerRef = spawn mailbox (Writer.actorName ()) (Writer.actorFn connection mailbox.Self)
        let rec receive pipeline leechOpt (downMeter: RateMeter) (upMeter: RateMeter) (state: State) = actor {
            match! mailbox.Receive() with
            | :? Action as action ->
                return! handleAction pipeline leechOpt downMeter upMeter state action
            
            | :? Message as message ->
                return! handleMessage pipeline leechOpt downMeter upMeter state message
            
            | :? Pieces.Response as response ->
                return! handlePiecesResponse pipeline leechOpt downMeter upMeter state response
            
            | :? Reader.ReadResult as result ->
                return! handleReadResult pipeline leechOpt downMeter upMeter state result
            
            | :? Writer.WriteResult as result ->
                return! handleWriteResult pipeline leechOpt downMeter upMeter state result
                        
            | message ->
                return! unhandled pipeline leechOpt downMeter upMeter state message }
        
        and handleAction pipeline leechOpt downMeter upMeter (state: State) action =
            match action with
            | Read ->
                readerRef <! Reader.Read
                receive pipeline leechOpt downMeter upMeter state
            | Leech leechType ->
                match leechType with
                | FirstLeech ->
                    if not (Bitfield.isEmpty state.SelfBitfield) then
                        writerRef <! Writer.Write (BitfieldMessage (state.SelfBitfield.ToArray()))
                    if not (Bitfield.isFull state.SelfBitfield) then
                        writerRef <! Writer.Write InterestedMessage
                        let nextState = { state with SelfInterested = true }
                        notifiedRef <! FlagsChanged (nextState.SelfChoking, nextState.SelfInterested, nextState.PeerChoking, nextState.PeerInterested)
                        receive pipeline leechOpt downMeter upMeter nextState
                    else
                        receive pipeline leechOpt downMeter upMeter state
                | NextLeech ->
                    match leechOpt, state with
                    | Some (idx, requests, responses), { PeerChoking = false } ->
                        match BlockRequests.pop requests with 
                        | Some block when not (BlockPipeline.isBacklogFilled requests responses pipeline)->
                            writerRef <! Writer.Write (RequestMessage (idx, block.Beginning, block.Length))
                            mailbox.Self <! Leech NextLeech
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
                RateMeter.update state.Downloaded downMeter 
                RateMeter.update state.Uploaded upMeter 
                let nextState =
                    { state with
                        DownRate = RateMeter.average downMeter
                        UpRate   = RateMeter.average upMeter }
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
                    state.SelfBitfield |> Bitfield.setBit idx true
                    receive pipeline None downMeter upMeter state
                | _ ->
                    writerRef <! Writer.Write (HaveMessage idx)
                    state.SelfBitfield |> Bitfield.setBit idx true
                    receive pipeline leechOpt downMeter upMeter state
        
        and handlePiecesResponse pipeline leechOpt downMeter upMeter (state: State) response =
            match response with
            | Pieces.Response.PieceToLeech (idx, length) ->
                match leechOpt with
                | Some _ ->
                    receive pipeline leechOpt downMeter upMeter state
                | None ->
                    mailbox.Self <! Leech NextLeech
                    receive pipeline (Some (idx, BlockRequests.create length, BlockResponses.create length)) downMeter upMeter state

        and handleReadResult pipeline leechOpt downMeter upMeter (state: State) result =
            match result with
            | Reader.Success message ->
                match message with
                | KeepAliveMessage ->
                    receive pipeline leechOpt downMeter upMeter state
                
                | ChokeMessage ->
                    let nextState = { state with PeerChoking = true }
                    notifiedRef <! FlagsChanged (nextState.SelfChoking, nextState.SelfInterested, nextState.PeerChoking, nextState.PeerInterested)
                    receive pipeline leechOpt downMeter upMeter nextState
                | UnChokeMessage ->
                    let nextState = { state with PeerChoking = false }
                    notifiedRef <! FlagsChanged (nextState.SelfChoking, nextState.SelfInterested, nextState.PeerChoking, nextState.PeerInterested)
                    match leechOpt with
                    | Some (_, requests, responses) ->
                        // In this case we were in the process of leeching when we got choked. We reset the requests
                        // collection to make sure we don't miss any blocks we requested but never received and continue leeching. 
                        BlockRequests.reset requests responses
                        mailbox.Self <! Leech NextLeech
                    | None ->
                        // We were idle when we got un-choked (usually the first un-choke for this peer), so we need to
                        // request for a new piece to start leeching. 
                        piecesRef <! Pieces.Request.PieceToLeech
                    receive pipeline leechOpt downMeter upMeter nextState
                
                | InterestedMessage ->
                    let nextState = { state with PeerInterested = true }
                    notifiedRef <! FlagsChanged (nextState.SelfChoking, nextState.SelfInterested, nextState.PeerChoking, nextState.PeerInterested)
                    receive pipeline leechOpt downMeter upMeter nextState
                | NotInterestedMessage ->
                    let nextState = { state with PeerInterested = false }
                    notifiedRef <! FlagsChanged (nextState.SelfChoking, nextState.SelfInterested, nextState.PeerChoking, nextState.PeerInterested)
                    receive pipeline leechOpt downMeter upMeter nextState
                
                | HaveMessage idx ->
                    state.PeerBitfield |> Bitfield.setBit idx true
                    notifiedRef <! BitfieldChanged (BitfieldBitChange idx)
                    piecesRef <! Pieces.BitfieldBitReceived idx
                    // Some clients (Deluge for example) will send blank bitfields even when peer has the data and will instead send
                    // the rest as 'have' messages. This is called 'lazy bitfield' and is supposed to help with ISP filtering of the BitTorrent protocol.
                    // When we are in a state where we don't have an actively leeching piece and we are un-choked,
                    // that means we are getting a 'lazy bitfield' and need to ask for pieces as we receive 'have' messages.    
                    match leechOpt, state with
                    | None, { PeerChoking = false } ->
                        piecesRef <! Pieces.Request.PieceToLeech
                    | _ -> ()
                    receive pipeline leechOpt downMeter upMeter state
                
                | BitfieldMessage bytes ->
                    if Bitfield.isEmpty state.PeerBitfield then
                        state.PeerBitfield |> Bitfield.setBytes bytes
                        notifiedRef <! BitfieldChanged (BitfieldBytesChange bytes)
                        piecesRef <! Pieces.BitfieldBytesReceived bytes 
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
                            piecesRef <! Pieces.Request.PieceToLeech
                            receive (BlockPipeline.update block.Length pipeline) None downMeter upMeter nextState
                        elif BlockRequests.isAllRequested requests then
                            receive (BlockPipeline.update block.Length pipeline) leechOpt downMeter upMeter nextState
                        else
                            mailbox.Self <! Leech NextLeech
                            receive (BlockPipeline.update block.Length pipeline) leechOpt downMeter upMeter nextState
                    | _ ->
                        receive (BlockPipeline.update block.Length pipeline) leechOpt downMeter upMeter nextState
            
                | message ->
                    mailbox.Unhandled(message)
                    receive pipeline leechOpt downMeter upMeter state
                    
            | Reader.Failure error ->
                notifiedRef <! Notification.Failed (Exception("Peer failed to read", error))
                receive pipeline leechOpt downMeter upMeter state
            
        and handleWriteResult pipeline leechOpt downMeter upMeter (state: State) result =
            match result with
            | Writer.Failure error -> notifiedRef <! Notification.Failed (Exception("Peer failed to write", error))
            receive pipeline leechOpt downMeter upMeter state
        
        and unhandled pipeline leechOpt downMeter upMeter (state: State) message =
            mailbox.Unhandled(message)
            receive pipeline leechOpt downMeter upMeter state
        
        mailbox.Self <! Read
        mailbox.Self <! Leech FirstLeech
        mailbox.Self <! KeepAlive
        mailbox.Self <! MeasureRate
        
        receive (BlockPipeline.create ()) None (RateMeter.create ()) (RateMeter.create ()) initialState
            
    let defaultActorFn notifiedRef piecesRef connection initialState mailbox =
        actorFn notifiedRef piecesRef connection initialState mailbox

module PeerExtensions =
    type IActorContext with
        member __.GetPeer(address: IPAddress, port: int) : IActorRef = __.Child(Peer.actorName address port)
        member __.GetPeers() : IEnumerable<IActorRef> = __.GetChildren().Where(fun ref -> ref.Path.Name.StartsWith("peer"))