namespace FBitTorrent.Core

open System
open System.Collections.Generic
open System.Net
open System.Linq
open System.Text
open Akka.FSharp
open Akka.Actor
open FBitTorrent.Core

module Peer =
    module Stream =
        type State =
            { Placeholder: unit }
        
        let createState () =
            { Placeholder = () }
        
        type private Action =
            | ReadHandshake
            | ReadMessage
        
        type Command =
            | WriteHandshake of Handshake: Handshake
            | WriteMessage   of Message: Message
        
        type CommandResult =
            | WriteFailure of Error: Exception
        
        type Notification =
            | ReceivedHandshake of Handshake: Handshake
            | ReceivedMessage   of Message: Message
            | Failed            of Error: Exception
        
        let actorName () = "stream"
        
        let actorBody notifiedRef (connection: IConnection) (initialState: State) (mailbox: Actor<obj>) =
            mailbox.Defer(connection.Dispose)
            let rec receive (state: State) = actor {
                match! mailbox.Receive() with
                | :? Action as action ->
                    return! handleAction state action
                
                | :? Command as command ->
                    return! handleCommand state command
                    
                | message ->
                    return! unhandled state message }
            
            and handleAction (state: State) action = 
                match action with
                | ReadHandshake -> 
                    Async.StartAsTask(async {
                        try
                            let! handshake = Handshake.asyncRead connection.Reader
                            mailbox.Self <! ReadMessage
                            return ReceivedHandshake handshake
                        with exn ->
                            return Failed (Exception("Failed to read handshake", exn))
                    }).PipeTo(notifiedRef) |> ignore
                | ReadMessage -> 
                    Async.StartAsTask(async {
                        try
                            let! message = Message.asyncRead connection.Reader
                            mailbox.Self <! ReadMessage
                            return ReceivedMessage message
                        with exn ->
                            return Failed (Exception("Failed to read message", exn))
                    }).PipeTo(notifiedRef) |> ignore
                receive state
            
            and handleCommand (state: State) command =
                match command with
                | WriteHandshake handshake ->
                    try
                        Handshake.write connection.Writer handshake
                    with exn ->
                        mailbox.Context.Sender <! WriteFailure (Exception("Failed to write handshake", exn))
                | WriteMessage message ->
                    try
                        Message.write connection.Writer message
                    with exn ->
                        mailbox.Context.Sender <! WriteFailure (Exception("Failed to write message", exn))
                receive state
                
            and unhandled (state: State) message =
                mailbox.Unhandled(message)
                receive state 
            
            mailbox.Self <! ReadHandshake
            
            receive initialState
            
        let defaultActorBody notifiedRef (connection: IConnection) (initialState: State) (mailbox: Actor<obj>) =
            actorBody notifiedRef connection initialState mailbox
            
        let spawn (actorFactory: IActorRefFactory) notifiedRef connection (initialState: State) =
            spawn actorFactory (actorName ()) (defaultActorBody notifiedRef connection initialState)
    
    module StreamExtensions =
        type IActorContext with
            member __.GetStream() : IActorRef = __.Child(Stream.actorName ())
    
    let [<Literal>] KeepAliveIntervalSec = 30.0
    
    let [<Literal>] MeasureRateIntervalSec = 1.0
    
    type State =
        { SelfHandshake:  Handshake
          SelfBitfield:   Bitfield
          SelfChoking:    bool
          SelfInterested: bool
          PeerHandshake:  Handshake option
          PeerBitfield:   Bitfield
          PeerChoking:    bool
          PeerInterested: bool
          Downloaded:     int64
          Uploaded:       int64
          DownRate:       Rate
          UpRate:         Rate }

    let createState (selfHandshake: Handshake) (selfBitfield: Bitfield) =
        { SelfHandshake  = selfHandshake
          SelfBitfield   = selfBitfield
          SelfChoking    = true
          SelfInterested = false
          PeerHandshake  = None  
          PeerBitfield   = Bitfield.create selfBitfield.Capacity
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
        | Leech of LeechType
        | KeepAlive
        | MeasureRate
    
    type Command =
        | InitiateHandshake
    
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
    
    let actorName (endpoint: IPEndPoint) = $"peer-%A{endpoint.Address}:%d{endpoint.Port}"
    
    let actorBody notifiedRef piecesRef (connection: IConnection) (initialState: State) (mailbox: Actor<obj>) =
        logDebug mailbox $"Initial state \n%A{initialState}" 
        let streamRef = Stream.spawn mailbox mailbox.Self connection (Stream.createState ())
        let rec receive pipeline leechOpt (downMeter: RateMeter) (upMeter: RateMeter) (state: State) = actor {
            match! mailbox.Receive() with
            | :? Action as action ->
                return! handleAction pipeline leechOpt downMeter upMeter state action
            
            | :? Command as command ->
                return! handleCommand pipeline leechOpt downMeter upMeter state command
            
            | :? Message as message ->
                return! handleMessage pipeline leechOpt downMeter upMeter state message
            
            | :? Pieces.Response as response ->
                return! handlePiecesResponse pipeline leechOpt downMeter upMeter state response
            
            | :? Stream.CommandResult as result ->
                return! handleStreamCommandResult pipeline leechOpt downMeter upMeter state result
            
            | :? Stream.Notification as notification ->
                return! handleStreamNotification pipeline leechOpt downMeter upMeter state notification
                        
            | message ->
                return! unhandled pipeline leechOpt downMeter upMeter state message }
        
        and handleAction pipeline leechOpt downMeter upMeter (state: State) action =
            match action with
            | Leech leechType ->
                match leechType with
                | FirstLeech ->
                    if not (Bitfield.isEmpty state.SelfBitfield) then
                        streamRef <! Stream.WriteMessage (BitfieldMessage (state.SelfBitfield.ToArray()))
                    if not (Bitfield.isFull state.SelfBitfield) then
                        streamRef <! Stream.WriteMessage InterestedMessage
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
                            streamRef <! Stream.WriteMessage (RequestMessage (idx, block.Beginning, block.Length))
                            mailbox.Self <! Leech NextLeech
                            receive pipeline leechOpt downMeter upMeter state
                        | Some block ->
                            streamRef <! Stream.WriteMessage (RequestMessage (idx, block.Beginning, block.Length))
                            receive pipeline leechOpt downMeter upMeter state
                        | None ->
                            receive pipeline leechOpt downMeter upMeter state
                    | _ ->
                        receive pipeline leechOpt downMeter upMeter state
            | KeepAlive ->
                streamRef <! Stream.WriteMessage KeepAliveMessage
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
        
        and handleCommand pipeline leechOpt downMeter upMeter (state: State) command =
            match command with
            | InitiateHandshake ->
                streamRef <! Stream.WriteHandshake state.SelfHandshake
                receive pipeline leechOpt downMeter upMeter state
        
        and handleMessage pipeline leechOpt downMeter upMeter (state: State) message =
            match message with
            | PieceLeeched idx ->
                match leechOpt with
                | Some (index, requests, _) when idx = index ->
                    for request in requests do
                        match request with
                        | BlockRequest.Requested { Beginning = beg; Length = length } ->
                            streamRef <! Stream.WriteMessage (CancelMessage (idx, beg, length))
                        | _ -> ()
                    streamRef <! Stream.WriteMessage (HaveMessage idx)
                    state.SelfBitfield |> Bitfield.setBit idx true
                    receive pipeline None downMeter upMeter state
                | _ ->
                    streamRef <! Stream.WriteMessage (HaveMessage idx)
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

        and handleStreamCommandResult pipeline leechOpt downMeter upMeter (state: State) result =
            match result with
            | Stream.WriteFailure error ->
                notifiedRef <! Notification.Failed (Exception("Peer failed to write", error))
            receive pipeline leechOpt downMeter upMeter state
        
        and handleStreamNotification pipeline leechOpt downMeter upMeter (state: State) notification =
            match notification with
            | Stream.ReceivedHandshake peerHandshake ->
                let isProtocolValid (selfProtocol: byte[]) (peerProtocol: byte[]) = Enumerable.SequenceEqual(selfProtocol, peerProtocol)
                let isInfoHashValid (selfInfoHash: byte[]) (peerInfoHash: byte[]) = Enumerable.SequenceEqual(selfInfoHash, peerInfoHash)
                match (state.SelfHandshake, peerHandshake) with
                | Handshake (selfProtocol, _, _, _), Handshake (peerProtocol, _, _, _) when not (isProtocolValid selfProtocol peerProtocol) ->
                    notifiedRef <! Notification.Failed (Exception($"Failed to handshake invalid protocol (expected: %s{Encoding.ASCII.GetString(selfProtocol)}, received: %s{Encoding.ASCII.GetString(peerProtocol)})"))
                    receive pipeline leechOpt downMeter upMeter state
                | Handshake (_, _, selfInfoHash, _), Handshake (_, _, peerInfoHash, _) when not (isInfoHashValid selfInfoHash peerInfoHash) ->
                    notifiedRef <! Notification.Failed (Exception($"Failed to handshake invalid info-hash (expected: %s{Encoding.ASCII.GetString(selfInfoHash)}, received: %s{Encoding.ASCII.GetString(peerInfoHash)})"))
                    receive pipeline leechOpt downMeter upMeter state
                | _ ->
                    mailbox.Self <! Leech FirstLeech
                    mailbox.Self <! KeepAlive
                    mailbox.Self <! MeasureRate
                    // TODO: Keep track of who initiated the handshake and if it was not us then write response handshake back.
                    receive pipeline leechOpt downMeter upMeter { state with PeerHandshake = Some peerHandshake }
                
            | Stream.ReceivedMessage message ->
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
                    
            | Stream.Failed error ->
                notifiedRef <! Notification.Failed (Exception("Peer failed to read", error))
                receive pipeline leechOpt downMeter upMeter state
        
        and unhandled pipeline leechOpt downMeter upMeter (state: State) message =
            mailbox.Unhandled(message)
            receive pipeline leechOpt downMeter upMeter state
        
        receive (BlockPipeline.create ()) None (RateMeter.create ()) (RateMeter.create ()) initialState
            
    let defaultActorBody notifiedRef piecesRef connection initialState mailbox =
        actorBody notifiedRef piecesRef connection initialState mailbox
        
    let spawn (actorFactory: IActorRefFactory) notifiedRef piecesRef (connection: IConnection) (initialState: State) =
        spawn actorFactory (actorName connection.RemoteEndpoint) (defaultActorBody notifiedRef piecesRef connection initialState)

module PeerExtensions =
    type IActorContext with
        member __.GetPeer(endpoint: IPEndPoint) : IActorRef = __.Child(Peer.actorName endpoint)
        member __.GetPeers() : IEnumerable<IActorRef> = __.GetChildren().Where(fun ref -> ref.Path.Name.StartsWith("peer"))