namespace FBitTorrent.Core

open System
open System.Collections.Generic
open System.IO
open System.Net
open System.Linq
open System.Text
open Akka.IO
open Akka.FSharp
open Akka.Actor
open FBitTorrent.Core

module Peer =
    module Stream =
        type Status =
            | AwaitingHandshakeHeader
            | AwaitingHandshakeBody of Length: byte
            | AwaitingMessageHeader
            | AwaitingMessageBody   of Length: int
        
        type State =
            { Status:  Status
              Pipe:    SomethingStream }
        
        let createState () =
            { Status  = AwaitingHandshakeHeader
              Pipe    = SomethingStream() }
        
        type private Action =
            | ReadCurrent
        
        type Command =
            | WriteHandshake of Handshake: Handshake
            | WriteMessage   of Message: Message
        
        type Notification =
            | ReceivedHandshake of Handshake: Handshake
            | ReceivedMessage   of Message: Message
            | Failed            of Error: Exception
        
        let writeHandshake (connectionRef: IActorRef) (Handshake (proto, res, ih, pid): Handshake) =
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes([| proto.Length |> byte |]))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(proto))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(res))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(ih))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(pid))
        
        let rec writeMessage (connectionRef: IActorRef) (message: Message) =
            match message with
            | KeepAliveMessage                   -> writeKeepAlive     connectionRef
            | ChokeMessage                       -> writeChoke         connectionRef 
            | UnChokeMessage                     -> writeUnChoke       connectionRef
            | InterestedMessage                  -> writeInterested    connectionRef
            | NotInterestedMessage               -> writeNotInterested connectionRef
            | HaveMessage     idx                -> writeHave          connectionRef idx
            | BitfieldMessage bitfield           -> writeBitfield      connectionRef bitfield
            | RequestMessage  (idx, beg, length) -> writeRequest       connectionRef idx beg length
            | PieceMessage    (idx, beg, block)  -> writePiece         connectionRef idx beg block
            | CancelMessage   (idx, beg, length) -> writeCancel        connectionRef idx beg length
            | PortMessage     port               -> writePort          connectionRef port
        and private writeKeepAlive connectionRef =
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 0))
        and private writeChoke connectionRef =
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 1))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes([| (ChokeType.ToByte()) |]))
        and private writeUnChoke connectionRef =
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 1))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes([| (UnChokeType.ToByte()) |]))
        and private writeInterested connectionRef =
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 1))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes([| (InterestedType.ToByte()) |]))
        and private writeNotInterested connectionRef =
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 1))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes([| (NotInterestedType.ToByte()) |]))
        and private writeHave connectionRef (idx: int) =
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 5))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes([| (HaveType.ToByte()) |]))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 idx))
        and private writeBitfield connectionRef (bitfield: byte[]) =
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 (1 + bitfield.Length)))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes([| (BitfieldType.ToByte()) |]))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(bitfield))
        and private writeRequest connectionRef (idx: int) (beg: int) (length: int) =
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 (1 + 12)))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes([| (RequestType.ToByte()) |]))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 idx))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 beg))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 length))
        and private writePiece connectionRef (idx: int) (beg: int) (block: ByteBuffer) =
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 (9 + block.Length)))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes([| (PieceType.ToByte()) |]))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 idx))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 beg))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(block.ToArray()))
        and private writeCancel connectionRef (idx: int) (beg: int) (length: int) =
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 (1 + 12)))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes([| (CancelType.ToByte()) |]))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 idx))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 beg))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 length))
        and private writePort connectionRef (port: int16) =
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt32 3))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes([| (PortType.ToByte()) |]))
            connectionRef <! Tcp.Write.Create(ByteString.FromBytes(BigEndianConverter.fromInt16 port))
        
        let readHandshake (reader: BigEndianReader) (length: byte) =
            let proto = reader.ReadBytes(int length)
            let res = reader.ReadBytes(8)
            let ih = reader.ReadBytes(20)
            let pid = reader.ReadBytes(20)
            Handshake (proto, res, ih, pid)
        
        let rec readMessage (reader: BigEndianReader) (length: int) =
            match MessageType.FromByte(reader.ReadByte()) with
            | ChokeType         -> readChoke         ()
            | UnChokeType       -> readUnChoke       ()
            | InterestedType    -> readInterested    ()
            | NotInterestedType -> readNotInterested ()
            | HaveType          -> readHave          reader 
            | BitfieldType      -> readBitfield      reader length
            | RequestType       -> readRequest       reader 
            | PieceType         -> readPiece         reader length
            | CancelType        -> readCancel        reader 
            | PortType          -> readPort          reader
            | messageType       -> failwith $"Unhandled MessageType %A{messageType}"
        and private readChoke () =
            ChokeMessage
        and private readUnChoke () =
            UnChokeMessage
        and private readInterested () =
            InterestedMessage
        and private readNotInterested () =
            NotInterestedMessage
        and private readHave reader =
            let idx = reader.ReadInt32()
            HaveMessage idx
        and private readBitfield reader length =
            let bitfield = reader.ReadBytes(length - 1)
            BitfieldMessage bitfield
        and private readRequest reader =
            let idx = reader.ReadInt32()
            let beg = reader.ReadInt32()
            let length = reader.ReadInt32()
            RequestMessage (idx, beg, length)
        and private readPiece reader length =
            let idx = reader.ReadInt32()
            let beg = reader.ReadInt32()
            let block = reader.ReadByteBuffer(length - 1 - 8);
            PieceMessage (idx, beg, block)
        and private readCancel reader =
            let idx = reader.ReadInt32()
            let beg = reader.ReadInt32()
            let length = reader.ReadInt32()
            CancelMessage (idx, beg, length)
        and private readPort reader =
            let port = reader.ReadInt16()
            PortMessage port
        
        let actorName () = "stream"
        
        let actorBody notifiedRef connectionRef (initialState: State) (mailbox: Actor<obj>) =
            mailbox.Defer(fun _ -> connectionRef <! Tcp.Close.Instance)
            let rec receive (state: State) = actor {
                match! mailbox.Receive() with
                | :? Action as action ->
                    return! handleAction state action
                
                | :? Command as command ->
                    return! handleCommand state command
                
                | :? Tcp.Event as event ->
                    return! handleTcpEvent state event
                    
                | message ->
                    return! unhandled state message }
            
            and handleAction (state: State) action =
                match action with
                | ReadCurrent ->
                    try
                        match state with
                        | { Status = AwaitingHandshakeHeader } when handshakeHeaderAvailable state.Pipe ->
                            handleHandshakeHeader state
                        | { Status = AwaitingHandshakeBody length } when handshakeBodyAvailable state.Pipe length ->
                            handleHandshakeBody state length
                        | { Status = AwaitingMessageHeader } when messageHeaderAvailable state.Pipe ->
                            handleMessageHeader state
                        | { Status = AwaitingMessageBody length } when messageBodyAvailable state.Pipe length ->
                            handleMessageBody state length
                        | _ ->
                            receive state
                    with exn ->
                        notifiedRef <! Failed (Exception("Failed to read bytes", exn))
                        receive state
            
            and handshakeHeaderAvailable (stream: Stream) =
                stream.Length >= sizeof<byte>
            
            and handshakeBodyAvailable (stream: Stream) (length: byte) =
                stream.Length >= int64 (length + 8uy + 20uy + 20uy)
            
            and messageHeaderAvailable (stream: Stream) =
                stream.Length >= sizeof<int32>
            
            and messageBodyAvailable (stream: Stream) (length: int) =
                stream.Length >= int64 length
                        
            and handleHandshakeHeader (state: State) =
                let reader = new BigEndianReader(state.Pipe)
                let length = reader.ReadByte()
                logInfo mailbox $"Read handshake header %d{length}"
                mailbox.Self <! ReadCurrent
                receive { state with Status = AwaitingHandshakeBody length }
                
            and handleHandshakeBody (state: State) length =
                let reader = new BigEndianReader(state.Pipe)
                let handshake = readHandshake reader length
                logInfo mailbox "Read handshake"
                notifiedRef <! ReceivedHandshake handshake
                mailbox.Self <! ReadCurrent
                receive { state with Status = AwaitingMessageHeader }
                
            and handleMessageHeader (state: State) =
                let reader = new BigEndianReader(state.Pipe)
                let length = reader.ReadInt32()
                logInfo mailbox $"Read message header %d{length}"
                if length < 0     then failwith "Received negative value for message length"
                if length > 16484 then failwith $"Not accepting messages longer than %d{16484} bytes"
                if length = 0 then
                    notifiedRef <! ReceivedMessage KeepAliveMessage
                    mailbox.Self <! ReadCurrent
                    receive { state with Status = AwaitingMessageHeader }
                else 
                    mailbox.Self <! ReadCurrent
                    receive { state with Status = AwaitingMessageBody length }
            
            and handleMessageBody (state: State) length =
                let reader = new BigEndianReader(state.Pipe)
                let message = readMessage reader length
                logInfo mailbox "Read message"
                notifiedRef <! ReceivedMessage message
                mailbox.Self <! ReadCurrent
                receive { state with Status = AwaitingMessageHeader }
            
            and handleCommand (state: State) command =
                match command with
                | WriteHandshake handshake ->
                    try
                        writeHandshake connectionRef handshake
                    with exn ->
                        notifiedRef <! Failed (Exception("Failed to write handshake", exn))
                | WriteMessage message ->
                    try
                        writeMessage connectionRef message
                    with exn ->
                        notifiedRef <! Failed (Exception("Failed to write message", exn))
                receive state
            
            and handleTcpEvent (state: State) event =
                match event with
                | :? Tcp.Received as received ->
                    try
                        received.Data.WriteTo(state.Pipe)
                        mailbox.Self <! ReadCurrent
                    with exn ->
                        notifiedRef <! Failed (Exception("Failed to receive bytes", exn))
                    receive state
                | :? Tcp.ConnectionClosed as closed ->
                    notifiedRef <! Failed (Exception($"Connection closed %s{closed.Cause}"))
                    receive state
                | :? Tcp.CommandFailed as failed ->
                    notifiedRef <! Failed (Exception($"Command failed %s{failed.CauseString}"))
                    receive state
                | message ->
                    unhandled state message
                
            and unhandled (state: State) message =
                mailbox.Unhandled(message)
                receive state 
            
            connectionRef <! Tcp.Register mailbox.Self
            
            receive initialState
            
        let defaultActorBody notifiedRef connectionRef (initialState: State) (mailbox: Actor<obj>) =
            actorBody notifiedRef connectionRef initialState mailbox
            
        let spawn (actorFactory: IActorRefFactory) notifiedRef connectionRef (initialState: State) =
            spawn actorFactory (actorName ()) (defaultActorBody notifiedRef connectionRef initialState)
    
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
    
    type private Action =
        | FirstLeech
        | NextLeech
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
    
    let actorName (endpoint: EndPoint) =
        let str = endpoint.ToString()
                      .Replace("[", "")
                      .Replace("]", "")
        $"peer-%s{str}"
    
    let actorBody notifiedRef piecesRef connectionRef (initialState: State) (mailbox: Actor<obj>) =
        logDebug mailbox $"Initial state \n%A{initialState}" 
        let streamRef = Stream.spawn mailbox mailbox.Self connectionRef (Stream.createState ())
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
            
            | :? Stream.Notification as notification ->
                return! handleStreamNotification pipeline leechOpt downMeter upMeter state notification
                        
            | message ->
                return! unhandled pipeline leechOpt downMeter upMeter state message }
        
        and handleAction pipeline leechOpt downMeter upMeter (state: State) action =
            match action with
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
                        mailbox.Self <! NextLeech
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
                    mailbox.Self <! NextLeech
                    receive pipeline (Some (idx, BlockRequests.create length, BlockResponses.create length)) downMeter upMeter state

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
                    mailbox.Self <! FirstLeech
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
                        mailbox.Self <! NextLeech
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
                            mailbox.Self <! NextLeech
                            receive (BlockPipeline.update block.Length pipeline) leechOpt downMeter upMeter nextState
                    | _ ->
                        receive (BlockPipeline.update block.Length pipeline) leechOpt downMeter upMeter nextState
            
                | message ->
                    mailbox.Unhandled(message)
                    receive pipeline leechOpt downMeter upMeter state
                    
            | Stream.Failed error ->
                notifiedRef <! Notification.Failed (Exception("Peer stream failed", error))
                receive pipeline leechOpt downMeter upMeter state
        
        and unhandled pipeline leechOpt downMeter upMeter (state: State) message =
            mailbox.Unhandled(message)
            receive pipeline leechOpt downMeter upMeter state
        
        receive (BlockPipeline.create ()) None (RateMeter.create ()) (RateMeter.create ()) initialState
            
    let defaultActorBody notifiedRef piecesRef connectionRef initialState mailbox =
        actorBody notifiedRef piecesRef connectionRef initialState mailbox
        
    let spawn (actorFactory: IActorRefFactory) notifiedRef piecesRef connectionRef localEndpoint remoteEndpoint (initialState: State) =
        spawn actorFactory (actorName remoteEndpoint) (defaultActorBody notifiedRef piecesRef connectionRef initialState)

module PeerExtensions =
    type IActorContext with
        member __.GetPeer(endpoint: EndPoint) : IActorRef = __.Child(Peer.actorName endpoint)
        member __.GetPeers() : IEnumerable<IActorRef> = __.GetChildren().Where(fun ref -> ref.Path.Name.StartsWith("peer"))