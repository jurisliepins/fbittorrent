namespace FBitTorrent.Experiments

open System
open System.IO
open System.Linq
open System.Net
open Akka.FSharp
open Akka.IO
open FBitTorrent.Core

module Connection =
    
    type Status =
        | Handshake
        | MessageFlow
    
    type State =
        { Status:     Status
          Connection: (EndPoint * EndPoint) option
          Stream:     Stream }
    
    let createState () =
        { Status     = Handshake
          Connection = None
          Stream     = MemoryStream() }
    
    type Command =
        | Write of Message
    
    type Event =
        | Read of Message
        
    type Notification =
        | HandshakeFailure of Exception 
        | ReadFailure      of Exception
        | WriteFailure     of Exception
    
    let actorName (endpoint: IPEndPoint) = $"connection-%A{endpoint.Address}:%d{endpoint.Port}"
    
    let actorFn ref (endpoint: EndPoint) (handshake: Handshake) (initialState: State) (mailbox: Actor<obj>) =
        let rec receive (state: State) = actor {
            match! mailbox.Receive() with
            | :? Tcp.Connected as connected ->
                match state with
                | { Connection = None } ->
                    logDebug mailbox $"Connected (local endpoint: %A{connected.LocalAddress}, remote endpoint: %A{connected.RemoteAddress})"
                    mailbox.Context.Sender <! Tcp.Register mailbox.Context.Self
                    mailbox.Context.Sender <! Tcp.Write.Create(ByteString.FromBytes(Handshake.toBytes handshake))
                    return! receive { state with Connection = Some (connected.LocalAddress, connected.RemoteAddress) }
                | { Connection = Some (localEndpoint, remoteEndpoint) } ->
                    logError mailbox $"Already connected (local endpoint: %A{localEndpoint}, remote endpoint: %A{remoteEndpoint}) (should never happen)"
            
            | :? Tcp.Received as received ->
                match state with
                 | { Connection = Some _ } ->
                    // logDebug mailbox $"Received %d{received.Data.Count} bytes from %A{remoteAddress}"
                    // logDebug mailbox $"Received %d{received.Data.Count}"
                    // mailbox.Context.Sender <! Tcp.Write.Create(ByteString.FromBytes([| 1uy |]))
                    // logDebug mailbox $"Requested bytes from %A{remoteAddress}"
                    received.Data.WriteTo(state.Stream)
                    match state with
                    | { Status = Handshake } ->
                        let reader = new ConnectionReader(state.Stream)
                        if state.Stream.Length >= 1 then
                            state.Stream.Position <- 0
                            let length = reader.ReadByte()
                            if state.Stream.Length - 1L >= int64 (int length + 8 + 20 + 20) then
                                let protocol = reader.ReadBytes(int length)
                                let reserved = reader.ReadBytes(8)
                                let ih = reader.ReadBytes(20)
                                let pid = reader.ReadBytes(20)
                                let peerHandshake = Handshake.create protocol reserved ih pid
                                logInfo mailbox $"Read handshake"
                                // let isProtocolValid (selfProtocol: byte[]) (peerProtocol: byte[]) = Enumerable.SequenceEqual(selfProtocol, peerProtocol)
                                // let isInfoHashValid (selfInfoHash: byte[]) (peerInfoHash: byte[]) = Enumerable.SequenceEqual(selfInfoHash, peerInfoHash)
                                
                                return! receive { state with Status = MessageFlow }
                        return! receive state
                    | { Status = MessageFlow } ->
                        return! receive state   
                 | { Connection = None } ->
                    logError mailbox $"Received %d{received.Data.Count} bytes but no active connection present (should never happen)"
                    return! receive state
            
            | :? Tcp.CommandFailed as commandFailed ->
                match commandFailed.Cmd with
                | :? Tcp.Register -> logError mailbox $"Register command failed with %A{commandFailed.CauseString}"
                | :? Tcp.Write    -> logError mailbox $"Write command failed with %A{commandFailed.CauseString}"
                | _               -> logError mailbox $"Unknown command failed with %A{commandFailed.CauseString}"
                
            | :? Tcp.ConnectionClosed as closed ->
                match state with
                | { Connection = Some _ } ->
                    logDebug mailbox $"Closed %s{closed.Cause}"
                | { Connection = None } ->
                    logError mailbox $"Closed but no connection present (should never happen) %s{closed.Cause}"
            
            | message ->
                logDebug mailbox $"Unhandled message %A{message}"
                mailbox.Unhandled()
                return! receive state }
        
        mailbox.Context.System.Tcp() <! (Tcp.Connect endpoint)
        receive initialState
        
module Connector2 =
    type Command =
        | Connect of IPEndPoint * Handshake
        
    let actorName () = "connector"
    
    let actorFn (mailbox: Actor<obj>) =
        let rec receive () = actor {
            match! mailbox.Receive() with
            | :? Command as command ->
                return! handleCommand command
                    
            | message ->
                mailbox.Unhandled(message)
                return! receive () }
        
        and handleCommand command =
            match command with
            | Connect (endpoint, handshake) ->
                let connectionRef = spawn mailbox (Connection.actorName endpoint) (Connection.actorFn mailbox.Context.Sender endpoint handshake (Connection.createState ()))
                receive ()
        
        receive ()