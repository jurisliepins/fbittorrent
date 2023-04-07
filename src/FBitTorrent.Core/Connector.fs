namespace FBitTorrent.Core

open System
open System.Linq
open System.Net
open System.Text
open Akka.Actor
open Akka.FSharp
open FBitTorrent.Core

module Connector =
    
    type Command =
        | Connect of IPAddress * int * Handshake
        
    type CommandResult =
        | Success of IConnection * Handshake
        | Failure of IPAddress * int * Exception
    
    let private isProtocolValid (selfProtocol: byte[]) (peerProtocol: byte[]) = Enumerable.SequenceEqual(selfProtocol, peerProtocol)
    
    let private isInfoHashValid (selfInfoHash: byte[]) (peerInfoHash: byte[]) = Enumerable.SequenceEqual(selfInfoHash, peerInfoHash)
    
    let private asyncPerformHandshake (selfHandshake: Handshake) address port (connection: IHandshakeConnection) = async {
        try
            do! connection.AsyncWriteHandshake(selfHandshake)
            let! peerHandshake = connection.AsyncReadHandshake()
            match (selfHandshake, peerHandshake) with
            | Handshake (selfProtocol, _, _, _), Handshake (peerProtocol, _, _, _) when not (isProtocolValid selfProtocol peerProtocol)  ->
                connection.Disconnect() 
                return Failure (address, port, Exception($"Failed to handshake with %A{address}:%d{port} invalid protocol (expected: %s{Encoding.ASCII.GetString(selfProtocol)}, received: %s{Encoding.ASCII.GetString(peerProtocol)})"))
            | Handshake (_, _, selfInfoHash, _), Handshake (_, _, peerInfoHash, _) when not (isInfoHashValid selfInfoHash peerInfoHash)  ->
                connection.Disconnect()
                return Failure (address, port, Exception($"Failed to handshake with %A{address}:%d{port} invalid info-hash (expected: %A{(Hash selfInfoHash)}, received: %A{Hash peerInfoHash})"))
            | _ ->
                return Success (connection, peerHandshake)
        with exn ->
            connection.Disconnect()
            return Failure (address, port, Exception($"Failed to handshake with %A{address}:%d{port}", exn)) }
    
    let actorName () = "connector"
    
    let actorFn (asyncConnect: IPEndPoint -> Async<IConnection>) (createHandshakeConnection: IConnection -> IHandshakeConnection) (mailbox: Actor<obj>) =
        let rec receive () = actor {
            match! mailbox.Receive() with
            | :? Command as command ->
                return! handleCommand command
                    
            | message ->
                mailbox.Unhandled(message)
                return! receive () }
        
        and handleCommand command =
            match command with
            | Connect (address, port, handshake) ->
                Async.StartAsTask(async {
                    try
                        let! connection = asyncConnect (IPEndPoint(address, port))
                        return! asyncPerformHandshake handshake address port (createHandshakeConnection connection)    
                    with exn ->
                        return Failure (address, port, Exception($"Failed to connect to %A{address}:%d{port}", exn)) }
                ).PipeTo(mailbox.Context.Sender) |> ignore
                receive ()
        
        receive ()
        
    let defaultActorFn mailbox = actorFn Connection.asyncTcpConnect Handshake.createConnection mailbox
    
module ConnectorExtensions =
    type IActorContext with
        member __.GetConnector() : IActorRef = __.Child(Connector.actorName ())