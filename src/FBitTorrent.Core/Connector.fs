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
        | Connect of Address: IPAddress * Port: int
        
    type CommandResult =
        | ConnectSuccess of Connection: IConnection
        | ConnectFailure of Address: IPAddress * Port: int * Error: Exception
    
    let actorName () = "connector"
    
    let actorFn (asyncConnect: IPEndPoint -> Async<IConnection>) (mailbox: Actor<obj>) =
        let rec receive () = actor {
            match! mailbox.Receive() with
            | :? Command as command ->
                return! handleCommand command
                    
            | message ->
                return! unhandled message }
        
        and handleCommand command =
            match command with
            | Connect (address, port) ->
                Async.StartAsTask(async {
                    try
                        let! connection = asyncConnect (IPEndPoint(address, port))
                        return ConnectSuccess connection    
                    with exn ->
                        return ConnectFailure (address, port, Exception($"Failed to connect to %A{address}:%d{port}", exn)) }
                ).PipeTo(mailbox.Context.Sender) |> ignore
                receive ()
        
        and unhandled message =
            mailbox.Unhandled(message)
            receive ()
        
        receive ()
        
    let defaultActorFn mailbox = actorFn Connection.asyncTcpConnect mailbox
    
module ConnectorExtensions =
    type IActorContext with
        member __.GetConnector() : IActorRef = __.Child(Connector.actorName ())