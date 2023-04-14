namespace FBitTorrent.Core

open System
open System.Net
open Akka.Actor
open Akka.FSharp
open FBitTorrent.Core

module Connector =
    
    type Command =
        | Connect of Endpoint: IPEndPoint
        
    type CommandResult =
        | ConnectSuccess of Connection: IConnection
        | ConnectFailure of Endpoint: IPEndPoint * Error: Exception
    
    let actorName () = "connector"
    
    let actorBody (asyncConnect: IPEndPoint -> Async<IConnection>) (mailbox: Actor<obj>) =
        let rec receive () = actor {
            match! mailbox.Receive() with
            | :? Command as command ->
                return! handleCommand command
                    
            | message ->
                return! unhandled message }
        
        and handleCommand command =
            match command with
            | Connect endpoint ->
                Async.StartAsTask(async {
                    try
                        let! connection = asyncConnect endpoint
                        return ConnectSuccess connection    
                    with exn ->
                        return ConnectFailure (endpoint, Exception($"Failed to connect to %A{endpoint.Address}:%d{endpoint.Port}", exn))
                }).PipeTo(mailbox.Context.Sender) |> ignore
                receive ()
       
        and unhandled message =
            mailbox.Unhandled(message)
            receive ()
        
        receive ()
        
    let defaultActorBody mailbox =
        actorBody Connection.asyncTcpConnect mailbox
    
    let spawn (actorFactory: IActorRefFactory) =
        spawn actorFactory (actorName ()) defaultActorBody 
    
module ConnectorExtensions =
    type IActorContext with
        member __.GetConnector() : IActorRef = __.Child(Connector.actorName ())