namespace FBitTorrent.Core

open System.Net
open Akka.Actor
open Akka.IO
open Akka.FSharp

module Connector =
    
    type Command =
        | Connect of Endpoint: IPEndPoint
        
    type Notification =
        | Connected of ConnectionRef: IActorRef * LocalEndpoint: EndPoint * RemoteEndpoint: EndPoint
    
    let actorName () = "connector"
    
    let actorBody notifiedRef (mailbox: Actor<obj>) =
        let rec receive () = actor {
            match! mailbox.Receive() with
            | :? Command as command ->
                return! handleCommand command
            
            | :? Tcp.Event as event ->
                return! handleTcpEvent event
                    
            | message ->
                return! unhandled message }
        
        and handleCommand command =
            match command with
            | Connect endpoint ->
                mailbox.Context.System.Tcp() <! (Tcp.Connect endpoint)
                receive ()
       
        and handleTcpEvent event =
            match event with
            | :? Tcp.Connected as connected ->
                notifiedRef <! Connected (mailbox.Context.Sender, connected.LocalAddress, connected.RemoteAddress)
                receive ()
            | message ->
                unhandled message
        
        and unhandled message =
            mailbox.Unhandled(message)
            receive ()
        
        receive ()
        
    let defaultActorBody mailbox =
        actorBody mailbox
    
    let spawn (actorFactory: IActorRefFactory) notifiedRef =
        spawn actorFactory (actorName ()) (defaultActorBody notifiedRef) 
    
module ConnectorExtensions =
    type IActorContext with
        member __.GetConnector() : IActorRef = __.Child(Connector.actorName ())