namespace FBitTorrent.Core

open System
open System.Net
open Akka.FSharp
open FBitTorrent.Core

module Connector =
    open Handshake
    
    type Command =
        | Connect of IPAddress * int * Handshake
        
    type CommandResult =
        | Success of IConnection * Handshake
        | Failure of Exception
    
    let actorName () = "connector"
    
    let actorFn (connect: Handshake.Connect) (mailbox: Actor<obj>) =
        let rec receive () = actor {
            match! mailbox.Receive() with
            | :? Command as command ->
                return! handleCommand command
                    
            | message ->
                mailbox.Unhandled message
                return! receive () }
        
        and handleCommand command =
            match command with
            | Connect (address, port, handshake) ->
                spawn mailbox null (fun _ -> actor {
                    try
                        let connection = connect (IPEndPoint(address, port))
                        connection.WriteHandshake(handshake)
                        mailbox.Context.Sender <! Success (connection.Connection, connection.ReadHandshake())
                    with exn ->
                        mailbox.Context.Sender <! Failure (Exception($"Failed to handshake with %A{address}:%d{port}", exn)) }) |> ignore
                receive ()
        
        receive ()
        
    let tcpActorFn (mailbox: Actor<obj>) = actorFn tcpConnect mailbox
    
    let defaultActorFn mailbox = tcpActorFn mailbox