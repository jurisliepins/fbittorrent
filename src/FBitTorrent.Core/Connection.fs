namespace FBitTorrent.Core

open System
open System.Net
open Akka.Actor
open Akka.FSharp
open FBitTorrent.Core

module Connection =

    type Status =
        | Handshake
        | MessageFlow
    
    type State =
        { Status:     Status
          Connection: IConnection option }
        
    let createState () =
        { Status     = Handshake
          Connection = None }
    
    type private Action = Read
    
    type private AsyncResult =
        | ConnectSuccess of Connection: IConnection
        | ConnectFailure of Address: IPAddress * Port: int * Error: Exception
        | ReadHandshake  of Handshake: Handshake
        | ReadMessage    of Message: Message
        | ReadFailure    of Error: Exception
    
    type Command =
        | Connect        of Address: IPAddress * Port: int
        | WriteHandshake of Handshake: Handshake
        | WriteMessage   of Message: Message
    
    type CommandResult =
        | ConnectSuccess        of LocalEndpoint: IPEndPoint * RemoteEndpoint: IPEndPoint
        | ConnectFailure        of Address: IPAddress * Port: int * Error: Exception
        | WriteHandshakeFailure of Error: Exception
        | WriteMessageFailure   of Error: Exception
    
    type Notification =
        | ReadHandshake of Handshake: Handshake
        | ReadMessage   of Message: Message
        | ReadFailure   of Error: Exception
    
    let actorName () = "connection"
    
    let actorFn (asyncConnect: IPEndPoint -> Async<IConnection>) notifiedRef (initialState: State) (mailbox: Actor<obj>) =
        let rec receive (state: State) = actor {
            match! mailbox.Receive() with
            | :? Action as action ->
                return! handleAction state action
            
            | :? AsyncResult as result ->
                return! handleAsyncResult state result
                        
            | :? Command as command ->
                return! handleCommand state command
            
            | message ->
                return! unhandled state message }
        
        and handleAction (state: State) action =
            match action with
            | Read ->
                Async.StartAsTask(async {
                    try
                        match state with
                        | { Status = Handshake; Connection = Some connection } ->
                            let! handshake = Handshake.asyncRead connection.Reader
                            mailbox.Context.Self <! Read
                            return AsyncResult.ReadHandshake handshake
                        | { Status = MessageFlow; Connection = Some connection } ->
                            let! message = Message.asyncRead connection.Reader
                            mailbox.Context.Self <! Read
                            return AsyncResult.ReadMessage message
                        | _ ->
                            return AsyncResult.ReadFailure (Exception("No active connection to read from"))        
                    with exn ->
                        return AsyncResult.ReadFailure (Exception("Failed to read handshake from connection", exn)) }
                ).PipeTo(mailbox.Context.Self) |> ignore
            receive state
        
        and handleAsyncResult (state: State) result =
            match result with
            | AsyncResult.ConnectSuccess connection ->
                mailbox.Defer(connection.Dispose)
                mailbox.Context.Self <! Read
                mailbox.Context.Sender <! ConnectSuccess (connection.LocalEndpoint, connection.RemoteEndpoint)
                receive { state with Connection = Some connection }    
            | AsyncResult.ConnectFailure (address, port, error) ->
                mailbox.Context.Sender <! ConnectFailure (address, port, error)
                receive state
            // | AsyncResult.ReadHandshake handshake ->
            //     notifiedRef <! ReadHandshake handshake
        
        and handleCommand (state: State) command =
            match command with
            | Connect (address, port) ->
                match state with
                | { Connection = Some connection } ->
                    mailbox.Context.Sender <! ConnectFailure (address, port, Exception($"Connection already established on %A{connection.RemoteEndpoint.Address}:%d{connection.RemoteEndpoint.Port}"))
                | { Connection = None } ->
                    Async.StartAsTask(async {
                        try
                            let! connection = asyncConnect (IPEndPoint(address, port))
                            return AsyncResult.ConnectSuccess connection    
                        with exn ->
                            return AsyncResult.ConnectFailure (address, port, Exception($"Failed to connect to %A{address}:%d{port}", exn)) }
                    ).PipeTo(mailbox.Context.Self, mailbox.Context.Sender) |> ignore
                receive state
            | WriteHandshake handshake ->
                match state with
                | { Connection = Some connection } ->
                    try
                        Handshake.write connection.Writer handshake
                    with exn ->
                        mailbox.Context.Sender <! WriteHandshakeFailure (Exception("Failed to write handshake", exn))    
                | { Connection = None } ->
                    mailbox.Context.Sender <! WriteHandshakeFailure (Exception("No active connection to write to"))
                receive state
            | WriteMessage message ->
                match state with
                | { Connection = Some connection } ->
                    try
                        Message.write connection.Writer message
                    with exn ->
                        mailbox.Context.Sender <! WriteMessageFailure (Exception("Failed to write message", exn))
                | { Connection = None } ->
                    mailbox.Context.Sender <! WriteMessageFailure (Exception("No active connection to write to"))
                receive state
        
        and unhandled (state: State) message =
            mailbox.Unhandled(message)
            receive state
        
        receive initialState
        
    let defaultActorFn notifiedRef initialState mailbox = actorFn Connection.asyncTcpConnect notifiedRef initialState mailbox
    
module ConnectionExtensions =
    type IActorContext with
        member __.GetConnection() : IActorRef = __.Child(Connection.actorName ())