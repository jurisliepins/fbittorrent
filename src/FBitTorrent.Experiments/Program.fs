open System
open System.Net
open Akka.Actor
open Akka.FSharp
open Akka.Configuration
open Akka.IO

module Experiments =
    
    module AkkaSystem = Akka.FSharp.System 

    let server () =
        let block: byte[] = Array.zeroCreate 16384
        
        let listen (endpoint: IPEndPoint) =
            let config =
                """
                akka { loglevel = debug }
                """
                |> ConfigurationFactory.ParseString
            let system = AkkaSystem.create "server-system" config
            
            let connectionFn (localAddress: EndPoint) (remoteAddress: EndPoint) (parent: Actor<obj>) (mailbox: Actor<obj>) =
                let rec receive () = actor {
                    match! mailbox.Receive() with
                    | :? Tcp.Received as received ->
                        logDebug mailbox $"Received %d{received.Data.Count} bytes from %A{remoteAddress}"
                        parent.Context.Sender <! Tcp.Write.Create(ByteString.FromBytes(block))
                        return! receive ()
                        
                    | :? Tcp.ConnectionClosed as closed ->
                        logDebug mailbox $"Closed %A{remoteAddress} %s{closed.Cause}"
                        
                    | :? Tcp.CommandFailed as commandFailed ->
                        logDebug mailbox $"Command failed with %A{commandFailed.CauseString}"
                        
                    | :? Terminated as terminated ->
                        logDebug mailbox "Terminated"
                    
                    | message ->
                        logDebug mailbox $"Unhandled message %A{message}"
                        mailbox.Unhandled()
                        return! receive () }
                
                parent.Context.Sender <! Tcp.Register mailbox.Context.Self
                receive ()
            
            let listenerFn (mailbox: Actor<obj>) =
                let rec receive (state: EndPoint option) = actor {
                    match! mailbox.Receive() with
                    | :? Tcp.Bound as bound ->
                        logDebug mailbox $"Bound (local address: %A{bound.LocalAddress})"
                        return! receive (Some bound.LocalAddress)
                        
                    | :? Tcp.Unbound as unbound ->
                        logDebug mailbox "Un-bound"
                        return! receive state
                        
                    | :? Tcp.Connected as connected ->
                        match state with
                        | Some localAddress ->
                            logDebug mailbox $"New connection (local address: %A{connected.LocalAddress}, remote address: %A{connected.RemoteAddress})"
                            let connectionRef = spawn mailbox $"connection-%A{connected.RemoteAddress}" (connectionFn connected.LocalAddress connected.RemoteAddress mailbox)
                            ()
                        | None ->
                            logDebug mailbox $"New connection (local address: %A{connected.LocalAddress}, remote address: %A{connected.RemoteAddress}) but no socket bound (should never happen)"
                        return! receive state
                    
                    | :? Tcp.CommandFailed as commandFailed ->
                        logDebug mailbox $"Command failed with %s{commandFailed.CauseString}"
                        return! receive state
                        
                    | :? Terminated as terminated ->
                        logDebug mailbox "Terminated"
                    
                    | message ->
                        logDebug mailbox $"Unhandled message %A{message}"
                        mailbox.Unhandled()
                        return! receive state }
                
                mailbox.Context.System.Tcp() <! Tcp.Bind(mailbox.Context.Self, endpoint)
                receive None
            
            let listenerRef = spawn system "listener" listenerFn

            ()
            
        listen (IPEndPoint(IPAddress.Loopback, 9765))

    let client () =
        let connect (endpoint: IPEndPoint) =
            let config =
                """
                akka { loglevel = debug }
                """
                |> ConfigurationFactory.ParseString
            let system = AkkaSystem.create "server-system" config
            
            let connectionFn (mailbox: Actor<obj>) =
                let rec receive (state: (EndPoint * EndPoint) option) = actor {
                    match! mailbox.Receive() with
                    | :? Tcp.Connected as connected ->
                        match state with
                        | Some (localAddress, remoteAddress) ->
                            logDebug mailbox $"Already connected to %A{remoteAddress}"
                            return! receive state
                        | None ->
                            logDebug mailbox $"Connected to %A{connected.RemoteAddress}"
                            mailbox.Context.Sender <! Tcp.Register mailbox.Context.Self
                            mailbox.Context.Sender <! Tcp.Write.Create(ByteString.FromBytes([| 1uy |]))
                            logDebug mailbox $"Requested bytes from %A{connected.RemoteAddress}"
                            return! receive (Some (connected.LocalAddress, connected.RemoteAddress))
                    
                    | :? Tcp.Received as received ->
                        match state with
                        | Some (localAddress, remoteAddress) ->
                            logDebug mailbox $"Received %d{received.Data.Count} bytes from %A{remoteAddress}"
                            mailbox.Context.Sender <! Tcp.Write.Create(ByteString.FromBytes([| 1uy |]))
                            logDebug mailbox $"Requested bytes from %A{remoteAddress}"
                            return! receive state
                        | None ->
                            logDebug mailbox $"Received %d{received.Data.Count} bytes but no active connection present (should never happen)"
                            return! receive state
                        
                    | :? Tcp.ConnectionClosed as closed ->
                        match state with
                        | Some (localAddress, remoteAddress) ->
                            logDebug mailbox $"Closed %A{remoteAddress} %s{closed.Cause}"
                        | None ->
                            logDebug mailbox $"Closed but no connection present (should never happen) %s{closed.Cause}"
                        
                    | :? Tcp.CommandFailed as commandFailed ->
                        logDebug mailbox $"Command failed with %A{commandFailed.CauseString}"
                        
                    | :? Terminated as terminated ->
                        logDebug mailbox "Terminated"
                    
                    | message ->
                        logDebug mailbox $"Unhandled message %A{message}"
                        mailbox.Unhandled()
                        return! receive state }
                
                mailbox.Context.System.Tcp() <! (Tcp.Connect endpoint)
                receive None

            let connectionRef = spawn system $"connection-%A{endpoint}" connectionFn
            
            ()
                    
        connect (IPEndPoint(IPAddress.Loopback, 9765))

module Program =
    [<EntryPoint>]
    let main args =
        match Array.head args with
        | "server" -> Experiments.server ()
        | "client" -> Experiments.client ()
        | arg ->
            failwith $"Unknown arg %s{arg}"
        Console.ReadKey() |> ignore
        0