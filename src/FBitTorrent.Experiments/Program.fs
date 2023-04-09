namespace FBitTorrent.Experiments

open System
open System.Net
open Akka.Actor
open Akka.FSharp
open Akka.Configuration
open Akka.IO
open FBitTorrent.Core

module AkkaSystem = Akka.FSharp.System

// module Experiments =
//     
//     let server () =
//         let block: byte[] = Array.zeroCreate 16384
//         
//         let listen (endpoint: IPEndPoint) =
//             let config =
//                 """
//                 akka { loglevel = debug }
//                 """
//                 |> ConfigurationFactory.ParseString
//             let system = AkkaSystem.create "server-system" config
//             
//             let connectionFn (localAddress: EndPoint) (remoteAddress: EndPoint) (parent: Actor<obj>) (mailbox: Actor<obj>) =
//                 let rec receive () = actor {
//                     match! mailbox.Receive() with
//                     | :? Tcp.Received as received ->
//                         // logDebug mailbox $"Received %d{received.Data.Count} bytes from %A{remoteAddress}"
//                         // logDebug mailbox $"Received %d{received.Data.Count}"
//                         parent.Context.Sender <! Tcp.Write.Create(ByteString.FromBytes(block))
//                         return! receive ()
//                         
//                     | :? Tcp.ConnectionClosed as closed ->
//                         logDebug mailbox $"Closed %A{remoteAddress} %s{closed.Cause}"
//                         
//                     | :? Tcp.CommandFailed as commandFailed ->
//                         logDebug mailbox $"Command failed with %A{commandFailed.CauseString}"
//                         
//                     | :? Terminated as terminated ->
//                         logDebug mailbox "Terminated"
//                     
//                     | message ->
//                         logDebug mailbox $"Unhandled message %A{message}"
//                         mailbox.Unhandled()
//                         return! receive () }
//                 
//                 parent.Context.Sender <! Tcp.Register mailbox.Context.Self
//                 receive ()
//             
//             let listenerFn (mailbox: Actor<obj>) =
//                 let rec receive (state: EndPoint option) = actor {
//                     match! mailbox.Receive() with
//                     | :? Tcp.Bound as bound ->
//                         logDebug mailbox $"Bound (local address: %A{bound.LocalAddress})"
//                         return! receive (Some bound.LocalAddress)
//                         
//                     | :? Tcp.Unbound as unbound ->
//                         logDebug mailbox "Un-bound"
//                         return! receive state
//                         
//                     | :? Tcp.Connected as connected ->
//                         match state with
//                         | Some localAddress ->
//                             logDebug mailbox $"New connection (local address: %A{connected.LocalAddress}, remote address: %A{connected.RemoteAddress})"
//                             let connectionRef = spawn mailbox $"connection-%A{connected.RemoteAddress}" (connectionFn connected.LocalAddress connected.RemoteAddress mailbox)
//                             ()
//                         | None ->
//                             logDebug mailbox $"New connection (local address: %A{connected.LocalAddress}, remote address: %A{connected.RemoteAddress}) but no socket bound (should never happen)"
//                         return! receive state
//                     
//                     | :? Tcp.CommandFailed as commandFailed ->
//                         logDebug mailbox $"Command failed with %s{commandFailed.CauseString}"
//                         return! receive state
//                         
//                     | :? Terminated as terminated ->
//                         logDebug mailbox "Terminated"
//                     
//                     | message ->
//                         logDebug mailbox $"Unhandled message %A{message}"
//                         mailbox.Unhandled()
//                         return! receive state }
//                 
//                 mailbox.Context.System.Tcp() <! Tcp.Bind(mailbox.Context.Self, endpoint)
//                 receive None
//             
//             let listenerRef = spawn system "listener" listenerFn
//
//             ()
//             
//         listen (IPEndPoint(IPAddress.Loopback, 9765))
//
//     let client () =
//         let connect (endpoint: IPEndPoint) =
//             let config =
//                 """
//                 akka { loglevel = debug }
//                 """
//                 |> ConfigurationFactory.ParseString
//             let system = AkkaSystem.create "client-system" config
//             
//             let connectionFn (mailbox: Actor<obj>) =
//                 let rec receive (state: (EndPoint * EndPoint) option) = actor {
//                     match! mailbox.Receive() with
//                     | :? Tcp.Connected as connected ->
//                         match state with
//                         | Some (localAddress, remoteAddress) ->
//                             logDebug mailbox $"Already connected to %A{remoteAddress}"
//                             return! receive state
//                         | None ->
//                             logDebug mailbox $"Connected to %A{connected.RemoteAddress}"
//                             mailbox.Context.Sender <! Tcp.Register mailbox.Context.Self
//                             mailbox.Context.Sender <! Tcp.Write.Create(ByteString.FromBytes([| 1uy |]))
//                             logDebug mailbox $"Requested bytes from %A{connected.RemoteAddress}"
//                             return! receive (Some (connected.LocalAddress, connected.RemoteAddress))
//                     
//                     | :? Tcp.Received as received ->
//                         match state with
//                         | Some (localAddress, remoteAddress) ->
//                             // logDebug mailbox $"Received %d{received.Data.Count} bytes from %A{remoteAddress}"
//                             // logDebug mailbox $"Received %d{received.Data.Count}"
//                             mailbox.Context.Sender <! Tcp.Write.Create(ByteString.FromBytes([| 1uy |]))
//                             // logDebug mailbox $"Requested bytes from %A{remoteAddress}"
//                             return! receive state
//                         | None ->
//                             logDebug mailbox $"Received %d{received.Data.Count} bytes but no active connection present (should never happen)"
//                             return! receive state
//                         
//                     | :? Tcp.ConnectionClosed as closed ->
//                         match state with
//                         | Some (localAddress, remoteAddress) ->
//                             logDebug mailbox $"Closed %A{remoteAddress} %s{closed.Cause}"
//                         | None ->
//                             logDebug mailbox $"Closed but no connection present (should never happen) %s{closed.Cause}"
//                         
//                     | :? Tcp.CommandFailed as commandFailed ->
//                         logDebug mailbox $"Command failed with %A{commandFailed.CauseString}"
//                         
//                     | :? Terminated as terminated ->
//                         logDebug mailbox "Terminated"
//                     
//                     | message ->
//                         logDebug mailbox $"Unhandled message %A{message}"
//                         mailbox.Unhandled()
//                         return! receive state }
//                 
//                 mailbox.Context.System.Tcp() <! (Tcp.Connect endpoint)
//                 receive None
//
//             let connectionRef = spawn system $"connection-%A{endpoint}" connectionFn
//             
//             ()
//                     
//         connect (IPEndPoint(IPAddress.Loopback, 9765))

module Program =
    [<EntryPoint>]
    let main args =
        // match Array.head args with
        // | "server" -> Experiments.server ()
        // | "client" -> Experiments.client ()
        // | arg ->
        //     failwith $"Unknown arg %s{arg}"
        let config =
            """
            akka { loglevel = debug }
            """
            |> ConfigurationFactory.ParseString
        let system = AkkaSystem.create "experiment-system" config
        let torrentFn (mailbox: Actor<obj>) =
            let connectorRef = spawn mailbox (Connector2.actorName ()) Connector2.actorFn
            let rec receive () = actor {
                match! mailbox.Receive() with
                | :? Connection.Notification as notification ->
                    match notification with
                    | Connection.HandshakeFailure error -> logError mailbox $"Handshake failed %A{error}"
                    | Connection.ReadFailure      error -> logError mailbox $"Read failed %A{error}"
                    | Connection.WriteFailure     error -> logError mailbox $"Write failed %A{error}"
                    return! receive ()
                
                | message ->
                    logDebug mailbox $"Unhandled message %A{message}"
                    mailbox.Unhandled()
                    return! receive () }
            
            let ih = Hash ("99c82bb73505a3c0b453f9fa0e881d6e5a32a0c1")
            let pid = PeerId.create ()
            let hs = Handshake.defaultCreate (ih.ToArray()) (pid.ToArray())
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2605:a601:a60a:f500:17d4:70e8:e024:b8b0"), 59359), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:569:5079:abd2::c8"), 55655), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a02:908:2051:149c:8440:99ff:fe56:39be"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2003:c9:2f49:e501:3c4a:92ff:fe03:aa47"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:41d0:8:d92e::1"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:470:dde0::2"), 64010), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:41d0:e:907::1"), 11889), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2409:8a60:841e:d40::f4e3"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a01:e0a:27e:1920:5054:ff:fe69:9938"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a09:8740:0:4::133"), 16881), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2604:a880:cad:d0::537:6001"), 51150), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a02:168:f405::70"), 51414), hs)
            
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a01:4f8:141:5192::2"), 58250), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("240e:3b9:30b0:8e70:211:32ff:fef0:db2b"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2804:145c:82de:d800:d4d4:60e3:df31:60c3"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2600:1700:3130:4270:86d3:f3d:3ae2:6ea1"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a01:e0a:352:2450:211:32ff:fed8:cacb"), 63810), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2602:fdb8:131:2001::7:1"), 61403), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a01:e34:ec20:ed90:3a60:77ff:fe60:cd58"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a00:6020:b2ba:d800:5ef4:abff:fe72:8214"), 51922), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:41d0:1:8c94::1"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a06:f900:100:e02:84f3:22ff:fee4:893d"), 51414), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a02:c206:3008:6973::1"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a02:1210:4831:9700:ba27:ebff:fe91:60cd"), 51316), hs)
            
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2600:1700:2000:8a5f::137"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:41d0:8:d311::1"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:8b0:a790:604a:ca60:ff:fe86:cf14"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2401:4900:1c8e:749c:c750:aae:5b57:2755"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2603:300c:1f:4100:a5c8:7fa3:2feb:627"), 50921), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a01:e0a:866:af30:c89a:e722:5b93:1e63"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2804:7f7:d413:a5f2:5af3:ad0:d4e9:7e10"), 24696), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:b07:5d31:71e:89b2:ee68:c27c:70bd"), 21214), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:41d0:e:81c::1"), 59868), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2601:601:200:813:a8a1:59ff:fe5a:a19"), 6881), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:41d0:602:14f::"), 23010), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("240e:351:7549:1500:c28a:9a5e:d61a:5"), 51413), hs)
            
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:41d0:e:8c2::1"), 51076), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a01:4ff:f0:a583::1"), 49164), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a04:ee41:81:c3a0:12dd:b1ff:fe99:8fbf"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:b011:f202:325f:a669:999d:ddb2:d2f0"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2a01:4f8:161:9332::2"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("185.125.190.5"), 6950), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2003:f1:6f0e:9f00:c0ab:7cff:febd:274a"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:41d0:a:62c9::1"), 63704), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:41d0:a:26de::"), 6866), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:41d0:401:3200::1f8f"), 51413), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2001:470:7a83:6f74:0:7069:7261:746"), 6979), hs)
            connectorRef <! Connector2.Connect (IPEndPoint(IPAddress.Parse("2607:5300:60:623::1"), 51413), hs)
            receive ()
        let torrentRef = spawn system "torrent" torrentFn
        Console.ReadKey() |> ignore
        0