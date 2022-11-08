namespace FBitTorrent.Core.Tests

open System
open System.IO
open System.Net
open System.Text
open Akka.FSharp
open Akka.TestKit.Xunit2
open FBitTorrent.Core
open Xunit

// type ConnectorTests() = 
//     inherit TestKit()
//
//     let successConnection =
//         { new IConnection with
//             member _.Stream with get() = new MemoryStream()
//             member _.Writer with get() = new ConnectionWriter(new MemoryStream())
//             member _.Reader with get() = new ConnectionReader(new MemoryStream())
//             member _.RemoteEndpoint with get() = IPEndPoint.Parse("127.0.0.1:6570")
//             member _.LocalEndpoint with get() = IPEndPoint.Parse("127.0.0.1:9570")
//             member _.Disconnect() = ()
//             member _.Dispose() = () }
//     
//     let failureConnection =
//         { new IConnection with
//             member _.Stream with get() = failwith "Failed to get stream"
//             member _.Writer with get() = failwith "Failed to get writer"
//             member _.Reader with get() = failwith "Failed to get reader"
//             member _.RemoteEndpoint with get() = failwith "Failed to get remote endpoint"
//             member _.LocalEndpoint with get() = failwith "Failed to get local endpoint"
//             member _.Disconnect() = ()
//             member _.Dispose() = () }
//     
//     let successHandshake =
//         let bytes = Encoding.Latin1.GetBytes("success")
//         Handshake.create bytes bytes bytes bytes
//         
//     let failureProtocolHandshake =
//         let bytes = Encoding.Latin1.GetBytes("success")
//         let proto = Encoding.Latin1.GetBytes("failure")
//         Handshake.create proto bytes bytes bytes
//         
//     let failureInfoHashHandshake =
//         let bytes = Encoding.Latin1.GetBytes("success")
//         let ih = Encoding.Latin1.GetBytes("failure")
//         Handshake.create bytes bytes ih bytes
//     
//     let successHandshakeConnection = async {
//         let connection =
//             { new IHandshakeConnection with
//                 member _.Connection with get() = successConnection
//                 member _.WriteHandshake(handshake: Handshake) = ()
//                 member _.AsyncWriteHandshake(handshake: Handshake) = async { () }
//                 member _.ReadHandshake() = successHandshake
//                 member _.AsyncReadHandshake() = async { return successHandshake }
//                 member _.Dispose() = () }
//         return connection}
//     
//     let failureHandshakeConnection = async {
//         let connection =
//             { new IHandshakeConnection with
//                 member _.Connection with get() = failureConnection
//                 member _.WriteHandshake(handshake: Handshake) = failwith "Failed to write handshake"
//                 member _.AsyncWriteHandshake(handshake: Handshake) = failwith "Failed to write handshake"
//                 member _.ReadHandshake() = failwith "Failed to read handshake"
//                 member _.AsyncReadHandshake() = failwith "Failed to read handshake" 
//                 member _.Dispose() = () }
//         return connection }
//     
//     let successConnectorFn mailbox =
//         let asyncConnect _ = successHandshakeConnection
//         Connector.actorFn asyncConnect mailbox
//     
//     let failureConnectionConnectorFn mailbox =
//         let connect _ = failwith "Failed to connect"
//         Connector.actorFn connect mailbox
//         
//     let rec failureHandshakeConnectorFn mailbox =
//         let connect _ = failureHandshakeConnection
//         Connector.actorFn connect mailbox
//     
//     let connectCommand =
//         Connector.Connect (IPAddress.Loopback, 0, Handshake.defaultCreate (Encoding.Latin1.GetBytes("")) (Encoding.Latin1.GetBytes("")))
//     
//     let assertSuccess (commandResult: Connector.CommandResult) =
//         match commandResult with
//         | Connector.Success (connection, handshake) ->
//             Assert.Equal(successConnection, connection)
//             Assert.Equal(successHandshake, handshake)
//         | _ ->
//             Assert.False(true, "Connect should have succeeded")
//     
//     let assertConnectFailure (commandResult: Connector.CommandResult) =
//         match commandResult with
//         | Connector.Failure (_, _, error) ->
//             Assert.Equal("Failed to connect to 127.0.0.1:0", error.Message)
//         | _ ->
//             Assert.False(true, "Connect should have failed")
//             
//     let assertHandshakeFailure (commandResult: Connector.CommandResult) =
//         match commandResult with
//         | Connector.Failure (_, _, error) ->
//             Assert.Equal("Failed to handshake with 127.0.0.1:0", error.Message)
//         | _ ->
//             Assert.False(true, "Connect should have failed")
//             
//     let assertProtocolNotValidFailure (commandResult: Connector.CommandResult) =
//         match commandResult with
//         | Connector.Failure (_, _, error) ->
//             Assert.Equal("Failed to handshake with 127.0.0.1:0", error.Message)
//         | _ ->
//             Assert.False(true, "Connect should have failed")
//             
//     let assertInfoHashNotValidFailure (commandResult: Connector.CommandResult) =
//         match commandResult with
//         | Connector.Failure (_, _, error) ->
//             Assert.Equal("Failed to handshake with 127.0.0.1:0", error.Message)
//         | _ ->
//             Assert.False(true, "Connect should have failed")
//     
//     [<Fact>]
//     member __. ``Test should connect succeed``() =
//         let connectorRef = spawn __.Sys (Connector.actorName ()) successConnectorFn
//         let connectorCommandResult = connectorRef.Ask<Connector.CommandResult>(connectCommand, TimeSpan.FromSeconds 3) |> Async.RunSynchronously
//         assertSuccess connectorCommandResult
//         
//     [<Fact>]
//     member __. ``Test should connect fail``() =
//         let connectorRef = spawn __.Sys (Connector.actorName ()) failureConnectorFn
//         let connectorCommandResult = connectorRef.Ask<Connector.CommandResult>(connectCommand, TimeSpan.FromSeconds 3) |> Async.RunSynchronously
//         assertConnectFailure connectorCommandResult