namespace FBitTorrent.Core.Tests

open System
open System.IO
open FBitTorrent.Core
open Xunit
open System.Net
open System.Text

module ConnectionStreamTests =

    let shouldWriteReadBigEndian (write: ConnectionWriter -> 'a -> unit) (read: ConnectionReader -> 'a) (value: 'a) =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        use reader = new ConnectionReader(stream)
        write writer value
        stream.Position <- 0
        Assert.Equal<'a>((read reader), value)

    [<Fact>]
    let ``Test should write/read big-endian Byte`` () =
        let min = Byte.MinValue
        let max = Byte.MaxValue
        let w (writer: ConnectionWriter) (value: Byte) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadByte()
        shouldWriteReadBigEndian w r min
        shouldWriteReadBigEndian w r max

    [<Fact>]
    let ``Test should write/read big-endian Int16`` () =
        let min = Int16.MinValue
        let max = Int16.MaxValue
        let w (writer: ConnectionWriter) (value: Int16) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadInt16()
        shouldWriteReadBigEndian w r min
        shouldWriteReadBigEndian w r max

    [<Fact>]
    let ``Test should write/read big-endian UInt16`` () =
        let min = UInt16.MinValue
        let max = UInt16.MaxValue
        let w (writer: ConnectionWriter) (value: UInt16) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadUInt16()
        shouldWriteReadBigEndian w r min
        shouldWriteReadBigEndian w r max

    [<Fact>]
    let ``Test should write/read big-endian Int32`` () =
        let min = Int32.MinValue
        let max = Int32.MaxValue
        let w (writer: ConnectionWriter) (value: Int32) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadInt32()
        shouldWriteReadBigEndian w r min
        shouldWriteReadBigEndian w r max

    [<Fact>]
    let ``Test should write/read big-endian UInt32`` () =
        let min = UInt32.MinValue
        let max = UInt32.MaxValue
        let w (writer: ConnectionWriter) (value: UInt32) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadUInt32()
        shouldWriteReadBigEndian w r min
        shouldWriteReadBigEndian w r max

    [<Fact>]
    let ``Test should write/read big-endian Int64`` () =
        let min = Int64.MinValue
        let max = Int64.MaxValue
        let w (writer: ConnectionWriter) (value: Int64) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadInt64()
        shouldWriteReadBigEndian w r min
        shouldWriteReadBigEndian w r max

    [<Fact>]
    let ``Test should write/read big-endian UInt64`` () =
        let min = UInt64.MinValue
        let max = UInt64.MaxValue
        let w (writer: ConnectionWriter) (value: UInt64) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadUInt64()
        shouldWriteReadBigEndian w r min
        shouldWriteReadBigEndian w r max
        
module ConnectionTests =
        
    [<Fact>]
    let ``Test TCP connection should connect`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65000))
        Async.Start(async { listener.AcceptConnection() |> ignore })
        Connection.tcpConnect (IPEndPoint(IPAddress.Loopback, 65000)) |> ignore

    [<Fact>]
    let ``Test TCP connection should disconnect`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65001))
        Async.Start(async { listener.AcceptConnection() |> ignore })
        let connection = Connection.tcpConnect (IPEndPoint(IPAddress.Loopback, 65001))
        connection.Disconnect()

    [<Fact>]
    let ``Test TCP connection should have access to stream`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65002))
        Async.Start(async {
            let connection = listener.AcceptConnection()
            Assert.True(connection.Stream.CanRead, "Accepted stream should have been readable")
            Assert.True(connection.Stream.CanWrite, "Accepted stream should have been writeable")
        })
        let connection = Connection.tcpConnect (IPEndPoint(IPAddress.Loopback, 65002))
        Assert.True(connection.Stream.CanRead, "Connected stream should have been readable")
        Assert.True(connection.Stream.CanWrite, "Connected stream should have been writeable")
        
    [<Fact>]
    let ``Test TCP connection should have access to remote endpoint`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65003))
        Async.Start(async {
            let connection = listener.AcceptConnection()
            Assert.NotNull(connection.RemoteEndpoint)
        })
        let connection = Connection.tcpConnect (IPEndPoint(IPAddress.Loopback, 65003))
        Assert.NotNull(connection.RemoteEndpoint)
        
    [<Fact>]
    let ``Test TCP connection should have access to local endpoint`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65004))
        Async.Start(async {
            let connection = listener.AcceptConnection()
            Assert.NotNull(connection.RemoteEndpoint)
        })
        let connection = Connection.tcpConnect (IPEndPoint(IPAddress.Loopback, 65004))
        Assert.NotNull(connection.RemoteEndpoint)

    [<Fact>]
    let ``Test TCP connection should stream read and write`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65005))
        Async.Start(async {
            let connection = listener.AcceptConnection()
            let reader = new ConnectionReader(connection.Stream)
            let writer = new ConnectionWriter(connection.Stream)
            let x = reader.ReadInt32()
            let y = reader.ReadBytes(x)
            writer.Write x
            writer.Write y
            writer.Flush()
        })
        let connection = Connection.tcpConnect (IPEndPoint(IPAddress.Loopback, 65005))
        let reader = new ConnectionReader(connection.Stream)
        let writer = new ConnectionWriter(connection.Stream)
        writer.Write 13
        writer.Write (Encoding.ASCII.GetBytes "Hello, World!")
        writer.Flush()
        let x = reader.ReadInt32()
        let y = reader.ReadBytes(x)
        Assert.Equal(13, x)
        Assert.Equal<byte[]>((Encoding.ASCII.GetBytes "Hello, World!"), y)
        
module ConnectionListenerTests =

    [<Fact>]
    let ``Test TCP listener should listen`` () =
        ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65006)) |> ignore
        
    [<Fact>]
    let ``Test TCP listener should accept connection`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65007))
        Async.Start(async { listener.AcceptConnection() |> ignore })
        Connection.tcpConnect (IPEndPoint(IPAddress.Loopback, 65007)) |> ignore
        
    [<Fact>]
    let ``Test TCP listener should stop`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65008))
        listener.Stop()
        
    [<Fact>]
    let ``Test TCP listener should have access to local endpoint`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65009))
        Assert.NotNull(listener.LocalEndpoint)