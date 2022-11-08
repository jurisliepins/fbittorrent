namespace FBitTorrent.Core.Tests

open System
open System.IO
open FBitTorrent.Core
open Xunit
open System.Net
open System.Text

module ConnectionStreamTests =

    let shouldWriteRead (write: ConnectionWriter -> 'a -> unit) (read: ConnectionReader -> 'a) (expectedValue: 'a) =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        use reader = new ConnectionReader(stream)
        write writer expectedValue
        stream.Position <- 0
        let actualValue = read reader
        Assert.Equal<'a>(actualValue, expectedValue)

    let shouldAsyncWriteRead (asyncWrite: ConnectionWriter -> 'a -> Async<unit>) (read: ConnectionReader -> Async<'a>) (expectedValue: 'a) = async {
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        use reader = new ConnectionReader(stream)
        do! asyncWrite writer expectedValue
        stream.Position <- 0
        let! actualValue = read reader
        Assert.Equal<'a>(actualValue, expectedValue) }
    
    [<Fact>]
    let ``Test should write/read Byte[]`` () =
        let buffer = [| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; |]
        let w (writer: ConnectionWriter) (value: Byte[]) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadBytes(8)
        shouldWriteRead w r buffer
        
    [<Fact>]
    let ``Test should async write/read Byte[]`` () = async{
        let buffer = [| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; |]
        let w (writer: ConnectionWriter) (value: Byte[]) = writer.AsyncWrite(value)
        let r (reader: ConnectionReader) = reader.AsyncReadBytes(8)
        do! shouldAsyncWriteRead w r buffer }
    
    [<Fact>]
    let ``Test should write/read ByteBuffer`` () =
        let buffer = ByteBuffer([| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; |])
        let w (writer: ConnectionWriter) (value: ByteBuffer) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadByteBuffer(8)
        shouldWriteRead w r buffer
        
    [<Fact>]
    let ``Test should async write/read ByteBuffer`` () = async{
        let buffer = ByteBuffer([| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; |])
        let w (writer: ConnectionWriter) (value: ByteBuffer) = writer.AsyncWrite(value)
        let r (reader: ConnectionReader) = reader.AsyncReadByteBuffer(8)
        do! shouldAsyncWriteRead w r buffer }
    
    [<Fact>]
    let ``Test should write/read Byte`` () =
        let min = Byte.MinValue
        let max = Byte.MaxValue
        let w (writer: ConnectionWriter) (value: Byte) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadByte()
        shouldWriteRead w r min
        shouldWriteRead w r max
        
    [<Fact>]
    let ``Test should async write/read Byte`` () = async {
        let min = Byte.MinValue
        let max = Byte.MaxValue
        let w (writer: ConnectionWriter) (value: Byte) = writer.AsyncWrite(value)
        let r (reader: ConnectionReader) = reader.AsyncReadByte()
        do! shouldAsyncWriteRead w r min
        do! shouldAsyncWriteRead w r max }

    [<Fact>]
    let ``Test should write/read big-endian Int16`` () =
        let min = Int16.MinValue
        let max = Int16.MaxValue
        let w (writer: ConnectionWriter) (value: Int16) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadInt16()
        shouldWriteRead w r min
        shouldWriteRead w r max
        
    [<Fact>]
    let ``Test should async write/read big-endian Int16`` () = async {
        let min = Int16.MinValue
        let max = Int16.MaxValue
        let w (writer: ConnectionWriter) (value: Int16) = writer.AsyncWrite(value)
        let r (reader: ConnectionReader) = reader.AsyncReadInt16()
        do! shouldAsyncWriteRead w r min
        do! shouldAsyncWriteRead w r max }

    [<Fact>]
    let ``Test should write/read big-endian UInt16`` () =
        let min = UInt16.MinValue
        let max = UInt16.MaxValue
        let w (writer: ConnectionWriter) (value: UInt16) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadUInt16()
        shouldWriteRead w r min
        shouldWriteRead w r max
        
    [<Fact>]
    let ``Test should async write/read big-endian UInt16`` () = async {
        let min = UInt16.MinValue
        let max = UInt16.MaxValue
        let w (writer: ConnectionWriter) (value: UInt16) = writer.AsyncWrite(value)
        let r (reader: ConnectionReader) = reader.AsyncReadUInt16()
        do! shouldAsyncWriteRead w r min
        do! shouldAsyncWriteRead w r max }

    [<Fact>]
    let ``Test should write/read big-endian Int32`` () =
        let min = Int32.MinValue
        let max = Int32.MaxValue
        let w (writer: ConnectionWriter) (value: Int32) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadInt32()
        shouldWriteRead w r min
        shouldWriteRead w r max
        
    [<Fact>]
    let ``Test should async write/read big-endian Int32`` () = async {
        let min = Int32.MinValue
        let max = Int32.MaxValue
        let w (writer: ConnectionWriter) (value: Int32) = writer.AsyncWrite(value)
        let r (reader: ConnectionReader) = reader.AsyncReadInt32()
        do! shouldAsyncWriteRead w r min
        do! shouldAsyncWriteRead w r max }

    [<Fact>]
    let ``Test should write/read big-endian UInt32`` () =
        let min = UInt32.MinValue
        let max = UInt32.MaxValue
        let w (writer: ConnectionWriter) (value: UInt32) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadUInt32()
        shouldWriteRead w r min
        shouldWriteRead w r max
        
    [<Fact>]
    let ``Test should async write/read big-endian UInt32`` () = async {
        let min = UInt32.MinValue
        let max = UInt32.MaxValue
        let w (writer: ConnectionWriter) (value: UInt32) = writer.AsyncWrite(value)
        let r (reader: ConnectionReader) = reader.AsyncReadUInt32()
        do! shouldAsyncWriteRead w r min
        do! shouldAsyncWriteRead w r max }
        
    [<Fact>]
    let ``Test should write/read big-endian Int64`` () =
        let min = Int64.MinValue
        let max = Int64.MaxValue
        let w (writer: ConnectionWriter) (value: Int64) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadInt64()
        shouldWriteRead w r min
        shouldWriteRead w r max
        
    [<Fact>]
    let ``Test should async write/read big-endian Int64`` () = async {
        let min = Int64.MinValue
        let max = Int64.MaxValue
        let w (writer: ConnectionWriter) (value: Int64) = writer.AsyncWrite(value)
        let r (reader: ConnectionReader) = reader.AsyncReadInt64()
        do! shouldAsyncWriteRead w r min
        do! shouldAsyncWriteRead w r max }

    [<Fact>]
    let ``Test should write/read big-endian UInt64`` () =
        let min = UInt64.MinValue
        let max = UInt64.MaxValue
        let w (writer: ConnectionWriter) (value: UInt64) = writer.Write(value)
        let r (reader: ConnectionReader) = reader.ReadUInt64()
        shouldWriteRead w r min
        shouldWriteRead w r max
        
    [<Fact>]
    let ``Test should async write/read big-endian UInt64`` () = async {
        let min = UInt64.MinValue
        let max = UInt64.MaxValue
        let w (writer: ConnectionWriter) (value: UInt64) = writer.AsyncWrite(value)
        let r (reader: ConnectionReader) = reader.AsyncReadUInt64()
        do! shouldAsyncWriteRead w r min
        do! shouldAsyncWriteRead w r max }
        
        
module ConnectionTests =
        
    [<Fact>]
    let ``Test TCP connection should connect`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65000))
        Async.Start(async { let! _ = listener.AsyncAcceptConnection() in () })
        Async.RunSynchronously(async { let! _ = Connection.asyncTcpConnect (IPEndPoint(IPAddress.Loopback, 65000)) in () })

    [<Fact>]
    let ``Test TCP connection should disconnect`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65001))
        Async.Start(async { let! _ = listener.AsyncAcceptConnection() in () })
        Async.RunSynchronously(async { let! connection = Connection.asyncTcpConnect (IPEndPoint(IPAddress.Loopback, 65001)) in connection.Disconnect() })

    [<Fact>]
    let ``Test TCP connection should have access to stream`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65002))
        Async.Start(async {
            let! connection = listener.AsyncAcceptConnection()
            Assert.True(connection.Stream.CanRead, "Accepted stream should have been readable")
            Assert.True(connection.Stream.CanWrite, "Accepted stream should have been writeable") })
        Async.RunSynchronously(async {
            let! connection = Connection.asyncTcpConnect (IPEndPoint(IPAddress.Loopback, 65002))
            Assert.True(connection.Stream.CanRead, "Connected stream should have been readable")
            Assert.True(connection.Stream.CanWrite, "Connected stream should have been writeable") })
        
    [<Fact>]
    let ``Test TCP connection should have access to remote endpoint`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65003))
        Async.Start(async {
            let! connection = listener.AsyncAcceptConnection()
            Assert.NotNull(connection.RemoteEndpoint) })
        Async.RunSynchronously(async {
            let! connection = Connection.asyncTcpConnect (IPEndPoint(IPAddress.Loopback, 65003))
            Assert.NotNull(connection.RemoteEndpoint) })
        
    [<Fact>]
    let ``Test TCP connection should have access to local endpoint`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65004))
        Async.Start(async {
            let! connection = listener.AsyncAcceptConnection()
            Assert.NotNull(connection.RemoteEndpoint)
        })
        Async.RunSynchronously(async {
            let! connection = Connection.asyncTcpConnect (IPEndPoint(IPAddress.Loopback, 65004))
            Assert.NotNull(connection.RemoteEndpoint) })

    [<Fact>]
    let ``Test TCP connection should stream read and write`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65005))
        Async.Start(async {
            let! connection = listener.AsyncAcceptConnection()
            let reader = new ConnectionReader(connection.Stream)
            let writer = new ConnectionWriter(connection.Stream)
            let x = reader.ReadInt32()
            let y = reader.ReadBytes(x)
            writer.Write x
            writer.Write y
            writer.Flush()
        })
        Async.RunSynchronously(async {
            let! connection = Connection.asyncTcpConnect (IPEndPoint(IPAddress.Loopback, 65005))
            let reader = new ConnectionReader(connection.Stream)
            let writer = new ConnectionWriter(connection.Stream)
            writer.Write 13
            writer.Write (Encoding.ASCII.GetBytes "Hello, World!")
            writer.Flush()
            let x = reader.ReadInt32()
            let y = reader.ReadBytes(x)
            Assert.Equal(13, x)
            Assert.Equal<byte[]>((Encoding.ASCII.GetBytes "Hello, World!"), y) })
    
    [<Fact>]
    let ``Test TCP connection should stream async read and write`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65006))
        Async.Start(async {
            let! connection = listener.AsyncAcceptConnection()
            let reader = new ConnectionReader(connection.Stream)
            let writer = new ConnectionWriter(connection.Stream)
            let! x = reader.AsyncReadInt32()
            let! y = reader.AsyncReadBytes(x)
            do! writer.AsyncWrite x
            do! writer.AsyncWrite y
            do! writer.AsyncFlush()
        })
        Async.RunSynchronously(async {
            let! connection = Connection.asyncTcpConnect (IPEndPoint(IPAddress.Loopback, 65006))
            let reader = new ConnectionReader(connection.Stream)
            let writer = new ConnectionWriter(connection.Stream)
            do! writer.AsyncWrite 13
            do! writer.AsyncWrite (Encoding.ASCII.GetBytes "Hello, World!")
            do! writer.AsyncFlush()
            let! x = reader.AsyncReadInt32()
            let! y = reader.AsyncReadBytes(x)
            Assert.Equal(13, x)
            Assert.Equal<byte[]>((Encoding.ASCII.GetBytes "Hello, World!"), y) })
        
module ConnectionListenerTests =

    [<Fact>]
    let ``Test TCP listener should listen`` () =
        ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65007)) |> ignore
        
    [<Fact>]
    let ``Test TCP listener should accept connection`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65008))
        Async.Start(async { let! _ = listener.AsyncAcceptConnection() in () })
        Async.RunSynchronously(async { let! _ = Connection.asyncTcpConnect (IPEndPoint(IPAddress.Loopback, 65008)) in () })
        
    [<Fact>]
    let ``Test TCP listener should stop`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65009))
        listener.Stop()
        
    [<Fact>]
    let ``Test TCP listener should have access to local endpoint`` () =
        let listener = ConnectionListener.tcpListen (IPEndPoint(IPAddress.Loopback, 65010))
        Assert.NotNull(listener.LocalEndpoint)