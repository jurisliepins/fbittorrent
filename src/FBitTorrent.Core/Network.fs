namespace FBitTorrent.Core

open System
open System.IO
open System.Net
open System.Net.Sockets

module BigEndianConverter =
    let convert (bytes: byte[]) =
        if BitConverter.IsLittleEndian then
            Array.Reverse(bytes)
        bytes
        
    let toInt16 (bytes: byte[]) = BitConverter.ToInt16(convert bytes, 0)
    
    let toUInt16 (bytes: byte[]) = BitConverter.ToUInt16(convert bytes, 0)
    
    let toInt32 (bytes: byte[]) = BitConverter.ToInt32(convert bytes, 0)
    
    let toUInt32 (bytes: byte[]) = BitConverter.ToUInt32(convert bytes, 0)
    
    let toInt64 (bytes: byte[]) = BitConverter.ToInt64(convert bytes, 0)
    
    let toUInt64 (bytes: byte[]) = BitConverter.ToUInt64(convert bytes, 0)

    let fromInt16 (value: int16) = BitConverter.GetBytes(value) |> convert
    
    let fromUInt16 (value: uint16) = BitConverter.GetBytes(value) |> convert
    
    let fromInt32 (value: int32) = BitConverter.GetBytes(value) |> convert
    
    let fromUInt32 (value: uint32) = BitConverter.GetBytes(value) |> convert
    
    let fromInt64 (value: int64) = BitConverter.GetBytes(value) |> convert
    
    let fromUInt64 (value: uint64) = BitConverter.GetBytes(value) |> convert

type ConnectionReader(stream: Stream) =
    interface IDisposable with
        member _.Dispose() = stream.Dispose()
    
    member _.ReadBytes(count: int) =
        let mutable offset = 0
        let mutable length = count
        let buffer = Array.zeroCreate count
        while length > 0 do
           let n = stream.Read(buffer, offset, length)   
           if n = 0 then
               failwith "Reached end of stream (connection closed at the other end)"
           offset <- offset + n
           length <- length - n
        buffer

    member _.AsyncReadBytes(count: int) = async {
        let mutable offset = 0
        let mutable length = count
        let buffer = Array.zeroCreate count
        while length > 0 do
           let! n = stream.AsyncRead(buffer, offset, length)   
           if n = 0 then
               failwith "Reached end of stream (connection closed at the other end)"
           offset <- offset + n
           length <- length - n
        return buffer }
    
    member _.ReadByteBuffer(count: int) =
        let mutable offset = 0
        let mutable length = count
        let buffer = ByteBuffer(count)
        while length > 0 do
           let n = buffer.ReadFrom(stream, offset, length)   
           if n = 0 then
               failwith "Reached end of stream (connection closed at the other end)"
           offset <- offset + n
           length <- length - n
        buffer
    
    member _.AsyncReadByteBuffer(count: int) = async {
        let mutable offset = 0
        let mutable length = count
        let buffer = ByteBuffer(count)
        while length > 0 do
           let! n = buffer.AsyncReadFrom(stream, offset, length)   
           if n = 0 then
               failwith "Reached end of stream (connection closed at the other end)"
           offset <- offset + n
           length <- length - n
        return buffer }
    
    member _.ReadByte() =
        let b = stream.ReadByte();
        if b = -1 then
            failwith "Reached end of stream (connection closed at the other end)"
        byte b
    
    member __.AsyncReadByte() = async {
        return __.ReadByte() }
    
    member __.ReadInt16() =
        BigEndianConverter.toInt16 (__.ReadBytes(sizeof<int16>))

    member __.AsyncReadInt16() = async {
        let! bytes = __.AsyncReadBytes(sizeof<int16>)
        return BigEndianConverter.toInt16 bytes }
    
    member __.ReadUInt16() =
        BigEndianConverter.toUInt16 (__.ReadBytes(sizeof<uint16>))

    member __.AsyncReadUInt16() = async {
        let! bytes = __.AsyncReadBytes(sizeof<uint16>)
        return BigEndianConverter.toUInt16 bytes }
    
    member __.ReadInt32() =
        BigEndianConverter.toInt32 (__.ReadBytes(sizeof<int32>))

    member __.AsyncReadInt32() = async {
        let! bytes = __.AsyncReadBytes(sizeof<int32>)
        return BigEndianConverter.toInt32 bytes }
    
    member __.ReadUInt32() =
        BigEndianConverter.toUInt32 (__.ReadBytes(sizeof<uint32>))

    member __.AsyncReadUInt32() = async {
        let! bytes = __.AsyncReadBytes(sizeof<uint32>)
        return BigEndianConverter.toUInt32 bytes }
    
    member __.ReadInt64() =
        BigEndianConverter.toInt64 (__.ReadBytes(sizeof<int64>))

    member __.AsyncReadInt64() = async {
        let! bytes = __.AsyncReadBytes(sizeof<int64>)
        return BigEndianConverter.toInt64 bytes }
    
    member __.ReadUInt64() =
        BigEndianConverter.toUInt64 (__.ReadBytes(sizeof<uint64>))
    
    member __.AsyncReadUInt64() = async {
        let! bytes = __.AsyncReadBytes(sizeof<uint64>)
        return BigEndianConverter.toUInt64 bytes }

type ConnectionWriter(stream: Stream) =
    interface IDisposable with
        member _.Dispose() = stream.Dispose()
    
    member _.Write(value: byte[]) =
        stream.Write(value)
    
    member _.AsyncWrite(value: byte[]) = async {
        return! stream.AsyncWrite(value) }
    
    member _.Write(value: ByteBuffer) =
        value.WriteTo(stream)
    
    member _.AsyncWrite(value: ByteBuffer) = async {
        return! value.AsyncWriteTo(stream) }
    
    member _.Write(value: byte) =
        stream.WriteByte(value)
    
    member __.AsyncWrite(value: byte) = async {
        return __.Write(value) }
    
    member __.Write(value: int16) =
        __.Write(BigEndianConverter.fromInt16 value)

    member __.AsyncWrite(value: int16) = async {
        return! __.AsyncWrite(BigEndianConverter.fromInt16 value) }
    
    member __.Write(value: uint16) =
        __.Write(BigEndianConverter.fromUInt16 value)

    member __.AsyncWrite(value: uint16) = async {
        return! __.AsyncWrite(BigEndianConverter.fromUInt16 value) }
    
    member __.Write(value: int32) =
        __.Write(BigEndianConverter.fromInt32 value)

    member __.AsyncWrite(value: int32) = async {
        return! __.AsyncWrite(BigEndianConverter.fromInt32 value) }
    
    member __.Write(value: uint32) =
        __.Write(BigEndianConverter.fromUInt32 value)

    member __.AsyncWrite(value: uint32) = async {
        return! __.AsyncWrite(BigEndianConverter.fromUInt32 value) }
    
    member __.Write(value: int64) =
        __.Write(BigEndianConverter.fromInt64 value)

    member _.AsyncWrite(value: int64) = async {
        return! stream.AsyncWrite(BigEndianConverter.fromInt64 value) }
    
    member __.Write(value: uint64) =
        __.Write(BigEndianConverter.fromUInt64 value)

    member __.AsyncWrite(value: uint64) = async {
        return! __.AsyncWrite(BigEndianConverter.fromUInt64 value) }
    
    member _.Flush() = stream.Flush()

    member _.AsyncFlush() = Async.AwaitTask(stream.FlushAsync())

type IConnection =
    inherit IDisposable
    abstract member Stream: Stream with get
    abstract member Writer: ConnectionWriter with get
    abstract member Reader: ConnectionReader with get
    abstract member RemoteEndpoint: IPEndPoint with get
    abstract member LocalEndpoint: IPEndPoint with get
    abstract member Disconnect: unit -> unit

module Connection =
    type private TcpClient with
        static member AsyncConnect(endpoint: IPEndPoint) = Async.AwaitTask(task {
            let client = new TcpClient()
            do! client.ConnectAsync(endpoint)
            return client })
            
    let createTcpConnection (client: TcpClient) =
        let stream = client.GetStream()
        let writer = new ConnectionWriter(stream)
        let reader = new ConnectionReader(stream)
        { new IConnection with
            member _.Stream with get() = stream
            member _.Writer with get() = writer
            member _.Reader with get() = reader
            member _.RemoteEndpoint with get() = client.Client.RemoteEndPoint :?> IPEndPoint
            member _.LocalEndpoint with get() = client.Client.LocalEndPoint :?> IPEndPoint
            member _.Disconnect() = client.Close()
            member _.Dispose() = client.Dispose() }
        
    let asyncTcpConnect endpoint = async {
        let! client = TcpClient.AsyncConnect(endpoint) 
        return createTcpConnection client }
    
type IConnectionListener =
    abstract member LocalEndpoint: IPEndPoint with get
    abstract member AsyncAcceptConnection: unit -> Async<IConnection>
    abstract member Stop: unit -> unit

module ConnectionListener =
    type private TcpListener with
        static member Start(endpoint: IPEndPoint) = 
            let listener = TcpListener(endpoint)
            listener.Start()
            listener
            
    let createTcpListener (listener: TcpListener) =
        { new IConnectionListener with
            member _.LocalEndpoint with get() = listener.LocalEndpoint :?> IPEndPoint
            member _.AsyncAcceptConnection() = Async.AwaitTask(task {
                let! client = listener.AcceptTcpClientAsync()
                return client |> Connection.createTcpConnection })
            member _.Stop() = listener.Stop()}
        
    let tcpListen endpoint =
        let listener = TcpListener.Start(endpoint)
        createTcpListener listener