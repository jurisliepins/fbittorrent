namespace FBitTorrent.Core

open System
open System.IO
open System.Net
open System.Net.Sockets

module BigEndianConverter =
    let convert (bytes: byte[]) =
        if BitConverter.IsLittleEndian then
            Array.Reverse bytes
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
    inherit BinaryReader(stream)

    member private _.ReadNBytes(count: int) =
        let bytes = base.ReadBytes count
        if bytes.Length = 0 then
            failwith "Encountered end of stream - connection closed at the other end"
        else
            bytes

    override __.ReadInt16() =
        BigEndianConverter.toInt16 (__.ReadNBytes(sizeof<int16>))

    override __.ReadUInt16() =
        BigEndianConverter.toUInt16 (__.ReadNBytes(sizeof<uint16>))

    override __.ReadInt32() =
        BigEndianConverter.toInt32 (__.ReadNBytes(sizeof<int32>))

    override __.ReadUInt32() =
        BigEndianConverter.toUInt32 (__.ReadNBytes(sizeof<uint32>))

    override __.ReadInt64() =
        BigEndianConverter.toInt64 (__.ReadNBytes(sizeof<int64>))

    override __.ReadUInt64() =
        BigEndianConverter.toUInt64 (__.ReadNBytes(sizeof<uint64>))

type ConnectionWriter(stream: Stream) =
    inherit BinaryWriter(stream)

    member private _.WriteNBytes(bytes: byte[]) =
        base.Write(bytes)

    override __.Write(value: int16) =
        __.WriteNBytes(BigEndianConverter.fromInt16(value))

    override __.Write(value: uint16) =
        __.WriteNBytes(BigEndianConverter.fromUInt16(value))

    override __.Write(value: int32) =
        __.WriteNBytes(BigEndianConverter.fromInt32(value))

    override __.Write(value: uint32) =
        __.WriteNBytes(BigEndianConverter.fromUInt32(value))

    override __.Write(value: int64) =
        __.WriteNBytes(BigEndianConverter.fromInt64(value))

    override __.Write(value: uint64) =
        __.WriteNBytes(BigEndianConverter.fromUInt64(value))

type IConnection =
    inherit IDisposable
    abstract member Stream: Stream with get
    abstract member Reader: ConnectionReader with get
    abstract member Writer: ConnectionWriter with get
    abstract member RemoteEndpoint: IPEndPoint with get
    abstract member LocalEndpoint: IPEndPoint with get
    abstract member Disconnect: unit -> unit

module Connection =
    type Connect = IPEndPoint -> IConnection
    
    type private TcpClient with
        static member Connect(endpoint: IPEndPoint) =
            let client = new TcpClient()
            client.Connect endpoint
            client
            
    let createTcpConnection (client: TcpClient) =
        let stream = client.GetStream()
        let reader = new ConnectionReader(stream)
        let writer = new ConnectionWriter(stream)
        { new IConnection with
            member _.Stream with get() = stream
            member _.Reader with get() = reader
            member _.Writer with get() = writer
            member _.RemoteEndpoint with get() = client.Client.RemoteEndPoint :?> IPEndPoint
            member _.LocalEndpoint with get() = client.Client.LocalEndPoint :?> IPEndPoint
            member _.Disconnect() = client.Close()
            member _.Dispose() = client.Dispose() }
        
    let tcpConnect endpoint = TcpClient.Connect endpoint |> createTcpConnection
    
type IConnectionListener =
    abstract member LocalEndpoint: IPEndPoint with get
    abstract member AcceptConnection: unit -> IConnection
    abstract member Stop: unit -> unit

module ConnectionListener =
    type Listen = IPEndPoint -> IConnectionListener
            
    type private TcpListener with
        static member Start(endpoint: IPEndPoint) =
            let listener = TcpListener(endpoint)
            listener.Start()
            listener
            
    let createTcpListener (listener: TcpListener) =
        { new IConnectionListener with
            member _.LocalEndpoint with get() = listener.LocalEndpoint :?> IPEndPoint
            member _.AcceptConnection() = listener.AcceptTcpClient() |> Connection.createTcpConnection
            member _.Stop() = listener.Stop()}
        
    let tcpListen endpoint = TcpListener.Start endpoint |> createTcpListener