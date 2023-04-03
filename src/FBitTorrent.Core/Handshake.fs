namespace FBitTorrent.Core

open System
open System.IO
open System.Net
open System.Text
open FBitTorrent.Core

[<Struct>]
type Handshake =
    Handshake of 
        Protocol: byte[] * 
        Reserved: byte[] * 
        InfoHash: byte[] * 
        PeerId:   byte[]

type IHandshakeConnection =
    inherit IDisposable
    abstract member Connection: IConnection with get
    abstract member WriteHandshake: Handshake * int -> unit
    abstract member ReadHandshake: int -> Handshake
    abstract member AsyncWriteHandshake: Handshake * int -> Async<unit>
    abstract member AsyncReadHandshake: int -> Async<Handshake>
    abstract member WriteHandshake: Handshake -> unit
    abstract member ReadHandshake: unit -> Handshake
    abstract member AsyncWriteHandshake: Handshake -> Async<unit>
    abstract member AsyncReadHandshake: unit -> Async<Handshake>
    
module Handshake =
    
    let protocolBytes = [| 66uy; 105uy; 116uy; 84uy; 111uy; 114uy; 114uy; 101uy; 110uy; 116uy; 32uy; 112uy; 114uy; 111uy; 116uy; 111uy; 99uy; 111uy; 108uy |]
    let reservedBytes = [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; |]

    let [<Literal>] DefaultWriteTimeoutMillis = 5_000
    
    let [<Literal>] DefaultReadTimeoutMillis = 5_000
    
    let create (proto: byte[]) (res: byte[]) (ih: byte[]) (pid: byte[]) =
        Handshake (proto, res, ih, pid)
    
    let defaultCreate (ih: byte[]) (pid: byte[]) =
        create protocolBytes reservedBytes ih pid
    
    let write (writer: ConnectionWriter) (Handshake (proto, res, ih, pid): Handshake) =
        writer.Write(proto.Length |> byte)
        writer.Write(proto)
        writer.Write(res)
        writer.Write(ih)
        writer.Write(pid)
        writer.Flush()
        
    let asyncWrite (writer: ConnectionWriter) (Handshake (proto, res, ih, pid): Handshake) = async {
        do! writer.AsyncWrite(proto.Length |> byte)
        do! writer.AsyncWrite(proto)
        do! writer.AsyncWrite(res)
        do! writer.AsyncWrite(ih)
        do! writer.AsyncWrite(pid)
        do! writer.AsyncFlush() }
    
    let read (reader: ConnectionReader) =
        let len = reader.ReadByte()
        let proto = reader.ReadBytes(int len)
        let res = reader.ReadBytes(8)
        let ih = reader.ReadBytes(20)
        let pid = reader.ReadBytes(20)
        Handshake (proto, res, ih, pid)
    
    let asyncRead (reader: ConnectionReader) = async {
        let! len = reader.AsyncReadByte()
        let! proto = reader.AsyncReadBytes(int len)
        let! res = reader.AsyncReadBytes(8)
        let! ih = reader.AsyncReadBytes(20)
        let! pid = reader.AsyncReadBytes(20)
        return Handshake (proto, res, ih, pid) }
        
    let toBytes handshake =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        write writer handshake
        stream.ToArray()
        
    let toString handshake =
        Encoding.Latin1.GetString(toBytes handshake)
        
    let fromBytes (bytes: byte[]) =
        use stream = new MemoryStream(bytes)
        use reader = new ConnectionReader(stream)
        read reader
        
    let fromString (string: string) =
         fromBytes (Encoding.Latin1.GetBytes(string))

    let createConnection (connection: IConnection) =
        { new IHandshakeConnection with
            member _.Connection with get() = connection
            member _.WriteHandshake(handshake: Handshake, timeoutMillis: int) =
                connection.Stream.WriteTimeout <- timeoutMillis
                write connection.Writer handshake
            member _.ReadHandshake(timeoutMillis: int) =
                connection.Stream.ReadTimeout <- timeoutMillis
                read connection.Reader
            member _.AsyncWriteHandshake(handshake: Handshake, timeoutMillis: int) =
                connection.Stream.WriteTimeout <- timeoutMillis
                asyncWrite connection.Writer handshake
            member _.AsyncReadHandshake(timeoutMillis: int) =
                connection.Stream.ReadTimeout <- timeoutMillis
                asyncRead connection.Reader
            member __.WriteHandshake(handshake: Handshake) = __.WriteHandshake(handshake, DefaultWriteTimeoutMillis)
            member __.ReadHandshake() = __.ReadHandshake(DefaultReadTimeoutMillis)
            member __.AsyncWriteHandshake(handshake: Handshake) = __.AsyncWriteHandshake(handshake, DefaultWriteTimeoutMillis)
            member __.AsyncReadHandshake() = __.AsyncReadHandshake(DefaultReadTimeoutMillis)
            member _.Dispose() = connection.Dispose() }