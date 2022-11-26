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
    abstract member WriteHandshake: Handshake -> unit
    abstract member ReadHandshake: unit -> Handshake
    
module Handshake =
    type Connect = IPEndPoint -> IHandshakeConnection
    
    let ProtocolBytes = [| 66uy; 105uy; 116uy; 84uy; 111uy; 114uy; 114uy; 101uy; 110uy; 116uy; 32uy;
                            112uy; 114uy; 111uy; 116uy; 111uy; 99uy; 111uy; 108uy |]
    let ReservedBytes = [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; |]

    let DefaultWriteTimeoutMillis = 5_000
    let DefaultReadTimeoutMillis = 5_000
    
    let create (proto: byte[]) (res: byte[]) (ih: byte[]) (pid: byte[]) =
        Handshake (proto, res, ih, pid)
    
    let defaultCreate (ih: byte[]) (pid: byte[]) =
        create ProtocolBytes ReservedBytes ih pid
        
    let write (writer: ConnectionWriter) (Handshake (proto, res, ih, pid): Handshake) =
        writer.Write(proto.Length |> byte)
        writer.Write(proto)
        writer.Write(res)
        writer.Write(ih)
        writer.Write(pid)
        writer.Flush()
    
    let read (reader: ConnectionReader) =
        let len = reader.ReadByte()
        let proto = reader.ReadBytes(int len)
        let res = reader.ReadBytes(8)
        let ih = reader.ReadBytes(20)
        let pid = reader.ReadBytes(20)
        Handshake (proto, res, ih, pid)
        
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
            member _.WriteHandshake(handshake: Handshake) = write connection.Writer handshake
            member _.ReadHandshake() = read connection.Reader
            member _.Dispose() = connection.Dispose() }
    
    let tcpConnect endpoint = Connection.tcpConnect endpoint |> createConnection    