namespace FBitTorrent.Core

open System.IO
open System.Text
open FBitTorrent.Core

[<Struct>]
type Handshake =
    Handshake of 
        Protocol: byte[] * 
        Reserved: byte[] * 
        InfoHash: byte[] * 
        PeerId:   byte[]
    
module Handshake =
    
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
        
module HandshakeExtensions =
    type IConnection with
        member __.WriteHandshake(handshake: Handshake, ?timeout: int) =
            __.Stream.WriteTimeout <- defaultArg timeout Handshake.DefaultWriteTimeoutMillis
            Handshake.write __.Writer handshake
        member __.ReadHandshake(?timeout: int) =
            __.Stream.ReadTimeout <- defaultArg timeout Handshake.DefaultReadTimeoutMillis
            Handshake.read __.Reader