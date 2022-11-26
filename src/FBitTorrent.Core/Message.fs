namespace FBitTorrent.Core

open System.IO
open System.Text

type MessageType =
    | KeepAliveType
    | ChokeType
    | UnChokeType
    | InterestedType
    | NotInterestedType
    | HaveType
    | BitfieldType
    | RequestType
    | PieceType
    | CancelType
    | PortType
with
    member __.ToByte() =
        match __ with
        | ChokeType         -> 0uy
        | UnChokeType       -> 1uy
        | InterestedType    -> 2uy
        | NotInterestedType -> 3uy
        | HaveType          -> 4uy
        | BitfieldType      -> 5uy
        | RequestType       -> 6uy
        | PieceType         -> 7uy
        | CancelType        -> 8uy
        | PortType          -> 9uy       
        | messageType       ->
            failwith $"Cannot convert MessageType %A{messageType} to byte"
            
    static member FromByte(byte: byte) =
        match byte with
        | 0uy -> ChokeType
        | 1uy -> UnChokeType
        | 2uy -> InterestedType
        | 3uy -> NotInterestedType
        | 4uy -> HaveType
        | 5uy -> BitfieldType
        | 6uy -> RequestType
        | 7uy -> PieceType
        | 8uy -> CancelType
        | 9uy -> PortType
        | _   ->
            failwith $"Cannot convert byte %d{byte} to MessageType"
                
    override __.ToString() =
        match __ with
        | KeepAliveType     -> "KeepAlive"
        | ChokeType         -> "Choke"
        | UnChokeType       -> "UnChoke"
        | InterestedType    -> "Interested"
        | NotInterestedType -> "NotInterested"
        | HaveType          -> "Have"
        | BitfieldType      -> "Bitfield"
        | RequestType       -> "Request"
        | PieceType         -> "Piece"
        | CancelType        -> "Cancel"
        | PortType          -> "Port"

[<Struct>]
type Message =
    | KeepAliveMessage
    | ChokeMessage
    | UnChokeMessage
    | InterestedMessage
    | NotInterestedMessage
    | HaveMessage of
        HaveIndex: int
    | BitfieldMessage of
        Bitfield: byte[]
    | RequestMessage of
        RequestIndex:     int *
        RequestBeginning: int *
        RequestLength:    int
    | PieceMessage of
        PieceIndex:     int *
        PieceBeginning: int *
        PieceBlock:     byte[]
    | CancelMessage of
        CancelIndex:     int *
        CancelBeginning: int *
        CancelLength:    int
    | PortMessage of
        ListenPort: int16
with
    override __.ToString() =
        match __ with
        | KeepAliveMessage                   -> "KeepAlive"
        | ChokeMessage                       -> "Choke"
        | UnChokeMessage                     -> "UnChoke"
        | InterestedMessage                  -> "Interested"
        | NotInterestedMessage               -> "NotInterested"
        | HaveMessage     idx                -> $"Have(haveIndex=%d{idx})"
        | BitfieldMessage bitfield           -> $"Bitfield(bitfield=%A{bitfield})"
        | RequestMessage  (idx, beg, length) -> $"Request(index=%d{idx},begin=%d{beg},length=%d{length})"
        | PieceMessage    (idx, beg, block)  -> $"Piece(index=%d{idx},begin=%d{beg},block=%A{block})"
        | CancelMessage   (idx, beg, length) -> $"Cancel(index=%d{idx},begin=%d{beg},length=%d{length})"
        | PortMessage     port               -> $"Port(listenPort=%d{port})"

module Message =

    let DefaultWriteTimeoutMillis = 120_000
    let DefaultReadTimeoutMillis = 120_000
    
    let rec write (writer: ConnectionWriter) (message: Message) =
        match message with
        | KeepAliveMessage                   -> writeKeepAlive writer
        | ChokeMessage                       -> writeChoke writer 
        | UnChokeMessage                     -> writeUnChoke writer
        | InterestedMessage                  -> writeInterested writer
        | NotInterestedMessage               -> writeNotInterested writer
        | HaveMessage     idx                -> writeHave writer idx
        | BitfieldMessage bitfield           -> writeBitfield writer bitfield
        | RequestMessage  (idx, beg, length) -> writeRequest writer idx beg length
        | PieceMessage    (idx, beg, block)  -> writePiece writer idx beg block
        | CancelMessage   (idx, beg, length) -> writeCancel writer idx beg length
        | PortMessage     port               -> writePort writer port
    and private writeKeepAlive writer =
        writer.Write(0)
        writer.Flush()
    and private writeChoke writer =
        writer.Write(1)
        writer.Write(ChokeType.ToByte())
        writer.Flush()
    and private writeUnChoke writer =
        writer.Write(1)
        writer.Write(UnChokeType.ToByte())
        writer.Flush()
    and private writeInterested writer =
        writer.Write(1)
        writer.Write(InterestedType.ToByte())
        writer.Flush()
    and private writeNotInterested writer =
        writer.Write(1)
        writer.Write(NotInterestedType.ToByte())
        writer.Flush()
    and private writeHave writer (idx: int) =
        writer.Write(5)
        writer.Write(HaveType.ToByte())
        writer.Write(idx)
        writer.Flush()
    and private writeBitfield writer (bitfield: byte[]) =
        writer.Write(1 + bitfield.Length)
        writer.Write(BitfieldType.ToByte())
        writer.Write(bitfield)
        writer.Flush()
    and private writeRequest writer (idx: int) (beg: int) (length: int) =
        writer.Write(1 + 12)
        writer.Write(RequestType.ToByte())
        writer.Write(idx)
        writer.Write(beg)
        writer.Write(length)
        writer.Flush()
    and private writePiece writer (idx: int) (beg: int) (block: byte[]) =
        writer.Write(9 + block.Length)
        writer.Write(PieceType.ToByte())
        writer.Write(idx)
        writer.Write(beg)
        writer.Write(block)
        writer.Flush()
    and private writeCancel writer (idx: int) (beg: int) (length: int) =
        writer.Write(1 + 12)
        writer.Write(CancelType.ToByte())
        writer.Write(idx)
        writer.Write(beg)
        writer.Write(length)
        writer.Flush()
    and private writePort writer (port: int16) =
        writer.Write(3)
        writer.Write(PortType.ToByte())
        writer.Write(port)
        writer.Flush()
        
    let rec read (reader: ConnectionReader) =
        match reader.ReadInt32() with
        | length when length < 0 -> readFailure ()
        | length when length = 0 -> readKeepAlive ()
        | length ->
            match MessageType.FromByte(reader.ReadByte()) with
            | ChokeType         -> readChoke ()
            | UnChokeType       -> readUnChoke ()
            | InterestedType    -> readInterested ()
            | NotInterestedType -> readNotInterested ()
            | HaveType          -> readHave reader 
            | BitfieldType      -> readBitfield reader length
            | RequestType       -> readRequest reader 
            | PieceType         -> readPiece reader length
            | CancelType        -> readCancel reader 
            | PortType          -> readPort reader
            | messageType       ->
                failwith $"Cannot convert MessageType %A{messageType} to byte"
    
    and private readFailure () =
        failwith "Received negative value for message length"
    and private readKeepAlive () =
        KeepAliveMessage
    and private readChoke () =
        ChokeMessage
    and private readUnChoke () =
        UnChokeMessage
    and private readInterested () =
        InterestedMessage
    and private readNotInterested () =
        NotInterestedMessage
    and private readHave reader =
        let idx = reader.ReadInt32()
        HaveMessage idx
    and private readBitfield reader length =
        let bitfield = reader.ReadBytes(length - 1)
        BitfieldMessage bitfield
    and private readRequest reader =
        let idx = reader.ReadInt32()
        let beg = reader.ReadInt32()
        let length = reader.ReadInt32()
        RequestMessage (idx, beg, length)
    and private readPiece reader length =
        let idx = reader.ReadInt32()
        let beg = reader.ReadInt32()
        // TODO: Optimize how we read bytes. Right now GC is having to work hard to clear all this up.
        let block = reader.ReadBytes(length - 1 - 8) 
        PieceMessage (idx, beg, block)
    and private readCancel reader =
        let idx = reader.ReadInt32()
        let beg = reader.ReadInt32()
        let length = reader.ReadInt32()
        CancelMessage (idx, beg, length)
    and private readPort reader =
        let port = reader.ReadInt16()
        PortMessage port

    let toBytes message =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        write writer message
        stream.ToArray()
        
    let toString message =
        Encoding.Latin1.GetString(toBytes message)
    
    let fromBytes (bytes: byte[]) =
        use stream = new MemoryStream(bytes)
        use reader = new ConnectionReader(stream)
        read reader
        
    let fromString (string: string) =
        fromBytes (Encoding.Latin1.GetBytes(string))
        
module MessageExtensions =
    type IConnection with
        member __.WriteMessage(message: Message, ?timeout: int) =
            __.Stream.WriteTimeout <- defaultArg timeout Message.DefaultWriteTimeoutMillis
            Message.write __.Writer message
        member __.ReadMessage(?timeout: int) =
            __.Stream.ReadTimeout <- defaultArg timeout Message.DefaultReadTimeoutMillis 
            Message.read __.Reader