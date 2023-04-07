namespace FBitTorrent.Core

open System
open System.IO
open System.Net
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
        PieceBlock:     ByteBuffer
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

type IMessageConnection =
    inherit IConnection
    abstract member WriteMessage: Message * int -> unit
    abstract member ReadMessage: int -> Message
    abstract member AsyncWriteMessage: Message * int -> Async<unit>
    abstract member AsyncReadMessage: int -> Async<Message>
    abstract member WriteMessage: Message -> unit
    abstract member ReadMessage: unit -> Message
    abstract member AsyncWriteMessage: Message -> Async<unit>
    abstract member AsyncReadMessage: unit -> Async<Message>

module Message =

    let [<Literal>] DefaultWriteTimeoutMillis = 120_000
    
    let [<Literal>] DefaultReadTimeoutMillis = 120_000
    
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
    and private writePiece writer (idx: int) (beg: int) (block: ByteBuffer) =
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
    
    let rec asyncWrite (writer: ConnectionWriter) (message: Message) = async {
        match message with
        | KeepAliveMessage                   -> return! asyncWriteKeepAlive writer
        | ChokeMessage                       -> return! asyncWriteChoke writer 
        | UnChokeMessage                     -> return! asyncWriteUnChoke writer
        | InterestedMessage                  -> return! asyncWriteInterested writer
        | NotInterestedMessage               -> return! asyncWriteNotInterested writer
        | HaveMessage     idx                -> return! asyncWriteHave writer idx
        | BitfieldMessage bitfield           -> return! asyncWriteBitfield writer bitfield
        | RequestMessage  (idx, beg, length) -> return! asyncWriteRequest writer idx beg length
        | PieceMessage    (idx, beg, block)  -> return! asyncWritePiece writer idx beg block
        | CancelMessage   (idx, beg, length) -> return! asyncWriteCancel writer idx beg length
        | PortMessage     port               -> return! asyncWritePort writer port }
    and private asyncWriteKeepAlive writer = async {
        do! writer.AsyncWrite(0)
        do! writer.AsyncFlush() }
    and private asyncWriteChoke writer = async {
        do! writer.AsyncWrite(1)
        do! writer.AsyncWrite(ChokeType.ToByte())
        do! writer.AsyncFlush() }
    and private asyncWriteUnChoke writer = async {
        do! writer.AsyncWrite(1)
        do! writer.AsyncWrite(UnChokeType.ToByte())
        do! writer.AsyncFlush() }
    and private asyncWriteInterested writer = async {
        do! writer.AsyncWrite(1)
        do! writer.AsyncWrite(InterestedType.ToByte())
        do! writer.AsyncFlush() }
    and private asyncWriteNotInterested writer = async {
        do! writer.AsyncWrite(1)
        do! writer.AsyncWrite(NotInterestedType.ToByte())
        do! writer.AsyncFlush() }
    and private asyncWriteHave writer (idx: int) = async {
        do! writer.AsyncWrite(5)
        do! writer.AsyncWrite(HaveType.ToByte())
        do! writer.AsyncWrite(idx)
        do! writer.AsyncFlush() }
    and private asyncWriteBitfield writer (bitfield: byte[]) = async {
        do! writer.AsyncWrite(1 + bitfield.Length)
        do! writer.AsyncWrite(BitfieldType.ToByte())
        do! writer.AsyncWrite(bitfield)
        do! writer.AsyncFlush() }
    and private asyncWriteRequest writer (idx: int) (beg: int) (length: int) = async {
        do! writer.AsyncWrite(1 + 12)
        do! writer.AsyncWrite(RequestType.ToByte())
        do! writer.AsyncWrite(idx)
        do! writer.AsyncWrite(beg)
        do! writer.AsyncWrite(length)
        do! writer.AsyncFlush() }
    and private asyncWritePiece writer (idx: int) (beg: int) (block: ByteBuffer) = async {
        do! writer.AsyncWrite(9 + block.Length)
        do! writer.AsyncWrite(PieceType.ToByte())
        do! writer.AsyncWrite(idx)
        do! writer.AsyncWrite(beg)
        do! writer.AsyncWrite(block)
        do! writer.AsyncFlush() }
    and private asyncWriteCancel writer (idx: int) (beg: int) (length: int) = async {
        do! writer.AsyncWrite(1 + 12)
        do! writer.AsyncWrite(CancelType.ToByte())
        do! writer.AsyncWrite(idx)
        do! writer.AsyncWrite(beg)
        do! writer.AsyncWrite(length)
        do! writer.AsyncFlush() }
    and private asyncWritePort writer (port: int16) = async {
        do! writer.AsyncWrite(3)
        do! writer.AsyncWrite(PortType.ToByte())
        do! writer.AsyncWrite(port)
        do! writer.AsyncFlush() }
    
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
        let block = reader.ReadByteBuffer(length - 1 - 8);
        PieceMessage (idx, beg, block)
    and private readCancel reader =
        let idx = reader.ReadInt32()
        let beg = reader.ReadInt32()
        let length = reader.ReadInt32()
        CancelMessage (idx, beg, length)
    and private readPort reader =
        let port = reader.ReadInt16()
        PortMessage port
        
    let rec asyncRead (reader: ConnectionReader) = async {
        match! reader.AsyncReadInt32() with
        | length when length < 0 -> return! asyncReadFailure ()
        | length when length = 0 -> return! asyncReadKeepAlive ()
        | length ->
            match MessageType.FromByte(reader.ReadByte()) with
            | ChokeType         -> return! asyncReadChoke ()
            | UnChokeType       -> return! asyncReadUnChoke ()
            | InterestedType    -> return! asyncReadInterested ()
            | NotInterestedType -> return! asyncReadNotInterested ()
            | HaveType          -> return! asyncReadHave reader 
            | BitfieldType      -> return! asyncReadBitfield reader length
            | RequestType       -> return! asyncReadRequest reader 
            | PieceType         -> return! asyncReadPiece reader length
            | CancelType        -> return! asyncReadCancel reader 
            | PortType          -> return! asyncReadPort reader
            | messageType       ->
                return failwith $"Cannot convert MessageType %A{messageType} to byte" }
    and private asyncReadFailure () = async {
        return failwith "Received negative value for message length" }
    and private asyncReadKeepAlive () = async {
        return KeepAliveMessage }
    and private asyncReadChoke () = async {
        return ChokeMessage }
    and private asyncReadUnChoke () = async {
        return UnChokeMessage }
    and private asyncReadInterested () = async {
        return InterestedMessage }
    and private asyncReadNotInterested () = async {
        return NotInterestedMessage }
    and private asyncReadHave reader = async {
        let! idx = reader.AsyncReadInt32()
        return HaveMessage idx }
    and private asyncReadBitfield reader length = async {
        let! bitfield = reader.AsyncReadBytes(length - 1)
        return BitfieldMessage bitfield }
    and private asyncReadRequest reader = async {
        let! idx = reader.AsyncReadInt32()
        let! beg = reader.AsyncReadInt32()
        let! length = reader.AsyncReadInt32()
        return RequestMessage (idx, beg, length) }
    and private asyncReadPiece reader length = async {
        let! idx = reader.AsyncReadInt32()
        let! beg = reader.AsyncReadInt32()
        let! block = reader.AsyncReadByteBuffer(length - 1 - 8);
        return PieceMessage (idx, beg, block) }
    and private asyncReadCancel reader = async {
        let! idx = reader.AsyncReadInt32()
        let! beg = reader.AsyncReadInt32()
        let! length = reader.AsyncReadInt32()
        return CancelMessage (idx, beg, length) }
    and private asyncReadPort reader = async {
        let! port = reader.AsyncReadInt16()
        return PortMessage port }

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

    let createConnection (connection: IConnection) =
        { new IMessageConnection with
            member _.Stream with get() = connection.Stream
            member _.Writer with get() = connection.Writer
            member _.Reader with get() = connection.Reader
            member _.RemoteEndpoint with get() = connection.RemoteEndpoint
            member _.LocalEndpoint with get() = connection.LocalEndpoint
            member _.Disconnect() = connection.Disconnect()
            member _.Dispose() = connection.Dispose()
            member _.WriteMessage(message: Message, timeoutMillis: int) = connection.Stream.WriteTimeout <- timeoutMillis; write connection.Writer message
            member _.ReadMessage(timeoutMillis: int) = connection.Stream.ReadTimeout <- timeoutMillis; read connection.Reader
            member _.AsyncWriteMessage(message: Message, timeoutMillis: int) = connection.Stream.WriteTimeout <- timeoutMillis; asyncWrite connection.Writer message
            member _.AsyncReadMessage(timeoutMillis: int) = connection.Stream.ReadTimeout <- timeoutMillis; asyncRead connection.Reader
            member __.WriteMessage(message: Message) = __.WriteMessage(message, DefaultWriteTimeoutMillis)
            member __.ReadMessage() = __.ReadMessage(DefaultReadTimeoutMillis)
            member __.AsyncWriteMessage(message: Message) = __.AsyncWriteMessage(message, DefaultWriteTimeoutMillis)
            member __.AsyncReadMessage() = __.AsyncReadMessage(DefaultReadTimeoutMillis) }