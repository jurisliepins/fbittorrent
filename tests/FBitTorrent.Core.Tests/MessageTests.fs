namespace FBitTorrent.Core.Tests

open System
open System.IO
open FBitTorrent.Core
open Xunit

module MessageTests =
    
    [<Fact>]
    let ``Test should convert message type to byte success`` () =
        Assert.Equal(ChokeType.ToByte(), 0uy)
        Assert.Equal(UnChokeType.ToByte(), 1uy)
        Assert.Equal(InterestedType.ToByte(), 2uy)
        Assert.Equal(NotInterestedType.ToByte(), 3uy)
        Assert.Equal(HaveType.ToByte(), 4uy)
        Assert.Equal(BitfieldType.ToByte(), 5uy)
        Assert.Equal(RequestType.ToByte(), 6uy)
        Assert.Equal(PieceType.ToByte(), 7uy)
        Assert.Equal(CancelType.ToByte(), 8uy)
        Assert.Equal(PortType.ToByte(), 9uy)
        
    [<Fact>]
    let ``Test should convert message type from byte success`` () =
        Assert.Equal(MessageType.FromByte(0uy), ChokeType)
        Assert.Equal(MessageType.FromByte(1uy), UnChokeType)
        Assert.Equal(MessageType.FromByte(2uy), InterestedType)
        Assert.Equal(MessageType.FromByte(3uy), NotInterestedType)
        Assert.Equal(MessageType.FromByte(4uy), HaveType)
        Assert.Equal(MessageType.FromByte(5uy), BitfieldType)
        Assert.Equal(MessageType.FromByte(6uy), RequestType)
        Assert.Equal(MessageType.FromByte(7uy), PieceType)
        Assert.Equal(MessageType.FromByte(8uy), CancelType)
        Assert.Equal(MessageType.FromByte(9uy), PortType)
        
    [<Fact>]
    let ``Test should convert message type to byte failure`` () =
        Assert.ThrowsAny<Exception>(fun () -> KeepAliveType.ToByte() |> ignore)
        
    [<Fact>]
    let ``Test should convert message type from byte failure`` () =
        Assert.ThrowsAny<Exception>(fun () -> MessageType.FromByte 10uy |> ignore)
        
    [<Fact>]
    let ``Test should write keep-alive message`` () =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        Message.write writer Message.KeepAliveMessage
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 0uy; |])
        
    [<Fact>]
    let ``Test should async write keep-alive message`` () = async {
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        do! Message.asyncWrite writer Message.KeepAliveMessage
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 0uy; |]) }
    
    [<Fact>]
    let ``Test should read keep-alive message`` () =
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 0uy; |])
        use reader = new ConnectionReader(stream)
        match Message.read reader with
        | KeepAliveMessage -> ()
        | _ -> 
            Assert.True(false, "Should have read keep-alive message")
            
    [<Fact>]
    let ``Test should async read keep-alive message`` () = async {
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 0uy; |])
        use reader = new ConnectionReader(stream)
        match! Message.asyncRead reader with
        | KeepAliveMessage -> ()
        | _ -> 
            Assert.True(false, "Should have read keep-alive message") }
        
    [<Fact>]
    let ``Test should write choke message`` () =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        Message.write writer Message.ChokeMessage
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 1uy; 0uy; |])
        
    [<Fact>]
    let ``Test should async write choke message`` () = async {
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        do! Message.asyncWrite writer Message.ChokeMessage
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 1uy; 0uy; |]) }
        
    [<Fact>]
    let ``Test should read choke message`` () =
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 1uy; 0uy; |])
        use reader = new ConnectionReader(stream)
        match Message.read reader with
        | ChokeMessage -> ()
        | _ -> 
            Assert.True(false, "Should have read choke message")
            
    [<Fact>]
    let ``Test should async read choke message`` () = async {
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 1uy; 0uy; |])
        use reader = new ConnectionReader(stream)
        match! Message.asyncRead reader with
        | ChokeMessage -> ()
        | _ -> 
            Assert.True(false, "Should have read choke message") }
        
    [<Fact>]
    let ``Test should write un-choke message`` () =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        Message.write writer Message.UnChokeMessage
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 1uy; 1uy; |])
        
    [<Fact>]
    let ``Test should async write un-choke message`` () = async {
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        do! Message.asyncWrite writer Message.UnChokeMessage
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 1uy; 1uy; |]) }
        
    [<Fact>]
    let ``Test should read un-choke message`` () =
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 1uy; 1uy; |])
        use reader = new ConnectionReader(stream)
        match Message.read reader with
        | UnChokeMessage -> ()
        | _ -> 
            Assert.True(false, "Should have read un-choke message")
            
    [<Fact>]
    let ``Test should async read un-choke message`` () = async {
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 1uy; 1uy; |])
        use reader = new ConnectionReader(stream)
        match! Message.asyncRead reader with
        | UnChokeMessage -> ()
        | _ -> 
            Assert.True(false, "Should have read un-choke message") }
        
    [<Fact>]
    let ``Test should write interested message`` () =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        Message.write writer Message.InterestedMessage
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 1uy; 2uy; |])
        
    [<Fact>]
    let ``Test should async write interested message`` () = async {
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        do! Message.asyncWrite writer Message.InterestedMessage
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 1uy; 2uy; |]) }
        
    [<Fact>]
    let ``Test should read interested message`` () =
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 1uy; 2uy; |])
        use reader = new ConnectionReader(stream)
        match Message.read reader with
        | InterestedMessage -> ()
        | _ -> 
            Assert.True(false, "Should have read interested message")
            
    [<Fact>]
    let ``Test should async read interested message`` () = async {
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 1uy; 2uy; |])
        use reader = new ConnectionReader(stream)
        match! Message.asyncRead reader with
        | InterestedMessage -> ()
        | _ -> 
            Assert.True(false, "Should have read interested message") }
        
    [<Fact>]
    let ``Test should write not interested message`` () =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        Message.write writer Message.NotInterestedMessage
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 1uy; 3uy; |])
        
    [<Fact>]
    let ``Test should async write not interested message`` () = async {
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        do! Message.asyncWrite writer Message.NotInterestedMessage
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 1uy; 3uy; |]) }
        
    [<Fact>]
    let ``Test should read not interested message`` () =
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 1uy; 3uy; |])
        use reader = new ConnectionReader(stream)
        match Message.read reader with
        | NotInterestedMessage -> ()
        | _ -> 
            Assert.True(false, "Should have read not interested message")
            
    [<Fact>]
    let ``Test should async read not interested message`` () = async {
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 1uy; 3uy; |])
        use reader = new ConnectionReader(stream)
        match! Message.asyncRead reader with
        | NotInterestedMessage -> ()
        | _ -> 
            Assert.True(false, "Should have read not interested message") }
        
    [<Fact>]
    let ``Test should write have message`` () =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        Message.write writer (Message.HaveMessage(100))
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 5uy; 4uy; 0uy; 0uy; 0uy; 100uy |])
        
    [<Fact>]
    let ``Test should async write have message`` () = async {
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        do! Message.asyncWrite writer (Message.HaveMessage(100))
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 5uy; 4uy; 0uy; 0uy; 0uy; 100uy |]) }
    
    [<Fact>]
    let ``Test should read have message`` () =
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 5uy; 4uy; 0uy; 0uy; 0uy; 100uy |])
        use reader = new ConnectionReader(stream)
        match Message.read reader with
        | HaveMessage(100) -> ()
        | _ -> 
            Assert.True(false, "Should have read have message")
            
    [<Fact>]
    let ``Test should async read have message`` () = async {
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 5uy; 4uy; 0uy; 0uy; 0uy; 100uy |])
        use reader = new ConnectionReader(stream)
        match! Message.asyncRead reader with
        | HaveMessage(100) -> ()
        | _ -> 
            Assert.True(false, "Should have read have message") }
        
    [<Fact>]
    let ``Test should write bitfield message`` () =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        Message.write writer (Message.BitfieldMessage([| 0uy; 1uy; 2uy; 3uy |]))
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 5uy; 5uy; 0uy; 1uy; 2uy; 3uy; |])
        
    [<Fact>]
    let ``Test should async write bitfield message`` () = async {
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        do! Message.asyncWrite writer (Message.BitfieldMessage([| 0uy; 1uy; 2uy; 3uy |]))
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 5uy; 5uy; 0uy; 1uy; 2uy; 3uy; |]) }
    
    [<Fact>]
    let ``Test should read bitfield message`` () =
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 5uy; 5uy; 0uy; 1uy; 2uy; 3uy; |])
        use reader = new ConnectionReader(stream)
        match Message.read reader with
        | Message.BitfieldMessage([| 0uy; 1uy; 2uy; 3uy |]) -> ()
        | _ -> 
            Assert.True(false, "Should have read bitfield message")
            
    [<Fact>]
    let ``Test should async read bitfield message`` () = async {
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 5uy; 5uy; 0uy; 1uy; 2uy; 3uy; |])
        use reader = new ConnectionReader(stream)
        match! Message.asyncRead reader with
        | Message.BitfieldMessage([| 0uy; 1uy; 2uy; 3uy |]) -> ()
        | _ -> 
            Assert.True(false, "Should have read bitfield message") }
        
    [<Fact>]
    let ``Test should write request message`` () =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        Message.write writer (Message.RequestMessage(1, 2, 3))
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 13uy; 6uy; 0uy; 0uy; 0uy
                                       1uy; 0uy; 0uy; 0uy; 2uy; 0uy; 0uy; 0uy; 3uy |])
        
    [<Fact>]
    let ``Test should async write request message`` () = async {
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        do! Message.asyncWrite writer (Message.RequestMessage(1, 2, 3))
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 13uy; 6uy; 0uy; 0uy; 0uy
                                       1uy; 0uy; 0uy; 0uy; 2uy; 0uy; 0uy; 0uy; 3uy |]) }
    
    [<Fact>]
    let ``Test should read request message`` () =
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 13uy; 6uy; 0uy; 0uy; 0uy
                                         1uy; 0uy; 0uy; 0uy; 2uy; 0uy; 0uy; 0uy; 3uy |])
        use reader = new ConnectionReader(stream)
        match Message.read reader with
        | Message.RequestMessage(1, 2, 3) -> ()
        | _ -> 
            Assert.True(false, "Should have read request message")
            
    [<Fact>]
    let ``Test should async read request message`` () = async {
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 13uy; 6uy; 0uy; 0uy; 0uy
                                         1uy; 0uy; 0uy; 0uy; 2uy; 0uy; 0uy; 0uy; 3uy |])
        use reader = new ConnectionReader(stream)
        match! Message.asyncRead reader with
        | Message.RequestMessage(1, 2, 3) -> ()
        | _ -> 
            Assert.True(false, "Should have read request message") }
        
    [<Fact>]
    let ``Test should write piece message`` () =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        Message.write writer (Message.PieceMessage(1, 2, ByteBuffer([| 100uy; 100uy |])))
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 11uy; 7uy; 0uy; 0uy; 0uy; 1uy; 0uy; 0uy; 0uy; 2uy; 100uy; 100uy; |])
        
    [<Fact>]
    let ``Test should async write piece message`` () = async {
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        do! Message.asyncWrite writer (Message.PieceMessage(1, 2, ByteBuffer([| 100uy; 100uy |])))
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 11uy; 7uy; 0uy; 0uy; 0uy; 1uy; 0uy; 0uy; 0uy; 2uy; 100uy; 100uy; |]) }
            
    [<Fact>]
    let ``Test should read piece message`` () =
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 11uy; 7uy; 0uy; 0uy; 0uy; 1uy; 0uy; 0uy; 0uy; 2uy; 100uy; 100uy; |])
        use reader = new ConnectionReader(stream)
        match Message.read reader with
        | Message.PieceMessage(1, 2, piece) when ByteBuffer([| 100uy; 100uy |]).Equals(piece) -> ()
        | _ -> 
            Assert.True(false, "Should have read piece message")
            
    [<Fact>]
    let ``Test should async read piece message`` () = async {
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 11uy; 7uy; 0uy; 0uy; 0uy; 1uy; 0uy; 0uy; 0uy; 2uy; 100uy; 100uy; |])
        use reader = new ConnectionReader(stream)
        match! Message.asyncRead reader with
        | Message.PieceMessage(1, 2, piece) when ByteBuffer([| 100uy; 100uy |]).Equals(piece) -> ()
        | _ -> 
            Assert.True(false, "Should have read piece message") }
        
    [<Fact>]
    let ``Test should write cancel message`` () =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        Message.write writer (Message.CancelMessage(1, 2, 3))
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 13uy; 8uy; 0uy; 0uy; 0uy
                                       1uy; 0uy; 0uy; 0uy; 2uy; 0uy; 0uy; 0uy; 3uy |])
        
    [<Fact>]
    let ``Test should async write cancel message`` () = async {
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        do! Message.asyncWrite writer (Message.CancelMessage(1, 2, 3))
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 13uy; 8uy; 0uy; 0uy; 0uy
                                       1uy; 0uy; 0uy; 0uy; 2uy; 0uy; 0uy; 0uy; 3uy |]) }

    [<Fact>]
    let ``Test should read cancel message`` () =
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 13uy; 8uy; 0uy; 0uy; 0uy
                                         1uy; 0uy; 0uy; 0uy; 2uy; 0uy; 0uy; 0uy; 3uy |])
        use reader = new ConnectionReader(stream)
        match Message.read reader with
        | Message.CancelMessage(1, 2, 3) -> ()
        | _ -> 
            Assert.True(false, "Should have read cancel message")
            
    [<Fact>]
    let ``Test should async read cancel message`` () = async {
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 13uy; 8uy; 0uy; 0uy; 0uy
                                         1uy; 0uy; 0uy; 0uy; 2uy; 0uy; 0uy; 0uy; 3uy |])
        use reader = new ConnectionReader(stream)
        match! Message.asyncRead reader with
        | Message.CancelMessage(1, 2, 3) -> ()
        | _ -> 
            Assert.True(false, "Should have read cancel message") }
        
    [<Fact>]
    let ``Test should write port message`` () =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        Message.write writer (Message.PortMessage(3500s))
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 3uy; 9uy; 13uy; 172uy; |])
        
    [<Fact>]
    let ``Test should async write port message`` () = async {
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        do! Message.asyncWrite writer (Message.PortMessage(3500s))
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>(bytes, [| 0uy; 0uy; 0uy; 3uy; 9uy; 13uy; 172uy; |]) }

    [<Fact>]
    let ``Test should read port message`` () =
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 3uy; 9uy; 13uy; 172uy; |])
        use reader = new ConnectionReader(stream)
        match Message.read reader with
        | Message.PortMessage(3500s) -> ()
        | _ -> 
            Assert.True(false, "Should have read port message")
            
    [<Fact>]
    let ``Test should async read port message`` () = async {
        use stream = new MemoryStream([| 0uy; 0uy; 0uy; 3uy; 9uy; 13uy; 172uy; |])
        use reader = new ConnectionReader(stream)
        match! Message.asyncRead reader with
        | Message.PortMessage(3500s) -> ()
        | _ -> 
            Assert.True(false, "Should have read port message") }