namespace FBitTorrent.Core.Tests

open System.IO
open System.Text
open FBitTorrent.Core
open Xunit

module HandshakeTests =
    let infoHashBytes =
        [| 100uy; 169uy; 128uy; 171uy; 230uy; 228uy; 72uy; 34uy; 107uy; 185uy
           48uy; 186uy; 6uy; 21uy; 146uy; 228uy; 76uy; 55uy; 129uy; 161uy |]

    let peerIdBytes =
        [| 100uy; 169uy; 128uy; 171uy; 230uy; 228uy; 72uy; 34uy; 107uy; 185uy
           48uy; 186uy; 6uy; 21uy; 146uy; 228uy; 76uy; 55uy; 129uy; 161uy |]

    let handshakeBytes =
        [| 19uy; 66uy; 105uy; 116uy; 84uy; 111uy; 114uy; 114uy; 101uy; 110uy
           116uy; 32uy; 112uy; 114uy; 111uy; 116uy; 111uy; 99uy; 111uy; 108uy
           0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 100uy; 169uy
           128uy; 171uy; 230uy; 228uy; 72uy; 34uy; 107uy; 185uy; 48uy; 186uy
           6uy; 21uy; 146uy; 228uy; 76uy; 55uy; 129uy; 161uy; 100uy; 169uy
           128uy; 171uy; 230uy; 228uy; 72uy; 34uy; 107uy; 185uy; 48uy; 186uy
           6uy; 21uy; 146uy; 228uy; 76uy; 55uy; 129uy; 161uy |]

    [<Fact>]
    let ``Test should create handshake`` () =
        let (Handshake (proto, res, ih, pid)) = Handshake.defaultCreate infoHashBytes peerIdBytes
        Assert.Equal<byte[]>(Handshake.protocolBytes, proto)
        Assert.Equal<byte[]>(Handshake.reservedBytes, res)
        Assert.Equal<byte[]>(infoHashBytes, ih)
        Assert.Equal<byte[]>(peerIdBytes, pid)
        
    [<Fact>]
    let ``Test should write handshake`` () =
        let handshake = Handshake.defaultCreate infoHashBytes peerIdBytes
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        Handshake.write writer handshake
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>([| Handshake.protocolBytes.Length |> byte |], bytes.[0..0])
        Assert.Equal<byte[]>(Handshake.protocolBytes, bytes.[1..19])
        Assert.Equal<byte[]>(Handshake.reservedBytes, bytes.[20..27])
        Assert.Equal<byte[]>(infoHashBytes, bytes.[28..47])
        Assert.Equal<byte[]>(peerIdBytes, bytes.[48..67])
        
    [<Fact>]
    let ``Test should read handshake`` () =
        use stream = new MemoryStream(handshakeBytes)
        use reader = new ConnectionReader(stream)
        let (Handshake (proto, res, ih, pid)) = Handshake.read reader
        Assert.Equal<byte[]>(Handshake.protocolBytes, proto)
        Assert.Equal<byte[]>(Handshake.reservedBytes, res)
        Assert.Equal<byte[]>(infoHashBytes, ih)
        Assert.Equal<byte[]>(peerIdBytes, pid)
        
    [<Fact>]
    let ``Test should write/read handshake`` () =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        use reader = new ConnectionReader(stream)
        Handshake.write writer (Handshake.defaultCreate infoHashBytes peerIdBytes)
        stream.Position <- 0
        let (Handshake (proto, res, ih, pid)) = Handshake.read reader
        Assert.Equal<byte[]>(Handshake.protocolBytes, proto)
        Assert.Equal<byte[]>(Handshake.reservedBytes, res)
        Assert.Equal<byte[]>(infoHashBytes, ih)
        Assert.Equal<byte[]>(peerIdBytes, pid)
        
    [<Fact>]
    let ``Test should read/write handshake`` () =
        use rstream = new MemoryStream(handshakeBytes)
        use wstream = new MemoryStream()
        use reader = new ConnectionReader(rstream)
        use writer = new ConnectionWriter(wstream)
        Handshake.write writer (Handshake.read reader)
        Assert.Equal<byte[]>(rstream.ToArray(), wstream.ToArray())
        
    [<Fact>]
    let ``Test should convert handshake to bytes`` () =
        let converted = Handshake.toBytes (Handshake.defaultCreate infoHashBytes peerIdBytes)
        Assert.Equal<byte[]>(handshakeBytes, converted)
        
    [<Fact>]
    let ``Test should convert handshake to string`` () =
        let converted = Handshake.toString (Handshake.defaultCreate infoHashBytes peerIdBytes)
        Assert.Equal<string>((Encoding.Latin1.GetString handshakeBytes), converted)
        
    [<Fact>]
    let ``Test should convert handshake from bytes`` () =
        let (Handshake (proto, res, ih, pid)) = Handshake.fromBytes handshakeBytes
        Assert.Equal<byte[]>(Handshake.protocolBytes, proto)
        Assert.Equal<byte[]>(Handshake.reservedBytes, res)
        Assert.Equal<byte[]>(infoHashBytes, ih)
        Assert.Equal<byte[]>(peerIdBytes, pid)
        
    [<Fact>]
    let ``Test should convert handshake from string`` () =
        let (Handshake (proto, res, ih, pid)) = Handshake.fromString (Encoding.Latin1.GetString handshakeBytes)
        Assert.Equal<byte[]>(Handshake.protocolBytes, proto)
        Assert.Equal<byte[]>(Handshake.reservedBytes, res)
        Assert.Equal<byte[]>(infoHashBytes, ih)
        Assert.Equal<byte[]>(peerIdBytes, pid)
    
    [<Fact>]
    let ``Test should same handshake protocols be equal`` () =
        let (Handshake (lproto, _, _, _)) = Handshake.defaultCreate [||] [||]
        let (Handshake (rproto, _, _, _)) = Handshake.defaultCreate [||] [||]
        Assert.True(lproto.Equals rproto)
        
    [<Fact>]
    let ``Test should different handshake protocols be not equal`` () =
        let (Handshake (lproto, _, _, _)) = Handshake.defaultCreate [||] [||]
        let (Handshake (rproto, _, _, _)) = Handshake([||], [||], [||], [||])
        Assert.False(lproto.Equals rproto)
        
    [<Fact>]
    let ``Test should same handshake reserved be equal`` () =
        let (Handshake (_, lres, _, _)) = Handshake.defaultCreate [||] [||]
        let (Handshake (_, rres, _, _)) = Handshake.defaultCreate [||] [||]
        Assert.True(lres.Equals rres)
        
    [<Fact>]
    let ``Test should different handshake reserved be not equal`` () =
        let (Handshake (_, lres, _, _)) = Handshake.defaultCreate [||] [||]
        let (Handshake (_, rres, _, _)) = Handshake([||], [||], [||], [||])
        Assert.False(lres.Equals rres)
        
    [<Fact>]
    let ``Test should same handshake info hash be equal`` () =
        let (Handshake (_, _, lih, _)) = Handshake.defaultCreate infoHashBytes [||]
        let (Handshake (_, _, rih, _)) = Handshake.defaultCreate infoHashBytes [||]
        Assert.True(lih.Equals rih)
        
    [<Fact>]
    let ``Test should different handshake info hash be not equal`` () =
        let (Handshake (_, _, lih, _)) = Handshake.defaultCreate infoHashBytes [||]
        let (Handshake (_, _, rih, _)) = Handshake.defaultCreate [||] [||] 
        Assert.False(lih.Equals rih)
      
    [<Fact>]
    let ``Test should same handshake peer id be equal`` () =
        let (Handshake (_, _, _, lpid)) = Handshake.defaultCreate [||] peerIdBytes
        let (Handshake (_, _, _, rpid)) = Handshake.defaultCreate [||] peerIdBytes
        Assert.True(lpid.Equals rpid)
        
    [<Fact>]
    let ``Test should different handshake peer id be not equal`` () =
        let (Handshake (_, _, _, lpid)) = Handshake.defaultCreate [||] peerIdBytes
        let (Handshake (_, _, _, rpid)) = Handshake.defaultCreate [||] [||]
        Assert.False(lpid.Equals rpid)
        
    [<Fact>]
    let ``Test should same handshake be equal`` () =
        let lhs = Handshake.defaultCreate infoHashBytes peerIdBytes
        let rhs = Handshake.defaultCreate infoHashBytes peerIdBytes
        Assert.True(lhs.Equals rhs)
        
    [<Fact>]
    let ``Test should different handshake not be equal`` () =
        let lhs = Handshake.defaultCreate infoHashBytes peerIdBytes
        let rhs = Handshake.defaultCreate [||] [||]
        Assert.False(lhs.Equals rhs)
        
    [<Fact>]
    let ``Test should same handshake hash codes be equal`` () =
        let lhs = Handshake.defaultCreate infoHashBytes peerIdBytes
        let rhs = Handshake.defaultCreate infoHashBytes peerIdBytes
        Assert.Equal(lhs.GetHashCode(), rhs.GetHashCode())
        
    [<Fact>]
    let ``Test should different handshake hash codes be not equal`` () =
        let lhs = Handshake.defaultCreate infoHashBytes peerIdBytes
        let rhs = Handshake.defaultCreate [||] [||]
        Assert.NotEqual(lhs.GetHashCode(), rhs.GetHashCode())
    
    [<Fact>]
    let ``Test should equal expected protocol value`` () =
        let exp =
            [| 66uy; 105uy; 116uy; 84uy;  111uy; 114uy; 114uy; 101uy; 110uy; 116uy
               32uy; 112uy; 114uy; 111uy; 116uy; 111uy; 99uy;  111uy; 108uy |]
        Assert.Equal<byte[]>(exp, Handshake.protocolBytes)
    
    [<Fact>]
    let ``Test should equal expected reserved value`` () =
        let exp =
            [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; |]
        Assert.Equal<byte[]>(exp, Handshake.reservedBytes)