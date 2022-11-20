namespace FBitTorrent.Core.Tests

open System.IO
open FBitTorrent.Core
open Xunit

module HandshakeTests =
    let private InfoHashBytes =
        [| 100uy; 169uy; 128uy; 171uy; 230uy; 228uy; 72uy; 34uy; 107uy; 185uy
           48uy; 186uy; 6uy; 21uy; 146uy; 228uy; 76uy; 55uy; 129uy; 161uy |]

    let private PeerIdBytes =
        [| 100uy; 169uy; 128uy; 171uy; 230uy; 228uy; 72uy; 34uy; 107uy; 185uy
           48uy; 186uy; 6uy; 21uy; 146uy; 228uy; 76uy; 55uy; 129uy; 161uy |]

    let private HandshakeBytes =
        [| 19uy; 66uy; 105uy; 116uy; 84uy; 111uy; 114uy; 114uy; 101uy; 110uy
           116uy; 32uy; 112uy; 114uy; 111uy; 116uy; 111uy; 99uy; 111uy; 108uy
           0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 100uy; 169uy
           128uy; 171uy; 230uy; 228uy; 72uy; 34uy; 107uy; 185uy; 48uy; 186uy
           6uy; 21uy; 146uy; 228uy; 76uy; 55uy; 129uy; 161uy; 100uy; 169uy
           128uy; 171uy; 230uy; 228uy; 72uy; 34uy; 107uy; 185uy; 48uy; 186uy
           6uy; 21uy; 146uy; 228uy; 76uy; 55uy; 129uy; 161uy |]

    [<Fact>]
    let ``Test should create handshake`` () =
        let (Handshake (proto, res, ih, pid)) = Handshake.defaultCreate InfoHashBytes PeerIdBytes
        Assert.Equal<byte[]>(Handshake.ProtocolBytes, proto)
        Assert.Equal<byte[]>(Handshake.ReservedBytes, res)
        Assert.Equal<byte[]>(InfoHashBytes, ih)
        Assert.Equal<byte[]>(PeerIdBytes, pid)
        
    [<Fact>]
    let ``Test should write handshake`` () =
        let handshake = Handshake.defaultCreate InfoHashBytes PeerIdBytes
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        Handshake.write writer handshake
        let bytes = stream.ToArray()
        Assert.Equal<byte[]>([| Handshake.ProtocolBytes.Length |> byte |], bytes.[0..0])
        Assert.Equal<byte[]>(Handshake.ProtocolBytes, bytes.[1..19])
        Assert.Equal<byte[]>(Handshake.ReservedBytes, bytes.[20..27])
        Assert.Equal<byte[]>(InfoHashBytes, bytes.[28..47])
        Assert.Equal<byte[]>(PeerIdBytes, bytes.[48..67])
        
    [<Fact>]
    let ``Test should read handshake`` () =
        use stream = new MemoryStream(HandshakeBytes)
        use reader = new ConnectionReader(stream)
        let (Handshake (proto, res, ih, pid)) = Handshake.read reader
        Assert.Equal<byte[]>(Handshake.ProtocolBytes, proto)
        Assert.Equal<byte[]>(Handshake.ReservedBytes, res)
        Assert.Equal<byte[]>(InfoHashBytes, ih)
        Assert.Equal<byte[]>(PeerIdBytes, pid)
        
    [<Fact>]
    let ``Test should write/read handshake`` () =
        use stream = new MemoryStream()
        use writer = new ConnectionWriter(stream)
        use reader = new ConnectionReader(stream)
        Handshake.write writer (Handshake.defaultCreate InfoHashBytes PeerIdBytes)
        stream.Position <- 0
        let (Handshake (proto, res, ih, pid)) = Handshake.read reader
        Assert.Equal<byte[]>(Handshake.ProtocolBytes, proto)
        Assert.Equal<byte[]>(Handshake.ReservedBytes, res)
        Assert.Equal<byte[]>(InfoHashBytes, ih)
        Assert.Equal<byte[]>(PeerIdBytes, pid)
        
    [<Fact>]
    let ``Test should read/write handshake`` () =
        use rstream = new MemoryStream(HandshakeBytes)
        use wstream = new MemoryStream()
        use reader = new ConnectionReader(rstream)
        use writer = new ConnectionWriter(wstream)
        Handshake.write writer (Handshake.read reader)
        Assert.Equal<byte[]>(rstream.ToArray(), wstream.ToArray())
        
    [<Fact>]
    let ``Test should convert handshake to bytes`` () =
        let converted = Handshake.toBytes (Handshake.defaultCreate InfoHashBytes PeerIdBytes)
        Assert.Equal<byte[]>(HandshakeBytes, converted)
        
    [<Fact>]
    let ``Test should convert handshake to string`` () =
        let converted = Handshake.toString (Handshake.defaultCreate InfoHashBytes PeerIdBytes)
        Assert.Equal<string>((Handshake.DefaultEncoding.GetString HandshakeBytes), converted)
        
    [<Fact>]
    let ``Test should convert handshake from bytes`` () =
        let (Handshake (proto, res, ih, pid)) = Handshake.fromBytes HandshakeBytes
        Assert.Equal<byte[]>(Handshake.ProtocolBytes, proto)
        Assert.Equal<byte[]>(Handshake.ReservedBytes, res)
        Assert.Equal<byte[]>(InfoHashBytes, ih)
        Assert.Equal<byte[]>(PeerIdBytes, pid)
        
    [<Fact>]
    let ``Test should convert handshake from string`` () =
        let (Handshake (proto, res, ih, pid)) = Handshake.fromString (Handshake.DefaultEncoding.GetString HandshakeBytes)
        Assert.Equal<byte[]>(Handshake.ProtocolBytes, proto)
        Assert.Equal<byte[]>(Handshake.ReservedBytes, res)
        Assert.Equal<byte[]>(InfoHashBytes, ih)
        Assert.Equal<byte[]>(PeerIdBytes, pid)
    
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
        let (Handshake (_, _, lih, _)) = Handshake.defaultCreate InfoHashBytes [||]
        let (Handshake (_, _, rih, _)) = Handshake.defaultCreate InfoHashBytes [||]
        Assert.True(lih.Equals rih)
        
    [<Fact>]
    let ``Test should different handshake info hash be not equal`` () =
        let (Handshake (_, _, lih, _)) = Handshake.defaultCreate InfoHashBytes [||]
        let (Handshake (_, _, rih, _)) = Handshake.defaultCreate [||] [||] 
        Assert.False(lih.Equals rih)
      
    [<Fact>]
    let ``Test should same handshake peer id be equal`` () =
        let (Handshake (_, _, _, lpid)) = Handshake.defaultCreate [||] PeerIdBytes
        let (Handshake (_, _, _, rpid)) = Handshake.defaultCreate [||] PeerIdBytes
        Assert.True(lpid.Equals rpid)
        
    [<Fact>]
    let ``Test should different handshake peer id be not equal`` () =
        let (Handshake (_, _, _, lpid)) = Handshake.defaultCreate [||] PeerIdBytes
        let (Handshake (_, _, _, rpid)) = Handshake.defaultCreate [||] [||]
        Assert.False(lpid.Equals rpid)
        
    [<Fact>]
    let ``Test should same handshake be equal`` () =
        let lhs = Handshake.defaultCreate InfoHashBytes PeerIdBytes
        let rhs = Handshake.defaultCreate InfoHashBytes PeerIdBytes
        Assert.True(lhs.Equals rhs)
        
    [<Fact>]
    let ``Test should different handshake not be equal`` () =
        let lhs = Handshake.defaultCreate InfoHashBytes PeerIdBytes
        let rhs = Handshake.defaultCreate [||] [||]
        Assert.False(lhs.Equals rhs)
        
    [<Fact>]
    let ``Test should same handshake hash codes be equal`` () =
        let lhs = Handshake.defaultCreate InfoHashBytes PeerIdBytes
        let rhs = Handshake.defaultCreate InfoHashBytes PeerIdBytes
        Assert.Equal(lhs.GetHashCode(), rhs.GetHashCode())
        
    [<Fact>]
    let ``Test should different handshake hash codes be not equal`` () =
        let lhs = Handshake.defaultCreate InfoHashBytes PeerIdBytes
        let rhs = Handshake.defaultCreate [||] [||]
        Assert.NotEqual(lhs.GetHashCode(), rhs.GetHashCode())
    
    [<Fact>]
    let ``Test should equal expected protocol value`` () =
        let exp =
            [| 66uy; 105uy; 116uy; 84uy;  111uy; 114uy; 114uy; 101uy; 110uy; 116uy
               32uy; 112uy; 114uy; 111uy; 116uy; 111uy; 99uy;  111uy; 108uy |]
        Assert.Equal<byte[]>(exp, Handshake.ProtocolBytes)
    
    [<Fact>]
    let ``Test should equal expected reserved value`` () =
        let exp =
            [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; |]
        Assert.Equal<byte[]>(exp, Handshake.ReservedBytes)