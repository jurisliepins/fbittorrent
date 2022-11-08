namespace FBitTorrent.Core.Tests

open FBitTorrent.Core
open Xunit

module BitfieldTests =
    [<Fact>]
    let ``Test bitfield should get get on`` () =
        let capacity = 4 * 8
        let bitfield = Bitfield.create capacity
        bitfield |> Bitfield.setBytes [| 1uy; 2uy; 3uy; 4uy |] 
        Assert.True(bitfield |> Bitfield.getBit 7)
        Assert.True(bitfield |> Bitfield.getBit 14)
        Assert.True(bitfield |> Bitfield.getBit 22)
        Assert.True(bitfield |> Bitfield.getBit 23)
        Assert.True(bitfield |> Bitfield.getBit 29)
        Assert.False(bitfield |> Bitfield.getBit 0)
        Assert.False(bitfield |> Bitfield.getBit 1)
        Assert.False(bitfield |> Bitfield.getBit 2)
        Assert.False(bitfield |> Bitfield.getBit 3)
        Assert.False(bitfield |> Bitfield.getBit 4)
        Assert.False(bitfield |> Bitfield.getBit 5)
        Assert.False(bitfield |> Bitfield.getBit 6)
        
    [<Fact>]
    let ``Test bitfield should get bit off`` () =
        let capacity = 4 * 8
        let bitfield = Bitfield.create capacity 
        bitfield |> Bitfield.setBytes [| 1uy; 2uy; 3uy; 4uy |]
        Assert.False(not (bitfield |> Bitfield.getBit 7))
        Assert.False(not (bitfield |> Bitfield.getBit 14))
        Assert.False(not (bitfield |> Bitfield.getBit 22))
        Assert.False(not (bitfield |> Bitfield.getBit 23))
        Assert.False(not (bitfield |> Bitfield.getBit 29))
        Assert.True(not (bitfield |> Bitfield.getBit 0))
        Assert.True(not (bitfield |> Bitfield.getBit 1))
        Assert.True(not (bitfield |> Bitfield.getBit 2))
        Assert.True(not (bitfield |> Bitfield.getBit 3))
        Assert.True(not (bitfield |> Bitfield.getBit 4))
        Assert.True(not (bitfield |> Bitfield.getBit 5))
        Assert.True(not (bitfield |> Bitfield.getBit 6))
    
    [<Fact>]
    let ``Test bitfield should set bit on`` () =
        let capacity = 4 * 8
        let bitfield = Bitfield.create capacity
        bitfield |> Bitfield.setBytes [| 1uy; 2uy; 3uy; 4uy |]
        bitfield |> Bitfield.setBit 0 true
        bitfield |> Bitfield.setBit 1 true
        bitfield |> Bitfield.setBit 2 true
        bitfield |> Bitfield.setBit 3 true
        bitfield |> Bitfield.setBit 4 true
        bitfield |> Bitfield.setBit 5 true
        bitfield |> Bitfield.setBit 6 true
        Assert.True(bitfield |> Bitfield.getBit 0)
        Assert.True(bitfield |> Bitfield.getBit 1)
        Assert.True(bitfield |> Bitfield.getBit 2)
        Assert.True(bitfield |> Bitfield.getBit 3)
        Assert.True(bitfield |> Bitfield.getBit 4)
        Assert.True(bitfield |> Bitfield.getBit 5)
        Assert.True(bitfield |> Bitfield.getBit 6)
        
    [<Fact>]
    let ``Test bitfield should set bit off`` () =
        let capacity = 4 * 8
        let bitfield = Bitfield.create capacity
        bitfield |> Bitfield.setBytes [| 1uy; 2uy; 3uy; 4uy |]
        bitfield |> Bitfield.setBit 7 false
        bitfield |> Bitfield.setBit 14 false
        bitfield |> Bitfield.setBit 22 false
        bitfield |> Bitfield.setBit 23 false
        bitfield |> Bitfield.setBit 29 false
        Assert.True(not (bitfield |> Bitfield.getBit 7))
        Assert.True(not (bitfield |> Bitfield.getBit 14))
        Assert.True(not (bitfield |> Bitfield.getBit 22))
        Assert.True(not (bitfield |> Bitfield.getBit 23))
        Assert.True(not (bitfield |> Bitfield.getBit 29))
        
    [<Fact>]
    let ``Test bitfield should set bytes`` () =
        let capacity = 4 * 8
        let bitfield = Bitfield.create capacity
        for idx in 0..capacity - 1 do
            Assert.True(not (bitfield |> Bitfield.getBit idx))
        bitfield |> Bitfield.setBytes [| 1uy; 2uy; 3uy; 4uy |]
        Assert.True(bitfield |> Bitfield.getBit 7)
        Assert.True(bitfield |> Bitfield.getBit 14)
        Assert.True(bitfield |> Bitfield.getBit 22)
        Assert.True(bitfield |> Bitfield.getBit 23)
        Assert.True(bitfield |> Bitfield.getBit 29)
        Assert.False(bitfield |> Bitfield.getBit 0)
        Assert.False(bitfield |> Bitfield.getBit 1)
        Assert.False(bitfield |> Bitfield.getBit 2)
        Assert.False(bitfield |> Bitfield.getBit 3)
        Assert.False(bitfield |> Bitfield.getBit 4)
        Assert.False(bitfield |> Bitfield.getBit 5)
        Assert.False(bitfield |> Bitfield.getBit 6)
        bitfield |> Bitfield.setBytes [| 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; 255uy; |]
        Assert.Equal<byte[]>([| 255uy; 255uy; 255uy; 255uy |], bitfield.ToArray())
    
    [<Fact>]
    let ``Test bitfield should and`` () =
        ()
        
    [<Fact>]
    let ``Test bitfield should or`` () =
        ()
        
    [<Fact>]
    let ``Test bitfield should xor`` () =
        ()
        
    [<Fact>]
    let ``Test bitfield should not bits`` () =
        let capacity = 4 * 8
        let bitfield = Bitfield.create capacity
        for idx in 0..capacity - 1 do
            Assert.True(not (bitfield |> Bitfield.getBit idx))
        bitfield |> Bitfield.setBytes [| 1uy; 2uy; 3uy; 4uy |]
        let bitfield = bitfield |> Bitfield.notBits
        Assert.True(not (bitfield |> Bitfield.getBit 7))
        Assert.True(not (bitfield |> Bitfield.getBit 14))
        Assert.True(not (bitfield |> Bitfield.getBit 22))
        Assert.True(not (bitfield |> Bitfield.getBit 23))
        Assert.True(not (bitfield |> Bitfield.getBit 29))
        Assert.False(not (bitfield |> Bitfield.getBit 0))
        Assert.False(not (bitfield |> Bitfield.getBit 1))
        Assert.False(not (bitfield |> Bitfield.getBit 2))
        Assert.False(not (bitfield |> Bitfield.getBit 3))
        Assert.False(not (bitfield |> Bitfield.getBit 4))
        Assert.False(not (bitfield |> Bitfield.getBit 5))
        Assert.False(not (bitfield |> Bitfield.getBit 6))
    
    [<Fact>]
    let ``Test bitfield should get bit count`` () =
        let capacity = 4 * 8
        let bitfield = Bitfield.create capacity
        Assert.Equal(0, bitfield |> Bitfield.count)
        bitfield |> Bitfield.setBit 0 true
        Assert.Equal(1, bitfield |> Bitfield.count)
        bitfield |> Bitfield.setBit 1 true
        Assert.Equal(2, bitfield |> Bitfield.count)
        bitfield |> Bitfield.setBit 0 true
        Assert.Equal(2, bitfield |> Bitfield.count)
        bitfield |> Bitfield.setBit 1 true
        Assert.Equal(2, bitfield |> Bitfield.count)
        bitfield |> Bitfield.setBit 2 false
        Assert.Equal(2, bitfield |> Bitfield.count)
        bitfield |> Bitfield.setBit 3 false
        Assert.Equal(2, bitfield |> Bitfield.count)
        bitfield |> Bitfield.setBit 0 false
        Assert.Equal(1, bitfield |> Bitfield.count)
        bitfield |> Bitfield.setBit 1 false
        Assert.Equal(0, bitfield |> Bitfield.count)
        bitfield |> Bitfield.setBytes [| 255uy; 255uy |]
        Assert.Equal(capacity / 2, bitfield |> Bitfield.count)
        bitfield |> Bitfield.setBytes 
                                 [| 255uy; 255uy; 255uy; 255uy
                                    255uy; 255uy; 255uy; 255uy
                                    255uy; 255uy; 255uy; 255uy
                                    255uy; 255uy; 255uy; 255uy |]
        Assert.Equal(capacity, bitfield |> Bitfield.count)
        let bitfield = bitfield |> Bitfield.notBits
        Assert.Equal(0, bitfield |> Bitfield.count)
        
    [<Fact>]
    let ``Test bitfield should get bit capacity`` () =
        let bitfield = Bitfield.create 31
        Assert.Equal(31, bitfield |> Bitfield.capacity)
        let bitfield = Bitfield.create 32
        Assert.Equal(32, bitfield |> Bitfield.capacity)
        let bitfield = Bitfield.create 33
        Assert.Equal(33, bitfield |> Bitfield.capacity)
        let bitfield = Bitfield.create 34
        Assert.Equal(34, bitfield |> Bitfield.capacity)
        
    [<Fact>]
    let ``Test bitfield should check is empty`` () =
        let bitfield = Bitfield.create (4 * 8)
        Assert.True(bitfield |> Bitfield.isEmpty)
        bitfield |> Bitfield.setBit 0 true
        bitfield |> Bitfield.setBit 1 true
        bitfield |> Bitfield.setBit 2 true
        bitfield |> Bitfield.setBit 3 true
        Assert.False(bitfield |> Bitfield.isEmpty)
        
    [<Fact>]
    let ``Test bitfield should check is full`` () =
        let bitfield = Bitfield.create 4
        Assert.False(bitfield |> Bitfield.isFull)
        bitfield |> Bitfield.setBit 0 true
        bitfield |> Bitfield.setBit 1 true
        bitfield |> Bitfield.setBit 2 true
        bitfield |> Bitfield.setBit 3 true
        Assert.True(bitfield |> Bitfield.isFull)
    
    [<Fact>]
    let ``Test should to array`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let bitfield = Bitfield(32)
        bitfield.SetBytes(value)
        Assert.Equal<byte[]>(value, bitfield.ToArray())
    
    [<Fact>]
    let ``Test should to string`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let bitfield = Bitfield(32)
        bitfield.SetBytes(value)
        Assert.Equal("00000001 00000010 00000011 00000100", bitfield.ToString())
    
    [<Fact>]
    let ``Test should same bitfields be equal`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let bitfield1 = Bitfield(32)
        let bitfield2 = Bitfield(32)
        bitfield1.SetBytes(value)
        bitfield2.SetBytes(value)
        Assert.True(bitfield1.Equals(bitfield2))
        
    [<Fact>]
    let ``Test should different bitfields be not equal`` () =
        let value1 = [| 1uy; 2uy; 3uy; 4uy |]
        let value2 = [| 4uy; 3uy; 2uy; 1uy |]
        let bitfield1 = Bitfield(32)
        let bitfield2 = Bitfield(32)
        bitfield1.SetBytes(value1)
        bitfield2.SetBytes(value2)
        Assert.False(bitfield1.Equals(bitfield2))
        
    [<Fact>]
    let ``Test should same buffer hash codes be equal`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let bitfield1 = Bitfield(32)
        let bitfield2 = Bitfield(32)
        bitfield1.SetBytes(value)
        bitfield2.SetBytes(value)
        Assert.True(bitfield1.GetHashCode().Equals(bitfield2.GetHashCode()))
        
    [<Fact>]
    let ``Test should different buffer hash codes be not equal`` () =
        let value1 = [| 1uy; 2uy; 3uy; 4uy |]
        let value2 = [| 4uy; 3uy; 2uy; 1uy |]
        let bitfield1 = Bitfield(32)
        let bitfield2 = Bitfield(32)
        bitfield1.SetBytes(value1)
        bitfield2.SetBytes(value2)
        Assert.False(bitfield1.GetHashCode().Equals(bitfield2.GetHashCode()))
        
module BitfieldSelectorTests =
    [<Fact>]
    let ``Test should bitfield selector add bitfield``() =
        let bitfieldSelector = BitfieldSelector.create 32
        let bitfield1 = Bitfield.create 32
        bitfield1 |> Bitfield.setBit 0 true
        bitfieldSelector |> BitfieldSelector.addBitfield bitfield1
        bitfieldSelector |> BitfieldSelector.addBitfield bitfield1
        let array = bitfieldSelector.ToArray()
        Assert.Equal(2, array[0])
        
    [<Fact>]
    let ``Test should bitfield selector add bit``() =
        let bitfieldSelector = BitfieldSelector.create 32
        bitfieldSelector |> BitfieldSelector.addBit 0
        bitfieldSelector |> BitfieldSelector.addBit 1
        let array = bitfieldSelector.ToArray()
        Assert.Equal(1, array[0])
        
    [<Fact>]
    let ``Test should bitfield selector subtract bitfield``() =
        let bitfieldSelector = BitfieldSelector.create 32
        let bitfield1 = Bitfield.create 32
        bitfield1 |> Bitfield.setBit 0 true
        let bitfield2 = Bitfield.create 32
        bitfield2 |> Bitfield.setBit 0 true
        bitfieldSelector |> BitfieldSelector.addBitfield bitfield1
        bitfieldSelector |> BitfieldSelector.subtractBitfield bitfield2
        let array = bitfieldSelector.ToArray()
        Assert.Equal(0, array[0])
        
    [<Fact>]
    let ``Test should bitfield selector subtract bit``() =
        let bitfieldSelector = BitfieldSelector.create 32
        bitfieldSelector |> BitfieldSelector.addBit 0
        bitfieldSelector |> BitfieldSelector.subtractBit 0
        let array = bitfieldSelector.ToArray()
        Assert.Equal(0, array[0])
        
    [<Fact>]
    let ``Test should bitfield selector select first bit``() =
        let bitfieldSelector = BitfieldSelector.create 32
        let bitfield1 = Bitfield.create 32
        bitfield1 |> Bitfield.setBit 0 true
        bitfield1 |> Bitfield.setBit 1 true
        let bitfield2 = Bitfield.create 32
        bitfield2 |> Bitfield.setBit 0 true
        bitfield2 |> Bitfield.setBit 1 true
        bitfieldSelector |> BitfieldSelector.addBitfield bitfield1
        bitfieldSelector |> BitfieldSelector.addBitfield bitfield2
        let selfBitfield = Bitfield.create 32
        selfBitfield |> Bitfield.setBit 0 true
        selfBitfield |> Bitfield.setBit 1 true
        let peerBitfield = Bitfield.create 32
        peerBitfield |> Bitfield.setBit 0 true
        peerBitfield |> Bitfield.setBit 1 true
        match bitfieldSelector |> BitfieldSelector.firstBit selfBitfield peerBitfield with
        | Some idx ->
            Assert.Equal(0, idx)
        | None ->
            Assert.True(false, "Should have found a bit")
        
    [<Fact>]
    let ``Test should bitfield selector select rarest bit``() =
        let bitfieldSelector = BitfieldSelector.create 32
        let bitfield1 = Bitfield.create 32
        bitfield1 |> Bitfield.setBit 0 true
        bitfield1 |> Bitfield.setBit 1 true
        bitfield1 |> Bitfield.setBit 2 true
        let bitfield2 = Bitfield.create 32
        bitfield2 |> Bitfield.setBit 0 true
        bitfield2 |> Bitfield.setBit 1 true
        bitfieldSelector |> BitfieldSelector.addBitfield bitfield1
        bitfieldSelector |> BitfieldSelector.addBitfield bitfield2
        let selfBitfield = Bitfield.create 32
        selfBitfield |> Bitfield.setBit 0 true
        selfBitfield |> Bitfield.setBit 1 true
        selfBitfield |> Bitfield.setBit 2 true
        let peerBitfield = Bitfield.create 32
        peerBitfield |> Bitfield.setBit 0 true
        peerBitfield |> Bitfield.setBit 1 true
        peerBitfield |> Bitfield.setBit 2 true
        match bitfieldSelector |> BitfieldSelector.rarestBit selfBitfield peerBitfield with
        | Some idx ->
            Assert.Equal(2, idx)
        | None ->
            Assert.True(false, "Should have found a bit")