namespace FBitTorrent.Core.Tests

open FBitTorrent.Core
open Xunit

module BitfieldTests =
    [<Fact>]
    let ``Test bitfield should get value on`` () =
        let bitfield = Bitfield(4 * 8)
        bitfield.Set(([| 1uy; 2uy; 3uy; 4uy |]))
        Assert.True(bitfield.Get(7))
        Assert.True(bitfield.Get(14))
        Assert.True(bitfield.Get(22))
        Assert.True(bitfield.Get(23))
        Assert.True(bitfield.Get(29))
        Assert.False(bitfield.Get(0))
        Assert.False(bitfield.Get(1))
        Assert.False(bitfield.Get(2))
        Assert.False(bitfield.Get(3))
        Assert.False(bitfield.Get(4))
        Assert.False(bitfield.Get(5))
        Assert.False(bitfield.Get(6))
        
    [<Fact>]
    let ``Test bitfield should get value off`` () =
        let bitfield = Bitfield(4 * 8)
        bitfield.Set([| 1uy; 2uy; 3uy; 4uy |])
        Assert.False(not (bitfield.Get(7)))
        Assert.False(not (bitfield.Get(14)))
        Assert.False(not (bitfield.Get(22)))
        Assert.False(not (bitfield.Get(23)))
        Assert.False(not (bitfield.Get(29)))
        Assert.True(not (bitfield.Get(0)))
        Assert.True(not (bitfield.Get(1)))
        Assert.True(not (bitfield.Get(2)))
        Assert.True(not (bitfield.Get(3)))
        Assert.True(not (bitfield.Get(4)))
        Assert.True(not (bitfield.Get(5)))
        Assert.True(not (bitfield.Get(6)))
    
    [<Fact>]
    let ``Test bitfield should set value on`` () =
        let bitfield = Bitfield(4 * 8)
        bitfield.Set([| 1uy; 2uy; 3uy; 4uy |])
        bitfield.Set(0, true)
        bitfield.Set(1, true)
        bitfield.Set(2, true)
        bitfield.Set(3, true)
        bitfield.Set(4, true)
        bitfield.Set(5, true)
        bitfield.Set(6, true)
        Assert.True(bitfield.Get(0))
        Assert.True(bitfield.Get(1))
        Assert.True(bitfield.Get(2))
        Assert.True(bitfield.Get(3))
        Assert.True(bitfield.Get(4))
        Assert.True(bitfield.Get(5))
        Assert.True(bitfield.Get(6))
        
    [<Fact>]
    let ``Test bitfield should set value off`` () =
        let bitfield = Bitfield(4 * 8)
        bitfield.Set([| 1uy; 2uy; 3uy; 4uy |])
        bitfield.Set(7, false)
        bitfield.Set(14, false)
        bitfield.Set(22, false)
        bitfield.Set(23, false)
        bitfield.Set(29, false)
        Assert.True(not (bitfield.Get(7)))
        Assert.True(not (bitfield.Get(14)))
        Assert.True(not (bitfield.Get(22)))
        Assert.True(not (bitfield.Get(23)))
        Assert.True(not (bitfield.Get(29)))
        
    
    [<Fact>]
    let ``Test bitfield should set values`` () =
        let bitfield = Bitfield(4 * 8)
        for idx in 0..(4 * 8) - 1 do
            Assert.True(not (bitfield.Get(idx)))
        bitfield.Set([| 1uy; 2uy; 3uy; 4uy |])
        Assert.True(bitfield.Get(7))
        Assert.True(bitfield.Get(14))
        Assert.True(bitfield.Get(22))
        Assert.True(bitfield.Get(23))
        Assert.True(bitfield.Get(29))
        Assert.False(bitfield.Get(0))
        Assert.False(bitfield.Get(1))
        Assert.False(bitfield.Get(2))
        Assert.False(bitfield.Get(3))
        Assert.False(bitfield.Get(4))
        Assert.False(bitfield.Get(5))
        Assert.False(bitfield.Get(6))
        bitfield.Set([| 255uy; 255uy; 255uy; 255uy
                        255uy; 255uy; 255uy; 255uy; |])
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
    let ``Test bitfield should not`` () =
        let bitfield = Bitfield(4 * 8)
        for idx in 0..(4 * 8) - 1 do
            Assert.True(not (bitfield.Get(idx)))
        bitfield.Set([| 1uy; 2uy; 3uy; 4uy |])
        bitfield.Not() |> ignore
        Assert.True(not (bitfield.Get(7)))
        Assert.True(not (bitfield.Get(14)))
        Assert.True(not (bitfield.Get(22)))
        Assert.True(not (bitfield.Get(23)))
        Assert.True(not (bitfield.Get(29)))
        Assert.False(not (bitfield.Get(0)))
        Assert.False(not (bitfield.Get(1)))
        Assert.False(not (bitfield.Get(2)))
        Assert.False(not (bitfield.Get(3)))
        Assert.False(not (bitfield.Get(4)))
        Assert.False(not (bitfield.Get(5)))
        Assert.False(not (bitfield.Get(6)))
    
    [<Fact>]
    let ``Test bitfield should get count`` () =
        let bitfield = Bitfield(4 * 8)
        bitfield.Set(0, true)
        Assert.Equal(1, bitfield.Count)
        bitfield.Set(1, true)
        Assert.Equal(2, bitfield.Count)
        bitfield.Set(0, true)
        Assert.Equal(2, bitfield.Count)
        bitfield.Set(1, true)
        Assert.Equal(2, bitfield.Count)
        bitfield.Set(2, false)
        Assert.Equal(2, bitfield.Count)
        bitfield.Set(3, false)
        Assert.Equal(2, bitfield.Count)
        bitfield.Set(0, false)
        Assert.Equal(1, bitfield.Count)
        bitfield.Set(1, false)
        Assert.Equal(0, bitfield.Count)
        bitfield.Set([| 255uy; 255uy; 255uy; 255uy
                        255uy; 255uy; 255uy; 255uy
                        255uy; 255uy; 255uy; 255uy
                        255uy; 255uy; 255uy; 255uy; |])
        Assert.Equal(4 * 8, bitfield.Count)
        
    [<Fact>]
    let ``Test bitfield should get capacity`` () =
        let bitfield = Bitfield(31)
        Assert.Equal(31, bitfield.Capacity)
        let bitfield = Bitfield(32)
        Assert.Equal(32, bitfield.Capacity)
        let bitfield = Bitfield(33)
        Assert.Equal(33, bitfield.Capacity)
        let bitfield = Bitfield(34)
        Assert.Equal(34, bitfield.Capacity)
        
    [<Fact>]
    let ``Test bitfield should check is empty`` () =
        let bitfield = Bitfield(4 * 8)
        Assert.True(bitfield.IsEmpty)
        bitfield.Set(0, true)
        bitfield.Set(1, true)
        bitfield.Set(2, true)
        bitfield.Set(3, true)
        Assert.False(bitfield.IsEmpty)
        
    [<Fact>]
    let ``Test bitfield should check is full`` () =
        let bitfield = Bitfield(4)
        Assert.False(bitfield.IsFull)
        bitfield.Set(0, true)
        bitfield.Set(1, true)
        bitfield.Set(2, true)
        bitfield.Set(3, true)
        Assert.True(bitfield.IsFull)