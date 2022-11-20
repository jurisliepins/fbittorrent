namespace FBitTorrent.Core.Tests

open System.Linq
open FBitTorrent.Core
open Xunit
      
module HashTests = 
    let private Value =
        [| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy |]

    let private ValueHashed =
        [| 73uy; 65uy; 121uy; 113uy; 74uy; 108uy; 214uy; 39uy; 35uy; 157uy
           254uy; 222uy; 223uy; 45uy; 233uy; 239uy; 153uy; 76uy; 175uy; 3uy |]

    let private ValueHashedString =
        "494179714a6cd627239dfededf2de9ef994caf03"

    [<Fact>]
    let ``Test should create hash from bytes`` () =
        Assert.Equal<byte[]>((Hash ValueHashed).ToArray(), ValueHashed)
    
    [<Fact>]
    let ``Test should create hash from string`` () =
        Assert.Equal<byte[]>((Hash ValueHashedString).ToArray(), ValueHashed)
    
    [<Fact>]
    let ``Test should compute hash from block`` () =
        Assert.Equal<Hash>(Hash ValueHashed, Hash.ComputeBlock(Value))
        
    [<Fact>]
    let ``Test should compute hash from blocks`` () =
        Assert.Equal<Hash>(Hash ValueHashed, Hash.ComputeBlocks(Value.Chunk(2).ToArray()))

    [<Fact>]
    let ``Test should same hashes be equal`` () =
        Assert.True(ValueHashed.Equals(ValueHashed))
        
    [<Fact>]
    let ``Test should different hashes be not equal`` () =
        Assert.False(ValueHashed.Equals(Hash [||]))
        
    [<Fact>]
    let ``Test should same hash codes be equal`` () =
        Assert.True(ValueHashed.GetHashCode().Equals(ValueHashed.GetHashCode()))
        
    [<Fact>]
    let ``Test should different hash codes be not equal`` () =
        Assert.False(ValueHashed.GetHashCode().Equals((Hash [||]).GetHashCode()))

    [<Fact>]
    let ``Test should hash string from block`` () =
        Assert.Equal(ValueHashedString, Hash.ComputeBlock(Value).ToString())
        
    [<Fact>]
    let ``Test should hash string from blocks`` () =
        Assert.Equal(ValueHashedString, Hash.ComputeBlocks(Value.Chunk(2).ToArray()).ToString())