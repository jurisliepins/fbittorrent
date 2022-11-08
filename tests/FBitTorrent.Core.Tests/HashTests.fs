namespace FBitTorrent.Core.Tests

open System.Linq
open FBitTorrent.Core
open Xunit
      
module HashTests = 
    let value =
        [| 0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy; 8uy; 9uy |]

    let valueHashed =
        [| 73uy; 65uy; 121uy; 113uy; 74uy; 108uy; 214uy; 39uy; 35uy; 157uy
           254uy; 222uy; 223uy; 45uy; 233uy; 239uy; 153uy; 76uy; 175uy; 3uy |]

    let valueHashedString =
        "494179714a6cd627239dfededf2de9ef994caf03"

    [<Fact>]
    let ``Test should create hash from bytes`` () =
        Assert.Equal<byte[]>((Hash valueHashed).ToArray(), valueHashed)
    
    [<Fact>]
    let ``Test should create hash from string`` () =
        Assert.Equal<byte[]>((Hash valueHashedString).ToArray(), valueHashed)
    
    [<Fact>]
    let ``Test should same hashes be equal`` () =
        Assert.True(valueHashed.Equals(valueHashed))
        
    [<Fact>]
    let ``Test should different hashes be not equal`` () =
        Assert.False(valueHashed.Equals(Hash [||]))
        
    [<Fact>]
    let ``Test should same hash codes be equal`` () =
        Assert.True(valueHashed.GetHashCode().Equals(valueHashed.GetHashCode()))
        
    [<Fact>]
    let ``Test should different hash codes be not equal`` () =
        Assert.False(valueHashed.GetHashCode().Equals((Hash [||]).GetHashCode()))