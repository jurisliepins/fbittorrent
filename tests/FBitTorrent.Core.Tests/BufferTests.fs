namespace FBitTorrent.Core.Tests

open System
open System.IO
open System.Linq
open System.Collections.Generic
open FBitTorrent.Core
open Xunit

module ByteBufferTests =
    [<Fact>]
    let ``Test should create from length`` () =
        let buffer = ByteBuffer(1)
        Assert.Equal(1, buffer.ToArray().Length)
        let buffer = ByteBuffer(10)
        Assert.Equal(10, buffer.ToArray().Length)
        let buffer = ByteBuffer(100)
        Assert.Equal(100, buffer.ToArray().Length)
    
    [<Fact>]
    let ``Test should create from bytes`` () =
        let buffer = ByteBuffer(Array.create 1 0uy)
        Assert.Equal(1, buffer.ToArray().Length)
        let buffer = ByteBuffer(Array.create 10 0uy)
        Assert.Equal(10, buffer.ToArray().Length)
        let buffer = ByteBuffer(Array.create 100 0uy)
        Assert.Equal(100, buffer.ToArray().Length)
        
    [<Fact>]
    let ``Test should create empty`` () =
        let buffer = ByteBuffer(Array.create 0 0uy)
        Assert.Equal(0, buffer.ToArray().Length)
    
    [<Fact>]
    let ``Test should get length`` () =
        let buffer = ByteBuffer(Array.create 1 0uy)
        Assert.Equal(1, buffer.Length)
        let buffer = ByteBuffer(Array.create 10 0uy)
        Assert.Equal(10, buffer.Length)
        let buffer = ByteBuffer(Array.create 100 0uy)
        Assert.Equal(100, buffer.Length)
        
    [<Fact>]
    let ``Test should get is released`` () =
        let buffer = ByteBuffer(Array.create 1 0uy)
        buffer.Release()
        Assert.True(buffer.IsReleased)
        let buffer = ByteBuffer(Array.create 10 0uy)
        buffer.Release()
        Assert.True(buffer.IsReleased)
        let buffer = ByteBuffer(Array.create 100 0uy)
        buffer.Release()
        Assert.True(buffer.IsReleased)
    
    [<Fact>]
    let ``Test should read from reader partial`` () =
        let value = [| 1uy; 2uy; |]
        use stream = new MemoryStream(value)
        let buffer = ByteBuffer(4)
        let n = buffer.ReadFrom(stream, 1, 2)
        Assert.Equal(2, n)
        Assert.Equal(4, buffer.Length)
        Assert.Equal<byte[]>([| 0uy; 1uy; 2uy; 0uy |], buffer.ToArray())
            
    [<Fact>]
    let ``Test should read from reader full`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        use stream = new MemoryStream(value)
        let buffer = ByteBuffer(4)
        let n = buffer.ReadFrom(stream)
        Assert.Equal(value.Length, n)
        Assert.Equal(value.Length, buffer.Length)
        Assert.Equal<byte[]>(value, buffer.ToArray())
    
    [<Fact>]
    let ``Test should write to writer partial`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        use stream = new MemoryStream()
        let buffer = ByteBuffer(value)
        buffer.WriteTo(stream, 1, 2)
        Assert.Equal(2, stream.ToArray().Length)
        Assert.Equal<byte[]>([| 2uy; 3uy |], stream.ToArray())
    
    [<Fact>]
    let ``Test should write to writer full`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        use stream = new MemoryStream()
        let buffer = ByteBuffer(value)
        buffer.WriteTo(stream)
        Assert.Equal(value.Length, stream.ToArray().Length)
        Assert.Equal<byte[]>(value, stream.ToArray())
        
    [<Fact>]
    let ``Test should get read only memory`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let buffer = ByteBuffer(value)
        let memory = buffer.AsReadOnlyMemory()
        Assert.Equal<byte[]>(value, memory.ToArray())
    
    [<Fact>]
    let ``Test should get read only span`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let buffer = ByteBuffer(value)
        let span = buffer.AsReadOnlySpan()
        Assert.Equal<byte[]>(value, span.ToArray())
    
    [<Fact>]
    let ``Test should get memory`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let buffer = ByteBuffer(value)
        let memory = buffer.AsMemory()
        Assert.Equal<byte[]>(value, memory.ToArray())
        
    [<Fact>]
    let ``Test should get span`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let buffer = ByteBuffer(value)
        let span = buffer.AsSpan()
        Assert.Equal<byte[]>(value, span.ToArray())
        
    [<Fact>]
    let ``Test should to array`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let buffer = ByteBuffer(value)
        Assert.Equal<byte[]>(value, buffer.ToArray())
    
    [<Fact>]
    let ``Test should copy to array with offset`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let buffer = ByteBuffer(value)
        let dest = Array.create 8 0uy
        buffer.CopyTo(dest, 4)
        Assert.Equal<byte[]>([| 0uy; 0uy; 0uy; 0uy; 1uy; 2uy; 3uy; 4uy |], dest)
    
    [<Fact>]
    let ``Test should copy to array`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let buffer = ByteBuffer(value)
        let dest = Array.create 4 0uy
        buffer.CopyTo(dest)
        Assert.Equal<byte[]>([| 1uy; 2uy; 3uy; 4uy |], dest)
    
    [<Fact>]
    let ``Test should copy to buffer with offset`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let buffer = ByteBuffer(value)
        let dest = ByteBuffer(8)
        buffer.CopyTo(dest, 4)
        Assert.Equal<byte[]>([| 0uy; 0uy; 0uy; 0uy; 1uy; 2uy; 3uy; 4uy |], dest.ToArray())
    
    [<Fact>]
    let ``Test should copy to buffer`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let buffer = ByteBuffer(value)
        let dest = ByteBuffer(4)
        buffer.CopyTo(dest)
        Assert.Equal<byte[]>([| 1uy; 2uy; 3uy; 4uy |], dest.ToArray())
    
    [<Fact>]
    let ``Test should release`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let buffer = ByteBuffer(value)
        buffer.Release()
        buffer.Release()
        
    [<Fact>]
    let ``Test should throw when accessing after released`` () =
        let createReleased () =  let buffer = ByteBuffer(1) in buffer.Release(); buffer
        
        Assert.ThrowsAny<Exception>(fun () -> (createReleased ()).ReadFrom(new MemoryStream([| 0uy |])) |> ignore) |> ignore
        Assert.ThrowsAny<Exception>(fun () -> (createReleased ()).ReadFrom(new MemoryStream([| 0uy |]), 0, 1) |> ignore) |> ignore
        Assert.ThrowsAny<Exception>(fun () -> (createReleased ()).WriteTo(new MemoryStream([| 0uy |]))) |> ignore
        Assert.ThrowsAny<Exception>(fun () -> (createReleased ()).WriteTo(new MemoryStream([| 0uy |]), 0, 1)) |> ignore
        
        Assert.ThrowsAny<Exception>(fun () -> (createReleased ()).AsReadOnlyMemory() |> ignore) |> ignore
        Assert.ThrowsAny<Exception>(fun () -> (createReleased ()).AsMemory() |> ignore) |> ignore
        Assert.ThrowsAny<Exception>(fun () -> let _ = (createReleased ()).AsReadOnlySpan() in ()) |> ignore
        Assert.ThrowsAny<Exception>(fun () -> let _ = (createReleased ()).AsSpan() in ()) |> ignore
        
        Assert.ThrowsAny<Exception>(fun () -> let dest = Array.create 2 0uy in (createReleased ()).CopyTo(dest, 1)) |> ignore
        Assert.ThrowsAny<Exception>(fun () -> let dest = Array.create 2 0uy in (createReleased ()).CopyTo(dest)) |> ignore
        Assert.ThrowsAny<Exception>(fun () -> let dest = ByteBuffer(2) in (createReleased ()).CopyTo(dest, 1)) |> ignore
        Assert.ThrowsAny<Exception>(fun () -> let dest = ByteBuffer(2) in (createReleased ()).CopyTo(dest)) |> ignore
        
    [<Fact>]
    let ``Test should same buffers be equal`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let buffer1 = ByteBuffer(value)
        let buffer2 = ByteBuffer(value)
        Assert.True(buffer1.Equals(buffer2))
        
    [<Fact>]
    let ``Test should different buffers be not equal`` () =
        let value1 = [| 1uy; 2uy; 3uy; 4uy |]
        let value2 = [| 4uy; 3uy; 2uy; 1uy |]
        let buffer1 = ByteBuffer(value1)
        let buffer2 = ByteBuffer(value2)
        Assert.False(buffer1.Equals(buffer2))
        
    [<Fact>]
    let ``Test should same buffer hash codes be equal`` () =
        let value = [| 1uy; 2uy; 3uy; 4uy |]
        let buffer1 = ByteBuffer(value)
        let buffer2 = ByteBuffer(value)
        Assert.True(buffer1.GetHashCode().Equals(buffer2.GetHashCode()))
        
    [<Fact>]
    let ``Test should different buffer hash codes be not equal`` () =
        let value1 = [| 1uy; 2uy; 3uy; 4uy |]
        let value2 = [| 4uy; 3uy; 2uy; 1uy |]
        let buffer1 = ByteBuffer(value1)
        let buffer2 = ByteBuffer(value2)
        Assert.False(buffer1.GetHashCode().Equals(buffer2.GetHashCode()))
        
module CircularBufferTests =
    [<Fact>]
    let ``Test should circular buffer push and enumerate`` () =
        let buffer = CircularBuffer<int>(5)
        Assert.Equal(0L, buffer.LongCount())
        let enumerator1 = (buffer :> IEnumerable<int>).GetEnumerator()
        Assert.False(enumerator1.MoveNext())
        buffer.Push(1)
        Assert.Equal(1L, buffer.LongCount())
        buffer.Push(2)
        Assert.Equal(2L, buffer.LongCount())
        buffer.Push(3)
        Assert.Equal(3L, buffer.LongCount())
        buffer.Push(4)
        Assert.Equal(4L, buffer.LongCount())
        buffer.Push(5)
        Assert.Equal(5L, buffer.LongCount())
        let enumerator2 = (buffer :> IEnumerable<int>).GetEnumerator()
        Assert.True(enumerator2.MoveNext())
        Assert.Equal(1, enumerator2.Current)
        Assert.True(enumerator2.MoveNext())
        Assert.Equal(2, enumerator2.Current)
        Assert.True(enumerator2.MoveNext())
        Assert.Equal(3, enumerator2.Current)
        Assert.True(enumerator2.MoveNext())
        Assert.Equal(4, enumerator2.Current)
        Assert.True(enumerator2.MoveNext())
        Assert.Equal(5, enumerator2.Current)
        buffer.Push(6)
        Assert.Equal(5L, buffer.LongCount())
        buffer.Push(7)
        Assert.Equal(5L, buffer.LongCount())
        buffer.Push(8)
        Assert.Equal(5L, buffer.LongCount())
        buffer.Push(9)
        Assert.Equal(5L, buffer.LongCount())
        buffer.Push(10)
        Assert.Equal(5L, buffer.LongCount())
        let enumerator3 = (buffer :> IEnumerable<int>).GetEnumerator()
        Assert.True(enumerator3.MoveNext())
        Assert.Equal(6, enumerator3.Current)
        Assert.True(enumerator3.MoveNext())
        Assert.Equal(7, enumerator3.Current)
        Assert.True(enumerator3.MoveNext())
        Assert.Equal(8, enumerator3.Current)
        Assert.True(enumerator3.MoveNext())
        Assert.Equal(9, enumerator3.Current)
        Assert.True(enumerator3.MoveNext())
        Assert.Equal(10, enumerator3.Current)