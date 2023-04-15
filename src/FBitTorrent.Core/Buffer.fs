namespace FBitTorrent.Core

open System
open System.Buffers
open System.Collections
open System.Collections.Generic
open System.IO
open System.Linq

type SomethingStream() =
    inherit Stream()

    let mutable bytes = ArrayPool<byte>.Shared.Rent(16384)

    let mutable readerPos = 0
    let mutable writerPos = 0
    
    override this.Read(buffer, offset, count) =
        Array.Copy(bytes, readerPos, buffer, offset, count)
        readerPos <- readerPos + count
        // if writerPos = bytes.Length && readerPos = writerPos then
            // let newBytes = ArrayPool<byte>.Shared.Rent(bytes.Length - readerPos)
            // Array.Copy(bytes, readerPos, newBytes, 0, bytes.Length - readerPos)
            // writerPos <- (writerPos - readerPos)
            // readerPos <- 0
            // ArrayPool<byte>.Shared.Return(bytes)
            // bytes <- newBytes
        readerPos
    
    override this.Write(buffer, offset, count) =
        if (writerPos + count) > bytes.Length then
            let newBytes = ArrayPool<byte>.Shared.Rent(bytes.Length * 2)
            Array.Copy(bytes, 0, newBytes, 0, writerPos)
            ArrayPool<byte>.Shared.Return(bytes)
            bytes <- newBytes
        Array.Copy(buffer, offset, bytes, writerPos, count)
        writerPos <- writerPos + count
    
    override this.Flush() = failwith "todo"
    override this.Seek(offset, origin) = failwith "todo"
    override this.SetLength(value) = failwith "todo"
    override this.CanRead = failwith "todo"
    override this.CanSeek = failwith "todo"
    override this.CanWrite = failwith "todo"
    override this.Length = writerPos
    override this.Position = readerPos
    override this.Position with set value = failwith "todo" 

type ByteBuffer =
    val private bytes: byte[]
    val private length: int
    val mutable private released: bool
    new(length: int) =
        { bytes    = ArrayPool<byte>.Shared.Rent(length)
          length   = length
          released = false }
    new(bytes: byte[]) as __ =
        { bytes    = ArrayPool<byte>.Shared.Rent(bytes.Length)
          length   = bytes.Length
          released = false } then
        bytes.CopyTo(__.bytes, 0)
    new() =
        { bytes    = ArrayPool<byte>.Shared.Rent(0)
          length   = 0
          released = false }
    
    static member Empty with get() = ByteBuffer()
    
    member private __.Bytes with get() : byte[] =
        if __.released then
            failwith "Buffer has already been released"
        __.bytes    
    member __.Length with get() : int = __.length
    member __.IsReleased with get() : bool = __.released

    member __.ReadFrom(reader: Stream, idx: int, count: int) = reader.Read(__.Bytes, idx, count)
    member __.AsyncReadFrom(reader: Stream, idx: int, count: int) = reader.AsyncRead(__.Bytes, idx, count)
    member __.ReadFrom(reader: Stream) = __.ReadFrom(reader, 0, __.Length)
    member __.AsyncReadFrom(reader: Stream) = __.AsyncReadFrom(reader, 0, __.Length)

    member __.WriteTo(writer: Stream, idx: int, count: int) = writer.Write(__.Bytes, idx, count)
    member __.AsyncWriteTo(writer: Stream, idx: int, count: int) = writer.AsyncWrite(__.Bytes, idx, count)
    member __.WriteTo(writer: Stream) = __.WriteTo(writer, 0, __.Length)
    member __.AsyncWriteTo(writer: Stream) = __.AsyncWriteTo(writer, 0, __.Length)
    
    member __.AsReadOnlyMemory() = ReadOnlyMemory(__.Bytes, 0, __.Length)
    member __.AsReadOnlySpan() = ReadOnlySpan(__.Bytes, 0, __.Length)
    member __.AsMemory() = Memory(__.Bytes, 0, __.Length)
    member __.AsSpan() = Span(__.Bytes, 0, __.Length)
    
    member __.ToArray() =
        let result = Array.create __.Length 0uy
        Array.Copy(__.Bytes, result, __.Length)
        result
    
    member __.CopyTo(dest: byte[], idx: int) = Array.Copy(__.Bytes, 0, dest, idx, __.Length)
    member __.CopyTo(dest: byte[]) = __.CopyTo(dest, 0)
    member __.CopyTo(dest: ByteBuffer, idx: int) = Array.Copy(__.Bytes, 0, dest.Bytes, idx, __.Length)
    member __.CopyTo(dest: ByteBuffer) = __.CopyTo(dest, 0)
    
    member __.Release() =
        if not __.released then
            ArrayPool.Shared.Return(__.Bytes)
            __.released <- true
    
    override __.GetHashCode() =
         __.Bytes.Aggregate(0, (fun result byte -> (result * 31) ^^^ (int byte)))
     
    override __.Equals(other) =
        match other with
        | :? ByteBuffer as value when __.Length = value.Length ->
            let rec equals (idx: int) =
                if idx >= __.Length then
                    true
                else
                    if __.Bytes[idx] <> value.Bytes[idx] then
                        false
                    else
                        equals (idx + 1)
            equals 0
        | _ -> false

type CircularBuffer<'a>(capacity: int) =
    let values = Array.zeroCreate<'a> capacity
    let mutable count = 0
    let mutable pointer = 0
    
    member _.Values with get() = values
    member _.Capacity with get() = capacity 
    member _.Count with get() = count
    member _.Pointer with get() = pointer
    
    member _.Push(value: 'a) =
        if count >= values.Length then
            values[(pointer + count) % values.Length] <- value
            pointer <- pointer + 1
        else
            values[(pointer + count) % values.Length] <- value
            count <- count + 1
    
    interface IEnumerable<'a> with
        member __.GetEnumerator() = new CircularBufferEnumerator<'a>(__)
            
    interface IEnumerable with
        member __.GetEnumerator() = new CircularBufferEnumerator<'a>(__)
            
and CircularBufferEnumerator<'a>(buffer: CircularBuffer<'a>) =
    let mutable current = Unchecked.defaultof<'a>
    let mutable index = 0
    
    interface IEnumerator<'a> with
        member _.Current with get() = current
            
        member _.Dispose() = ()
    
    interface IEnumerator with
        member _.Current with get() = current :> obj
        
        member _.MoveNext() =
            if index < buffer.Count then
                current <- buffer.Values[(buffer.Pointer + index) % buffer.Values.Length]
                index <- index + 1
                true
            else
                false
                
        member _.Reset() = index <- 0