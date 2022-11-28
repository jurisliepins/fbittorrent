namespace FBitTorrent.Core

open System.Linq
open System.Collections
open System.Collections.Generic

type CircularBuffer<'a>(capacity: int) =
    let mutable values = Array.zeroCreate<'a> capacity
    let mutable pointer = 0
    let mutable count = 0
    
    member _.Values with get() = values
    member _.Pointer with get() = pointer
    member _.Count with get() = count
    
    member _.Push(value: 'a) =
        if count >= values.Length then
            values[(pointer + count) % values.Length] <- value
            pointer <- pointer + 1
        else
            values[(pointer + count) % values.Length] <- value
            count <- count + 1
            
    interface IEnumerable with
        member __.GetEnumerator() = new CircularBufferEnumerator<'a>(__)
        
    interface IEnumerable<'a> with
        member __.GetEnumerator() = new CircularBufferEnumerator<'a>(__)
            
and CircularBufferEnumerator<'a>(buffer: CircularBuffer<'a>) =
    
    member val private Current = Unchecked.defaultof<'a> with get, set
    member val private Index = 0 with get, set
    
    interface IEnumerator<'a> with
        member __.Current with get() = __.Current
            
        member _.Dispose() = ()
    
    interface IEnumerator with
        member __.Current with get() = __.Current :> obj
        
        member __.MoveNext() =
            if __.Index < buffer.Count then
                __.Current <- buffer.Values[(buffer.Pointer + __.Index) % buffer.Values.Length]
                __.Index <- __.Index + 1
                true
            else
                false
                
        member __.Reset() = __.Index <- 0

module Rate =
    let [<Literal>] Smoothing = 0.02
    let [<Literal>] Capacity = 5

type Rate() =
    member val private ByteCounts = CircularBuffer(Rate.Capacity) with get
    member val private ByteCount = 0L with get, set
    
    member __.Update(byteCount: int64) =
        __.ByteCounts.Push(byteCount - __.ByteCount)
        __.ByteCount <- byteCount

    member __.GetSpeed() =
        if __.ByteCounts.Count < 1 then
            double 0.0
        else
            let last = double (__.ByteCounts.Last())
            let total = double (__.ByteCounts.Sum())
            let count = double __.ByteCounts.Count
            let average = total / count
            // Exponential moving average to smooth out the result.
            double ((Rate.Smoothing * last) + ((1.0 - Rate.Smoothing) * average))
