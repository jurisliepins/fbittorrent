namespace FBitTorrent.Core

open System.Linq
open System.Collections
open System.Collections.Generic

type CircularBuffer<'a>(capacity: int) =
    let values = Array.zeroCreate<'a> capacity
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

type Rate() =
    let [<Literal>] Smoothing = 0.02
    let [<Literal>] Capacity = 5
    
    let byteDeltas = CircularBuffer(Capacity)
    let mutable byteCount = 0L
    
    member _.Update(updatedByteCount: int64) =
        byteDeltas.Push(updatedByteCount - byteCount)
        byteCount <- updatedByteCount

    member _.GetSpeed() =
        if byteDeltas.Count < 1 then
            double 0.0
        else
            let last = double (byteDeltas.Last())
            let total = double (byteDeltas.Sum())
            let count = double byteDeltas.Count
            let average = total / count
            // Exponential moving average to smooth out the result.
            double ((Smoothing * last) + ((1.0 - Smoothing) * average))
