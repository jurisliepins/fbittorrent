namespace FBitTorrent.Core

open System
open System.Linq

type Bitfield =
    val mutable private bytes: byte[]
    val mutable private capacity: int
    val mutable private count: int
    new(capacity: int) = {
        bytes =
            if (capacity % 8) <> 0 then
                (Array.create ((capacity / 8) + 1) 0uy)
            else
                (Array.create (capacity / 8) 0uy)
        capacity = capacity
        count = 0 }
    new(bitfield: Bitfield) = {
        bytes = bitfield.ToArray()
        capacity = bitfield.Capacity
        count = bitfield.Count }

    member __.Capacity with get() = __.capacity
    
    member __.Count with get() = __.count
        
    member __.IsEmpty with get() = __.Count = 0
    
    member __.IsFull with get() = __.Count = __.Capacity
    
    member __.Get(idx: int) =
        if idx < 0 || idx >= __.Capacity then
            false
        else
            let index = idx / 8
            let offset = idx % 8
            ((__.bytes[index] >>> (7 - offset)) &&& 1uy) <> 0uy
    
    member __.Set(idx: int, value: bool) =
        if idx < 0 || idx >= __.Capacity then
            ()
        else
            let index = idx / 8
            let offset = idx % 8
            if value then
                if not (__.Get(idx)) then
                    __.bytes[index] <- __.bytes[index] ||| (1uy <<< (7 - offset))
                    __.count <- __.count + 1
            else
                if __.Get(idx) then
                    __.bytes[index] <- __.bytes[index] &&& ~~~(1uy <<< (7 - offset))
                    __.count <- __.count - 1
    
    member __.Set(bytes: byte[]) =
        __.count <- 0
        for idx in 0..__.bytes.Length - 1 do
            if idx < bytes.Length then
                __.bytes[idx] <- bytes[idx]
                for offset in 0..7 do
                    if __.count < __.capacity then
                        __.count <- __.count + int ((__.bytes[idx] >>> (7 - offset)) &&& 1uy)
            else
                __.bytes[idx] <- 0uy
    
    member __.And(bitfield: Bitfield) = __
    
    member __.Or(bitfield: Bitfield) = __
    
    member __.Xor(bitfield: Bitfield) = __
    
    member __.Not() =
        for idx in 0..__.bytes.Length - 1 do
            __.bytes[idx] <- ~~~__.bytes[idx] 
        __.count <- __.capacity - __.count
        __
    
    member __.ToArray() = __.bytes.ToArray()
    
    override __.ToString() = String.Join("\n", [for byte in __.bytes do Convert.ToString(byte, 2).PadLeft(8, '0')])
    
    override __.GetHashCode() = __.bytes.Aggregate(0, (fun result byte -> (result * 31) ^^^ (int byte)))
    
    override __.Equals(other) =
        match other with
        | :? Bitfield as value ->
            Enumerable.SequenceEqual(__.bytes, value.ToArray())
        | _ -> false
    
    