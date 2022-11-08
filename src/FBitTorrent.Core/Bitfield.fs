namespace FBitTorrent.Core

open System
open System.Linq

type Bitfield =
    val mutable private bytes: byte[]
    val mutable private capacity: int
    val mutable private count: int
    new(capacity: int) =
        { bytes    = Array.create (if (capacity % 8) <> 0 then ((capacity / 8) + 1) else (capacity / 8)) 0uy
          capacity = capacity
          count    = 0 }
    new(bitfield: Bitfield) =
        { bytes    = bitfield.bytes.ToArray()
          capacity = bitfield.capacity
          count    = bitfield.count }
    
    member __.Capacity with get() : int = __.capacity
    member __.Count with get() : int = __.count
    member __.IsFull with get() : bool = __.count = __.capacity
    member __.IsEmpty with get() : bool = __.count = 0 
    
    member __.GetBit(index: int) =
        if index < 0 || index >= __.capacity then
            false
        else
            let idx = index / 8
            let pos = index % 8
            ((__.bytes[idx] >>> (7 - pos)) &&& 1uy) <> 0uy
   
    member __.SetBit(index: int, value: bool) =
        if index >= 0 && index < __.capacity then
            let idx = index / 8
            let pos = index % 8
            if value then
                if not (__.GetBit(index)) then
                    __.bytes[idx] <- __.bytes[idx] ||| (1uy <<< (7 - pos))
                    __.count <- __.count + 1
            else
                if __.GetBit(index) then
                    __.bytes[idx] <- __.bytes[idx] &&& ~~~(1uy <<< (7 - pos))
                    __.count <- __.count - 1
    
    member __.SetBytes(bytes: byte[]) =
        __.count <- 0
        for idx in 0..__.bytes.Length - 1 do
            if idx < bytes.Length then
                __.bytes[idx] <- bytes[idx]
                for offset in 0..7 do
                    if __.count < __.capacity then
                        __.count <- __.count + int ((__.bytes[idx] >>> (7 - offset)) &&& 1uy)
            else
                __.bytes[idx] <- 0uy
    
    static member AndBits(bitfield: Bitfield, andWith: Bitfield) = failwith "AND not supported"

    static member OrBits(bitfield: Bitfield, orWith: Bitfield) = failwith "OR not supported"

    static member XorBits(bitfield: Bitfield, xorWith: Bitfield) = failwith "XOR not supported"

    static member NotBits(bitfield: Bitfield) =
        let updated = Bitfield(bitfield.Capacity)
        updated.SetBytes(bitfield.bytes |> Array.map (fun byte -> ~~~byte))
        updated
     
    member __.ToArray() = __.bytes.ToArray()
    
    override __.ToString() =
        let lines = __.bytes
                          .Select(fun byte -> Convert.ToString(byte, 2).PadLeft(8, '0'))
                          .Chunk(4)
                          .Select(fun chunk -> String.Join(" ", chunk))
        if lines.Count() > 4 then 
            String.Join("\n", lines.Take(4).Append("..."))
        else
            String.Join("\n", lines)
    
    override __.GetHashCode() =
        __.bytes.Aggregate(0, (fun result byte -> (result * 31) ^^^ (int byte)))
    
    override __.Equals(other) =
        match other with
        | :? Bitfield as value ->
            Enumerable.SequenceEqual(__.bytes, value.bytes)
        | _ -> false

module Bitfield =
    let create (capacity: int) = Bitfield(capacity)
        
    let createFromBitfield (bitfield: Bitfield) = Bitfield(bitfield)
        
    let capacity (bitfield: Bitfield) = bitfield.Capacity

    let count (bitfield: Bitfield) = bitfield.Count
    
    let isEmpty (bitfield: Bitfield) = bitfield.IsEmpty
    
    let isFull (bitfield: Bitfield) = bitfield.IsFull

    let getBit (index: int) (bitfield: Bitfield) = bitfield.GetBit(index)
     
    let setBit (index: int) (value: bool) (bitfield: Bitfield) = bitfield.SetBit(index, value)
            
    let setBytes (bytes: byte[]) (bitfield: Bitfield) = bitfield.SetBytes(bytes)
    
    let andBits (andWith: Bitfield) (bitfield: Bitfield) = Bitfield.AndBits(bitfield, andWith)
        
    let orBits (orWith: Bitfield) (bitfield: Bitfield) = Bitfield.OrBits(bitfield, orWith)
        
    let xorBits (xorWith: Bitfield) (bitfield: Bitfield) = Bitfield.XorBits(bitfield, xorWith)

    let notBits (bitfield: Bitfield) = Bitfield.NotBits(bitfield)

type BitfieldSelector(capacity: int) =
    member val private Bitfields = Array.create capacity 0

    member __.Add(bitfield: Bitfield) =
        let rec add (idx: int) =
            if idx >= 0 && idx < __.Bitfields.Length then
                if bitfield |> Bitfield.getBit idx then
                    __.Bitfields[idx] <- if __.Bitfields[idx] = Int32.MaxValue then __.Bitfields[idx] else __.Bitfields[idx] + 1
                add (idx + 1)
        add 0
        
    member __.Add(idx: int) =
        if idx >= 0 && idx < __.Bitfields.Length then
            __.Bitfields[idx] <- if __.Bitfields[idx] = Int32.MaxValue then __.Bitfields[idx] else __.Bitfields[idx] + 1
            
    member __.Subtract(bitfield: Bitfield) =
        let rec subtract (idx: int) =
            if idx >= 0 && idx < __.Bitfields.Length then
                if bitfield |> Bitfield.getBit idx then
                    __.Bitfields[idx] <- if __.Bitfields[idx] = 0 then __.Bitfields[idx] else __.Bitfields[idx] - 1
                subtract (idx + 1)
        subtract 0
        
    member __.Subtract(idx: int) =
        if idx >= 0 && idx < __.Bitfields.Length then
            __.Bitfields[idx] <- if __.Bitfields[idx] = 0 then __.Bitfields[idx] else __.Bitfields[idx] - 1

    member __.FirstBit(selfBitfield: Bitfield, peerBitfield: Bitfield) =
        let rec findFirst (idx: int) (result: int) = 
            if idx >= __.Bitfields.Length then
                if result = -1 then None else Some result
            else
                let selfHas = selfBitfield |> Bitfield.getBit idx
                let peerHas = peerBitfield |> Bitfield.getBit idx
                if selfHas && peerHas then 
                    Some idx
                else
                    findFirst (idx + 1) result
        findFirst 0 -1
        
    member __.RarestBit(selfBitfield: Bitfield, peerBitfield: Bitfield) =
        // TODO: We need to randomise which bit we pick if there multiple bits of equal rarity. More details - https://blog.libtorrent.org/2011/11/writing-a-fast-piece-picker/  
        let rec findRarest (idx: int) (result: int) =
            if idx >= __.Bitfields.Length then
                if result = -1 then None else Some result
            else
                let selfHas = selfBitfield |> Bitfield.getBit idx
                let peerHas = peerBitfield |> Bitfield.getBit idx
                if selfHas && peerHas then
                    if   result <> -1 && __.Bitfields[idx] < __.Bitfields[result] then findRarest (idx + 1) idx
                    elif result <> -1 then findRarest (idx + 1) result
                    else findRarest (idx + 1) idx
                else
                    findRarest (idx + 1) result
        findRarest 0 -1
        
    member __.ToArray() = __.Bitfields.ToArray()

module BitfieldSelector =
     let create (capacity: int) = BitfieldSelector(capacity)
     
     let addBitfield (bitfield: Bitfield) (selector: BitfieldSelector) = selector.Add(bitfield)
     
     let addBit (idx: int) (selector: BitfieldSelector) = selector.Add(idx)
     
     let subtractBitfield (bitfield: Bitfield) (selector: BitfieldSelector) = selector.Subtract(bitfield)
     
     let subtractBit (idx: int) (selector: BitfieldSelector) = selector.Subtract(idx)

     let firstBit (selfBitfield: Bitfield) (peerBitfield: Bitfield) (selector: BitfieldSelector) = selector.FirstBit(selfBitfield, peerBitfield)

     let rarestBit (selfBitfield: Bitfield) (peerBitfield: Bitfield) (selector: BitfieldSelector) = selector.RarestBit(selfBitfield, peerBitfield)
