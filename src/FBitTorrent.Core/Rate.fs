namespace FBitTorrent.Core

module Rate =
    let [<Literal>] Smoothing = 0.02
    let [<Literal>] Capacity = 5

type Rate() =
    member val private Speeds = Array.create Rate.Capacity 0L with get 
    member val private ByteCount = 0L with get, set
    member val private Head = 0 with get, set
    member val private Length = 0 with get, set
    
    member __.Update(byteCount: int64) =
        let idx = (__.Head + __.Length) % __.Speeds.Length
        __.Speeds[idx] <- (byteCount - __.ByteCount)
        __.ByteCount <- byteCount
        if __.Length >= __.Speeds.Length then
            __.Head <- __.Head + 1
        else
            __.Length <- __.Length + 1

    member __.GetSpeed() =
        if __.Length < 1 then
            double 0.0
        else
            let last = double (__.Speeds[((__.Head + __.Length - 1) % __.Speeds.Length)])
            let total = double (Array.sum __.Speeds[0..(__.Length - 1)])
            let count = double __.Length
            let average = total / count
            // Exponential moving average to smooth out the result.
            double ((Rate.Smoothing * last) + ((1.0 - Rate.Smoothing) * average))