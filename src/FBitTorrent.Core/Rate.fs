namespace FBitTorrent.Core

module Rate =
    let [<Literal>] Smoothing = 0.02
    let [<Literal>] Capacity = 5

    let KB = 1024.0
    let MB = 1024.0 * 1024.0
    let GB = 1024.0 * 1024.0 * 1024.0
    let TB = 1024.0 * 1024.0 * 1024.0 * 1024.0

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

    override __.ToString() =
        let speed = __.GetSpeed()
        if speed < Rate.KB then $"%.3f{speed} B/s"
        elif speed < Rate.MB then $"%.3f{(speed / Rate.KB)} KB/s"
        elif speed < Rate.GB then $"%.3f{(speed / Rate.MB)} MB/s"
        elif speed < Rate.TB then $"%.3f{(speed / Rate.GB)} GB/s"
        else
            failwith $"Invalid speed value %.3f{speed}"
    
    override __.GetHashCode() = __.GetSpeed().GetHashCode()
    
    override __.Equals(other: obj) =
        match other with
        | :? Rate as other ->
            __.GetSpeed() = other.GetSpeed()
        | _ -> false        