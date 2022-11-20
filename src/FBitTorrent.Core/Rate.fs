namespace FBitTorrent.Core

module Rate =
    let [<Literal>] Smoothing = 0.02
    let [<Literal>] Capacity = 5

type Rate =
    val mutable private speeds: int64[]
    val mutable private byteCount: int64
    val mutable private pointer: int
    val mutable private length: int
    new(initialByteCount: int64) = {
        speeds = Array.create Rate.Capacity 0L
        byteCount = initialByteCount
        pointer = 0
        length = 0 }
    
    member __.Update(lastBytes: int64, per: int) =
        let idx = (__.pointer + __.length) % __.speeds.Length
        __.speeds[idx] <- (lastBytes - __.byteCount) / int64 per
        __.byteCount <- lastBytes
        if __.length >= __.speeds.Length then
            __.pointer <- __.pointer + 1
        else
            __.length <- __.length + 1

    member __.GetSpeed() =
        if __.length < 1 then
            double 0.0
        else
            let last = double (__.speeds[((__.pointer + __.length - 1) % __.speeds.Length)])
            let total = double (Array.sum __.speeds[0..(__.length - 1)])
            let count = double __.length
            let average = total / count
            // Exponential moving average to smooth out the result.
            double ((Rate.Smoothing * last) + ((1.0 - Rate.Smoothing) * average))

    override __.ToString() =
        let speed = __.GetSpeed()
        if   speed < (1024.0)                            then $"%.3f{(speed)} B/s"
        elif speed < (1024.0 * 1024.0)                   then $"%.3f{(speed / 1024.0)} KB/s"
        elif speed < (1024.0 * 1024.0 * 1024.0)          then $"%.3f{(speed / 1024.0 / 1024.0)} MB/s"
        elif speed < (1024.0 * 1024.0 * 1024.0 * 1024.0) then $"%.3f{(speed / 1024.0 / 1024.0 / 1024.0)} GB/s"
        else
            failwith $"Invalid speed value %.3f{speed}"
    
    override __.GetHashCode() = __.GetSpeed().GetHashCode()
    
    override __.Equals(other: obj) =
        match other with
        | :? Rate as other ->
            __.GetSpeed() = other.GetSpeed()
        | _ -> false        