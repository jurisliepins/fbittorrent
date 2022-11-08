namespace FBitTorrent.Core

open System
open System.Linq

module RateUnits =
    let bytesInKibiByte = 1024.0
    let bytesInMebiByte = bytesInKibiByte * 1024.0
    let bytesInGibiByte = bytesInMebiByte * 1024.0
    let bytesInTebiByte = bytesInGibiByte * 1024.0

[<Struct>]
type Rate =
    private Rate of double
with
    override __.ToString() =
         match __ with
         | Rate value when value < RateUnits.bytesInKibiByte -> $"%.2f{(value)} B/s"
         | Rate value when value < RateUnits.bytesInMebiByte -> $"%.2f{(value / RateUnits.bytesInKibiByte)} KiB/s"
         | Rate value when value < RateUnits.bytesInGibiByte -> $"%.2f{(value / RateUnits.bytesInMebiByte)} MiB/s"
         | Rate value when value < RateUnits.bytesInTebiByte -> $"%.2f{(value / RateUnits.bytesInGibiByte)} GiB/s"
         | Rate value                                        -> $"%.2f{(value / RateUnits.bytesInTebiByte)} TiB/s"

module Rate =
    let zero = Rate 0.0
    
    let toBytesPerSecond (Rate value: Rate) = value
    let toBytesPerMinute (Rate value: Rate) = value * 60.0
    let toBytesPerHour (Rate value: Rate) = value * 60.0 * 60.0
    
    let fromBytePerSecond (value: double) = Rate value
    let fromBytePerMinute (value: double) = Rate (value / 60.0)
    let fromBytePerHour (value: double) = Rate (value / 60.0 / 60.0)

[<Struct>]
type RateMeterEntry =
    private RateMeterEntry of int64 * DateTime

type RateMeter() =
    
    let [<Literal>] Smoothing = 0.02
    
    let [<Literal>] DefaultEntryCount = 6
    
    let entries = CircularBuffer<RateMeterEntry>(DefaultEntryCount)

    member private _.CalculateSpeeds() =
        Seq.zip entries (Seq.skip 1 entries)
        |> Seq.map (fun (RateMeterEntry (xBytes, xTimestamp),
                         RateMeterEntry (yBytes, yTimestamp)) ->
            (double (yBytes - xBytes)) / (double (yTimestamp - xTimestamp).TotalSeconds))
    
    member __.Average with get() =
        if entries.Count < 2 then
            Rate.zero
        else
            let speeds = __.CalculateSpeeds()
            Rate(Seq.average speeds)
    
    member __.AverageSmoothed with get() =
        if entries.Count < 2 then
            Rate.zero
        else
            let speeds = __.CalculateSpeeds()
            // Exponential moving average to smooth out the result.
            Rate ((Smoothing * (Seq.last speeds)) + ((1.0 - Smoothing) * (Seq.average speeds)))
    
    member _.Update(bytes: int64) =
        entries.Push(RateMeterEntry(bytes, DateTime.UtcNow))

module RateMeter =
    let create () = RateMeter()
    
    let createFromBytes (bytes: int64) =
        let rateMeter = RateMeter()
        rateMeter.Update(bytes)
        rateMeter
    
    let average (rateMeter: RateMeter) = rateMeter.Average
    
    let averageSmoothed (rateMeter: RateMeter) = rateMeter.AverageSmoothed
    
    let update (bytes: int64) (rateMeter: RateMeter) = rateMeter.Update(bytes)