namespace FBitTorrent.Core.Tests

open System.Threading
open FBitTorrent.Core
open Xunit

module RateTests =
    
    [<Fact>]
    let ``Test rate should to string`` () =
        let rateMeter = RateMeter.createFromBytes 0
        Thread.Sleep(100)
        rateMeter |> RateMeter.update 1L
        let speed = rateMeter.AverageSmoothed.ToString()
        Assert.True(speed.Contains("B/s"))
        let rateMeter = RateMeter.createFromBytes 0
        Thread.Sleep(100)
        rateMeter |> RateMeter.update (2L * 1024L)
        let speed = rateMeter.AverageSmoothed.ToString()
        Assert.True(speed.Contains("KiB/s"))
        let rateMeter = RateMeter.createFromBytes 0
        Thread.Sleep(100)
        rateMeter |> RateMeter.update (2L * 1024L * 1024L)
        let speed = rateMeter.AverageSmoothed.ToString()
        Assert.True(speed.Contains("MiB/s"))
        let rateMeter = RateMeter.createFromBytes 0
        Thread.Sleep(100)
        rateMeter |> RateMeter.update (2L * 1024L * 1024L * 1024L)
        let speed = rateMeter.AverageSmoothed.ToString()
        Assert.True(speed.Contains("GiB/s"))
        let rateMeter = RateMeter.createFromBytes 0
        Thread.Sleep(100)
        rateMeter |> RateMeter.update (2L * 1024L * 1024L * 1024L * 1024L)
        let speed = rateMeter.AverageSmoothed.ToString()
        Assert.True(speed.Contains("TiB/s"))
        
module RateMeterTests =
     let [<Literal>] BytesPerSecondLowerBound = 200.0
     let [<Literal>] BytesPerSecondUpperBound = 200.0
     
     let assertBytesPerSecondRange (expected: Rate) (actual: Rate) =
         let lower = Rate.toBytesPerSecond expected - BytesPerSecondLowerBound
         let upper = Rate.toBytesPerSecond expected + BytesPerSecondUpperBound
         Assert.InRange(Rate.toBytesPerSecond actual, lower, upper)
         
     let assertBytesPerMinuteRange (expected: Rate) (actual: Rate) =
         let lower = Rate.toBytesPerMinute expected - (BytesPerSecondLowerBound * 60.0)
         let upper = Rate.toBytesPerMinute expected + (BytesPerSecondLowerBound * 60.0)
         Assert.InRange(Rate.toBytesPerMinute actual, lower, upper)
         
     let assertBytesPerHourRange (expected: Rate) (actual: Rate) =
         let lower = Rate.toBytesPerHour expected - (BytesPerSecondLowerBound * 60.0 * 60.0)
         let upper = Rate.toBytesPerHour expected + (BytesPerSecondLowerBound * 60.0 * 60.0)
         Assert.InRange(Rate.toBytesPerHour actual, lower, upper)
     
     let assertBytesPerRange (expected: Rate) (actual: Rate) =
         assertBytesPerSecondRange expected actual
         assertBytesPerMinuteRange expected actual
         assertBytesPerHourRange expected actual
     
     [<Fact>]
     let ``Test rate meter should measure average speed`` () =
         let rateMeter = RateMeter.createFromBytes 0
         let bytes = 1000L
         Thread.Sleep(1000 / 10)
         rateMeter |> RateMeter.update (bytes / 10L)
         // (1000) / 1 = ~1000
         assertBytesPerRange (Rate.fromBytePerSecond 1000.0) (rateMeter |> RateMeter.average)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 1500L
         rateMeter |> RateMeter.update (bytes / 10L)
         // (1000 + 1500) / 2 = ~1750
         assertBytesPerRange (Rate.fromBytePerSecond 1250.0) (rateMeter |> RateMeter.average)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 1000L
         rateMeter |> RateMeter.update (bytes / 10L)
         // (1000 + 1500 + 1000) / 3 = ~1170
         assertBytesPerRange (Rate.fromBytePerSecond 1170.0) (rateMeter |> RateMeter.average)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 1250L
         rateMeter |> RateMeter.update (bytes / 10L)
         // (1000 + 1500 + 1000 + 1250) / 4 = ~1190
         assertBytesPerRange (Rate.fromBytePerSecond 1190.0) (rateMeter |> RateMeter.average)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 250L
         rateMeter |> RateMeter.update (bytes / 10L)
         // (1000 + 1500 + 1000 + 1250 + 250) / 5 = ~1000
         assertBytesPerRange (Rate.fromBytePerSecond 1000.0) (rateMeter |> RateMeter.average)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 0L
         rateMeter |> RateMeter.update (bytes / 10L)
         // (1500 + 1000 + 1250 + 250 + 0) / 5 = ~800
         assertBytesPerRange (Rate.fromBytePerSecond 800.0) (rateMeter |> RateMeter.average)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 0L
         rateMeter |> RateMeter.update (bytes / 10L)
         // (1000 + 1250 + 250 + 0 + 0) / 5 = ~500
         assertBytesPerRange (Rate.fromBytePerSecond 500.0) (rateMeter |> RateMeter.average)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 125L
         rateMeter |> RateMeter.update (bytes / 10L)
         // (1250 + 250 + 0 + 0 + 125) / 5 = ~325
         assertBytesPerRange (Rate.fromBytePerSecond 325.0) (rateMeter |> RateMeter.average)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 20L
         rateMeter |> RateMeter.update (bytes / 10L)
         // (250 + 0 + 0 + 125 + 20) / 5 = ~80
         assertBytesPerRange (Rate.fromBytePerSecond 80.0) (rateMeter |> RateMeter.average)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 80L
         rateMeter |> RateMeter.update (bytes / 10L)
         // (0 + 0 + 125 + 20 + 80) / 5 = ~45
         assertBytesPerRange (Rate.fromBytePerSecond 45.0) (rateMeter |> RateMeter.average)
     
     [<Fact>]
     let ``Test rate meter should measure average smoothed speed`` () =
         let rateMeter = RateMeter.createFromBytes 0
         let bytes = 1000L
         Thread.Sleep(1000 / 10)
         rateMeter |> RateMeter.update (bytes / 10L)
         assertBytesPerRange (Rate.fromBytePerSecond 1000.0) (rateMeter |> RateMeter.averageSmoothed)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 1500L
         rateMeter |> RateMeter.update (bytes / 10L)
         assertBytesPerRange (Rate.fromBytePerSecond 1000.0) (rateMeter |> RateMeter.averageSmoothed)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 1000L
         rateMeter |> RateMeter.update (bytes / 10L)
         assertBytesPerRange (Rate.fromBytePerSecond 1040.0) (rateMeter |> RateMeter.averageSmoothed)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 1250L
         rateMeter |> RateMeter.update (bytes / 10L)
         assertBytesPerRange (Rate.fromBytePerSecond 1080.0) (rateMeter |> RateMeter.averageSmoothed)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 250L
         rateMeter |> RateMeter.update (bytes / 10L)
         assertBytesPerRange (Rate.fromBytePerSecond 890.0) (rateMeter |> RateMeter.averageSmoothed)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 0L
         rateMeter |> RateMeter.update (bytes / 10L)
         assertBytesPerRange (Rate.fromBytePerSecond 700.0) (rateMeter |> RateMeter.averageSmoothed)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 0L
         rateMeter |> RateMeter.update (bytes / 10L)
         assertBytesPerRange (Rate.fromBytePerSecond 500.0) (rateMeter |> RateMeter.averageSmoothed)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 125L
         rateMeter |> RateMeter.update (bytes / 10L)
         assertBytesPerRange (Rate.fromBytePerSecond 325.0) (rateMeter |> RateMeter.averageSmoothed)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 20L
         rateMeter |> RateMeter.update (bytes / 10L)
         assertBytesPerRange (Rate.fromBytePerSecond 80.0) (rateMeter |> RateMeter.averageSmoothed)
         Thread.Sleep(1000 / 10)
         let bytes = bytes + 80L
         rateMeter |> RateMeter.update (bytes / 10L)
         assertBytesPerRange (Rate.fromBytePerSecond 45.0) (rateMeter |> RateMeter.averageSmoothed)
         
     [<Fact>]
     let ``Test rate meter should measure speed when empty`` () =
         let rateMeter = RateMeter.create ()
         Assert.Equal(Rate.zero, rateMeter.Average)
         Assert.Equal(Rate.zero, rateMeter.AverageSmoothed)
         
     [<Fact>]
     let ``Test rate meter should measure speed when no time delta`` () =
         let rateMeter = RateMeter.create ()
         rateMeter |> RateMeter.update 1000
