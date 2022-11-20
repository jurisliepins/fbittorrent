namespace FBitTorrent.Core.Tests

open FBitTorrent.Core
open Xunit

module RateTests =
    
    [<Fact>]
    let ``Test rate should measure`` () =
        let rate = Rate 0
        rate.Update(1000, 1)
        Assert.Equal(1000.0, rate.GetSpeed())
        rate.Update(2500, 1)
        Assert.Equal(1255.0, rate.GetSpeed())
        rate.Update(3500, 1)
        Assert.Equal(1163.3333333333335, rate.GetSpeed())
        rate.Update(4750, 1)
        Assert.Equal(1188.75, rate.GetSpeed())
        rate.Update(5000, 1)
        Assert.Equal(985.0, rate.GetSpeed())
        rate.Update(5000, 1)
        Assert.Equal(784.0, rate.GetSpeed())
        rate.Update(5000, 1)
        Assert.Equal(490.0, rate.GetSpeed())
        rate.Update(5125, 1)
        Assert.Equal(321.0, rate.GetSpeed())
        rate.Update(5145, 1)
        Assert.Equal(77.82000000000001, rate.GetSpeed())
        rate.Update(5225, 1)
        Assert.Equal(45.7000, rate.GetSpeed())
        
    [<Fact>]
    let ``Test rate should format bytes`` () =
        let exp = "10.000 B/s"
        let act = Rate(0L)
        act.Update(10L, 1)                      
        Assert.Equal(exp, act.ToString())
        
    [<Fact>]
    let ``Test rate should format kilo-bytes`` () =
        let exp = "10.000 KB/s"
        let act = Rate(0L)
        act.Update(10L * 1024L, 1)
        Assert.Equal(exp, act.ToString())
        
    [<Fact>]
    let ``Test rate should format mega-bytes`` () =
        let exp = "10.000 MB/s"
        let act = Rate(0L)
        act.Update(10L * 1024L * 1024L, 1)
        Assert.Equal(exp, act.ToString())
        
    [<Fact>]
    let ``Test rate should format giga-bytes`` () =
        let exp = "10.000 GB/s"
        let act = Rate(0L)
        act.Update(10L * 1024L * 1024L * 1024L, 1)
        Assert.Equal(exp, act.ToString())
