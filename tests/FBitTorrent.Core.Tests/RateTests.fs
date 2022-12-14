namespace FBitTorrent.Core.Tests

open System.Linq
open System.Collections.Generic
open FBitTorrent.Core
open Xunit

module RateTests =
    
    [<Fact>]
    let ``Test circular buffer should push and enumerate`` () =
        let buffer = CircularBuffer<int>(5)
        Assert.Equal(0L, buffer.LongCount())
        let enumerator1 = (buffer :> IEnumerable<int>).GetEnumerator()
        Assert.False(enumerator1.MoveNext())
        buffer.Push(1)
        Assert.Equal(1L, buffer.LongCount())
        buffer.Push(2)
        Assert.Equal(2L, buffer.LongCount())
        buffer.Push(3)
        Assert.Equal(3L, buffer.LongCount())
        buffer.Push(4)
        Assert.Equal(4L, buffer.LongCount())
        buffer.Push(5)
        Assert.Equal(5L, buffer.LongCount())
        let enumerator2 = (buffer :> IEnumerable<int>).GetEnumerator()
        Assert.True(enumerator2.MoveNext())
        Assert.Equal(1, enumerator2.Current)
        Assert.True(enumerator2.MoveNext())
        Assert.Equal(2, enumerator2.Current)
        Assert.True(enumerator2.MoveNext())
        Assert.Equal(3, enumerator2.Current)
        Assert.True(enumerator2.MoveNext())
        Assert.Equal(4, enumerator2.Current)
        Assert.True(enumerator2.MoveNext())
        Assert.Equal(5, enumerator2.Current)
        buffer.Push(6)
        Assert.Equal(5L, buffer.LongCount())
        buffer.Push(7)
        Assert.Equal(5L, buffer.LongCount())
        buffer.Push(8)
        Assert.Equal(5L, buffer.LongCount())
        buffer.Push(9)
        Assert.Equal(5L, buffer.LongCount())
        buffer.Push(10)
        Assert.Equal(5L, buffer.LongCount())
        let enumerator3 = (buffer :> IEnumerable<int>).GetEnumerator()
        Assert.True(enumerator3.MoveNext())
        Assert.Equal(6, enumerator3.Current)
        Assert.True(enumerator3.MoveNext())
        Assert.Equal(7, enumerator3.Current)
        Assert.True(enumerator3.MoveNext())
        Assert.Equal(8, enumerator3.Current)
        Assert.True(enumerator3.MoveNext())
        Assert.Equal(9, enumerator3.Current)
        Assert.True(enumerator3.MoveNext())
        Assert.Equal(10, enumerator3.Current)
    
    [<Fact>]
    let ``Test rate should measure`` () =
        let rate = Rate()
        rate.Update(1000)
        Assert.Equal(1000.0, rate.GetSpeed())
        rate.Update(2500)
        Assert.Equal(1255.0, rate.GetSpeed())
        rate.Update(3500)
        Assert.Equal(1163.3333333333335, rate.GetSpeed())
        rate.Update(4750)
        Assert.Equal(1188.75, rate.GetSpeed())
        rate.Update(5000)
        Assert.Equal(985.0, rate.GetSpeed())
        rate.Update(5000)
        Assert.Equal(784.0, rate.GetSpeed())
        rate.Update(5000)
        Assert.Equal(490.0, rate.GetSpeed())
        rate.Update(5125)
        Assert.Equal(321.0, rate.GetSpeed())
        rate.Update(5145)
        Assert.Equal(77.82000000000001, rate.GetSpeed())
        rate.Update(5225)
        Assert.Equal(45.7000, rate.GetSpeed())