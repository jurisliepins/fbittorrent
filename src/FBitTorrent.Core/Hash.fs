namespace FBitTorrent.Core

open System
open System.Linq
open System.Globalization

[<Struct; CustomEquality; NoComparison>]
type Hash =
    val private bytes: byte[]
    new(value: byte[]) = { bytes = value }
    new(value: string) =
        { bytes = [0..(value.Length - 1)]
                      .Where(fun idx -> idx % 2 = 0)
                      .Select(fun idx ->
                          Byte.Parse(value.Substring(idx, 2), NumberStyles.AllowHexSpecifier))
                      .ToArray() }

    member __.ToArray() = __.bytes.ToArray()
    
    override __.ToString() = String.Concat(__.bytes.Select(fun byte -> byte.ToString("x2")))
        
    override __.GetHashCode() = __.bytes.Aggregate(0, (fun result byte -> (result * 31) ^^^ (int byte)))
        
    override __.Equals(other: obj) =
        match other with
        | :? Hash as other ->
            Enumerable.SequenceEqual(__.bytes, other.bytes)
        | _ -> false