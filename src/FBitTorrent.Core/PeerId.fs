namespace FBitTorrent.Core

open System
open System.Linq
open System.Text

[<Struct; CustomEquality; NoComparison>]
type PeerId =
    val private bytes: byte[]
    new(value: byte[]) = { bytes = value }
    new(value: string) = { bytes = Encoding.ASCII.GetBytes(value) }

    member __.ToArray() = __.bytes.ToArray()
    
    override __.ToString() = Encoding.ASCII.GetString(__.bytes)
        
    override __.GetHashCode() = __.bytes.Aggregate(0, (fun result byte -> (result * 31) ^^^ (int byte)))
        
    override __.Equals(other: obj) =
        match other with
        | :? PeerId as other ->
            Enumerable.SequenceEqual(__.bytes, other.bytes)
        | _ -> false
        
module PeerId =
    let create () = PeerId($"-FX%.2f{Constants.AppVersion}-%012d{Random().NextInt64(0L, 1000000000000L)}")