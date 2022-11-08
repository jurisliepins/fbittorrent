namespace FBitTorrent.BEncode

open System
open System.IO
open System.Text

type BStreamWriter(stream: Stream, encoding: Encoding) =
    inherit BinaryWriter(stream, encoding)

type BStreamReader(stream: Stream, encoding: Encoding) =
    inherit BinaryReader(stream, encoding)

[<Struct>]
type BValueType =
    | BIntegerType
    | BStringType
    | BListType
    | BDictionaryType
    override __.ToString() =
        match __ with
        | BIntegerType    -> "BInteger"
        | BStringType     -> "BString"
        | BListType       -> "BList"
        | BDictionaryType -> "BDictionary"
    
    static member FromByte(value: byte) =
        match char value with
        | 'i' -> BIntegerType
        | c when
            c >= '0' &&
            c <= '9' -> BStringType
        | 'l' -> BListType
        | 'd' -> BDictionaryType
        | c   ->
            failwith $"Invalid BValueType '%c{c}'"
            
[<Struct; CustomEquality; CustomComparison>]
type BValue =
    | BInteger    of Integer:    int64
    | BString     of String:     string
    | BList       of List:       BValue list
    | BDictionary of Dictionary: Map<BValue, BValue>
    override __.ToString() =
        match __ with
        | BInteger     int -> $"BInteger(%A{int})"
        | BString      str -> $"BString(%A{str})"
        | BList       list -> $"BList(%A{list})"
        | BDictionary dict -> $"BDictionary(%A{dict})"

    override __.GetHashCode() =
        match __ with
        | BInteger     int -> int.GetHashCode()
        | BString      str -> str.GetHashCode()
        | BList       list -> list.GetHashCode()
        | BDictionary dict -> dict.GetHashCode()

    override __.Equals other =
        match other with
        | :? BValue as value ->
            match (__, value) with
            | BInteger left, BInteger right -> left.Equals(right)
            | BString  left, BString  right -> left.Equals(right)
            | BList    left, BList    right ->
                let rec equals (x: BValue list) (y: BValue list) =
                     match x, y with
                     | [], [] -> true
                     | x::xs, y::ys ->
                           if x.Equals(y) then
                               equals xs ys
                           else
                               false
                     | _ -> false
                equals left right
            | BDictionary left, BDictionary right ->
                let equals (x: Map<BValue, BValue>) (y: Map<BValue, BValue>) =
                    if x.Count = y.Count then
                        Seq.forall
                            (fun (KeyValue(xk, xv)) ->
                                if y.ContainsKey(xk) then
                                    y[xk] = xv
                                else
                                    false) x
                    else
                        false
                equals left right
            | _ -> false
        | _ -> false
        
    interface IComparable with
        member __.CompareTo other =
            match other with
            | :? BValue as value ->
                match (__, value) with
                | BInteger left, BInteger right -> left.CompareTo(right)
                | BString  left, BString  right -> left.CompareTo(right)
                | BList       _, BList        _ -> failwith $"IComparable not supported for %A{BListType}"
                | BDictionary _, BDictionary  _ -> failwith $"IComparable not supported for %A{BDictionaryType}"
                | _ ->
                    failwith "Must compare BValues of same type"
            | _ ->
                failwith "Must compare objects of same type"

module BValue =
    
    let defaultEncoding = Encoding.Latin1
    
    let bint int = BInteger int
    
    let bstr str = BString str
    
    let blist list = BList list
    
    let bdict dict = BDictionary dict
    
    let int value =
        match value with
        | BInteger int -> int
        | _ ->
            failwith $"BValue must be a %A{BIntegerType}"
            
    let str value =
        match value with
        | BString str -> str
        | _ ->
            failwith $"BValue must be a %A{BStringType}"
    
    let list value =
        match value with
        | BList list -> list
        | _ ->
            failwith $"BValue must be a %A{BListType}"
        
    let dict value =
        match value with
        | BDictionary dict -> dict
        | _ ->
            failwith $"BValue must be a %A{BDictionaryType}"
    
    let intOpt value =
        match value with BInteger int -> Some int | _ -> None
            
    let strOpt value =
        match value with BString str -> Some str | _ -> None
    
    let listOpt value =
        match value with BList list -> Some list | _ -> None
    
    let dictOpt value =
        match value with BDictionary dict -> Some dict | _ -> None
    
    let unpack key (table: Map<BValue, BValue>) =
        table[bstr key]
    
    let unpackWith key map (table: Map<BValue, BValue>) =
        table[bstr key] |> map
    
    let unpackByte key (table: Map<BValue, BValue>) =
        table[bstr key] |> int |> Operators.byte
    
    let unpackInt16 key (table: Map<BValue, BValue>) =
        table[bstr key] |> int |> Operators.int16
       
    let unpackInt32 key (table: Map<BValue, BValue>) =
        table[bstr key] |> int |> Operators.int32
        
    let unpackInt64 key (table: Map<BValue, BValue>) =
        table[bstr key] |> int |> Operators.int64
        
    let unpackStr key (table: Map<BValue, BValue>) =
        table[bstr key] |> str
        
    let unpackList key (table: Map<BValue, BValue>) =
        table[bstr key] |> list
        
    let unpackDict key (table: Map<BValue, BValue>) =
        table[bstr key] |> dict
    
    let unpackOpt key (table: Map<BValue, BValue>) =
        if table.ContainsKey(bstr key) then Some(table |> unpack key) else None
    
    let unpackOptWith key map (table: Map<BValue, BValue>) =
        if table.ContainsKey(bstr key) then Some(table |> unpack key |> map) else None
        
    let unpackIntOpt key (table: Map<BValue, BValue>) =
        if table.ContainsKey(bstr key) then Some(table |> unpack key |> int) else None
        
    let unpackStrOpt key (table: Map<BValue, BValue>) =
        if table.ContainsKey(bstr key) then Some(table |> unpack key |> str) else None
        
    let unpackListOpt key (table: Map<BValue, BValue>) =
        if table.ContainsKey(bstr key) then Some(table |> unpack key |> list) else None
        
    let unpackDictOpt key (table: Map<BValue, BValue>) =
        if table.ContainsKey(bstr key) then Some(table |> unpack key |> dict) else None
        
    let pack key value (table: Map<BValue, BValue>) =
        table.Add(bstr key, value)
        
    let packWith key value map (table: Map<BValue, BValue>) =
        table.Add(bstr key, value |> map)
    
    let packByte key (value: byte) (table: Map<BValue, BValue>) =
        table.Add(bstr key, value |> int64 |> bint)
    
    let packInt16 key (value: int16) (table: Map<BValue, BValue>) =
        table.Add(bstr key, value |> int64 |> bint)
        
    let packInt32 key (value: int32) (table: Map<BValue, BValue>) =
        table.Add(bstr key, value |> int64 |> bint)
        
    let packInt64 key (value: int64) (table: Map<BValue, BValue>) =
        table.Add(bstr key, value |> bint)
        
    let packStr key value (table: Map<BValue, BValue>) =
        table.Add(bstr key, value |> bstr)
        
    let packList key value (table: Map<BValue, BValue>) =
        table.Add(bstr key, value |> blist)
        
    let packDict key value (table: Map<BValue, BValue>) =
        table.Add(bstr key, value |> bdict)
        
    let packOpt key value (table: Map<BValue, BValue>) =
        match value with Some value -> table.Add(bstr key, value) | None -> table
        
    let packOptWith key value map (table: Map<BValue, BValue>) =
        match value with Some value -> table.Add(bstr key, value |> map) | None -> table
        
    let packIntOpt key value (table: Map<BValue, BValue>) =
        match value with Some value -> table.Add(bstr key, value |> bint) | None -> table
        
    let packStrOpt key value (table: Map<BValue, BValue>) =
        match value with Some value -> table.Add(bstr key, value |> bstr) | None -> table
        
    let packListOpt key value (table: Map<BValue, BValue>) =
        match value with Some value -> table.Add(bstr key, value |> blist) | None -> table
        
    let packDictOpt key value (table: Map<BValue, BValue>) =
        match value with Some value -> table.Add(bstr key, value |> bdict) | None -> table