namespace FBitTorrent.BEncode

open System.IO
open System.Text

module BEncode =
    open BValue
    
    let rec private write (writer: BStreamWriter) (encoding: Encoding) (value: BValue) =
        match value with
        | BInteger    _ -> writeInt writer value
        | BString     _ -> writeStr writer encoding value
        | BList       _ -> writeList writer encoding value
        | BDictionary _ -> writeDict writer encoding value 
    
    and private writeInt writer value =
        match value with
        | BInteger int -> 
            writer.Write(byte 'i')
            writer.Write(Encoding.ASCII.GetBytes(int.ToString()))
            writer.Write(byte 'e')
            writer.Flush()
        | _ ->
            failwith "Invalid value - not a BInteger"
            
    and private writeStr writer encoding value =
        match value with
        | BString str ->
            let bytes = encoding.GetBytes(str)
            writer.Write(Encoding.ASCII.GetBytes(bytes.Length.ToString())) 
            writer.Write(byte ':')
            writer.Write(bytes)
            writer.Flush()
        | _ ->
            failwith "Invalid value - not a BString"
    
    and private writeList writer encoding value =
        match value with
        | BList list -> 
            writer.Write(byte 'l')
            for value in list do
                write writer encoding value
            writer.Write(byte 'e')
            writer.Flush()
        | _ ->
            failwith "Invalid value - not a BList"
            
    and private writeDict writer encoding value =
        match value with
        | BDictionary dict -> 
            writer.Write(byte 'd')
            for KeyValue(k, v) in dict do
                write writer encoding k 
                write writer encoding v
            writer.Write(byte 'e')
            writer.Flush()
        | _ ->
            failwith "Invalid value - not a BDictionary"

    let toStream (encoding: Encoding) value =
        let stream = new MemoryStream()
        let writer = new BStreamWriter(stream, encoding)
        write writer encoding value
        stream :> Stream
    
    let toBytes (encoding: Encoding) value =
        use stream = toStream encoding value
        (stream :?> MemoryStream).ToArray()
        
    let toString (encoding: Encoding) value =
        encoding.GetString(toBytes encoding value)

    let defaultToStream value =
        toStream defaultEncoding value
            
    let defaultToBytes value =
        toBytes defaultEncoding value
        
    let defaultToString value =
        toString defaultEncoding value