namespace FBitTorrent.BEncode

open System.IO
open System.Text

module BDecode =
    open BValue
    
    let rec private read (reader: BStreamReader) (encoding: Encoding) =
        match BValueType.FromByte(byte (reader.PeekChar())) with
        | BIntegerType    -> readInt reader
        | BStringType     -> readStr reader encoding
        | BListType       -> readList reader encoding
        | BDictionaryType -> readDict reader encoding
    
    and private readInt reader =
        match BValueType.FromByte(reader.ReadByte()) with
        | BIntegerType ->
            let mutable s = 1L
            let mutable i = 0L
            let mutable p = char (reader.ReadByte())
            while (p <> 'e') do
                match p with  
                | '-' ->
                    s <- -1L
                | ptr when
                    ptr >= '0' &&
                    ptr <= '9' ->
                    i <- i * 10L + (int64 ((byte ptr) - (byte '0')))
                | ptr ->
                    failwith $"Unexpected char '%c{ptr}' when reading %A{BIntegerType}"
                p <- char (reader.ReadByte())
            match p with
            | 'e' ->
                bint (s * i)
            | _ ->
                failwith $"Invalid %A{BIntegerType} - missing 'e'"
        | _ -> 
            failwith $"Invalid encoded input - not a %A{BIntegerType}"
        
    and private readStr reader encoding =
        match BValueType.FromByte(byte (reader.PeekChar())) with
        | BStringType ->
            let mutable l = 0
            let mutable p = char (reader.ReadByte())
            while (p <> ':') do
                match p with  
                | ptr when
                    ptr >= '0' &&
                    ptr <= '9' ->
                    l <- l * 10 + (int32 ((byte ptr) - (byte '0')))
                | ptr ->
                    failwith $"Unexpected char '%A{char ptr}' when reading BString"
                p <- char (reader.ReadByte())
            match reader.ReadBytes(l) with
            | str when str.Length < l ->
                failwith "String length does not match byte count of encoded value"
            | str ->
                bstr (encoding.GetString(str))
        | _ -> 
            failwith $"Invalid encoded input - not a %A{BStringType}"
    
    and private readList reader encoding =
        match BValueType.FromByte(reader.ReadByte()) with
        | BListType ->
            let mutable l = []
            let mutable p = char (reader.PeekChar())
            while (p <> 'e') do
                l <- List.append l [read reader encoding]
                p <- char (reader.PeekChar())
            match char (reader.ReadByte()) with
            | 'e' -> 
                blist l
            | _ ->
                failwith $"Invalid %A{BListType} - missing 'e'"
        | _ -> 
            failwith $"Invalid encoded input - not a %A{BListType}"
        
    and private readDict reader encoding =
        match BValueType.FromByte(reader.ReadByte()) with
        | BDictionaryType ->
            let mutable d = Map.empty
            let mutable p = char (reader.PeekChar())
            while (p <> 'e') do
                let k = read reader encoding
                let v = read reader encoding
                d <- Map.add k v d
                p <- char (reader.PeekChar())
            match char (reader.ReadByte()) with
            | 'e' -> 
                bdict d
            | _ ->
                failwith $"Invalid %A{BDictionaryType} - missing 'e'"
        | _ -> 
            failwith $"Invalid encoded input - not a %A{BDictionaryType}"
    
    let fromStream encoding (stream: Stream) =
        let reader = new BStreamReader(stream, encoding)
        read reader encoding
    
    let fromBytes encoding (value: byte[]) =
        fromStream encoding (new MemoryStream(value))
        
    let fromString encoding (value: string) =
        fromBytes encoding (encoding.GetBytes(value))
        
    let defaultFromStream (value: Stream) =
        fromStream defaultEncoding value
        
    let defaultFromBytes (value: byte[]) =
        fromBytes defaultEncoding value
        
    let defaultFromString (value: string) =
        fromString defaultEncoding value