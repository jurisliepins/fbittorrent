namespace FBitTorrent.Core

open System
open System.Collections.ObjectModel
open System.IO
open System.Linq
open System.Security.Cryptography
open System.Text
open FBitTorrent.BEncode
open FBitTorrent.Core

type File =
    { Length: int64
      MD5Sum: string option
      Path:   string list }

type SingleFileInfo =
    { PieceLength: int
      Pieces:      ReadOnlyCollection<Hash>
      Private:     bool option
      Name:        string
      Length:      int64
      MD5Sum:      string option }

type MultiFileInfo =
    { PieceLength: int
      Pieces:      ReadOnlyCollection<Hash>
      Private:     bool option
      Name:        string
      Files:       File list }

type Info =
    | SingleFileInfo of SingleFileInfo
    | MultiFileInfo  of MultiFileInfo

type MetaInfo =
    { Info:         Info
      Announce:     string
      AnnounceList: string list list option
      CreationDate: DateTimeOffset option
      Comment:      string option
      CreatedBy:    string option
      Encoding:     string option }

module MetaInfo =

    let unpackPieceLength table =
        table
        |> BValue.unpackInt32 "piece length"
        
    let unpackPrivate table =
        let map value =
            BValue.int value = 1L
        table
        |> BValue.unpackOptWith "private" map
    
    let unpackPieces table =
        let map value =
            Encoding.Latin1
                .GetBytes(BValue.str value)
                .Chunk(20)
                .Select(fun bytes -> Hash bytes)
                .ToArray()
            |> Array.AsReadOnly
        table
        |> BValue.unpackWith "pieces" map
    
    let unpackName table =
        table
        |> BValue.unpackStr "name"
        |> Encoding.Latin1.GetBytes
        |> Encoding.UTF8.GetString
    
    let unpackLength table =
        table
        |> BValue.unpackInt64 "length"
    
    let unpackMD5SumSum table =
        table
        |> BValue.unpackStrOpt "md5sum"
    
    let unpackPath table =
        let map value =
            BValue.list value
            |> List.map (fun value ->
                            BValue.str value
                            |> Encoding.Latin1.GetBytes
                            |> Encoding.UTF8.GetString)
        table
        |> BValue.unpackWith "path" map
    
    let unpackFiles table files =
        files
        |> List.map (fun value ->
                        { Length = unpackLength (BValue.dict value)
                          MD5Sum = unpackMD5SumSum (BValue.dict value)
                          Path   = unpackPath (BValue.dict value) })

    let unpackSingleFileInfo table =
        SingleFileInfo
            { PieceLength = unpackPieceLength table
              Private     = unpackPrivate table 
              Pieces      = unpackPieces table
              Name        = unpackName table
              Length      = unpackLength table
              MD5Sum      = unpackMD5SumSum table }
    
    let unpackMultiFileInfo table files =
        MultiFileInfo 
            { PieceLength = unpackPieceLength table 
              Private     = unpackPrivate table
              Pieces      = unpackPieces table
              Name        = unpackName table
              Files       = unpackFiles table files }
            
    let unpackAnnounce table =
        table
        |> BValue.unpackStr "announce"
        |> Encoding.Latin1.GetBytes
        |> Encoding.UTF8.GetString
    
    let unpackAnnounceList table =
        let mapList2 value = 
            BValue.list value
            |> List.map (fun value ->
                             value
                             |> BValue.str
                             |> Encoding.Latin1.GetBytes
                             |> Encoding.UTF8.GetString)
        let mapList1 value =
            BValue.list value
            |> List.map mapList2
        let map =
            mapList1                                    
        table
        |> BValue.unpackOptWith "announce-list" map
    
    let unpackCreationDate table =
        let map value =
            DateTimeOffset.FromUnixTimeSeconds(BValue.int value)
        table
        |> BValue.unpackOptWith "creation date" map
    
    let unpackComment table =
        let map value =
            value
            |> BValue.str
            |> Encoding.Latin1.GetBytes
            |> Encoding.UTF8.GetString
        table
        |> BValue.unpackOptWith "comment" map
    
    let unpackCreatedBy table =
        let map value =
            value
            |> BValue.str
            |> Encoding.Latin1.GetBytes
            |> Encoding.UTF8.GetString
        table
        |> BValue.unpackOptWith "created by" map
    
    let unpackEncoding table =
        let map value =
            value
            |> BValue.str
            |> Encoding.Latin1.GetBytes
            |> Encoding.UTF8.GetString
        table
        |> BValue.unpackOptWith "encoding" map
        
    let unpackMetaInfo table =
        match table |> BValue.unpackDictOpt "info" with
        | Some info ->
            match info |> BValue.unpackListOpt "files" with
            | None ->
                { Info         = unpackSingleFileInfo info
                  Announce     = unpackAnnounce table
                  AnnounceList = unpackAnnounceList table
                  CreationDate = unpackCreationDate table
                  Comment      = unpackComment table
                  CreatedBy    = unpackCreatedBy table
                  Encoding     = unpackEncoding table }
            | Some files ->
                { Info         = unpackMultiFileInfo info files
                  Announce     = unpackAnnounce table
                  AnnounceList = unpackAnnounceList table
                  CreationDate = unpackCreationDate table
                  Comment      = unpackComment table
                  CreatedBy    = unpackCreatedBy table
                  Encoding     = unpackEncoding table }
        | None ->
            failwith "Invalid MetaInfo - missing 'info' dictionary"
    
    let packPieceLength value table =
        table
        |> BValue.packInt32 "piece length" value
    
    let packPrivate (value: bool option) table =
        let value =
            if value.IsSome then
                Some (if value.Value then 1L else 0L)
            else
                None
        table
        |> BValue.packOptWith "private" value BValue.bint
    
    let packPieces (value: ReadOnlyCollection<Hash>) table =
        let value =
            Encoding.Latin1.GetString(value
            |> Array.ofSeq
            |> Array.map (fun piece -> piece.ToArray())
            |> Array.concat)
        table
        |> BValue.packStr "pieces" value
        
    let packName (value: string) table =
        let value =
            value
            |> Encoding.UTF8.GetBytes
            |> Encoding.Latin1.GetString
        table
        |> BValue.packStr "name" value 

    let packLength value table =
        table
        |> BValue.packInt64 "length" value
    
    let packMD5Sum value table =
        table
        |> BValue.packStrOpt "md5sum" value
        
    let packPath (value: string list) table =
        let value =
            value
            |> List.map (fun value ->
                             value
                             |> Encoding.UTF8.GetBytes
                             |> Encoding.Latin1.GetString
                             |> BValue.bstr)
        table
        |> BValue.packList "path" value

    let packFiles (value: File list) table =
        let value =
            value
            |> List.map (fun file ->
                            Map.empty
                            |> packLength file.Length
                            |> packPath file.Path
                            |> packMD5Sum file.MD5Sum
                            |> BValue.bdict)
        table
        |> BValue.packList "files" value
    
    let packSingleFileInfo (value: SingleFileInfo) table =
        let value =
            Map.empty
            |> packPieceLength value.PieceLength
            |> packPieces value.Pieces
            |> packName value.Name
            |> packLength value.Length
            |> packPrivate value.Private
            |> packMD5Sum value.MD5Sum
        table
        |> BValue.packDict "info" value
        
    let packMultiFileInfo (value: MultiFileInfo) table =
        let value =
            Map.empty
            |> packPieceLength value.PieceLength
            |> packPieces value.Pieces
            |> packFiles value.Files
            |> packName value.Name
            |> packPrivate value.Private
        table
        |> BValue.packDict "info" value
    
    let packAnnounce (value: string) table =
        let value =
            value
            |> Encoding.UTF8.GetBytes
            |> Encoding.Latin1.GetString
        table
        |> BValue.packStr "announce" value
    
    let packAnnounceList (value: string list list option) table =
        let mapList2 value =
            (BValue.blist (value
                           |> List.map (fun (value: string) ->
                               BValue.bstr (value
                                            |> Encoding.UTF8.GetBytes
                                            |> Encoding.Latin1.GetString))))
        let mapList1 value =
            BValue.blist (value |> List.map mapList2)
        let map =
            mapList1
        table
        |> BValue.packOptWith "announce-list" value map

    let packCreationDate (value: DateTimeOffset option) table =
        let map (value: DateTimeOffset) =
            BValue.bint (value.ToUnixTimeSeconds())
        table
        |> BValue.packOptWith "creation date" value map

    let packComment (value: string option) table =
        let map (value: string) =
            value
            |> Encoding.UTF8.GetBytes
            |> Encoding.Latin1.GetString
            |> BValue.bstr
        table
        |> BValue.packOptWith "comment" value map

    let packCreatedBy (value: string option) table =
        let map (value: string) =
            value
            |> Encoding.UTF8.GetBytes
            |> Encoding.Latin1.GetString
            |> BValue.bstr
        table
        |> BValue.packOptWith "created by" value map

    let packEncoding (value: string option) table =
        let map (value: string) =
            value
            |> Encoding.UTF8.GetBytes
            |> Encoding.Latin1.GetString
            |> BValue.bstr
        table
        |> BValue.packOptWith "encoding" value map

    let packMetaInfo (value: MetaInfo) table =
        match value.Info with
        | SingleFileInfo sfi ->
            table
            |> packSingleFileInfo sfi
            |> packAnnounce value.Announce
            |> packAnnounceList value.AnnounceList
            |> packCreationDate value.CreationDate
            |> packComment value.Comment
            |> packCreatedBy value.CreatedBy
            |> packEncoding value.Encoding
        | MultiFileInfo mfi ->
            table
            |> packMultiFileInfo mfi
            |> packAnnounce value.Announce
            |> packAnnounceList value.AnnounceList
            |> packCreationDate value.CreationDate
            |> packComment value.Comment
            |> packCreatedBy value.CreatedBy
            |> packEncoding value.Encoding
    
    let fromBValue (value: BValue) =
        match value with
        | BDictionary mi ->
            unpackMetaInfo mi
        | value ->
            failwith $"Invalid encoded MetaInfo - expected %A{BDictionaryType} and got %A{value}"
    
    let fromStream (value: Stream) =
        fromBValue (BDecode.fromStream Encoding.Latin1 value)
    
    let fromBytes (value: byte[]) =
        fromBValue (BDecode.fromBytes Encoding.Latin1 value)

    let fromString (value: string) =
        fromBValue (BDecode.fromString Encoding.Latin1 value)

    let toBValue (mi: MetaInfo) =
        Map.empty
        |> packMetaInfo mi
        |> BValue.bdict

    let toStream (mi: MetaInfo) =
        toBValue mi
        |> BEncode.toStream Encoding.Latin1
    
    let toBytes (mi: MetaInfo) =
        toBValue mi
        |> BEncode.toBytes Encoding.Latin1
    
    let toString (mi: MetaInfo) =
        toBValue mi
        |> BEncode.toString Encoding.Latin1
    
    let singleFileInfoHash (info: SingleFileInfo) =
        Map.empty
        |> packPieceLength info.PieceLength
        |> packPieces info.Pieces
        |> packName info.Name
        |> packLength info.Length
        |> packPrivate info.Private
        |> packMD5Sum info.MD5Sum
        |> BValue.bdict
        |> BEncode.toBytes Encoding.Latin1
        |> SHA1.HashData
        |> Hash

    let multiFileInfoHash (info: MultiFileInfo) =
        Map.empty
        |> packPieceLength info.PieceLength
        |> packPieces info.Pieces
        |> packFiles info.Files
        |> packName info.Name
        |> packPrivate info.Private
        |> BValue.bdict
        |> BEncode.toBytes Encoding.Latin1
        |> SHA1.HashData
        |> Hash

    let infoHash (info: Info) =
        match info with
        | SingleFileInfo sfi -> singleFileInfoHash sfi
        | MultiFileInfo  mfi -> multiFileInfoHash mfi
        
    let infoPieceLength (info: Info) =
        match info with
        | SingleFileInfo sfi -> sfi.PieceLength
        | MultiFileInfo  mfi -> mfi.PieceLength
        
    let infoPieces (info: Info) =
        match info with
        | SingleFileInfo sfi -> sfi.Pieces
        | MultiFileInfo  mfi -> mfi.Pieces
        
    let infoPrivate (info: Info) =
        match info with
        | SingleFileInfo sfi -> sfi.Private
        | MultiFileInfo  mfi -> mfi.Private
        
    let infoName (info: Info) =
        match info with
        | SingleFileInfo sfi -> sfi.Name
        | MultiFileInfo  mfi -> mfi.Name

    let infoLength (info: Info) =
        match info with
        | SingleFileInfo sfi -> sfi.Length
        | MultiFileInfo  mfi -> mfi.Files.Sum(fun file -> file.Length)