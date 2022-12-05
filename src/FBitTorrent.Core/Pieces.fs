namespace FBitTorrent.Core

open System
open System.Collections.Generic
open System.IO
open System.Linq
open FBitTorrent.Core

module Pieces =
    
    type File =
        { Path:   string 
          Offset: int64
          Length: int64 }

    type Piece =
        { Index:  int
          Hash:   Hash
          Offset: int64
          Length: int
          Files:  File[] }

    type Status =
        | Started
        | Stopped

    type State =
        { Status:        Status
          Downloaded:    int64
          Uploaded:      int64
          Left:          int64
          PieceLength:   int
          Bitfield:      Bitfield
          OutputDir:     string
          OutputSubDirs: string[]
          Pieces:        IDictionary<int, Piece>
          DownRate:      Rate
          UpRate:        Rate }
    
    let private computePieceOffset (index: int) (pieceLength: int) =
        int64 index * int64 pieceLength
    
    let private computePieceLength (index: int) (pieceLength: int) (length: int64) =
        let head = int64 index * int64 pieceLength
        let tail = int64 index * int64 pieceLength + int64 pieceLength
        // Length is the tail index - head index, unless tail is longer than the entire length of the file.
        if tail > length then int (length - head) else pieceLength 
    
    let private piecesFromSingleFileInfo (outputDir: string) (sfi: SingleFileInfo) =
        sfi.Pieces
        |> Array.mapi (fun idx hash ->
            { Index  = idx
              Hash   = hash
              Offset = computePieceOffset idx sfi.PieceLength
              Length = computePieceLength idx sfi.PieceLength sfi.Length
              Files  = [| { Path   = Path.Combine(outputDir, sfi.Name)
                            Offset = 0L
                            Length = sfi.Length } |] })
        |> Array.map (fun piece -> (piece.Index, piece))
        |> dict

    let private piecesFromMultiFileInfo (outputDir: string) (mfi: MultiFileInfo) =
        let files, length =
            mfi.Files
            |> Array.ofList
            |> Array.mapFold (fun offset file ->
                { Path   = Path.Combine(outputDir, Path.Combine(mfi.Name, Path.Combine(file.Path.ToArray())))
                  Offset = offset
                  Length = file.Length }, offset + file.Length) 0L
        mfi.Pieces
        |> Array.mapi (fun idx hash ->
            { Index  = idx
              Hash   = hash
              Offset = computePieceOffset idx mfi.PieceLength 
              Length = computePieceLength idx mfi.PieceLength length
              Files  = files
                       |> Array.filter (fun file ->
                           // Keep only the files that this piece is contained in.
                           file.Offset + file.Length >= computePieceOffset idx mfi.PieceLength &&
                           file.Offset <= int64 (computePieceOffset idx mfi.PieceLength) +
                                          int64 (computePieceLength idx mfi.PieceLength length)) })
        |> Array.map (fun piece -> (piece.Index, piece))
        |> dict
    
    let private subDirsFromSingleFileInfo (sfi: SingleFileInfo) =
        [||]
    
    let private subDirsFromMultiFileInfo (mfi: MultiFileInfo) =
        let subDirs =
            mfi.Files
            |> Seq.map (fun file ->
                if file.Path.Length > 1 then
                    Some (Path.Combine(mfi.Name, Path.Combine(file.Path.Take(file.Path.Length - 1).ToArray())))
                else
                    None)
            |> Seq.choose id
            |> Seq.distinct
        Enumerable.DefaultIfEmpty(subDirs, mfi.Name).ToArray()
    
    let createStateFromInfo outputDir info =
        match info with
        | SingleFileInfo sfi ->
            { Status        = Stopped
              Downloaded    = 0L
              Uploaded      = 0L
              Left          = sfi.Length
              PieceLength   = sfi.PieceLength
              Bitfield      = Bitfield(sfi.Pieces.Length)
              OutputDir     = outputDir
              OutputSubDirs = subDirsFromSingleFileInfo sfi
              Pieces        = piecesFromSingleFileInfo outputDir sfi
              DownRate      = Rate()
              UpRate        = Rate() }
        | MultiFileInfo mfi ->
            { Status        = Stopped
              Downloaded    = 0L
              Uploaded      = 0L
              Left          = mfi.Files
                              |> List.sumBy (fun file -> file.Length) 
              PieceLength   = mfi.PieceLength
              Bitfield      = Bitfield(mfi.Pieces.Length)
              OutputDir     = outputDir
              OutputSubDirs = subDirsFromMultiFileInfo mfi
              Pieces        = piecesFromMultiFileInfo outputDir mfi
              DownRate      = Rate()
              UpRate        = Rate() }
    
    module ByteBuffer =
        let create capacity =
            Array.create capacity 0uy
        
        let copy (blocks: byte[][]) (buffer: byte[]) =
            let folder (offset: int) (block: byte[]) =
                Array.Copy(block, 0, buffer, offset, block.Length)
                offset + block.Length
            blocks |> Array.fold folder 0
            
        let tryCopy (blocks: byte[][]) (buffer: byte[]) =
            try Ok(copy blocks buffer) with exn -> Error exn