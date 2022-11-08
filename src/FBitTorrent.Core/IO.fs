namespace FBitTorrent.Core

open System
open System.Collections.ObjectModel
open System.IO
open System.Security.Cryptography
open Akka.Actor
open Akka.FSharp

module IO =
    type State =
        { RootDirPath: string
          DirPaths:    string list
          PieceLength: int
          Pieces:      ReadOnlyCollection<Pieces.Piece>
          Files:       ReadOnlyCollection<Pieces.File> }
    
    let private createDirPaths (pieces: ReadOnlyCollection<Pieces.Piece>) =
        pieces
        |> Seq.collect (fun piece ->
            piece.Files
            |> Seq.map(fun file -> Path.GetDirectoryName(file.Path)))
        |> Seq.distinct
        |> Seq.filter (String.IsNullOrEmpty >> not)
        |> Seq.toList
    
    let createState rootDirPath pieceLength pieces files =
        { RootDirPath = rootDirPath
          DirPaths    = createDirPaths pieces
          PieceLength = pieceLength
          Pieces      = pieces
          Files       = files }

    type Command =
        | CreateDirs
        | WritePiece of int * ByteBuffer
    
    type CommandResult =
        | DirsCreateSuccess of string list
        | DirsCreateFailure of Exception
        | PieceWriteSuccess of int
        | PieceWriteFailure of int * Exception
    
    let private createDirs (fs: IFileSystem) (rootDirPath: string) (dirPaths: string list) =
        if fs.DirectoryExists(rootDirPath) then
            dirPaths
            |> Seq.map (fun dirPath ->
                let path = Path.Combine(rootDirPath, dirPath)
                if fs.DirectoryExists(path) then
                    None
                else
                    fs.CreateDirectory(path)
                    Some path)
            |> Seq.choose id
            |> Seq.toList
        else
            failwith $"Output directory %s{rootDirPath} does not exist"
    
    let private writeStream (stream: Stream) (soffset: int64) (slength: int64) (bytes: ByteBuffer) (boffset: int64) (blength: int64) =
        if slength > stream.Length then
            stream.SetLength(slength)
        stream.Seek(soffset, SeekOrigin.Begin) |> ignore
        bytes.WriteTo(stream, int boffset, int blength)
        bytes.Release()
    
    let private writePiece (fs: IFileSystem) (rootDirPath: string) (piece: Pieces.Piece) (bytes: ByteBuffer) =
        let pbeg = piece.Offset
        let pend = piece.Offset + int64 piece.Length
        for file in piece.Files do
            use stream = fs.OpenOrCreate(Path.Combine(rootDirPath, file.Path))
            let fbeg = file.Offset
            let fend = file.Offset + file.Length
            // Piece fits fully within the bounds of the file.
            // |----file----|
            //   |-piece-|
            if pbeg >= fbeg && pend <= fend then writeStream stream (pbeg - fbeg) (pend - fbeg) bytes 0 (pend - pbeg)
            // Piece is out of bounds of the file on both ends.
            //   |-file-|
            // |----piece----|
            elif pbeg < fbeg && pend > fend then writeStream stream 0 (fend - fbeg) bytes (fbeg - pbeg) (fend - fbeg) 
            // Piece starts out of bounds of the file but ends within.
            //    |---file---|
            // |---piece---|
            elif pbeg < fbeg then writeStream stream 0 (pend - fbeg) bytes (fbeg - pbeg) (pend - fbeg)  
            // Piece starts within bounds of the file but ends outside.
            // |---file---|
            //     |---piece---|
            elif pend > fend then writeStream stream (pbeg - fbeg) (fend - pbeg) bytes 0 (fend - pbeg)
            else
                failwith $"Malformed piece or file data (piece range %A{pbeg}:%A{pend}, file range %A{fbeg}:%A{fend})"
    
    let actorName () = "io"
    
    let actorFn (fs: IFileSystem) (initialState: State) (mailbox: Actor<obj>) =
        let rec receive (state: State) = actor {
            match! mailbox.Receive() with
            | :? Command as command -> 
                return! handleCommand state command
            
            | message ->
                mailbox.Unhandled(message)
                return! receive state }
        
        and handleCommand (state: State) command =
            match command with
            | CreateDirs ->
                try
                    let createdDirs = createDirs fs state.RootDirPath state.DirPaths
                    mailbox.Context.Sender <! DirsCreateSuccess createdDirs
                with exn ->
                    mailbox.Context.Sender <! DirsCreateFailure (Exception("Failed to create directories", exn))
                receive state
            | WritePiece (idx, piece) ->
                try
                    writePiece fs state.RootDirPath state.Pieces[idx] piece
                    mailbox.Context.Sender <! PieceWriteSuccess idx
                with exn -> 
                    mailbox.Context.Sender <! PieceWriteFailure (idx, Exception($"Failed to write piece %d{idx}", exn))
                receive state

        receive initialState
        
    let defaultActorFn initialState mailbox =
        actorFn (FileSystem.createLocal ()) initialState mailbox
        
module IOExtensions =
    type IActorContext with
        member __.GetIO() : IActorRef = __.Child(IO.actorName ())