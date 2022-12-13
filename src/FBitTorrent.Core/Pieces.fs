namespace FBitTorrent.Core

open System
open System.Collections.Generic
open System.IO
open System.Linq
open Akka.FSharp.Actors
open FBitTorrent.Core

type ByteBuffer = byte[]
    
module ByteBuffer =
    let create (capacity: int) : ByteBuffer = Array.create capacity 0uy
    
    let copy (blocks: byte[][]) (buffer: byte[]) =
        let folder (offset: int) (block: byte[]) =
            Array.Copy(block, 0, buffer, offset, block.Length)
            offset + block.Length
        blocks |> Array.fold folder 0
        
    let tryCopy (blocks: byte[][]) (buffer: byte[]) =
        try Ok(copy blocks buffer) with exn -> Error exn

type Bitfields = uint32[]

module Bitfields =
    
    let private random = Random()
    
    let create (capacity: int) : Bitfields = Array.create capacity 0u
    
    let add (bitfield: Bitfield) (bitfields: Bitfields) =
         for idx in 0..(bitfields.Length - 1) do
             if bitfield.Get(idx) && bitfields[idx] < UInt32.MaxValue then
                 bitfields[idx] <- bitfields[idx] + 1u

    let findFirst (selfBitfield: Bitfield) (peerBitfield: Bitfield) (bitfields: Bitfields) =
         let rec findFirst idx result = 
             if idx >= bitfields.Length then
                 result
             else
                 if selfBitfield.Get(idx) && peerBitfield.Get(idx) then 
                     Some idx
                 else
                     findFirst (idx + 1) result
         findFirst 0 None        
    
    let findRarest (selfBitfield: Bitfield) (peerBitfield: Bitfield) (bitfields: Bitfields) =
         let rec findRarest idx result = 
             if idx >= bitfields.Length then
                 result
             else
                 if selfBitfield.Get(idx) && peerBitfield.Get(idx) then 
                     match result with
                     | Some found when bitfields[idx] < bitfields[found] ->
                         findRarest (idx + 1) (Some idx)
                     | Some found -> findRarest (idx + 1) (Some found) 
                     | None       -> findRarest (idx + 1) (Some idx)
                 else
                     findRarest (idx + 1) result
         findRarest 0 None
         
    let find (selfBitfield: Bitfield) (peerBitfield: Bitfield) (bitfields: Bitfields) =
        // Half of the time try to find the rarest piece, half of the time just get the first piece that matches.
        // This is done in order to avoid swarming on rarest pieces and potentially getting stuck.
        if random.Next(0, 2).Equals(0) then
            findRarest selfBitfield peerBitfield bitfields
        else
            findFirst selfBitfield peerBitfield bitfields

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
            
    module FileSystemIO =
        let inline private writeStream (stream: Stream) (soffset: int64) (slength: int64) (buffer: ByteBuffer) (boffset: int64) (blength: int64) =
            if slength > stream.Length then 
                stream.SetLength(slength)
            stream.Seek(soffset, SeekOrigin.Begin) |> ignore
            stream.Write(buffer, int boffset, int blength)
        
        let writePiece (fs: IFileSystem) (piece: Piece) (buffer: ByteBuffer) =
            let pbeg = piece.Offset
            let pend = piece.Offset + int64 piece.Length
            for file in piece.Files do
                use stream = fs.OpenOrCreate(file.Path)
                let fbeg = file.Offset
                let fend = file.Offset + file.Length
                if pbeg >= fbeg &&
                   pend <= fend then
                    // Piece fits fully within the bounds of the file.
                    // |----file----|
                    //   |-piece-|
                    writeStream stream (pbeg - fbeg) (pend - fbeg) buffer 0 (pend - pbeg)
                elif pbeg < fbeg &&
                     pend > fend then
                    // Piece is out of bounds of the file on both ends.
                    //   |-file-|
                    // |----piece----|
                    writeStream stream 0 (fend - fbeg) buffer (fbeg - pbeg) (fend - fbeg) 
                elif pbeg < fbeg then
                    // Piece starts out of bounds of the file but ends within.
                    //    |---file---|
                    // |---piece---|
                    writeStream stream 0 (pend - fbeg) buffer (fbeg - pbeg) (pend - fbeg)  
                elif pend > fend then
                    // Piece starts within bounds of the file but ends outside.
                    // |---file---|
                    //     |---piece---|
                    writeStream stream (pbeg - fbeg) (fend - pbeg) buffer 0 (fend - pbeg)
                else
                    failwith $"Malformed piece or file data (piece range %A{pbeg}:%A{pend}, file range %A{fbeg}:%A{fend})"
                    
        let writeDirTree (fs: IFileSystem) (outputDir: string) (outputSubDirs: string[]) =
            if fs.DirectoryExists(outputDir) then
                for outputSubDir in outputSubDirs do
                    fs.CreateDirectory(Path.Combine(outputDir, outputSubDir))
            else
                failwith $"Output directory %s{outputDir} does not exist"
    
        let tryWritePiece (fs: IFileSystem) (piece: Piece) (buffer: ByteBuffer) =
            try Ok (writePiece fs piece buffer) with exn -> Error exn
            
        let tryWriteDirTree (fs: IFileSystem) (outputDir: string) (outputSubDirs: string[]) =
            try Ok (writeDirTree fs outputDir outputSubDirs) with exn -> Error exn
    
    type Command =
        | Start
        | Stop
        | UpdateRates
    
    type Request =
        | SelfBitfield
        | LeechPiece   of Bitfield
        
    type Response =
        | SelfBitfield of Bitfield
        | LeechPiece   of int * int
    
    type Message =
        | LeechSuccess of int * byte[][]
        | LeechFailure of int * Exception
    
    type NotificationState =
        { Status:     Status
          Downloaded: int64
          Uploaded:   int64
          Left:       int64
          DownSpeed:  double
          UpSpeed:    double }
    with
        static member Create(state: State) =
            { Status     = state.Status
              Downloaded = state.Downloaded
              Uploaded   = state.Uploaded
              Left       = state.Left
              DownSpeed  = state.DownRate.GetSpeed()
              UpSpeed    = state.UpRate.GetSpeed() }
    
    type Notification =
        | StateChanged        of NotificationState
        | DirTreeWriteSuccess
        | DirTreeWriteFailure of Exception
        | PieceLeechSuccess   of int
        | PieceLeechFailure   of int * Exception
        | PieceWriteSuccess   of int
        | PieceWriteFailure   of int * Exception
        
    let actorName () = "pieces"
    
    let actorFn fs notifiedRef initialState (mailbox: Actor<obj>) =
        let rec receive (pieceBuffer: ByteBuffer) (piecePicker: Bitfields) (cleanBitfield: Bitfield) (dirtyBitfield: Bitfield) (state: State) = actor {
            match! mailbox.Receive() with
            | :? Command as command ->
                return! handleCommand pieceBuffer piecePicker cleanBitfield dirtyBitfield state command

            | :? Message as message ->
                return! handleMessage pieceBuffer piecePicker cleanBitfield dirtyBitfield state message
            
            | :? Request as request ->
                return! handleRequest pieceBuffer piecePicker cleanBitfield dirtyBitfield state request

            | message ->
                mailbox.Unhandled message
                return! receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state }

            and handleCommand pieceBuffer piecePicker cleanBitfield dirtyBitfield (state: State) command =
                match command with
                | Start ->
                    match state with
                    | { Status = Stopped } ->
                        match FileSystemIO.tryWriteDirTree fs state.OutputDir state.OutputSubDirs with
                        | Ok _ ->
                            let nextState = { state with
                                                Status   = Started
                                                DownRate = Rate()
                                                UpRate   = Rate() }
                            notifiedRef <! DirTreeWriteSuccess
                            notifiedRef <! StateChanged (NotificationState.Create(nextState))
                            mailbox.Self <! Command.UpdateRates
                            receive pieceBuffer piecePicker cleanBitfield dirtyBitfield nextState
                        | Error exn ->
                            notifiedRef <! DirTreeWriteFailure (Exception("Failed to setup output directory tree", exn))
                            receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state
                    | _ ->
                        receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state
                | Stop ->
                    match state with
                    | { Status = Started } ->
                        let nextState = { state with
                                            Status   = Stopped
                                            DownRate = Rate()
                                            UpRate   = Rate() }
                        notifiedRef <! StateChanged (NotificationState.Create(nextState))
                        receive pieceBuffer piecePicker cleanBitfield dirtyBitfield nextState
                    | _ ->
                        receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state
                | UpdateRates ->
                    match state with
                    | { Status = Started } when mailbox.Context.Sender.Equals(mailbox.Context.Self) ->
                        state.DownRate.Update(state.Downloaded)
                        state.UpRate.Update(state.Uploaded)
                        notifiedRef <! StateChanged (NotificationState.Create(state))
                        mailbox.Context.System.Scheduler.ScheduleTellOnce(TimeSpan.FromSeconds(float 1.0f), mailbox.Self, Command.UpdateRates)
                        receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state 
                    | _ ->
                        receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state

            and handleMessage pieceBuffer piecePicker cleanBitfield dirtyBitfield (state: State) message =
                match message with
                | LeechSuccess (idx, blocks) ->
                    match state.Pieces.TryGetValue(idx) with
                    | true, piece when dirtyBitfield.Get(idx) ->
                        if piece.Hash.Equals(Hash.ComputeBlocks(blocks)) then
                            notifiedRef <! PieceLeechSuccess idx
                            match ByteBuffer.tryCopy blocks pieceBuffer with
                            | Ok length ->
                                match FileSystemIO.tryWritePiece fs piece pieceBuffer with
                                | Ok _ -> 
                                    let nextState = { state with
                                                        Downloaded = state.Downloaded + int64 length
                                                        Left       = state.Left       - int64 length }
                                    nextState.Bitfield.Set(idx, true)
                                    dirtyBitfield.Set(idx, false)
                                    notifiedRef <! PieceWriteSuccess idx
                                    notifiedRef <! StateChanged (NotificationState.Create(nextState))
                                    receive pieceBuffer piecePicker cleanBitfield dirtyBitfield nextState
                                | Error error ->
                                    notifiedRef <! PieceWriteFailure (idx, Exception($"Failed to write piece %d{idx}", error))
                                    receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state
                            | Error error ->
                                notifiedRef <! PieceWriteFailure (idx, Exception($"Failed to copy received blocks into the buffer for piece %d{idx}", error))
                                receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state
                        else
                            receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state
                    | _ ->
                        receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state
                | LeechFailure (idx, error)  ->
                    match state.Pieces.TryGetValue(idx) with
                    | true, _ when dirtyBitfield.Get(idx) ->
                        notifiedRef <! PieceLeechFailure (idx, error)
                        receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state
                    | _ ->
                        receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state
            
            and handleRequest pieceBuffer piecePicker cleanBitfield dirtyBitfield (state: State) request =
                match request with
                | Request.SelfBitfield ->
                    mailbox.Context.Sender <! Response.SelfBitfield state.Bitfield
                    receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state
                | Request.LeechPiece bitfield ->
                    // Try to pick rarest piece that is in the clean bitfield and in the peer bitfield (a piece that we want and one that the peer has). 
                    match Bitfields.find cleanBitfield bitfield piecePicker with
                    | Some idx ->
                        Bitfields.add bitfield piecePicker
                        cleanBitfield.Set(idx, false)
                        dirtyBitfield.Set(idx, true)
                        mailbox.Context.Sender <! Response.LeechPiece (state.Pieces[idx].Index, state.Pieces[idx].Length)
                        receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state
                    | None ->
                        // If no match was found then look in the dirty pieces bitfield. These are the pieces that
                        // have been given to other peers to download but have not completed yet or have failed to download.
                        //
                        // End-game:
                        //  The goal of this algorithm is to get unique clean pieces from peers first (no peer will download the
                        //  same piece), but once those have been exhausted - make idle peers swarm on problematic, dirty pieces.
                        match Bitfields.find dirtyBitfield bitfield piecePicker with
                        | Some idx ->
                            Bitfields.add bitfield piecePicker
                            mailbox.Context.Sender <! Response.LeechPiece (state.Pieces[idx].Index, state.Pieces[idx].Length)
                            receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state
                        | _ ->
                            Bitfields.add bitfield piecePicker
                            receive pieceBuffer piecePicker cleanBitfield dirtyBitfield state

        let pieceBuffer = ByteBuffer.create initialState.PieceLength
        let piecePicker = Bitfields.create initialState.Bitfield.Capacity
        let cleanBitfield = Bitfield(initialState.Bitfield).Not()
        let dirtyBitfield = Bitfield(initialState.Bitfield.Capacity)
        receive pieceBuffer piecePicker cleanBitfield dirtyBitfield initialState
    
    let defaultActorFn notifiedRef initialState mailbox =
        actorFn (FileSystem.createLocal ()) notifiedRef initialState mailbox