namespace FBitTorrent.Core

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.IO
open System.Net
open System.Linq
open Akka.Actor
open Akka.FSharp

module Torrent =
    open PeerExtensions
    
    let [<Literal>] AnnounceRetryDelaySec = 300.0
    
    let [<Literal>] MeasureRateIntervalSec = 1.0
    
    type Settings =
        { RootDirPath:  string
          MaxSeedCount: int
          Protocol:     byte[]
          Reserved:     byte[] 
          Address:      IPAddress
          Port:         int }
        
    let defaultSettings =
        { RootDirPath  = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), "Downloads")
          MaxSeedCount = 200
          Protocol     = Handshake.protocolBytes
          Reserved     = Handshake.reservedBytes
          Address      = IPAddress.Any
          Port         = 6881 }
    
    type Status =
        | Started
        | Stopped
        | Errored
    
    type State =
        { Status:      Status
          InfoHash:    Hash
          PeerId:      PeerId
          Bitfield:    Bitfield
          PieceLength: int
          Pieces:      ReadOnlyCollection<Pieces.Piece>
          Files:       ReadOnlyCollection<Pieces.File>
          Name:        string
          Length:      int64
          Downloaded:  int64
          Uploaded:    int64
          Left:        int64
          Announce:    string
          Settings:    Settings
          DownRate:    Rate
          UpRate:      Rate }
    
    let createStateFromMetaInfo (settings: Settings) (mi: MetaInfo) =
        { Status      = Stopped
          InfoHash    = MetaInfo.infoHash mi.Info
          PeerId      = PeerId.create ()
          Bitfield    = Bitfield.create (MetaInfo.infoPieces mi.Info).Count
          PieceLength = MetaInfo.infoPieceLength mi.Info
          Pieces      = Pieces.createPiecesFromInfo mi.Info
          Files       = Pieces.createFilesFromInfo mi.Info
          Name        = MetaInfo.infoName mi.Info
          Length      = MetaInfo.infoLength mi.Info
          Downloaded  = 0L
          Uploaded    = 0L
          Left        = MetaInfo.infoLength mi.Info
          Announce    = mi.Announce
          Settings    = settings
          DownRate    = Rate.zero
          UpRate      = Rate.zero }
        
    type Command =
        | Start
        | Stop
        | MeasureRate
    
    type BitfieldChange =
        | BitfieldBytesChange of byte[]
        | BitfieldBitChange   of int
    
    type Notification =
        | StatusChanged   of Status
        | BitfieldChanged of BitfieldChange
        | BytesChanged    of int64 * int64 * int64
        | RateChanged     of Rate * Rate

    let actorName (ih: Hash) = $"torrent-%A{ih}"
    
    let actorFn announcerFn connectorFn piecesFn ioFn peerFn notifiedRef (initialState: State) (mailbox: Actor<obj>) =
        logDebug mailbox $"Initial state \n%A{initialState}"
        let announcerRef = spawn mailbox (Announcer.actorName ()) announcerFn
        let connectorRef = spawn mailbox (Connector.actorName ()) connectorFn
        let ioRef = spawn mailbox (IO.actorName ()) (ioFn (IO.createState initialState.Settings.RootDirPath initialState.PieceLength initialState.Pieces initialState.Files)) 
        let piecesRef = spawn mailbox (Pieces.actorName ()) (piecesFn mailbox.Self (Pieces.createState initialState.Bitfield initialState.Pieces))
        let rec receive (downMeter: RateMeter) (upMeter: RateMeter) (state: State) = actor {
            match! mailbox.Receive() with
            | :? Command as command -> 
                return! handleCommand downMeter upMeter state command
            
            | :? IO.CommandResult as result ->
                return! handleIOCommandResult downMeter upMeter state result
                
            | :? Announcer.CommandResult as result ->
                return! handleAnnouncerCommandResult downMeter upMeter state result
            
            | :? Connector.CommandResult as result ->
                return! handleConnectorCommandResult downMeter upMeter state result
            
            | :? Pieces.Notification as notification ->
                return! handlePiecesNotification downMeter upMeter state notification
            
            | :? Peer.Notification as notification ->
                return! handlePeerNotification downMeter upMeter state notification
            
            | :? Terminated as message ->
                return! handleTerminatedMessage downMeter upMeter state message
            
            | message ->
                mailbox.Unhandled(message)
                return! receive downMeter upMeter state }
        
        and handleCommand downMeter upMeter (state: State) command =
            match command with
            | Start ->
                match state with
                | { Status = Stopped } ->
                    ioRef <! IO.CreateDirs
                    notifiedRef <! StatusChanged Started
                    logInfo mailbox "Started"
                    receive downMeter upMeter { state with Status = Started }
                | _ ->
                    logDebug mailbox "Torrent already started"
                    receive downMeter upMeter state
            | Stop ->
                match state with
                | { Status = Started }
                | { Status = Errored } ->
                    logDebug mailbox $"Announcing '%A{Some Tracker.Stopped}' on %s{state.Announce}"
                    let args: Announcer.AnnounceArgs =
                         { Url        = state.Announce
                           InfoHash   = state.InfoHash
                           PeerId     = state.PeerId
                           Port       = state.Settings.Port
                           Downloaded = state.Downloaded
                           Uploaded   = state.Uploaded
                           Left       = state.Left
                           Event      = Some Announcer.Stopped
                           NumWant    = None }
                    announcerRef <! Announcer.Announce args
                    logDebug mailbox "Stopping peers"
                    for ref in mailbox.Context.GetPeers() do
                        mailbox.Context.Stop(ref)
                    notifiedRef <! StatusChanged Stopped
                    logInfo mailbox "Stopped"
                    receive downMeter upMeter { state with Status = Stopped }
                | _ ->
                    logDebug mailbox "Torrent already stopped"
                    receive downMeter upMeter state
            | MeasureRate ->
                downMeter |> RateMeter.update state.Downloaded
                upMeter |> RateMeter.update state.Uploaded
                let nextState =
                    { state with
                        DownRate = downMeter |> RateMeter.averageSmoothed
                        UpRate   = upMeter |> RateMeter.averageSmoothed }
                notifiedRef <! RateChanged (nextState.DownRate, nextState.UpRate)
                match state, nextState.DownRate, nextState.UpRate with
                | { Status = Stopped }, downRate, upRate when downRate.Equals(Rate.zero) && upRate.Equals(Rate.zero) ->
                    () // We're in a stopped state and the rates are at 0, so we can stop measuring.
                | _ ->
                    mailbox.Context.System.Scheduler.ScheduleTellOnce(TimeSpan.FromSeconds(MeasureRateIntervalSec), mailbox.Self, MeasureRate)
                receive downMeter upMeter nextState

        and handleIOCommandResult downMeter upMeter (state: State) result =
            match result with
            | IO.DirsCreateSuccess createdDirs ->
                logDebug mailbox $"Created directories %A{createdDirs})"
                match state with
                | { Status = Started } ->
                    logDebug mailbox $"Announcing '%A{Some Tracker.Started}' on %s{state.Announce}"
                    let args: Announcer.AnnounceArgs =
                         { Url        = state.Announce
                           InfoHash   = state.InfoHash
                           PeerId     = state.PeerId
                           Port       = state.Settings.Port
                           Downloaded = state.Downloaded
                           Uploaded   = state.Uploaded
                           Left       = state.Left
                           Event      = Some Announcer.Started
                           NumWant    = Some state.Settings.MaxSeedCount }
                    announcerRef <! Announcer.Announce args
                    receive downMeter upMeter state
                | _ ->
                    logDebug mailbox $"Not announcing '%A{Some Tracker.Started}' torrent not in started state anymore"
                    receive downMeter upMeter state
            | IO.DirsCreateFailure error ->
                logError mailbox $"Failed to create directories %A{error}"
                notifiedRef <! StatusChanged Errored
                receive downMeter upMeter { state with Status = Errored }
            | IO.PieceWriteSuccess idx ->
                logDebug mailbox $"Wrote piece %d{idx}"
                for ref in mailbox.Context.GetPeers() do
                    ref <! Peer.PieceLeeched idx
                state.Bitfield |> Bitfield.setBit idx true
                notifiedRef <! BitfieldChanged (BitfieldBitChange idx)
                let nextState =
                    { state with Left = state.Left - int64 state.Pieces[idx].Length }
                notifiedRef <! BytesChanged (nextState.Downloaded, nextState.Uploaded, nextState.Left)
                if nextState.Bitfield |> Bitfield.isFull then
                    logInfo mailbox "All pieces leeched"
                    logDebug mailbox $"Announcing '%A{Some Tracker.Completed}' on %s{state.Announce}"
                    let args: Announcer.AnnounceArgs =
                         { Url        = state.Announce
                           InfoHash   = state.InfoHash
                           PeerId     = state.PeerId
                           Port       = state.Settings.Port
                           Downloaded = state.Downloaded
                           Uploaded   = state.Uploaded
                           Left       = state.Left
                           Event      = Some Announcer.Completed
                           NumWant    = None }
                    announcerRef <! Announcer.Announce args
                else
                    logInfo mailbox $"%d{nextState.Bitfield.Capacity - nextState.Bitfield.Count} pieces left to leech"
                receive downMeter upMeter nextState 
            | IO.PieceWriteFailure (idx, error) ->
                logError mailbox $"Failed to write piece %d{idx} %A{error}"
                notifiedRef <! StatusChanged Errored
                receive downMeter upMeter { state with Status = Errored }
        
        and handleAnnouncerCommandResult downMeter upMeter (state: State) result =
            match result with
            | Announcer.Success
                { Complete   = complete
                  Incomplete = incomplete
                  Interval   = interval
                  Peers      = peers
                  Event      = eventOpt } ->
                logDebug mailbox $"Announced '%A{eventOpt}' on %s{state.Announce} (complete: %A{complete}, incomplete: %A{incomplete}, interval: %A{interval}, peers: %A{peers |> List.map (fun peer -> peer.ToString())})"
                match state with
                | { Status = Started } ->
                    match eventOpt with
                    | None 
                    | Some Announcer.Started when state.Left > 0 ->
                        for peer in peers
                                    |> List.filter (fun peer -> mailbox.Context.GetPeer(peer.Address, peer.Port).IsNobody())
                                    |> List.distinctBy (fun peer -> $"%A{peer.Address}:%d{peer.Port}")
                                    |> List.truncate (state.Settings.MaxSeedCount - mailbox.Context.GetPeers().Count()) do
                            logInfo mailbox $"Connecting to %A{peer.Address}:%d{peer.Port}"
                            connectorRef <! Connector.Connect (peer.Address, peer.Port, Handshake.defaultCreate (state.InfoHash.ToArray()) (state.PeerId.ToArray()))
                        logDebug mailbox $"Scheduling re-announce after '%.2f{float interval} sec' on %s{state.Announce}"
                        let args: Announcer.AnnounceArgs =
                             { Url        = state.Announce
                               InfoHash   = state.InfoHash
                               PeerId     = state.PeerId
                               Port       = state.Settings.Port
                               Downloaded = state.Downloaded
                               Uploaded   = state.Uploaded
                               Left       = state.Left
                               Event      = None
                               NumWant    = Some state.Settings.MaxSeedCount }
                        announcerRef <! Announcer.ScheduleAnnounce (args, float interval)
                        receive downMeter upMeter state
                    | _ ->
                        logDebug mailbox "Not connecting to peers or scheduling re-announce"
                        receive downMeter upMeter state
                | _ ->
                    logDebug mailbox "Not processing announced results torrent not in started state anymore"
                    receive downMeter upMeter state
            | Announcer.Failure (error, eventOpt) ->
                logError mailbox $"Failed to announce '%A{eventOpt}' %A{error}"
                match eventOpt with
                | None ->
                    logDebug mailbox $"Scheduling re-announce after '%.2f{AnnounceRetryDelaySec} sec' on %s{state.Announce}"
                    let args: Announcer.AnnounceArgs =
                         { Url        = state.Announce
                           InfoHash   = state.InfoHash
                           PeerId     = state.PeerId
                           Port       = state.Settings.Port
                           Downloaded = state.Downloaded
                           Uploaded   = state.Uploaded
                           Left       = state.Left
                           Event      = eventOpt
                           NumWant    = Some state.Settings.MaxSeedCount }
                    announcerRef <! Announcer.ScheduleAnnounce (args, AnnounceRetryDelaySec)
                    receive downMeter upMeter state
                | Some Announcer.Started ->
                    notifiedRef <! StatusChanged Errored
                    receive downMeter upMeter { state with Status = Errored }
                | Some Announcer.Stopped
                | Some Announcer.Completed ->
                    receive downMeter upMeter state
        
        and handleConnectorCommandResult downMeter upMeter (state: State) result =
            match result with
            | Connector.Success (connection, _) ->
                match state with
                | { Status = Started } ->
                    if mailbox.Context.GetPeers().Count() < state.Settings.MaxSeedCount then
                        let actorName = Peer.actorName connection.RemoteEndpoint.Address connection.RemoteEndpoint.Port
                        let actorFn = peerFn mailbox.Self piecesRef (Message.createConnection connection) (Peer.createState state.Bitfield.Capacity)
                        let ref = monitor (spawn mailbox actorName actorFn) mailbox
                        ref <! Peer.Read
                        ref <! Peer.Leech (Some (Bitfield.createFromBitfield state.Bitfield))
                        ref <! Peer.KeepAlive
                        ref <! Peer.MeasureRate
                        piecesRef <! Pieces.AddPeerBitfield (actorName, Bitfield.create state.Bitfield.Capacity)
                        logInfo mailbox $"Connected to %A{connection.RemoteEndpoint.Address}:%d{connection.RemoteEndpoint.Port}"
                        receive downMeter upMeter state
                    else 
                        logDebug mailbox $"Not connecting to %A{connection.RemoteEndpoint.Address}:%d{connection.RemoteEndpoint.Port} peer count of %d{state.Settings.MaxSeedCount} exceeded"
                        connection.Disconnect()
                        receive downMeter upMeter state
                | _ ->
                    logDebug mailbox $"Not connecting to %A{connection.RemoteEndpoint.Address}:%d{connection.RemoteEndpoint.Port} torrent not in started state anymore"
                    connection.Disconnect()
                    receive downMeter upMeter state
            | Connector.Failure (address, port, error) ->
                logError mailbox $"Failed to connect to %A{address}:%d{port} %A{error}"
                receive downMeter upMeter state
        
        and handlePiecesNotification downMeter upMeter (state: State) notification =
            match notification with
            | Pieces.PieceLeechSuccess (idx, piece) ->
                logDebug mailbox $"Leeched piece %d{idx}"
                ioRef <! IO.WritePiece (idx, piece)
                let nextState =
                    { state with Downloaded = state.Downloaded + int64 state.Pieces[idx].Length }
                notifiedRef <! BytesChanged (nextState.Downloaded, nextState.Uploaded, nextState.Left)
                receive downMeter upMeter nextState
            | Pieces.PieceLeechFailure (idx, error) ->
                logError mailbox $"Failed to leech piece %d{idx} %A{error}"
                receive downMeter upMeter state
                
        and handlePeerNotification downMeter upMeter (state: State) notification =
            match notification with
            | Peer.Failed error ->
                logError mailbox $"Peer %A{mailbox.Context.Sender.Path.Name} failed %A{error}"
                mailbox.Context.Stop(mailbox.Context.Sender)
            | _ -> ()
            receive downMeter upMeter state
        
        and handleTerminatedMessage downMeter upMeter (state: State) message =
            logDebug mailbox $"Peer %A{message.ActorRef.Path.Name} terminated (%d{mailbox.Context.GetPeers().Count()} peers left running)"
            piecesRef <! Pieces.RemovePeerBitfield message.ActorRef.Path.Name
            receive downMeter upMeter state
        
        receive (RateMeter.createFromBytes initialState.Downloaded) (RateMeter.createFromBytes initialState.Uploaded) initialState
    
    let defaultActorFn notifiedRef initialState (mailbox: Actor<obj>) =
        actorFn Announcer.defaultActorFn Connector.defaultActorFn Pieces.defaultActorFn IO.defaultActorFn Peer.defaultActorFn notifiedRef initialState mailbox
        
module TorrentExtensions =
    type IActorContext with
        member __.GetTorrent(ih: Hash) : IActorRef = __.Child(Torrent.actorName ih)
        member __.GetTorrents() : IEnumerable<IActorRef> = __.GetChildren().Where(fun ref -> ref.Path.Name.StartsWith("torrent"))