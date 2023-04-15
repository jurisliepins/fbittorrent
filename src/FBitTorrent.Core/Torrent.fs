namespace FBitTorrent.Core

open System
open System.Collections.Generic
open System.Collections.ObjectModel
open System.IO
open System.Net
open System.Linq
open Akka.IO
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
          BindEndpoint: IPEndPoint }
        
    let defaultSettings =
        { RootDirPath  = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), "Downloads")
          MaxSeedCount = 200
          Protocol     = Handshake.protocolBytes
          Reserved     = Handshake.reservedBytes
          BindEndpoint = IPEndPoint(IPAddress.Any, 6881) }
    
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
        
    type private Action =
        | MeasureRate
        
    type Command =
        | Start
        | Stop
    
    type BitfieldChange =
        | BitfieldBytesChange of Bytes: byte[]
        | BitfieldBitChange   of Bit: int
    
    type Notification =
        | StatusChanged   of Status: Status
        | BitfieldChanged of Change: BitfieldChange
        | BytesChanged    of Downloaded: int64 * Uploaded: int64 * Left: int64
        | RateChanged     of Down: Rate * Up: Rate

    let actorName (ih: Hash) = $"torrent-%A{ih}"
    
    let private createAnnounceArgs (state: State) eventOpt : Announcer.AnnounceArgs =
        { Url        = state.Announce
          InfoHash   = state.InfoHash
          PeerId     = state.PeerId
          Port       = state.Settings.BindEndpoint.Port
          Downloaded = state.Downloaded
          Uploaded   = state.Uploaded
          Left       = state.Left
          Event      = eventOpt
          NumWant    = Some state.Settings.MaxSeedCount }
    
    let private createStartedAnnounceArgs (state: State) : Announcer.AnnounceArgs =
        createAnnounceArgs state (Some Announcer.Started)
         
    let private createStoppedAnnounceArgs (state: State) : Announcer.AnnounceArgs =
        createAnnounceArgs state (Some Announcer.Stopped)
         
    let private createCompletedAnnounceArgs (state: State) : Announcer.AnnounceArgs =
        createAnnounceArgs state (Some Announcer.Completed)
        
    let private createRefreshedAnnounceArgs (state: State) : Announcer.AnnounceArgs =
        createAnnounceArgs state None
    
    let actorBody notifiedRef (initialState: State) (mailbox: Actor<obj>) =
        logDebug mailbox $"Initial state \n%A{initialState}"
        let announcerRef = Announcer.spawn mailbox
        let connectorRef = Connector.spawn mailbox mailbox.Self
        let ioRef = IO.spawn mailbox (IO.createState initialState.Settings.RootDirPath initialState.PieceLength initialState.Pieces initialState.Files) 
        let piecesRef = Pieces.spawn mailbox mailbox.Self (Pieces.createState initialState.Bitfield initialState.Pieces)
        let rec receive (downMeter: RateMeter) (upMeter: RateMeter) (state: State) = actor {
            match! mailbox.Receive() with
            | :? Action as action -> 
                return! handleAction downMeter upMeter state action
            
            | :? Command as command -> 
                return! handleCommand downMeter upMeter state command
            
            | :? Announcer.CommandResult as result ->
                return! handleAnnouncerCommandResult downMeter upMeter state result
            
            | :? IO.CommandResult as result ->
                return! handleIOCommandResult downMeter upMeter state result
            
            | :? Connector.Notification as notification ->
                return! handleConnectorNotification downMeter upMeter state notification
            
            | :? Pieces.Notification as notification ->
                return! handlePiecesNotification downMeter upMeter state notification
            
            | :? Peer.Notification as notification ->
                return! handlePeerNotification downMeter upMeter state notification
            
            | :? Terminated as message ->
                return! handleTerminatedMessage downMeter upMeter state message
            
            | message ->
                return! unhandled downMeter upMeter state message }
        
        and handleAction downMeter upMeter (state: State) action =
            match action with
            | MeasureRate ->
                RateMeter.update state.Downloaded downMeter
                RateMeter.update state.Uploaded upMeter
                let nextState =
                    { state with
                        DownRate = RateMeter.averageSmoothed downMeter
                        UpRate   = RateMeter.averageSmoothed upMeter }
                notifiedRef <! RateChanged (nextState.DownRate, nextState.UpRate)
                match state with
                | { Status = Stopped } when nextState.DownRate.Equals(Rate.zero) && nextState.UpRate.Equals(Rate.zero) ->
                    () // We're in a stopped state and the rates are at 0, so we can stop measuring.
                | _ ->
                    mailbox.Context.System.Scheduler.ScheduleTellOnce(TimeSpan.FromSeconds(MeasureRateIntervalSec), mailbox.Self, MeasureRate)
                receive downMeter upMeter nextState
        
        and handleCommand downMeter upMeter (state: State) command =
            match command with
            | Start ->
                match state with
                | { Status = Stopped } ->
                    logDebug mailbox $"Announcing '%A{Some Tracker.Started}' on %s{state.Announce}"
                    announcerRef <! Announcer.Announce (createStartedAnnounceArgs state)
                    notifiedRef <! StatusChanged Started
                    mailbox.Self <! MeasureRate
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
                    announcerRef <! Announcer.Announce (createStoppedAnnounceArgs state)
                    logDebug mailbox "Stopping peers"
                    for ref in mailbox.Context.GetPeers() do
                        mailbox.Context.Stop(ref)
                    notifiedRef <! StatusChanged Stopped
                    logInfo mailbox "Stopped"
                    receive downMeter upMeter { state with Status = Stopped }
                | _ ->
                    logDebug mailbox "Torrent already stopped"
                    receive downMeter upMeter state
        
        and handleAnnouncerCommandResult downMeter upMeter (state: State) result =
            match result with
            | Announcer.AnnounceSuccess
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
                                    |> List.filter (fun peer -> mailbox.Context.GetPeer(peer).IsNobody())
                                    |> List.distinctBy (fun peer -> $"%A{peer.Address}:%d{peer.Port}")
                                    |> List.truncate (state.Settings.MaxSeedCount - mailbox.Context.GetPeers().Count()) do
                            logInfo mailbox $"Connecting to %A{peer.Address}:%d{peer.Port}"
                            connectorRef <! Connector.Connect peer
                        logDebug mailbox $"Scheduling re-announce after '%.2f{float interval} sec' on %s{state.Announce}"
                        announcerRef <! Announcer.ScheduleAnnounce ((createRefreshedAnnounceArgs state), float interval)
                        receive downMeter upMeter state
                    | _ ->
                        logDebug mailbox "Not connecting to peers or scheduling re-announce"
                        receive downMeter upMeter state
                | _ ->
                    logDebug mailbox "Not processing announced results torrent not in started state anymore"
                    receive downMeter upMeter state
            | Announcer.AnnounceFailure (eventOpt, error) ->
                logError mailbox $"Failed to announce '%A{eventOpt}' %A{error}"
                match eventOpt with
                | None ->
                    logDebug mailbox $"Scheduling re-announce after '%.2f{AnnounceRetryDelaySec} sec' on %s{state.Announce}"
                    announcerRef <! Announcer.ScheduleAnnounce ((createAnnounceArgs state eventOpt), AnnounceRetryDelaySec)
                    receive downMeter upMeter state
                | Some Announcer.Started ->
                    notifiedRef <! StatusChanged Errored
                    receive downMeter upMeter { state with Status = Errored }
                | Some Announcer.Stopped
                | Some Announcer.Completed ->
                    receive downMeter upMeter state
        
        and handleIOCommandResult downMeter upMeter (state: State) result =
            match result with
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
                    announcerRef <! Announcer.Announce (createCompletedAnnounceArgs state)
                else
                    logInfo mailbox $"%d{nextState.Bitfield.Capacity - nextState.Bitfield.Count} pieces left to leech"
                receive downMeter upMeter nextState 
            | IO.PieceWriteFailure (idx, error) ->
                logError mailbox $"Failed to write piece %d{idx} %A{error}"
                notifiedRef <! StatusChanged Errored
                receive downMeter upMeter { state with Status = Errored }
        
        and handleConnectorNotification downMeter upMeter (state: State) result =
            match result with
            | Connector.Connected (connectionRef, localEndpoint, remoteEndpoint) ->
                match state with
                | { Status = Started } ->
                    if mailbox.Context.GetPeers().Count() < state.Settings.MaxSeedCount then
                        let initialState = (Peer.createState (Handshake.defaultCreate (state.InfoHash.ToArray()) (state.PeerId.ToArray())) (Bitfield.create state.Bitfield.Capacity))
                        let ref = monitor (Peer.spawn mailbox mailbox.Self piecesRef connectionRef localEndpoint remoteEndpoint initialState) mailbox
                        ref <! Peer.InitiateHandshake
                        piecesRef <! Pieces.PeerJoined (Peer.actorName remoteEndpoint, Bitfield.create state.Bitfield.Capacity)
                        logInfo mailbox $"Connected to %A{remoteEndpoint}"
                        receive downMeter upMeter state
                    else 
                        logDebug mailbox $"Not connecting to %A{remoteEndpoint} peer count of %d{state.Settings.MaxSeedCount} exceeded"
                        connectionRef <! Tcp.Close.Instance 
                        receive downMeter upMeter state
                | _ ->
                    logDebug mailbox $"Not connecting to %A{remoteEndpoint} torrent not in started state anymore"
                    connectionRef <! Tcp.Close.Instance
                    receive downMeter upMeter state
        
        and handlePiecesNotification downMeter upMeter (state: State) notification =
            match notification with
            | Pieces.PieceReceived (idx, piece) ->
                logDebug mailbox $"Leeched piece %d{idx}"
                ioRef <! IO.WritePiece (idx, piece)
                let nextState =
                    { state with Downloaded = state.Downloaded + int64 state.Pieces[idx].Length }
                notifiedRef <! BytesChanged (nextState.Downloaded, nextState.Uploaded, nextState.Left)
                receive downMeter upMeter nextState
                
        and handlePeerNotification downMeter upMeter (state: State) notification =
            match notification with
            | Peer.Failed error ->
                logError mailbox $"Peer %A{mailbox.Context.Sender.Path.Name} failed %A{error}"
                mailbox.Context.Stop(mailbox.Context.Sender)
            | _ -> ()
            receive downMeter upMeter state
        
        and handleTerminatedMessage downMeter upMeter (state: State) message =
            logDebug mailbox $"Peer %A{message.ActorRef.Path.Name} terminated (%d{mailbox.Context.GetPeers().Count()} peers left running)"
            piecesRef <! Pieces.PeerLeft message.ActorRef.Path.Name
            receive downMeter upMeter state
        
        and unhandled downMeter upMeter (state: State) message =
            mailbox.Unhandled(message)
            receive downMeter upMeter state

        receive (RateMeter.createFromBytes initialState.Downloaded) (RateMeter.createFromBytes initialState.Uploaded) initialState
    
    let defaultActorBody notifiedRef initialState mailbox =
        actorBody notifiedRef initialState mailbox
        
    let spawn (actorFactory: IActorRefFactory) notifiedRef (initialState: State) =
        spawn actorFactory (actorName initialState.InfoHash) (defaultActorBody notifiedRef initialState)
        
module TorrentExtensions =
    type IActorContext with
        member __.GetTorrent(ih: Hash) : IActorRef = __.Child(Torrent.actorName ih)
        member __.GetTorrents() : IEnumerable<IActorRef> = __.GetChildren().Where(fun ref -> ref.Path.Name.StartsWith("torrent"))