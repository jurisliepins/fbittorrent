namespace FBitTorrent.Core

open System.Collections.Generic
open Akka.FSharp
open Akka.Actor

module Client =
    open TorrentExtensions

    type Torrent =
        { Status:      Torrent.Status
          InfoHash:    Hash
          PeerId:      PeerId
          Bitfield:    Bitfield
          Name:        string
          Length:      int64
          Downloaded:  int64
          Uploaded:    int64
          Left:        int64
          DownRate:    Rate
          UpRate:      Rate }
    
    let private createTorrentFromState (state: Torrent.State) =
        { Status      = state.Status
          InfoHash    = state.InfoHash
          PeerId      = state.PeerId
          Bitfield    = Bitfield.createFromBitfield state.Bitfield
          Name        = state.Name
          Length      = state.Length
          Downloaded  = state.Downloaded
          Uploaded    = state.Uploaded
          Left        = state.Left
          DownRate    = Rate.zero
          UpRate      = Rate.zero }
        
    type State =
        { Torrents: Dictionary<string, Torrent>
          Watchers: Dictionary<string, ICanTell> }
    
    let createState () =
        { Torrents = Dictionary()
          Watchers = Dictionary() }
    
    type WatchedTorrent =
        { Status:      Torrent.Status
          InfoHash:    Hash
          PeerId:      PeerId
          Name:        string
          Length:      int64
          Downloaded:  int64
          Uploaded:    int64
          Left:        int64
          DownRate:    Rate
          UpRate:      Rate }

    let createWatchedTorrentFromTorrent (torrent: Torrent) =
        { Status      = torrent.Status
          InfoHash    = torrent.InfoHash
          PeerId      = torrent.PeerId
          Name        = torrent.Name
          Length      = torrent.Length
          Downloaded  = torrent.Downloaded
          Uploaded    = torrent.Uploaded
          Left        = torrent.Left
          DownRate    = torrent.DownRate
          UpRate      = torrent.UpRate }
    
    type Command =
        | Add           of Torrent.State
        | Remove        of Hash
        | Start         of Hash
        | Stop          of Hash
        | AttachWatcher of Hash * ICanTell
        | DetachWatcher of Hash

    type CommandResult =
        | Success of (Hash option * string)
        | Failure of (Hash option * string)
    
    type Request =
        | Get of Hash option
            
    type Response =
        | Get of Torrent list
    
    type WatcherNotification =
        | WatchedTorrentChanged of WatchedTorrent option
    
    let actorName () = "client"
    
    let actorFn torrentFn (initialState: State) (mailbox: Actor<obj>) =
        let rec receive (state: State) = actor {
            match! mailbox.Receive() with
            | :? Command as command ->
                return! handleTorrentCommand state command
            
            | :? Request as request ->
                return! handleTorrentRequest state request 
            
            | :? Torrent.Notification as notification ->
                return! handleTorrentNotification state notification    
            
            | :? Terminated as message ->
                return! handleTerminatedMessage state message
            
            | message ->
                mailbox.Unhandled(message)
                return! receive state }
        
        and handleTorrentCommand (state: State) command =
            match command with
            | Command.Add torrentState ->
                match mailbox.Context.GetTorrent(torrentState.InfoHash) with
                | ref when ref.IsNobody() ->
                    let actorName = Torrent.actorName torrentState.InfoHash
                    let _ = monitor (spawn mailbox actorName (torrentFn mailbox.Self torrentState)) mailbox
                    mailbox.Context.Sender <! Success (Some torrentState.InfoHash, "Torrent added")
                    state.Torrents.Add(actorName, createTorrentFromState torrentState)
                | _ ->
                    mailbox.Context.Sender <! Failure (Some torrentState.InfoHash, "Torrent already exists")
                receive state
            | Command.Remove ih ->
                match mailbox.Context.GetTorrent(ih) with
                | ref when not (ref.IsNobody()) ->
                    mailbox.Context.Stop(ref)
                    mailbox.Context.Sender <! Success (Some ih, "Torrent removed")
                | _ ->
                    mailbox.Context.Sender <! Failure (Some ih, "Torrent not found")
                receive state
            | Command.Start ih ->
                match mailbox.Context.GetTorrent(ih) with
                | ref when not (ref.IsNobody()) ->
                    ref <! Torrent.Start
                    ref <! Torrent.MeasureRate
                    mailbox.Context.Sender <! Success (Some ih, "Torrent started")
                | _ ->
                    mailbox.Context.Sender <! Failure (Some ih, "Torrent not found")
                receive state
            | Command.Stop ih ->
                match mailbox.Context.GetTorrent(ih) with
                | ref when not (ref.IsNobody()) ->
                    ref <! Torrent.Stop
                    mailbox.Context.Sender <! Success (Some ih, "Torrent stopped")
                | _ ->
                    mailbox.Context.Sender <! Failure (Some ih, "Torrent not found")
                receive state
            | Command.AttachWatcher (ih, watcherRef) ->
                match mailbox.Context.GetTorrent(ih) with
                | ref when not (ref.IsNobody()) ->
                    if state.Watchers.TryAdd(Torrent.actorName ih, watcherRef) then
                        mailbox.Context.Sender <! Success (Some ih, "Watcher attached")
                    else
                        mailbox.Context.Sender <! Failure (Some ih, "Watcher already exists")    
                | _ ->
                    mailbox.Context.Sender <! Failure (Some ih, "Torrent not found")
                receive state
            | Command.DetachWatcher ih ->
                if state.Watchers.Remove(Torrent.actorName ih) then
                    mailbox.Context.Sender <! Success (Some ih, "Watcher detached")
                else
                    mailbox.Context.Sender <! Failure (Some ih, "Watcher not found")
                receive state
        
        and handleTorrentRequest (state: State) request =
            match request with
            | Request.Get ihOpt ->
                match ihOpt with
                | Some ih -> 
                    match state.Torrents.TryGetValue(Torrent.actorName ih) with
                    | true, torrent -> mailbox.Context.Sender <! Response.Get [torrent]
                    |             _ -> mailbox.Context.Sender <! Response.Get []
                    receive state
                | None ->
                    mailbox.Context.Sender <! Response.Get (state.Torrents.Values |> Seq.toList)
                    receive state
            
        and handleTorrentNotification (state: State) notification =
            match notification with
            | Torrent.StatusChanged status ->
                let updatedState =
                    { state.Torrents[mailbox.Context.Sender.Path.Name] with Status = status }
                state.Torrents[mailbox.Context.Sender.Path.Name] <- updatedState
            | Torrent.BitfieldChanged change ->
                match change with
                | Torrent.BitfieldBytesChange bytes -> state.Torrents[mailbox.Context.Sender.Path.Name].Bitfield |> Bitfield.setBytes bytes
                | Torrent.BitfieldBitChange     idx -> state.Torrents[mailbox.Context.Sender.Path.Name].Bitfield |> Bitfield.setBit idx true
            | Torrent.BytesChanged (downloaded, uploaded, left) ->
                let updatedState =
                    { state.Torrents[mailbox.Context.Sender.Path.Name] with
                        Downloaded = downloaded
                        Uploaded   = uploaded
                        Left       = left }
                state.Torrents[mailbox.Context.Sender.Path.Name] <- updatedState
            | Torrent.RateChanged (downRate, upRate) ->
                let updatedState =
                    { state.Torrents[mailbox.Context.Sender.Path.Name] with
                        DownRate = downRate
                        UpRate   = upRate }
                state.Torrents[mailbox.Context.Sender.Path.Name] <- updatedState
            match state.Watchers.TryGetValue(mailbox.Context.Sender.Path.Name) with
            | true, ref -> ref <! WatchedTorrentChanged (Some (createWatchedTorrentFromTorrent state.Torrents[mailbox.Context.Sender.Path.Name]))
            | _ -> ()
            receive state
        
        and handleTerminatedMessage (state: State) message =
            match state.Watchers.TryGetValue(mailbox.Context.Sender.Path.Name) with
            | true, ref -> ref <! WatchedTorrentChanged None
            | _ -> ()
            state.Torrents.Remove(message.ActorRef.Path.Name) |> ignore
            receive state
        
        receive initialState
        
    let defaultActorFn initialState (mailbox: Actor<obj>) =
        actorFn Torrent.defaultActorFn initialState mailbox
        
module ClientExtensions =
    type IActorContext with
        member __.GetClient() : IActorRef = __.Child(Client.actorName ())