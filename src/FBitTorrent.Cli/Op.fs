namespace FBitTorrent.Cli

open Akka.Actor
open FBitTorrent.Core

module Op =
    let private command (ref: IActorRef) command = async {
        match! Async.AwaitTask(ref.Ask(command)) with
        | :? Client.CommandResult as result ->
            match result with
            | Client.Success (_, message) -> return message
            | Client.Failure (_, error)   -> return failwith $"%s{error}"
        | _ -> return failwith "Failed to receive response from client actor" }
    
    let private request (ref: IActorRef) request = async {
        match! Async.AwaitTask(ref.Ask(request)) with
        | :? Client.Response as response ->
            return response
        | _ ->
            return failwith "Failed to receive response from client actor" }
    
    let add ref (state: Torrent.State) =
        command ref (Client.Add state)
      
    let remove ref (ih: Hash) =
        command ref (Client.Remove ih)
    
    let start ref (ih: Hash) =
        command ref (Client.Start ih)
    
    let stop ref (ih: Hash) =
        command ref (Client.Stop ih)
    
    let get ref (ihOpt: Hash option) = async {
        match! request ref (Client.Request.Get ihOpt) with
        | Client.Response.Get torrents -> return torrents }
    
    let attachWatcher ref (ih: Hash) (watcherRef: ICanTell) =
        command ref (Client.AttachWatcher (ih, watcherRef))
        
    let detachWatcher ref (ih: Hash)  =
        command ref (Client.DetachWatcher ih)