namespace FBitTorrent.Core

open System
open System.Net
open Akka.Actor
open Akka.FSharp

module Announcer =
    
    type AnnounceEvent =
        | Started
        | Stopped
        | Completed
    with
        member __.ToTrackerEvent() =
            match __ with
            | Started   -> Tracker.Started
            | Stopped   -> Tracker.Stopped
            | Completed -> Tracker.Completed
    
    type AnnounceArgs =
        { Url:        string
          InfoHash:   Hash
          PeerId:     PeerId
          Port:       int
          Downloaded: int64
          Uploaded:   int64
          Left:       int64
          Event:      AnnounceEvent option
          NumWant:    int option }
        
    type AnnounceResult =
        { Complete:   int64 option
          Incomplete: int64 option
          Interval:   int64
          Peers:      IPEndPoint list
          Event:      AnnounceEvent option }
        
    type Command =
        | Announce         of Args: AnnounceArgs
        | ScheduleAnnounce of Args: AnnounceArgs * AfterSec: float 
    
    type CommandResult =
        | AnnounceSuccess of Result: AnnounceResult 
        | AnnounceFailure of EventType: AnnounceEvent option * Error: Exception 
    
    let actorName () = "announcer"
    
    let actorBody query (mailbox: Actor<obj>) =
        let rec receive () = actor {
            match! mailbox.Receive() with
            | :? Command as command -> 
                return! handleCommand command
            
            | message ->
                return! unhandled message }
        
        and handleCommand command =
            match command with
            | Announce
                { Url        = url
                  InfoHash   = ih
                  PeerId     = pid
                  Port       = port
                  Downloaded = downloaded
                  Uploaded   = uploaded
                  Left       = left
                  Event      = eventOpt
                  NumWant    = numWantOpt } ->
                try
                    match Tracker.defaultAnnounce url ih pid port downloaded uploaded left (eventOpt |> Option.map (fun event -> event.ToTrackerEvent())) numWantOpt query with
                    | { Complete   = complete
                        Incomplete = incomplete
                        Interval   = interval
                        Peers      = peers } -> 
                        mailbox.Context.Sender <! AnnounceSuccess {
                           Complete   = complete
                           Incomplete = incomplete
                           Interval   = interval
                           Peers      = peers
                           Event      = eventOpt }
                with exn -> 
                    mailbox.Context.Sender <! AnnounceFailure (eventOpt, Exception($"Failed to announce on %s{url}", exn))
                receive ()
            
            | ScheduleAnnounce (args, afterSec) ->
                mailbox.Context.System.Scheduler.ScheduleTellOnce(TimeSpan.FromSeconds(afterSec), mailbox.Self, Announce args, mailbox.Context.Sender)
                receive () 
        
        and unhandled message =
            mailbox.Unhandled(message)
            receive () 
        
        receive ()

    let defaultActorBody mailbox =
        actorBody Tracker.httpQuery mailbox
    
    let spawn (actorFactory: IActorRefFactory) =
        spawn actorFactory (actorName ()) defaultActorBody
    
module AnnouncerExtensions =
    type IActorContext with
        member __.GetAnnouncer() : IActorRef = __.Child(Announcer.actorName ())
