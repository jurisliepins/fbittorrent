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
        | Announce         of AnnounceArgs
        | ScheduleAnnounce of AnnounceArgs * float 
    
    type CommandResult =
        | Success of AnnounceResult 
        | Failure of Exception * AnnounceEvent option
    
    let actorName () = "announcer"
    
    let actorFn call (mailbox: Actor<obj>) =
        let rec receive () = actor {
            match! mailbox.Receive() with
            | :? Command as command -> 
                return! handleCommand command
            
            | message ->
                mailbox.Unhandled(message)
                return! receive () }
        
        and handleCommand command =
            match command with
            | Announce ({
                Url        = url
                InfoHash   = ih
                PeerId     = pid
                Port       = port
                Downloaded = downloaded
                Uploaded   = uploaded
                Left       = left
                Event      = eventOpt
                NumWant    = numWantOpt }) ->
                try
                    match Tracker.defaultAnnounce url ih pid port downloaded uploaded left (eventOpt |> Option.map (fun event -> event.ToTrackerEvent())) numWantOpt call with
                    | { Complete   = complete
                        Incomplete = incomplete
                        Interval   = interval
                        Peers      = peers } -> 
                        mailbox.Context.Sender <! Success {
                           Complete   = complete
                           Incomplete = incomplete
                           Interval   = interval
                           Peers      = peers
                           Event      = eventOpt }
                with exn -> 
                    mailbox.Context.Sender <! Failure (Exception($"Failed to announce on %s{url}", exn), eventOpt)
                receive ()
            
            | ScheduleAnnounce (args, afterSec) ->
                mailbox.Context.System.Scheduler.ScheduleTellOnce(TimeSpan.FromSeconds(afterSec), mailbox.Self, Announce args, mailbox.Context.Sender)
                receive () 
        
        receive ()
        
    let defaultActorFn mailbox = actorFn Tracker.httpCall mailbox
    
module AnnouncerExtensions =
    type IActorContext with
        member __.GetAnnouncer() : IActorRef = __.Child(Announcer.actorName ())
