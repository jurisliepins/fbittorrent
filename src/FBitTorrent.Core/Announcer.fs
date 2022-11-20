namespace FBitTorrent.Core

open System
open System.Net
open Akka.FSharp.Actors

module Announcer =
    open Tracker
    
    type Command =
        | Announce         of string * byte[] * byte[] * int * int64 * int64 * int64 * Event option * int option
        | ScheduleAnnounce of string * byte[] * byte[] * int * int64 * int64 * int64 * Event option * int option * int 
    
    type CommandResult =
        | Success of int64 option * int64 option * int64 * IPEndPoint list * Tracker.Event option 
        | Failure of Exception * Tracker.Event option
    
    let actorName () = "announcer"
    
    let actorFn call (mailbox: Actor<obj>) =
        let rec receive () = actor {
            match! mailbox.Receive() with
            | :? Command as command -> 
                return! handleCommand command
            
            | message ->
                mailbox.Unhandled message
                return! receive () }
        
        and handleCommand command =
            match command with
            | Announce (url, ih, pid, port, downloaded, uploaded, left, eventOpt, numWantOpt) ->
                try
                    match defaultAnnounce url ih pid port downloaded uploaded left eventOpt numWantOpt call with
                    | { Complete   = complete
                        Incomplete = incomplete
                        Interval   = interval
                        Peers      = peers } -> 
                        mailbox.Context.Sender <! Success (complete, incomplete, interval, peers, eventOpt)
                with exn -> 
                    mailbox.Context.Sender <! Failure (Exception("Failed to announce", exn), eventOpt)
                receive ()
            
            | ScheduleAnnounce (url, ih, pid, port, downloaded, uploaded, left, eventOpt, numWantOpt, after) ->
                mailbox.Context.System.Scheduler.ScheduleTellOnce(
                    TimeSpan.FromSeconds(float after), mailbox.Self,
                        Announce (url, ih, pid, port, downloaded, uploaded, left, eventOpt, numWantOpt), mailbox.Context.Sender)
                receive () 
        
        receive ()
        
    let httpActorFn mailbox = actorFn httpCall mailbox
    
    let defaultActorFn mailbox = httpActorFn mailbox