namespace FBitTorrent.Core.Tests

open System
open System.Collections
open Akka.FSharp
open Akka.TestKit.Xunit2
open FBitTorrent.BEncode
open FBitTorrent.Core
open Xunit

// type AnnouncerTests() = 
//     inherit TestKit()
//
//     let successAnnouncerFn mailbox =
//         let call _ =
//             Map.empty
//                 .Add(BValue.bstr "complete", BValue.bint 1330L)
//                 .Add(BValue.bstr "incomplete", BValue.bint 20L)
//                 .Add(BValue.bstr "interval", BValue.bint 0L)
//                 .Add(BValue.bstr "peers", BValue.blist [])
//             |> BValue.bdict
//             |> BEncode.defaultToBytes
//         Announcer.actorFn call mailbox
//         
//     let failureAnnouncerFn mailbox =
//         let call _ =
//             Map.empty
//                 .Add(BValue.bstr "failure reason", BValue.bstr "Test failure")
//             |> BValue.bdict
//             |> BEncode.defaultToBytes
//         Announcer.actorFn call mailbox
//
//     let announceCommand =
//         Announcer.Announce {
//            Url        = Constants.Announce
//            InfoHash   = Hash [||]
//            PeerId     = PeerId [||]
//            Port       = 0
//            Downloaded = 0L
//            Uploaded   = 0L
//            Left       = 0L
//            Event      = (Some Tracker.Event.Started)
//            NumWant    = None }
//     
//     let scheduleAnnounceCommand =
//         Announcer.ScheduleAnnounce ({
//            Url        = Constants.Announce
//            InfoHash   = Hash [||]
//            PeerId     = PeerId [||]
//            Port       = 0
//            Downloaded = 0L
//            Uploaded   = 0L
//            Left       = 0L
//            Event      = (Some Tracker.Event.Started)
//            NumWant    = None }, 
//            0)
//     
//     let assertSuccess (commandResult: Announcer.CommandResult) =
//         match commandResult with
//         | Announcer.Success ({
//             Complete   = complete
//             Incomplete = incomplete
//             Interval   = interval
//             Peers      = peers
//             Event      = eventOpt }) ->
//             Assert.Equal(Some 1330L, complete)
//             Assert.Equal(Some 20L, incomplete)
//             Assert.Equal(0L, interval)
//             Assert.Equal<IEnumerable>([], peers)
//             Assert.Equal(Some Tracker.Event.Started, eventOpt)
//         | Announcer.Failure _ ->
//             Assert.False(true, "Announce should have succeeded")
//     
//     let assertFailure (commandResult: Announcer.CommandResult) =
//         match commandResult with
//         | Announcer.Failure (exn, eventOpt) ->
//             Assert.Contains("Failed to announce on", exn.Message)
//             Assert.Equal((Some Tracker.Started), eventOpt)
//         | Announcer.Success _ ->
//             Assert.False(true, "Announce should have failed")
//         
//     [<Fact>]
//     member __.``Test should announce succeed``() =
//         let announcerRef = spawn __.Sys (Announcer.actorName ()) successAnnouncerFn
//         let announcerCommandResult = announcerRef.Ask(announceCommand, TimeSpan.FromSeconds 3) |> Async.RunSynchronously
//         assertSuccess announcerCommandResult
//         
//     [<Fact>]
//     member __.``Test should announce fail``() =
//         let announcerRef = spawn __.Sys (Announcer.actorName ()) failureAnnouncerFn
//         let announcerCommandResult = announcerRef.Ask<Announcer.CommandResult>(announceCommand, TimeSpan.FromSeconds 3) |> Async.RunSynchronously
//         assertFailure announcerCommandResult 
//         
//     [<Fact>]
//     member __.``Test should schedule announce succeed``() =
//         let announcerRef = spawn __.Sys (Announcer.actorName ()) successAnnouncerFn
//         let announcerCommandResult = announcerRef.Ask(scheduleAnnounceCommand, TimeSpan.FromSeconds 3) |> Async.RunSynchronously
//         assertSuccess announcerCommandResult
//         
//     [<Fact>]
//     member __.``Test should schedule announce fail``() =
//         let announcerRef = spawn __.Sys (Announcer.actorName ()) failureAnnouncerFn
//         let announcerCommandResult = announcerRef.Ask<Announcer.CommandResult>(scheduleAnnounceCommand, TimeSpan.FromSeconds 3) |> Async.RunSynchronously
//         assertFailure announcerCommandResult