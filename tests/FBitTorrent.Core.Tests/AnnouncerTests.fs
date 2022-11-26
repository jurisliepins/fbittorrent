namespace FBitTorrent.Core.Tests

open System
open System.Collections
open Akka.FSharp
open Akka.TestKit.Xunit2
open FBitTorrent.BEncode
open FBitTorrent.Core
open Xunit

type AnnouncerTests() = 
    inherit TestKit()

    let [<Literal>] SuccessAnnouncerName = "success-announcer"
    
    let [<Literal>] FailureAnnouncerName = "failure-announcer"
    
    let successAnnouncerFn mailbox =
        let call _ =
            Map.empty
                .Add(BValue.bstr "complete", BValue.bint 1330L)
                .Add(BValue.bstr "incomplete", BValue.bint 20L)
                .Add(BValue.bstr "interval", BValue.bint 0L)
                .Add(BValue.bstr "peers", BValue.blist [])
            |> BValue.bdict
            |> BEncode.defaultToBytes
        Announcer.actorFn call mailbox
        
    let failureAnnouncerFn mailbox =
        let call _ =
            Map.empty
                .Add(BValue.bstr "failure reason", BValue.bstr "Test failure")
            |> BValue.bdict
            |> BEncode.defaultToBytes
        Announcer.actorFn call mailbox

    let announceCommand = Announcer.Announce (Constants.Announce, [||], [||], 0, 0L, 0L, 0L, (Some Tracker.Event.Started), None)
    
    let scheduleAnnounceCommand = Announcer.ScheduleAnnounce (Constants.Announce, [||], [||], 0, 0L, 0L, 0L, (Some Tracker.Event.Started), None, 0)
    
    let assertSuccess (commandResult: Announcer.CommandResult) =
        match commandResult with
        | Announcer.Success (complete, incomplete, interval, peers, eventOpt) ->
            Assert.Equal(Some 1330L, complete)
            Assert.Equal(Some 20L, incomplete)
            Assert.Equal(0L, interval)
            Assert.Equal<IEnumerable>([], peers)
            Assert.Equal(Some Tracker.Event.Started, eventOpt)
        | Announcer.Failure _ ->
            Assert.False(true, "Announce should have succeeded")
    
    let assertFailure (commandResult: Announcer.CommandResult) =
        match commandResult with
        | Announcer.Success _ ->
            Assert.False(true, "Should have failed to announce")
        | Announcer.Failure (exn, eventOpt) ->
            Assert.Equal("Failed to announce", exn.Message)
            Assert.Equal((Some Tracker.Started), eventOpt)
        
    [<Fact>]
    member __.``Test should announce succeed`` () =
        let announcerRef = spawn __.Sys SuccessAnnouncerName successAnnouncerFn
        let announcerCommandResult = announcerRef.Ask(announceCommand, TimeSpan.FromSeconds 3) |> Async.RunSynchronously
        assertSuccess announcerCommandResult
        
    [<Fact>]
    member __.``Test should announce fail`` () =
        let announcerRef = spawn __.Sys FailureAnnouncerName failureAnnouncerFn
        let announcerCommandResult = announcerRef.Ask<Announcer.CommandResult>(announceCommand, TimeSpan.FromSeconds 3) |> Async.RunSynchronously
        assertFailure announcerCommandResult 
        
    [<Fact>]
    member __.``Test should schedule announce succeed`` () =
        let announcerRef = spawn __.Sys SuccessAnnouncerName successAnnouncerFn
        let announcerCommandResult = announcerRef.Ask(scheduleAnnounceCommand, TimeSpan.FromSeconds 3) |> Async.RunSynchronously
        assertSuccess announcerCommandResult
        
    [<Fact>]
    member __.``Test should schedule announce fail`` () =
        let announcerRef = spawn __.Sys FailureAnnouncerName failureAnnouncerFn
        let announcerCommandResult = announcerRef.Ask<Announcer.CommandResult>(scheduleAnnounceCommand, TimeSpan.FromSeconds 3) |> Async.RunSynchronously
        assertFailure announcerCommandResult