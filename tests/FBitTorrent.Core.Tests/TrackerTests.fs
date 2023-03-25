namespace FBitTorrent.Core.Tests

open System
open System.Net
open System.Text
open Xunit
open FBitTorrent.BEncode
open FBitTorrent.Core

module TrackerTests =
    open FBitTorrent.Core.Tracker
  
    let peerNotCompact =
        (BValue.bdict (
            Map.empty
               .Add(BValue.bstr "ip", BValue.bstr "5.18.147.143")
               .Add(BValue.bstr "peer id", BValue.bstr "-TR3000-8bvvpy39oirv")
               .Add(BValue.bstr "port", BValue.bint 60446L)))
    
    let peerCompact =
        [| 5uy; 18uy; 147uy; 143uy; 236uy; 30uy |]
    
    let notCompactResponse =
        Map.empty
            .Add(BValue.bstr "interval", BValue.bint 1800L)
            .Add(BValue.bstr "peers", BValue.blist [peerNotCompact])
    
    let compactResponse =
        Map.empty
            .Add(BValue.bstr "interval", BValue.bint 1800L)
            .Add(BValue.bstr "peers", BValue.bstr (Encoding.Latin1.GetString(peerCompact)) )
    
    let failureResponse =
        Map.empty
            .Add(BValue.bstr "failure reason", BValue.bstr "Test failure")
    
    let assertTrackerResponse (response: Response) =
        Assert.Equal(None, response.Complete)
        Assert.Equal(None, response.Incomplete)
        Assert.Equal(1800L, response.Interval)
        Assert.Equal(None, response.MinInterval)
        Assert.Equal(1, response.Peers.Length)
        Assert.Equal(IPAddress.Parse "5.18.147.143", response.Peers.Head.Address)
        Assert.Equal(60446, response.Peers.Head.Port)
        Assert.Equal(None, response.TrackerId)
        Assert.Equal(None, response.WarningMessage)
    
    [<Fact>]
    let ``Test tracker announce success with peer list not compact`` () =
        let query _ =
            notCompactResponse
            |> BValue.bdict
            |> BEncode.defaultToBytes
        let response = defaultAnnounce Constants.Announce (Hash [||]) (PeerId [||]) 0 0L 0L 0L (Some Tracker.Event.Started) (Some 25) query
        assertTrackerResponse response
        
    [<Fact>]
    let ``Test tracker announce success with peer list compact`` () =
        let query _ =
            compactResponse
            |> BValue.bdict
            |> BEncode.defaultToBytes
        let response = defaultAnnounce Constants.Announce (Hash [||]) (PeerId [||]) 0 0L 0L 0L (Some Tracker.Event.Started) (Some 25) query
        assertTrackerResponse response
            
    [<Fact>]
    let ``Test tracker announce failure`` () =
        let query _ =
            failureResponse
            |> BValue.bdict
            |> BEncode.defaultToBytes
        Assert.ThrowsAny<Exception>(fun () ->
            defaultAnnounce Constants.Announce (Hash [||]) (PeerId [||]) 0 0L 0L 0L (Some Tracker.Event.Started) (Some 25) query |> ignore)