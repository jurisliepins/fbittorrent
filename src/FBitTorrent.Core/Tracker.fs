namespace FBitTorrent.Core

open System
open System.Net
open System.Net.Http
open System.Text
open System.Web
open FBitTorrent.BEncode
open FBitTorrent.Core

module Tracker =
    
    let DefaultEncoding = Encoding.Latin1
    
    type Event =
        | Started
        | Stopped
        | Completed
    with
        override __.ToString() =
            match __ with
            | Started   -> "started"
            | Stopped   -> "stopped"
            | Completed -> "completed"
    
    type Request =
        { InfoHash:   byte[]
          PeerId:     byte[]
          Port:       int
          Uploaded:   int64
          Downloaded: int64
          Left:       int64
          Compact:    bool
          NoPeerId:   bool
          Event:      Event option
          Ip:         string option
          NumWant:    int option
          Key:        string option
          TrackerId:  string option }
    with
        member __.GetEncodedInfoHash() =
            $"info_hash=%s{HttpUtility.UrlEncode(__.InfoHash).ToString()}"
        
        member __.GetEncodedPeerId() =
            $"peer_id=%s{HttpUtility.UrlEncode(__.PeerId).ToString()}"
        
        member __.GetEncodedPort() =
            $"port=%s{HttpUtility.UrlEncode(__.Port.ToString()).ToString()}"

        member __.GetEncodedUploaded() =
            $"uploaded=%s{HttpUtility.UrlEncode(__.Uploaded.ToString()).ToString()}"
        
        member __.GetEncodedDownloaded() =
            $"downloaded=%s{HttpUtility.UrlEncode(__.Downloaded.ToString()).ToString()}"
        
        member __.GetEncodedLeft() =
            $"left=%s{HttpUtility.UrlEncode(__.Left.ToString()).ToString()}"
        
        member __.GetEncodedCompact() =
            match __.Compact with | true -> "compact=1" | false -> "compact=0"
        
        member __.GetEncodedNoPeerId() =
            match __.NoPeerId with | true -> "no_peer_id" | false -> ""
                
        member __.GetEncodedEvent() =
            match __.Event with
            | Some value ->
                $"event=%s{HttpUtility.UrlEncode(value.ToString()).ToString()}"
            | None -> ""
            
        member __.GetEncodedIp() =
            match __.Ip with
            | Some value ->
                $"ip=%s{HttpUtility.UrlEncode(value.ToString()).ToString()}"
            | None -> ""
            
        member __.GetEncodedNumWant() =
            match __.NumWant with
            | Some value ->
                $"numwant=%s{HttpUtility.UrlEncode(value.ToString()).ToString()}"
            | None -> ""
            
        member __.GetEncodedKey() =
            match __.Key with
            | Some value ->
                $"key=%s{HttpUtility.UrlEncode(value.ToString()).ToString()}"
            | None -> ""
            
        member __.GetEncodedTrackerId() =
            match __.TrackerId with
            | Some value ->
                $"trackerid=%s{HttpUtility.UrlEncode(value.ToString()).ToString()}"
            | None -> ""
            
        member __.GetEncoded() =
            [ __.GetEncodedInfoHash()
              __.GetEncodedPeerId()
              __.GetEncodedPort()
              __.GetEncodedUploaded()
              __.GetEncodedDownloaded()
              __.GetEncodedLeft()
              __.GetEncodedCompact()
              __.GetEncodedNoPeerId()
              __.GetEncodedEvent()
              __.GetEncodedIp()
              __.GetEncodedNumWant()
              __.GetEncodedKey()
              __.GetEncodedTrackerId() ]
            |> List.filter
                   (String.IsNullOrEmpty >> not)
            |> String.concat "&"

    type Response =
        { Complete:       int64 option
          Incomplete:     int64 option
          Interval:       int64
          MinInterval:    int64 option
          Peers:          IPEndPoint list
          TrackerId:      string option
          WarningMessage: string option }

    let private unpackPeerList list =
        let rec unpack list peers = 
            match list with
            | value::values ->
                match value with
                | BDictionary dict ->
                    let ip = dict |> BValue.unpackStr "ip" |> IPAddress.Parse
                    let port = dict |> BValue.unpackInt32 "port"
                    unpack values (IPEndPoint(ip, port)::peers)
                | _ -> unpack values peers
            | [] -> peers
        unpack list []

    let private unpackPeerStr bytes =
        let rec unpack (bytes: byte[]) peers =
            match bytes with
            | bytes when bytes.Length > 0 ->
                let ip = IPAddress(bytes[0..3])
                let port = BigEndianConverter.toUInt16 (bytes[4..5]) |> int
                unpack bytes[6..] (IPEndPoint(ip, port)::peers)
            | _ -> peers
        unpack bytes []
    
    let httpCall (url: string) =
        let client = new HttpClient()
        client.DefaultRequestHeaders.UserAgent.ParseAdd($"%s{Application.Name}/%.2f{Application.Version}")
        client.DefaultRequestHeaders.Accept.ParseAdd("*/*")
        client.DefaultRequestHeaders.Connection.ParseAdd("keep-alive")
        let response =
            client.GetAsync(url)
            |> Async.AwaitTask
            |> Async.RunSynchronously
        response
            .EnsureSuccessStatusCode().Content
            .ReadAsByteArrayAsync()
        |> Async.AwaitTask
        |> Async.RunSynchronously
    
    let parse (bytes: byte[]) =
        match BDecode.fromBytes DefaultEncoding bytes with
        | BDictionary dict ->
            let parsePeers value =
                match value with
                | BList   packed -> unpackPeerList packed
                | BString packed -> unpackPeerStr (DefaultEncoding.GetBytes(packed))
                | value ->
                    failwith $"Unexpected 'peer' value %A{value} - expected %A{BListType} or %A{BStringType}"
            match dict |> BValue.unpackStrOpt "failure reason" with
            | None ->
                { WarningMessage = dict |> BValue.unpackStrOpt "warning message"
                  Interval       = dict |> BValue.unpackInt64  "interval"
                  MinInterval    = dict |> BValue.unpackIntOpt "min interval"
                  TrackerId      = dict |> BValue.unpackStrOpt "tracker id"
                  Complete       = dict |> BValue.unpackIntOpt "complete"
                  Incomplete     = dict |> BValue.unpackIntOpt "incomplete"
                  Peers          = dict |> BValue.unpackWith   "peers" parsePeers }
            | Some failureReason ->
                failwith $"Server responded with failure reason - %s{failureReason}"
        | value ->
            failwith $"Unexpected encoded response type %A{value} - expected %A{BDictionaryType}"
        
    let announce (url: string) (request: Request) call =
        match UriBuilder(url) with
        | builder when
            builder.Scheme.Equals "http" ||
            builder.Scheme.Equals "https" ->
            builder.Query <- request.GetEncoded()
            parse (call (builder.ToString()))
        | builder ->
            failwith $"Unsupported protocol %A{builder.Scheme}"

    let defaultAnnounce url ih pid port uploaded downloaded left event numWant call =
        let request =
            { InfoHash   = ih
              PeerId     = pid
              Port       = port
              Uploaded   = uploaded
              Downloaded = downloaded
              Left       = left
              Compact    = true
              NoPeerId   = false
              Event      = event 
              Ip         = None
              NumWant    = numWant
              Key        = None
              TrackerId  = None }
        announce url request call