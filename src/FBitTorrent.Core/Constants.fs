namespace FBitTorrent.Core

open System
open System.Net
open System.Text

module Constants =
    let ApplicationName = "FBitTorrent"
    let ApplicationVersion = 0.01
    
    module MetaInfo =
        let Encoding = Encoding.Latin1

    module Tracker =
        let Encoding = Encoding.Latin1

    module Handshake =
        let ProtocolBytes = [| 66uy; 105uy; 116uy; 84uy; 111uy; 114uy; 114uy; 101uy; 110uy; 116uy; 32uy;
                               112uy; 114uy; 111uy; 116uy; 111uy; 99uy; 111uy; 108uy |]
        let ReservedBytes = [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; |]

        let Encoding = Encoding.Latin1
        
        let DefaultWriteTimeoutMillis = 5_000
        let DefaultReadTimeoutMillis = 5_000
        
    module Message =
        let Encoding = Encoding.Latin1
        
        let DefaultWriteTimeoutMillis = 120_000
        let DefaultReadTimeoutMillis = 120_000
    
    module Block =
        let BlockLength = 16384
        
    module Rate =
        let Smoothing = 0.02
        let Capacity = 5
        
    module Announcer =
        let ActorNamePref = "announcer"
        
    module Connector =
        let ActorNamePref = "connector"
        
    module Pieces =
        let ActorNamePref = "pieces"
        
    module Peer =
        module Reader = let ActorNamePref = "reader"
        module Writer = let ActorNamePref = "writer"
        
        let Id = Encoding.ASCII.GetBytes(
           $"-FX%.2f{ApplicationVersion}-%012d{Random().NextInt64(0L, 1000000000000L)}")
        
        let ActorNamePref = "peer"
        
        let KeepAliveIntervalMillis = 30_000
        
    module Torrent =
        let ActorNamePref = "torrent"
        
        let DefaultPeerLimit = 30
        let DefaultAddress = IPAddress.Any
        let DefaultPort = 6881
        
    module Client =
        let ActorNamePref = "client"