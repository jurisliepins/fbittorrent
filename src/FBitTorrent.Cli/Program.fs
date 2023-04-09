﻿namespace FBitTorrent.Cli

open System
open System.Runtime.InteropServices
open Akka.FSharp
open Akka.Configuration
open FBitTorrent.Core

module Program =
    
    module AkkaSystem = Akka.FSharp.System
    
    let private runClient () =
        let config =
            """
            akka { loglevel = info }
            """
            |> ConfigurationFactory.ParseString
        let system = AkkaSystem.create "fbittorrent-system" config
        let clientRef = spawn system (Client.actorName ()) (Client.defaultActorFn (Client.createState ()))
        Repl.repl system clientRef
        
    [<EntryPoint>]
    let main args =
        Console.WriteLine($"%s{Application.AppName} v%.2f{Application.AppVersion} [%A{RuntimeInformation.ProcessArchitecture}] on %A{RuntimeInformation.OSDescription}")
        try
            runClient ()
        with exn ->
            Console.WriteLine($"%A{exn}")
        0