namespace FBitTorrent.Cli

open System
open System.Runtime.InteropServices
open Akka.FSharp
open Akka.Configuration
open FBitTorrent.Core

module ActorSystem = Akka.FSharp.System

module Program =
    let private runClient () =
        let config =
            """
            akka { loglevel = off }
            """
            |> ConfigurationFactory.ParseString
        let system = ActorSystem.create "fbittorrent-system" config
        let clientRef = Client.spawn system (Client.createState ())
        Repl.repl system clientRef
        
    [<EntryPoint>]
    let main args =
        Console.WriteLine($"%s{Constants.AppName} v%.2f{Constants.AppVersion} [%A{RuntimeInformation.ProcessArchitecture}] on %A{RuntimeInformation.OSDescription}")
        try
            runClient ()
        with exn ->
            Console.WriteLine($"%A{exn}")
        0