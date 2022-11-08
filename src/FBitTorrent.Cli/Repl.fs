namespace FBitTorrent.Cli

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open Akka.Actor
open Akka.FSharp
open FBitTorrent.Core

module Repl =
    let private parseCommand command =
        Regex.Split(command, " (?=(?:[^\"]*\"[^\"]*\")*(?![^\"]*\"))")
        |> List.ofArray
        |> List.map (fun value -> value.Replace("\"", ""))
    
    let [<Literal>] CommandAdd = "add"
    let [<Literal>] CommandRemove = "remove"
    let [<Literal>] CommandStart = "start"
    let [<Literal>] CommandStop = "stop"
    let [<Literal>] CommandGet = "get"
    let [<Literal>] CommandWatch = "watch"
    let [<Literal>] CommandExit = "exit"
    let [<Literal>] CommandHelp = "help"
    
    let private parseAddArgs (args: string list) =
        match args with | pathArg::_ -> pathArg | _ -> failwith $"Invalid '%s{CommandAdd}' command args"
        
    let private parseRemoveArgs (args: string list) =
        match args with | ihArg::_ -> Hash ihArg | _ -> failwith $"Invalid '$%s{CommandRemove}' command args"
    
    let private parseStartArgs (args: string list) =
        match args with | ihArg::_ -> Hash ihArg | _ -> failwith $"Invalid '$%s{CommandStart}' command args"
        
    let private parseStopArgs (args: string list) =
        match args with | ihArg::_ -> Hash ihArg | _ -> failwith $"Invalid '$%s{CommandStop}' command args"
        
    let private parseGetArgs (args: string list) =
        match args with | [] -> None | ihArg::_ -> Some (Hash ihArg)
        
    let private parseWatchArgs (args: string list) =
        match args with | ihArg::_ -> Hash ihArg | _ -> failwith $"Invalid '$%s{CommandWatch}' command args"
    
    let rec repl (system: ActorSystem) (clientRef: IActorRef) =
        Console.Write(">>> ")
        try
            match parseCommand (Console.ReadLine()) with
            | CommandAdd    ::args -> add    system clientRef (parseAddArgs    args)
            | CommandRemove ::args -> remove system clientRef (parseRemoveArgs args)
            | CommandStart  ::args -> start  system clientRef (parseStartArgs  args)
            | CommandStop   ::args -> stop   system clientRef (parseStopArgs   args)
            | CommandGet    ::args -> get    system clientRef (parseGetArgs    args)
            | CommandWatch  ::args -> watch  system clientRef (parseWatchArgs  args)
            | CommandExit   ::_    -> exit   system clientRef
            | CommandHelp   ::_    -> help   system clientRef
            | _ ->
                failwith "Unknown command"
        with exn ->
            Console.WriteLine($"%s{exn.Message}")
            repl system clientRef

    and add system clientRef (path: string) =
        let mi = MetaInfo.fromBytes (File.ReadAllBytes(path))
        let message = Op.add clientRef (Torrent.createStateFromMetaInfo Torrent.defaultSettings mi) |> Async.RunSynchronously
        Console.WriteLine($"%s{message} %A{(MetaInfo.infoHash mi.Info)}")
        repl system clientRef
    
    and remove system clientRef (ih: Hash) =
        let message = Op.remove clientRef ih |> Async.RunSynchronously
        Console.WriteLine($"%s{message}")
        repl system clientRef
    
    and start system clientRef (ih: Hash) =
        let message = Op.start clientRef ih |> Async.RunSynchronously
        Console.WriteLine($"%s{message}")
        repl system clientRef
    
    and stop system clientRef (ih: Hash) =
        let message = Op.stop clientRef ih |> Async.RunSynchronously
        Console.WriteLine($"%s{message}")
        repl system clientRef
    
    and get system clientRef (ihOpt: Hash option) =
        let formatString (str: string) =
            match str with
            | _ when str.Length > 40 ->
                String.Format("{0}...", str.Substring(0, 37))
            | _ -> str
        let formatBytes (bytes: int64) = String.Format("{0} B", bytes)
        let formatPercentage (percentage: float) = String.Format("{0:F2}%", percentage)
        let print (torrents: Client.Torrent list) =
            match torrents with
            | []       -> ()
            | torrents -> 
                Console.WriteLine("+" + new string('-', 55) + "+")
                for torrent in torrents do
                    Console.Write(StringBuilder()
                        .AppendLine(String.Format("| {0,-10} | {1,-40} |", formatString (torrent.Status.ToString()), (formatString (torrent.Name))))
                        .AppendLine(String.Format("|" + new string('-', 55) + "|"))
                        .AppendLine(String.Format("| {0,-10} : {1,40} |", "Info Hash", formatString (torrent.InfoHash.ToString())))
                        .AppendLine(String.Format("| {0,-10} : {1,40} |", "Peer ID", formatString (torrent.PeerId.ToString())))
                        .AppendLine(String.Format("|" + new string('-', 55) + "|"))
                        .AppendLine(String.Format("| {0,-10} : {1,40} |", "Length", formatBytes torrent.Length))
                        .AppendLine(String.Format("| {0,-10} : {1,40} |", "Downloaded", formatBytes torrent.Downloaded))
                        .AppendLine(String.Format("| {0,-10} : {1,40} |", "Uploaded", formatBytes torrent.Uploaded))
                        .AppendLine(String.Format("| {0,-10} : {1,40} |", "Complete", formatPercentage ((float torrent.Length - float torrent.Left) / float torrent.Length * 100.0)))
                        .AppendLine(String.Format("| {0,-10} : {1,40} |", "Down speed", torrent.DownRate))
                        .AppendLine(String.Format("| {0,-10} : {1,40} |", "Up speed", torrent.UpRate)))
                    Console.WriteLine("+" + new string('-', 55) + "+")
        match ihOpt with
        | Some ih -> print (Op.get clientRef (Some ih) |> Async.RunSynchronously)
        | None    -> print (Op.get clientRef None |> Async.RunSynchronously)
        repl system clientRef
        
    and watch system clientRef (ih: Hash) =
        let print (torrentOpt: Client.WatchedTorrent option) =
            match torrentOpt with
            | Some torrent ->
                let percentage = int ((float torrent.Length - float torrent.Left) / float torrent.Length * 100.0)
                Console.SetCursorPosition(0, Console.CursorTop)
                Console.Write(new string(' ', Console.WindowWidth - 1))
                Console.SetCursorPosition(0, Console.CursorTop)
                Console.Write(StringBuilder()
                    .Append($"[%A{torrent.Status}] ")
                    .Append($"[%s{new string('#', percentage)}%s{new string('-', 100 - percentage)}]")
                    .Append(" " + ("|/-\\".ToCharArray()[DateTime.Now.Second % 4]).ToString())
                    .Append($" %d{percentage}%% [Down speed: %s{torrent.DownRate.ToString()} | Up speed: %s{torrent.UpRate.ToString()}]")) 
            | None ->
                Console.SetCursorPosition(0, Console.CursorTop)
                Console.Write(new string(' ', Console.WindowWidth - 1))
                Console.WriteLine("Torrent has been removed")
        match Op.get clientRef (Some ih) |> Async.RunSynchronously with
        | [] ->
            Console.WriteLine("Torrent not found")
        | torrent::_ -> 
            print (Some (Client.createWatchedTorrentFromTorrent torrent))
            let watcherRef = spawn system "watcher" (actorOf (fun (notification: Client.WatcherNotification) ->
                match notification with
                | Client.WatchedTorrentChanged torrentOpt -> print torrentOpt))
            Op.attachWatcher clientRef ih watcherRef |> Async.RunSynchronously |> ignore
            Console.ReadKey() |> ignore
            Console.SetCursorPosition(0, Console.CursorTop)
            Console.Write(new string(' ', Console.WindowWidth - 1))
            Console.SetCursorPosition(0, Console.CursorTop)
            Op.detachWatcher clientRef ih |> Async.RunSynchronously |> ignore
            system.Stop(watcherRef)
        repl system clientRef
    
    and exit system clientRef =
        Console.WriteLine("Exiting")
            
    and help system clientRef =
        Console.WriteLine("Help")
        Console.WriteLine("    add     <torrent-file-path>")
        Console.WriteLine("    remove  <info-hash>")
        Console.WriteLine("    start   <info-hash>")
        Console.WriteLine("    stop    <info-hash>")
        Console.WriteLine("    get     [<info-hash>]")
        Console.WriteLine("    watch   <info-hash>")
        Console.WriteLine("    exit               ")
        Console.WriteLine("    help               ")
        repl system clientRef
