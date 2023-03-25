FBitTorrent
===========

Implementation of the BitTorrent protocol in F# using Akka.NET. 

## Details

The client is implemented using [Akka.NET](https://getakka.net), which is the .NET implementation of the [Actor Model](https://en.wikipedia.org/wiki/Actor_model). In its current state the client will allow you to dowload from a `.torrent` file only. Single file and multi file torrents are supported. 

Main optimisations to maximise download speed have been implemented - pipelining, slow start, end game and rare piece selection. Buffer pooling has been implemented to minimise GC collection, which results in a stable memory profile. The client also supports an unlimited number of concurrently running torrents.

Because this is based on Akka.NET there is potential to extend this to deploy torrent actors on remote hosts, while controlling them from a local client. 

## Supported BEPs

* [BEP-3: The BitTorrent Protocol Specification](http://bittorrent.org/beps/bep_0003.html)
* [BEP-20: Peer ID Conventions](http://bittorrent.org/beps/bep_0020.html)
* [BEP-23: Tracker Returns Compact Peer Lists](http://bittorrent.org/beps/bep_0023.html)

## TODOs

* **Persistence**. Right now the client will not persist state and will not know where it left off after a restart.
* **Throttling**. No way to limit bandwidth usage. 
* **Seeding**. Only leeching is supported.
* **Peer selection**. Disconnect from slow peers and prioritise fast one. 
* Other extensions.

# Usage

## Building

Make sure you have [.NET 6](https://dotnet.microsoft.com/en-us/download/dotnet/6.0) installed and run `build.sh` or `build.cmd`. Self contained binary will be produced in the `bin` directory for the OS on which it's compiled. A different [runtime identifier](https://docs.microsoft.com/en-us/dotnet/core/rid-catalog) can be passed in to compile for a different platform.   

[Packet](https://fsprojects.github.io/Paket/get-started.html) is used for dependency management and [FAKE](https://fake.build/guide/getting-started.html) is used to compile the application. 

## Running

To run the client call the CLI binary from `./bin/<your OS runtime identifier>/FBitTorrent.Cli`. This will let you interact with the client through the REPL. Type `help` to get a list of supported commands. 

To add, start and watch progress of a torrent:
```
FBitTorrent v0.01 [X64] on "Darwin 22.2.0 Darwin Kernel Version 22.2.0: Fri Nov 11 02:06:26 PST 2022; root:xnu-8792.61.2~4/RELEASE_ARM64_T8112"
>>> add /Users/jurisliepins/Downloads/ubuntu-22.10-live-server-amd64.iso.torrent
Torrent added 5f48f68d0e7cfe2c4cce3d33104fcffaf6145135
>>> start 5f48f68d0e7cfe2c4cce3d33104fcffaf6145135
Torrent started
>>> watch 5f48f68d0e7cfe2c4cce3d33104fcffaf6145135
[Started] [#---------------------------------------------------------------------------------------------------] / 1% [Down speed: 6.94 MiB/s | Up speed: 0.00 B/s]  

```
