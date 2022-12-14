FBitTorrent
===========

Experimental implementation of the BitTorrent protocol in F# using Akka.NET. 

## Supported BEPs

* [BEP-3: The BitTorrent Protocol Specification](http://bittorrent.org/beps/bep_0003.html)
* [BEP-20: Peer ID Conventions](http://bittorrent.org/beps/bep_0020.html)
* [BEP-23: Tracker Returns Compact Peer Lists](http://bittorrent.org/beps/bep_0023.html)

## TODOs

* Persistence - right now the client will not persist state and will not know where it left off after a restart.
* Throttling - no way to limit bandwidth usage. 
* Seeding - only leeching is supported.
* Other extensions.

# Usage

## Building

Make sure you have (at least) [.NET 6](https://dotnet.microsoft.com/en-us/download/dotnet/6.0) installed and run `build.sh` or `build.cmd`.

[Packet](https://fsprojects.github.io/Paket/get-started.html) is used for dependency management and [FAKE](https://fake.build/guide/getting-started.html) is used to compile the application. 

## Running

## Testing
