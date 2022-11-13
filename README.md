FBitTorrent
===========

# What and why?

This is an experimental implementation of the BitTorrent protocol in F# using Akka.NET written for fun :slightly_smiling_face:. 

## Supported BEPs

* [BEP-3: The BitTorrent Protocol Specification](http://bittorrent.org/beps/bep_0003.html)
* [BEP-20: Peer ID Conventions](http://bittorrent.org/beps/bep_0020.html)
* [BEP-23: Tracker Returns Compact Peer Lists](http://bittorrent.org/beps/bep_0023.html)

## TODOs

* Persistence - right now the client will not persist state and will not know where it left off after a restart.
* Throttling - no way to limit bandwidth usage. 
* Seeding - only leeching is supported.

# Usage

## Building

Run `build.sh` or `build.cmd`.

[Packet](https://fsprojects.github.io/Paket/get-started.html) is used for dependency management and [FAKE](https://fake.build/guide/getting-started.html) is used to compile the application. 

## Running

## Testing
