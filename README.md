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
* Seeding - only leeching is supported right now.

# Usage

## Building

## Running

## Testing
