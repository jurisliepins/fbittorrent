namespace FBitTorrent.Core.Tests

open System.IO

module Constants =
    let announce = "https://torrent.ubuntu.com/announce"
    
    let singleFileMetaInfoPath = ".\\single_file.torrent"
    let multiFileMetaInfoPath = ".\\multi_file.torrent"
    
    let singleFileTorrentPath = ".\\war_and_peace.txt"
    let multiFileTorrentPath1 = ".\\war_and_peace\\file_1.txt"
    let multiFileTorrentPath2 = ".\\war_and_peace\\file_2.txt"
    let multiFileTorrentPath3 = ".\\war_and_peace\\file_3.txt"
    let multiFileTorrentPath4 = ".\\war_and_peace\\file_4.txt"
    
    let singleFileMetaInfoBytes = File.ReadAllBytes(singleFileMetaInfoPath)
    let multiFileMetaInfoBytes  = File.ReadAllBytes(multiFileMetaInfoPath)
    