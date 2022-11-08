namespace FBitTorrent.Core.Tests

open System.IO
open FBitTorrent.Core

module Constants =
    let [<Literal>] Announce = "https://torrent.ubuntu.com/announce"
    
    let [<Literal>] SingleFileMetaInfoPath = "./single_file.torrent"
    let [<Literal>] MultiFileMetaInfoPath = "./multi_file.torrent"
    
    let [<Literal>] SingleFileTorrentPath = "war_and_peace.txt"
    let [<Literal>] MultiFileTorrentPath1 = "war_and_peace/file_1.txt"
    let [<Literal>] MultiFileTorrentPath2 = "war_and_peace/file_2.txt"
    let [<Literal>] MultiFileTorrentPath3 = "war_and_peace/file_3.txt"
    let [<Literal>] MultiFileTorrentPath4 = "war_and_peace/file_4.txt"
    
    let singleFileMetaInfoBytes = File.ReadAllBytes(SingleFileMetaInfoPath)
    let multiFileMetaInfoBytes  = File.ReadAllBytes(MultiFileMetaInfoPath)
    
    let singleFileMetaInfo = singleFileMetaInfoBytes |> MetaInfo.fromBytes
    
    let multiFileMetaInfo = multiFileMetaInfoBytes |> MetaInfo.fromBytes
    
    let singleFileInfo =
        match singleFileMetaInfo.Info with
        | SingleFileInfo sfi -> sfi
        | _ ->
            failwith "Should have loaded single file info"
            
    let multiFileInfo =
        match multiFileMetaInfo.Info with
        | MultiFileInfo mfi -> mfi
        | _ ->
            failwith "Should have loaded multi file info"
    
    let singleFileMetaInfoHash =
        [| 170uy; 23uy; 28uy; 167uy; 127uy; 20uy; 245uy; 93uy; 110uy; 194uy
           61uy; 126uy; 149uy; 65uy; 183uy; 121uy; 30uy; 108uy; 56uy; 60uy |]
        |> Hash
    
    let multiFileMetaInfoHash =
        [| 110uy; 84uy; 14uy; 187uy; 201uy; 33uy; 49uy; 19uy; 135uy; 70uy
           35uy; 31uy; 243uy; 228uy; 79uy; 22uy; 95uy; 211uy; 179uy; 115uy |]
        |> Hash
    
    let singleFilePieces =
        match singleFileMetaInfo.Info with
        | SingleFileInfo sfi ->
            File.ReadAllBytes(SingleFileTorrentPath)
            |> Array.chunkBySize sfi.PieceLength
        | _ ->
            failwith "Should have loaded single file torrent"
            
    let multiFilePieces =
        match multiFileMetaInfo.Info with
        | MultiFileInfo mfi ->
            Array.concat [|
                File.ReadAllBytes(MultiFileTorrentPath1)
                File.ReadAllBytes(MultiFileTorrentPath2)
                File.ReadAllBytes(MultiFileTorrentPath3)
                File.ReadAllBytes(MultiFileTorrentPath4) |]
            |> Array.chunkBySize mfi.PieceLength
        | _ ->
            failwith "Should have loaded multi file torrent"