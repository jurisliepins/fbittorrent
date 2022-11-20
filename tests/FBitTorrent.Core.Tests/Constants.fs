namespace FBitTorrent.Core.Tests

open System.IO
open FBitTorrent.Core

module Constants =
    let announce = "https://torrent.ubuntu.com/announce"
    
    let singleFileMetaInfoPath = "./single_file.torrent"
    let multiFileMetaInfoPath = "./multi_file.torrent"
    
    let singleFileTorrentPath = "./war_and_peace.txt"
    let multiFileTorrentPath1 = "./war_and_peace/file_1.txt"
    let multiFileTorrentPath2 = "./war_and_peace/file_2.txt"
    let multiFileTorrentPath3 = "./war_and_peace/file_3.txt"
    let multiFileTorrentPath4 = "./war_and_peace/file_4.txt"
    
    let singleFileMetaInfoBytes = File.ReadAllBytes(singleFileMetaInfoPath)
    let multiFileMetaInfoBytes  = File.ReadAllBytes(multiFileMetaInfoPath)
    
    let singleFileMetaInfo = singleFileMetaInfoBytes |> MetaInfo.fromBytes
    let multiFileMetaInfo  = multiFileMetaInfoBytes |> MetaInfo.fromBytes
    
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
            File.ReadAllBytes(singleFileTorrentPath)
            |> Array.chunkBySize sfi.PieceLength
        | _ ->
            failwith "Should have loaded single file torrent"
            
    let multiFilePieces =
        match multiFileMetaInfo.Info with
        | MultiFileInfo mfi ->
            Array.concat [|
                File.ReadAllBytes(multiFileTorrentPath1)
                File.ReadAllBytes(multiFileTorrentPath2)
                File.ReadAllBytes(multiFileTorrentPath3)
                File.ReadAllBytes(multiFileTorrentPath4) |]
            |> Array.chunkBySize mfi.PieceLength
        | _ ->
            failwith "Should have loaded multi file torrent"