namespace FBitTorrent.Core

open System
    
[<Struct>] 
type RequestBlock =
    { Beginning: int
      Length:    int }

[<Struct>]
type ResponseBlock =
    { Beginning: int
      Data:      ByteBuffer }
    
[<Struct>]
type BlockRequest =
    | Requested of RequestedBlock: RequestBlock
    | Pending   of PendingBlock:   RequestBlock

[<Struct>]
type BlockResponse =
    | Responded of RespondedBlock: ResponseBlock
    | Pending   of PendingBlock:   ResponseBlock
    
module Block =
    
    let [<Literal>] Length = 16384
    
    let computeBlockCount blockLength length =
        if length % blockLength <> 0 then
            (length / blockLength) + 1
        else
            (length / blockLength)
    
    let computeBlockLength blockLength length beginning =
        if length - beginning < blockLength then length - beginning else blockLength
        
    let computeOffset blockLength beginning =
        beginning / blockLength
    
module BlockRequests =
    let create length =
        let map idx _ =
            BlockRequest.Pending
                { Beginning = idx * Block.Length
                  Length    = Block.computeBlockLength Block.Length length (idx * Block.Length) }
        Array.create (Block.computeBlockCount Block.Length length) 0uy
        |> Array.mapi map
        
    let pop (requests: BlockRequest[]) =
        let rec pop idx =
            if idx >= requests.Length then
                None
            else
                match requests[idx] with 
                | BlockRequest.Requested   _ -> pop (idx + 1)
                | BlockRequest.Pending block ->
                    requests[idx] <- BlockRequest.Requested block
                    Some block
        pop 0
        
    let reset (requests: BlockRequest[]) (responses: BlockResponse[]) =
        let rec reset idx =
            if idx < requests.Length && idx < responses.Length then
                match requests[idx], responses[idx] with
                | BlockRequest.Requested req, BlockResponse.Pending _ ->
                    requests[idx] <- BlockRequest.Pending req
                | _ -> ()
                reset (idx + 1)
        reset 0
        
    let requestedCount (requests: BlockRequest[]) =
        let rec count idx requestedCount =
            if idx >= requests.Length then
                requestedCount
            else
                match requests[idx] with
                | BlockRequest.Requested _ -> count (idx + 1) (requestedCount + 1)
                | BlockRequest.Pending   _ -> count (idx + 1) requestedCount 
        count 0 0
        
    let pendingCount (requests: BlockRequest[]) =
        requests.Length - requestedCount requests
        
    let isAllRequested (requests: BlockRequest[]) =
        (requestedCount requests) = requests.Length
    
module BlockResponses =
    let create length =
        let map idx _ =
            BlockResponse.Pending
                { Beginning = idx * Block.Length
                  Data      = ByteBuffer.Empty }
        Array.create (Block.computeBlockCount Block.Length length) 0uy
        |> Array.mapi map

    let push block (responses: BlockResponse[]) =
        match (Block.computeOffset Block.Length block.Beginning) with
        | idx when idx > -1 && idx < responses.Length -> 
            responses[idx] <- BlockResponse.Responded block
        | _ -> ()
    
    let toPiece (responses: BlockResponse[]) =
        let blocks = responses
                     |> Array.map (function
                                   | BlockResponse.Responded block -> block
                                   | _ -> failwith "Failed to collect responded blocks not all blocks received")
                     |> Array.map (fun block -> block.Data)
        let piece = ByteBuffer(blocks |> Array.sumBy (fun block -> block.Length))
        blocks
        |> Array.fold (fun offset (block: ByteBuffer) ->
            block.CopyTo(piece, offset)
            block.Release()
            offset + block.Length) 0
        |> ignore
        piece
    
    let respondedCount (responses: BlockResponse[]) =
        let rec count idx respondedCount =
            if idx >= responses.Length then
                respondedCount
            else
                match responses[idx] with
                | BlockResponse.Responded _ -> count (idx + 1) (respondedCount + 1)
                | BlockResponse.Pending   _ -> count (idx + 1) (respondedCount)
        count 0 0
        
    let pendingCount (responses: BlockResponse[]) =
        responses.Length - respondedCount responses
        
    let isAllResponded (responses: BlockResponse[]) =
        (respondedCount responses) = responses.Length

[<Struct>]
type BlockPipeline =
    { Timestamp: DateTime
      Rate:      Rate
      Backlog:   int }

module BlockPipeline =

    let [<Literal>] DefaultBacklog = 5
    
    let [<Literal>] BacklogMarginBytesPerSec = 10_000.0
    
    let create () =
        { Timestamp = DateTime.UtcNow
          Rate      = Rate.zero
          Backlog   = DefaultBacklog }
        
    let update (bytes: int64) (pipeline: BlockPipeline) =
        let nextTimestamp = DateTime.UtcNow
        let nextRate = Rate.fromBytePerSecond ((double bytes) / (double (nextTimestamp - pipeline.Timestamp).TotalSeconds))
        let nextBacklog =
            // If the amount of bytes we download keeps increasing, then we can continue to increase the backlog.
            // Once we see that the speed starts to stagnate, then we know we're reached peer's maximum capacity.
            if Rate.toBytesPerSecond pipeline.Rate + BacklogMarginBytesPerSec < Rate.toBytesPerSecond nextRate then
                pipeline.Backlog + 1
            else
                pipeline.Backlog
        { Timestamp = nextTimestamp
          Rate      = nextRate
          Backlog   = nextBacklog }
    
    let reset (pipeline: BlockPipeline) =
        { Timestamp = pipeline.Timestamp
          Rate      = pipeline.Rate
          Backlog   = DefaultBacklog }
        
    let isBacklogFilled requests responses (pipeline: BlockPipeline) =
        if BlockRequests.isAllRequested requests || BlockResponses.isAllResponded responses then
            true
        else
            BlockRequests.requestedCount requests - BlockResponses.respondedCount responses >= pipeline.Backlog