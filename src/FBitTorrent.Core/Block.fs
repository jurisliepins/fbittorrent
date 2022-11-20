namespace FBitTorrent.Core
    
module Block =
    
    let [<Literal>] BlockLength = 16384
    
    [<Struct>] 
    type RequestBlock =
        { Beginning: int
          Length:    int }

    [<Struct>]
    type ResponseBlock =
        { Beginning: int
          Data:      byte[] }
        
    [<Struct>]
    type Request =
        | Requested of RequestedBlock: RequestBlock
        | Pending   of PendingBlock:   RequestBlock

    [<Struct>]
    type Response =
        | Responded of RespondedBlock: ResponseBlock
        | Pending   of PendingBlock:   ResponseBlock
    
    let private computeBlockCount blockLength length =
        if length % blockLength <> 0 then
            (length / blockLength) + 1
        else
            (length / blockLength)
    
    let private computeBlockLength blockLength length beginning =
        if length - beginning < blockLength then length - beginning else blockLength
        
    let private computeOffset blockLength beginning =
        beginning / blockLength
    
    module Requests =
        let create length =
            let map idx _ =
                Request.Pending
                    { Beginning = idx * BlockLength
                      Length    = computeBlockLength BlockLength length (idx * BlockLength) }
            Array.create (computeBlockCount BlockLength length) 0uy
            |> Array.mapi map
            
        let pop (blocks: Request[]) =
            let rec pop idx =
                if idx >= blocks.Length then
                    None, blocks
                else
                    match blocks[idx] with 
                    | Request.Requested _ ->
                        pop (idx + 1)
                    | Request.Pending block ->
                        blocks[idx] <- Request.Requested block
                        Some block, blocks
            pop 0
            
        let reset (requests: Request[]) (responses: Response[]) =
            let rec reset idx =
                if idx >= requests.Length || idx >= responses.Length then
                    requests
                else
                    match requests[idx], responses[idx] with
                    | Request.Requested req, Response.Pending _ ->
                        requests[idx] <- Request.Pending req
                    | _ -> ()
                    reset (idx + 1)
            reset 0
            
        let requestedCount (blocks: Request[]) =
            let rec count idx requestedCount =
                if idx >= blocks.Length then
                    requestedCount
                else
                    match blocks[idx] with
                    | Request.Requested _ -> count (idx + 1) (requestedCount + 1)
                    | Request.Pending   _ -> count (idx + 1) requestedCount 
            count 0 0
            
        let pendingCount (blocks: Request[]) =
            blocks.Length - requestedCount blocks
            
        let isAllRequested (blocks: Request[]) =
            (requestedCount blocks) = blocks.Length
        
    module Responses =
        let create length =
            let map idx _ =
                Response.Pending
                    { Beginning = idx * BlockLength
                      Data      = [||] }
            Array.create (computeBlockCount BlockLength length) 0uy
            |> Array.mapi map

        let push block (blocks: Response[]) =
            match (computeOffset BlockLength block.Beginning) with
            | idx when idx > -1 &&
                       idx < blocks.Length -> 
                blocks[idx] <- Response.Responded block
                blocks
            | _ ->
                blocks
        
        let drain (blocks: Response[]) =
            blocks
            |> Array.map (function
                          | Response.Responded block -> block
                          | _ -> failwith "Failed to drain blocks - not all blocks received")
            |> Array.map (fun block -> block.Data)
        
        let respondedCount (blocks: Response[]) =
            let rec count idx respondedCount =
                if idx >= blocks.Length then
                    respondedCount
                else
                    match blocks[idx] with
                    | Response.Responded _ -> count (idx + 1) (respondedCount + 1)
                    | Response.Pending   _ -> count (idx + 1) (respondedCount)
            count 0 0
            
        let pendingCount (blocks: Response[]) =
            blocks.Length - respondedCount blocks
            
        let isAllResponded (blocks: Response[]) =
            (respondedCount blocks) = blocks.Length

    let isBacklogFilled requests responses backlog =
        if Requests.isAllRequested requests || Responses.isAllResponded responses then
            true
        else
            let unfilledCount = Requests.requestedCount requests - Responses.respondedCount responses
            unfilledCount >= backlog