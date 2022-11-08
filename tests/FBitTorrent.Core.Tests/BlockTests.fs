namespace FBitTorrent.Core.Tests

open System
open System.Threading
open FBitTorrent.Core
open Xunit

module BlockTests =
    [<Fact>]
    let ``Test should compute block count when length divides evenly`` () =
        let blockLength = 100
        let length = 1000
        Assert.Equal(10, Block.computeBlockCount blockLength length)
        
    [<Fact>]
    let ``Test should compute block count when length doesn't divide evenly`` () =
        let blockLength = 100
        let length = 1001
        Assert.Equal(11, Block.computeBlockCount blockLength length)
    
    [<Fact>]
    let ``Test should compute block length when length divides evenly`` () =
        let blockLength = 100
        let length = 1000
        for beginning in 0..blockLength..length do
            if beginning <> 1000 then
                Assert.Equal(100, Block.computeBlockLength blockLength length beginning)
            else
                Assert.Equal(0, Block.computeBlockLength blockLength length beginning)
    
    [<Fact>]
    let ``Test should compute block length when length doesn't divide evenly`` () =
        let blockLength = 100
        let length = 1001
        for beginning in 0..blockLength..length do
            if beginning <> 1000 then
                Assert.Equal(100, Block.computeBlockLength blockLength length beginning)
            else
                Assert.Equal(1, Block.computeBlockLength blockLength length beginning)
        
    [<Fact>]
    let ``Test should compute offset`` () =
        let blockLength = 100
        for beginning in 0..blockLength..1000 do
            Assert.Equal(beginning / blockLength, Block.computeOffset blockLength beginning)

module BlockRequestsTests = 
    [<Fact>]
    let ``Test block request should create with no remainder`` () =
        let blocks = BlockRequests.create (Block.Length * 4)
        Assert.Equal(((Block.Length * 4) / Block.Length), blocks.Length)
        
    [<Fact>]
    let ``Test block request should create with remainder`` () =
        let blocks = BlockRequests.create ((Block.Length * 4) + 1)
        Assert.Equal(((Block.Length * 4) / Block.Length) + 1, blocks.Length)

    [<Fact>]
    let ``Test should pop request block when pending block exists`` () =
        let blocks = BlockRequests.create (Block.Length * 4)
        match BlockRequests.pop blocks with
        | Some _ -> ()
        | _ ->
            Assert.True(false, "Should have popped a new request block")

    [<Fact>]
    let ``Test should not pop request block when all blocks are requested`` () =
        let blocks = BlockRequests.create (Block.Length * 4)
        for idx in 0..(4 - 1) do
            match BlockRequests.pop blocks with
            | Some _ -> ()
            | _ -> ()
        match BlockRequests.pop blocks with
        | Some _ ->
            Assert.True(false, "Should not have popped a new request block")
        | _ -> ()
     
    [<Fact>]
    let ``Test should reset requested blocks to pending when there are all pending responses`` () =
        let req = Array.create 2 (BlockRequest.Requested({ Beginning = 0; Length = 0 }))
        req[0] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        req[1] <- BlockRequest.Pending({ Beginning = 0; Length = 0 })
        let res = Array.create 2 (BlockResponse.Pending({ Beginning = 0; Data = ByteBuffer.Empty }))
        BlockRequests.reset req res
        match req[0] with
        | BlockRequest.Pending _ -> ()
        | _ ->
            Assert.True(false, "Block request should have been reset to pending")
        match req[1] with
        | BlockRequest.Pending _ -> ()
        | _ ->
            Assert.True(false, "Block request should have been reset to pending")
         
    [<Fact>]
    let ``Test should reset requested blocks to pending when there are some pending responses`` () =
        let req = Array.create 2 (BlockRequest.Requested({ Beginning = 0; Length = 0 }))
        req[0] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        req[1] <- BlockRequest.Pending({ Beginning = 0; Length = 0 })
        let res = Array.create 2 (BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty }))
        res[0] <- BlockResponse.Pending({ Beginning = 0; Data = ByteBuffer.Empty })
        res[1] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        BlockRequests.reset req res
        match req[0] with
        | BlockRequest.Pending _ -> ()
        | _ ->
            Assert.True(false, "Block request should have been reset to pending")
        match req[1] with
        | BlockRequest.Pending _ -> ()
        | _ ->
            Assert.True(false, "Block request should have been reset to pending")
        
    [<Fact>]
    let ``Test should not reset requested blocks to pending when there are no pending responses`` () =
        let req = Array.create 2 (BlockRequest.Requested({ Beginning = 0; Length = 0 }))
        req[0] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        req[1] <- BlockRequest.Pending({ Beginning = 0; Length = 0 })
        let res = Array.create 2 (BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty }))
        res[0] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        res[1] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        BlockRequests.reset req res
        match req[0] with
        | BlockRequest.Requested _ -> ()
        | _ ->
            Assert.True(false, "Block request should not have been reset to pending")
        match req[1] with
        | BlockRequest.Pending _ -> ()
        | _ ->
            Assert.True(false, "Block request should have been reset to pending")
        
    [<Fact>]
    let ``Test should get requested block count when all requested`` () =
        let blocks = BlockRequests.create (Block.Length * 4)
        blocks[0] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        blocks[1] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        blocks[2] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        blocks[3] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        Assert.Equal(4, (BlockRequests.requestedCount blocks))
        
    [<Fact>]
    let ``Test should get requested block count when some requested`` () =
        let blocks = BlockRequests.create (Block.Length * 4)
        blocks.[0] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        blocks.[2] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        Assert.Equal(2, (BlockRequests.requestedCount blocks))
        
    [<Fact>]
    let ``Test should get requested block count when none requested`` () =
        let blocks = BlockRequests.create (Block.Length * 4)
        Assert.Equal(0, (BlockRequests.requestedCount blocks))
        
    [<Fact>]
    let ``Test should get pending block count when all pending (requests)`` () =
        let blocks = BlockRequests.create (Block.Length * 4)
        Assert.Equal(4, (BlockRequests.pendingCount blocks))
        
    [<Fact>]
    let ``Test should get pending block count when some pending (requests)`` () =
        let blocks = BlockRequests.create (Block.Length * 4)
        blocks.[0] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        blocks.[2] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        Assert.Equal(2, (BlockRequests.pendingCount blocks))
        
    [<Fact>]
    let ``Test should get pending block count when none pending (requests)`` () =
        let blocks = BlockRequests.create (Block.Length * 4)
        blocks.[0] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        blocks.[1] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        blocks.[2] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        blocks.[3] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        Assert.Equal(0, (BlockRequests.pendingCount blocks))
        
    [<Fact>]
    let ``Test should get all requested`` () =
        let blocks = BlockRequests.create (Block.Length * 4)
        blocks.[0] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        blocks.[1] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        blocks.[2] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        blocks.[3] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        if BlockRequests.isAllRequested blocks then
            ()
        else
            Assert.True(false, "All blocks should have been requested")
        
    [<Fact>]
    let ``Test should get some pending (requests)`` () =
        let blocks = BlockRequests.create (Block.Length * 4)
        blocks.[0] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        blocks.[2] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        if BlockRequests.isAllRequested blocks then
            Assert.True(false, "Some blocks should have been pending")
        else
            ()

module BlockResponsesTests =
    [<Fact>]
    let ``Test block response should create with no remainder`` () =
        let blocks = BlockResponses.create (Block.Length * 4)
        Assert.Equal(((Block.Length * 4) / Block.Length), blocks.Length)
        
    [<Fact>]
    let ``Test block response should create with remainder`` () =
        let blocks = BlockResponses.create ((Block.Length * 4) + 1)
        Assert.Equal(((Block.Length * 4) / Block.Length) + 1, blocks.Length)

    [<Fact>]
    let ``Test should push response block when pending block exists`` () =
        let blocks = BlockResponses.create (Block.Length * 4)
        BlockResponses.push { Beginning = 0; Data = ByteBuffer.Empty } blocks
        match blocks.[0] with
        | Responded _ -> ()
        | _ ->
            Assert.True(false, "Should have pushed a new response block")

    [<Fact>]
    let ``Test should not fail on push response block when all blocks are responded`` () =
        let blocks = BlockResponses.create (Block.Length * 4)
        BlockResponses.push ({ Beginning = (0 * Block.Length); Data = ByteBuffer.Empty }) blocks
        BlockResponses.push ({ Beginning = (1 * Block.Length); Data = ByteBuffer.Empty }) blocks
        BlockResponses.push ({ Beginning = (2 * Block.Length); Data = ByteBuffer.Empty }) blocks
        BlockResponses.push ({ Beginning = (3 * Block.Length); Data = ByteBuffer.Empty }) blocks
        (BlockResponses.push ({ Beginning = 0; Data = ByteBuffer.Empty }) blocks) |> ignore
    
    [<Fact>]
    let ``Test should convert response blocks to piece when there are no more pending`` () =
        let blocks = BlockResponses.create (Block.Length * 4)
        BlockResponses.push ({ Beginning = (0 * Block.Length); Data = ByteBuffer.Empty }) blocks
        BlockResponses.push ({ Beginning = (1 * Block.Length); Data = ByteBuffer.Empty }) blocks
        BlockResponses.push ({ Beginning = (2 * Block.Length); Data = ByteBuffer.Empty }) blocks
        BlockResponses.push ({ Beginning = (3 * Block.Length); Data = ByteBuffer.Empty }) blocks
        (BlockResponses.toPiece blocks) |> ignore
        
    [<Fact>]
    let ``Test should fail to convert response blocks to piece when there are some pending`` () =
        let blocks = BlockResponses.create (Block.Length * 4)
        BlockResponses.push ({ Beginning = (0 * Block.Length); Data = ByteBuffer.Empty }) blocks
        BlockResponses.push ({ Beginning = (2 * Block.Length); Data = ByteBuffer.Empty }) blocks
        try
            (BlockResponses.toPiece blocks) |> ignore
            Assert.True(false, "Converting response blocks to piece that are still pending should have failed")
        with exn -> ()
        
    [<Fact>]
    let ``Test should fail to convert response blocks to piece when there are all pending`` () =
        let blocks = BlockResponses.create (Block.Length * 4)
        BlockResponses.push ({ Beginning = (0 * Block.Length); Data = ByteBuffer.Empty }) blocks
        BlockResponses.push ({ Beginning = (1 * Block.Length); Data = ByteBuffer.Empty }) blocks
        BlockResponses.push ({ Beginning = (2 * Block.Length); Data = ByteBuffer.Empty }) blocks
        BlockResponses.push ({ Beginning = (3 * Block.Length); Data = ByteBuffer.Empty }) blocks
        try
            (BlockResponses.toPiece blocks) |> ignore
            Assert.True(false, "Converting response blocks to piece that are still pending should have failed")
        with exn -> ()
        
    [<Fact>]
    let ``Test should get responded block count when all responded`` () =
        let blocks = BlockResponses.create (Block.Length * 4)
        blocks.[0] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        blocks.[1] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        blocks.[2] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        blocks.[3] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        Assert.Equal(4, (BlockResponses.respondedCount blocks))
        
    [<Fact>]
    let ``Test should get responded block count when some responded`` () =
        let blocks = BlockResponses.create (Block.Length * 4)
        blocks.[0] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        blocks.[2] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        Assert.Equal(2, (BlockResponses.respondedCount blocks))
        
    [<Fact>]
    let ``Test should get responded block count when none responded`` () =
        let blocks = BlockResponses.create (Block.Length * 4)
        Assert.Equal(0, (BlockResponses.respondedCount blocks))
        
    [<Fact>]
    let ``Test should get pending block count when all pending (responses)`` () =
        let blocks = BlockResponses.create (Block.Length * 4)
        Assert.Equal(4, (BlockResponses.pendingCount blocks))
        
    [<Fact>]
    let ``Test should get pending block count when some pending (responses)`` () =
        let blocks = BlockResponses.create (Block.Length * 4)
        blocks.[0] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        blocks.[2] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        Assert.Equal(2, (BlockResponses.pendingCount blocks))
        
    [<Fact>]
    let ``Test should get pending block count when none pending (responses)`` () =
        let blocks = BlockResponses.create (Block.Length * 4)
        blocks.[0] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        blocks.[1] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        blocks.[2] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        blocks.[3] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        Assert.Equal(0, (BlockResponses.pendingCount blocks))
        
    [<Fact>]
    let ``Test should get all responded`` () =
        let blocks = BlockResponses.create (Block.Length * 4)
        blocks.[0] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        blocks.[1] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        blocks.[2] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        blocks.[3] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        if BlockResponses.isAllResponded blocks then
            ()
        else
            Assert.True(false, "All blocks should have been responded")
        
    [<Fact>]
    let ``Test should get some pending (responses)`` () =
        let blocks = BlockResponses.create (Block.Length * 4)
        blocks.[0] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        blocks.[2] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        if BlockResponses.isAllResponded blocks then
            Assert.True(false, "Some blocks should have been pending")
        else
            ()

module BlockPipelineTests = 
    [<Fact>]
    let ``Test should pipeline be created`` () =
        let pipeline = BlockPipeline.create ()
        Assert.Equal(DateTime.UtcNow.Second, pipeline.Timestamp.Second)
        Assert.Equal(Rate.zero, pipeline.Rate)
        Assert.Equal(5, pipeline.Backlog)
        
    [<Fact>]
    let ``Test should pipeline increase backlog on update`` () =
        let pipeline = BlockPipeline.create ()
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 5_000
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 10_000
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 20_000
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 40_000
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 80_000
        Assert.Equal(10, pipeline.Backlog)
        
    [<Fact>]
    let ``Test should pipeline not increase backlog on update`` () =
        let pipeline = BlockPipeline.create ()
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 1_000
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 1_000
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 1_000
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 1_000
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 1_000
        Assert.Equal(5, pipeline.Backlog)
    
    [<Fact>]
    let ``Test should pipeline reset`` () =
        let pipeline = BlockPipeline.create ()
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 5_000
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 10_000
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 20_000
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 40_000
        Thread.Sleep(100)
        let pipeline = pipeline |> BlockPipeline.update 80_000
        Assert.Equal(10, pipeline.Backlog)
        let pipeline = pipeline |> BlockPipeline.reset
        Assert.Equal(5, pipeline.Backlog)
    
    [<Fact>]
    let ``Test should pipeline backlog be filled`` () =
        let req = BlockRequests.create (Block.Length * 10)
        req.[0] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        req.[1] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        req.[2] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        req.[3] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        req.[4] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        let res = BlockResponses.create (Block.Length * 10)
        let pipeline = BlockPipeline.create ()
        if BlockPipeline.isBacklogFilled req res pipeline then
            ()
        else
            Assert.True(false, "Backlog should have been filled")
        
    [<Fact>]
    let ``Test should pipeline backlog not be filled`` () =
        let req = BlockRequests.create (Block.Length * 10)
        req.[0] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        req.[1] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        req.[2] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        req.[3] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        req.[4] <- BlockRequest.Requested({ Beginning = 0; Length = 0 })
        let res = BlockResponses.create (Block.Length * 10)
        res.[0] <- BlockResponse.Responded({ Beginning = 0; Data = ByteBuffer.Empty })
        let pipeline = BlockPipeline.create ()
        if BlockPipeline.isBacklogFilled req res pipeline then
            Assert.True(false, "Backlog should not have been filled")
        else
            ()