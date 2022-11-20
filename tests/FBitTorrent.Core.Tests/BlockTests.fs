namespace FBitTorrent.Core.Tests

open FBitTorrent.Core
open Xunit

module BlockTests =

    [<Fact>]
    let ``Test block request should create with no remainder`` () =
        let blocks = Block.Requests.create (Block.BlockLength * 4)
        Assert.Equal(((Block.BlockLength * 4) / Block.BlockLength), blocks.Length)
        
    [<Fact>]
    let ``Test block request should create with remainder`` () =
        let blocks = Block.Requests.create ((Block.BlockLength * 4) + 1)
        Assert.Equal(((Block.BlockLength * 4) / Block.BlockLength) + 1, blocks.Length)

    [<Fact>]
    let ``Test should pop request block when pending block exists`` () =
        let blocks = Block.Requests.create (Block.BlockLength * 4)
        match Block.Requests.pop blocks with
        | Some _, _ -> ()
        | _ ->
            Assert.True(false, "Should have popped a new request block")

    [<Fact>]
    let ``Test should not pop request block when all blocks are requested`` () =
        let blocks = Block.Requests.create (Block.BlockLength * 4)
        for idx in 0..(4 - 1) do
            match Block.Requests.pop blocks with
            | Some _, _ -> ()
            | _ -> ()
        match Block.Requests.pop blocks with
        | Some _, _ ->
            Assert.True(false, "Should not have popped a new request block")
        | _ -> ()
    
    [<Fact>]
    let ``Test should reset requested blocks to pending when there are all pending responses`` () =
        let req = Array.create 2 (Block.Request.Requested({ Beginning = 0; Length = 0 }))
        req[0] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        req[1] <- Block.Request.Pending({ Beginning = 0; Length = 0 })
        let res = Array.create 2 (Block.Response.Pending({ Beginning = 0; Data = [||] }))
        let req = Block.Requests.reset req res
        match req[0] with
        | Block.Request.Pending _ -> ()
        | _ ->
            Assert.True(false, "Block request should have been reset to pending")
        match req[1] with
        | Block.Request.Pending _ -> ()
        | _ ->
            Assert.True(false, "Block request should have been reset to pending")
        
    [<Fact>]
    let ``Test should reset requested blocks to pending when there are some pending responses`` () =
        let req = Array.create 2 (Block.Request.Requested({ Beginning = 0; Length = 0 }))
        req[0] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        req[1] <- Block.Request.Pending({ Beginning = 0; Length = 0 })
        let res = Array.create 2 (Block.Response.Responded({ Beginning = 0; Data = [||] }))
        res[0] <- Block.Response.Pending({ Beginning = 0; Data = [||] })
        res[1] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        let req = Block.Requests.reset req res
        match req[0] with
        | Block.Request.Pending _ -> ()
        | _ ->
            Assert.True(false, "Block request should have been reset to pending")
        match req[1] with
        | Block.Request.Pending _ -> ()
        | _ ->
            Assert.True(false, "Block request should have been reset to pending")
        
    [<Fact>]
    let ``Test should not reset requested blocks to pending when there are no pending responses`` () =
        let req = Array.create 2 (Block.Request.Requested({ Beginning = 0; Length = 0 }))
        req[0] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        req[1] <- Block.Request.Pending({ Beginning = 0; Length = 0 })
        let res = Array.create 2 (Block.Response.Responded({ Beginning = 0; Data = [||] }))
        res[0] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        res[1] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        let req = Block.Requests.reset req res
        match req[0] with
        | Block.Request.Requested _ -> ()
        | _ ->
            Assert.True(false, "Block request should not have been reset to pending")
        match req[1] with
        | Block.Request.Pending _ -> ()
        | _ ->
            Assert.True(false, "Block request should have been reset to pending")
        
    [<Fact>]
    let ``Test should get requested block count when all requested`` () =
        let blocks = Block.Requests.create (Block.BlockLength * 4)
        blocks[0] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        blocks[1] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        blocks[2] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        blocks[3] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        Assert.Equal(4, (Block.Requests.requestedCount blocks))
        
    [<Fact>]
    let ``Test should get requested block count when some requested`` () =
        let blocks = Block.Requests.create (Block.BlockLength * 4)
        blocks.[0] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        blocks.[2] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        Assert.Equal(2, (Block.Requests.requestedCount blocks))
        
    [<Fact>]
    let ``Test should get requested block count when none requested`` () =
        let blocks = Block.Requests.create (Block.BlockLength * 4)
        Assert.Equal(0, (Block.Requests.requestedCount blocks))
        
    [<Fact>]
    let ``Test should get pending block count when all pending (requests)`` () =
        let blocks = Block.Requests.create (Block.BlockLength * 4)
        Assert.Equal(4, (Block.Requests.pendingCount blocks))
        
    [<Fact>]
    let ``Test should get pending block count when some pending (requests)`` () =
        let blocks = Block.Requests.create (Block.BlockLength * 4)
        blocks.[0] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        blocks.[2] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        Assert.Equal(2, (Block.Requests.pendingCount blocks))
        
    [<Fact>]
    let ``Test should get pending block count when none pending (requests)`` () =
        let blocks = Block.Requests.create (Block.BlockLength * 4)
        blocks.[0] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        blocks.[1] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        blocks.[2] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        blocks.[3] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        Assert.Equal(0, (Block.Requests.pendingCount blocks))
        
    [<Fact>]
    let ``Test should get all requested`` () =
        let blocks = Block.Requests.create (Block.BlockLength * 4)
        blocks.[0] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        blocks.[1] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        blocks.[2] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        blocks.[3] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        if Block.Requests.isAllRequested blocks then
            ()
        else
            Assert.True(false, "All blocks should have been requested")
        
    [<Fact>]
    let ``Test should get some pending (requests)`` () =
        let blocks = Block.Requests.create (Block.BlockLength * 4)
        blocks.[0] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        blocks.[2] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        if Block.Requests.isAllRequested blocks then
            Assert.True(false, "Some blocks should have been pending")
        else
            ()

    [<Fact>]
    let ``Test block response should create with no remainder`` () =
        let blocks = Block.Responses.create (Block.BlockLength * 4)
        Assert.Equal(((Block.BlockLength * 4) / Block.BlockLength), blocks.Length)
        
    [<Fact>]
    let ``Test block response should create with remainder`` () =
        let blocks = Block.Responses.create ((Block.BlockLength * 4) + 1)
        Assert.Equal(((Block.BlockLength * 4) / Block.BlockLength) + 1, blocks.Length)

    [<Fact>]
    let ``Test should push response block when pending block exists`` () =
        let blocks = Block.Responses.create (Block.BlockLength * 4)
        let blocks = Block.Responses.push { Beginning = 0; Data = [||] } blocks
        match blocks.[0] with
        | Block.Responded _ -> ()
        | _ ->
            Assert.True(false, "Should have pushed a new response block")

    [<Fact>]
    let ``Test should not fail on push response block when all blocks are responded`` () =
        let blocks = Block.Responses.create (Block.BlockLength * 4)
        let blocks = Block.Responses.push ({ Beginning = (0 * Block.BlockLength); Data = [||] }) blocks
        let blocks = Block.Responses.push ({ Beginning = (1 * Block.BlockLength); Data = [||] }) blocks
        let blocks = Block.Responses.push ({ Beginning = (2 * Block.BlockLength); Data = [||] }) blocks
        let blocks = Block.Responses.push ({ Beginning = (3 * Block.BlockLength); Data = [||] }) blocks
        (Block.Responses.push ({ Beginning = 0; Data = [||] }) blocks) |> ignore
    
    [<Fact>]
    let ``Test should drain response blocks when there are no more pending`` () =
        let blocks = Block.Responses.create (Block.BlockLength * 4)
        let blocks = Block.Responses.push ({ Beginning = (0 * Block.BlockLength); Data = [||] }) blocks
        let blocks = Block.Responses.push ({ Beginning = (1 * Block.BlockLength); Data = [||] }) blocks
        let blocks = Block.Responses.push ({ Beginning = (2 * Block.BlockLength); Data = [||] }) blocks
        let blocks = Block.Responses.push ({ Beginning = (3 * Block.BlockLength); Data = [||] }) blocks
        (Block.Responses.drain blocks) |> ignore
        
    [<Fact>]
    let ``Test should fail to drain response blocks when there are some pending`` () =
        let blocks = Block.Responses.create (Block.BlockLength * 4)
        let blocks = Block.Responses.push ({ Beginning = (0 * Block.BlockLength); Data = [||] }) blocks
        let blocks = Block.Responses.push ({ Beginning = (2 * Block.BlockLength); Data = [||] }) blocks
        try
            (Block.Responses.drain blocks) |> ignore
            Assert.True(false, "Draining response blocks that are still pending should have failed")
        with exn -> ()
        
    [<Fact>]
    let ``Test should fail to drain response blocks when there are all pending`` () =
        let blocks = Block.Responses.create (Block.BlockLength * 4)
        let blocks = Block.Responses.push ({ Beginning = (0 * Block.BlockLength); Data = [||] }) blocks
        let blocks = Block.Responses.push ({ Beginning = (1 * Block.BlockLength); Data = [||] }) blocks
        let blocks = Block.Responses.push ({ Beginning = (2 * Block.BlockLength); Data = [||] }) blocks
        let blocks = Block.Responses.push ({ Beginning = (3 * Block.BlockLength); Data = [||] }) blocks
        try
            (Block.Responses.drain blocks) |> ignore
            Assert.True(false, "Draining response blocks that are still pending should have failed")
        with exn -> ()
        
    [<Fact>]
    let ``Test should get responded block count when all responded`` () =
        let blocks = Block.Responses.create (Block.BlockLength * 4)
        blocks.[0] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        blocks.[1] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        blocks.[2] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        blocks.[3] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        Assert.Equal(4, (Block.Responses.respondedCount blocks))
        
    [<Fact>]
    let ``Test should get responded block count when some responded`` () =
        let blocks = Block.Responses.create (Block.BlockLength * 4)
        blocks.[0] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        blocks.[2] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        Assert.Equal(2, (Block.Responses.respondedCount blocks))
        
    [<Fact>]
    let ``Test should get responded block count when none responded`` () =
        let blocks = Block.Responses.create (Block.BlockLength * 4)
        Assert.Equal(0, (Block.Responses.respondedCount blocks))
        
    [<Fact>]
    let ``Test should get pending block count when all pending (responses)`` () =
        let blocks = Block.Responses.create (Block.BlockLength * 4)
        Assert.Equal(4, (Block.Responses.pendingCount blocks))
        
    [<Fact>]
    let ``Test should get pending block count when some pending (responses)`` () =
        let blocks = Block.Responses.create (Block.BlockLength * 4)
        blocks.[0] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        blocks.[2] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        Assert.Equal(2, (Block.Responses.pendingCount blocks))
        
    [<Fact>]
    let ``Test should get pending block count when none pending (responses)`` () =
        let blocks = Block.Responses.create (Block.BlockLength * 4)
        blocks.[0] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        blocks.[1] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        blocks.[2] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        blocks.[3] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        Assert.Equal(0, (Block.Responses.pendingCount blocks))
        
    [<Fact>]
    let ``Test should get all responded`` () =
        let blocks = Block.Responses.create (Block.BlockLength * 4)
        blocks.[0] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        blocks.[1] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        blocks.[2] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        blocks.[3] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        if Block.Responses.isAllResponded blocks then
            ()
        else
            Assert.True(false, "All blocks should have been responded")
        
    [<Fact>]
    let ``Test should get some pending (responses)`` () =
        let blocks = Block.Responses.create (Block.BlockLength * 4)
        blocks.[0] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        blocks.[2] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        if Block.Responses.isAllResponded blocks then
            Assert.True(false, "Some blocks should have been pending")
        else
            ()
            
    [<Fact>]
    let ``Test should backlog be filled`` () =
        let req = Block.Requests.create (Block.BlockLength * 10)
        req.[0] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        req.[1] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        req.[2] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        req.[3] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        req.[4] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        let res = Block.Responses.create (Block.BlockLength * 10)
        if Block.isBacklogFilled req res 5 then
            ()
        else
            Assert.True(false, "Backlog should have been filled")
        
    [<Fact>]
    let ``Test should backlog not be filled`` () =
        let req = Block.Requests.create (Block.BlockLength * 10)
        req.[0] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        req.[1] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        req.[2] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        req.[3] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        req.[4] <- Block.Request.Requested({ Beginning = 0; Length = 0 })
        let res = Block.Responses.create (Block.BlockLength * 10)
        res.[0] <- Block.Response.Responded({ Beginning = 0; Data = [||] })
        if Block.isBacklogFilled req res 5 then
            Assert.True(false, "Backlog should not have been filled")
        else
            ()