namespace FBitTorrent.BEncode.Tests

open System
open System.IO
open FBitTorrent.BEncode
open Xunit
open System.Text
open BValue
open Xunit.Abstractions

type BEncodeTests(output: ITestOutputHelper) =
    
    let shouldDecodeEncode (value: string) =
        let decoded = BDecode.fromString Encoding.UTF8 value
        let encoded = BEncode.toString Encoding.UTF8 decoded
        Assert.Equal(value, encoded)
        
    let shouldEncodeDecode (value: BValue) =
        let encoded = BEncode.toString Encoding.UTF8 value
        let decoded = BDecode.fromString Encoding.UTF8 encoded
        Assert.Equal(value, decoded)
    
    [<Fact>]
    let ``Test should decode single file torrent`` () =
        let exp = File.ReadAllBytes("./single_file.torrent")
        let act = BEncode.defaultToBytes (BDecode.defaultFromBytes exp)
        Assert.Equal<byte[]>(exp, act)
        
    [<Fact>]
    let ``Test should decode multi file torrent`` () =
        let exp = File.ReadAllBytes("./multi_file.torrent")
        let act = BEncode.defaultToBytes (BDecode.defaultFromBytes exp)
        Assert.Equal<byte[]>(exp, act)

    [<Fact>]
    let ``Test should throw corrupt data decode`` () =
        Assert.ThrowsAny<Exception>(fun () -> (BDecode.fromString Encoding.UTF8 "corruption!") |> ignore)

    [<Fact>]
    let ``Test should throw corrupt string decode`` () =
        Assert.ThrowsAny<Exception>(fun () -> (BDecode.fromString Encoding.UTF8 "50:i'm too short") |> ignore)
    
    [<Fact>]
    let ``Test should throw corrupt integer decode`` () =
        Assert.ThrowsAny<Exception>(fun () -> (BDecode.fromString Encoding.UTF8 "i9223372036854775807") |> ignore)
    
    [<Fact>]
    let ``Test should throw corrupt list decode`` () =
        Assert.ThrowsAny<Exception>(fun () -> (BDecode.fromString Encoding.UTF8 "l3:3521:a3:ae") |> ignore)
    
    [<Fact>]
    let ``Test should throw corrupt dictionary decode`` () =
        Assert.ThrowsAny<Exception>(fun () -> (BDecode.fromString Encoding.UTF8 "d3:3521:a3:aedddd") |> ignore)

    [<Fact>]
    let ``Test should decode/encode string`` () =
        shouldDecodeEncode "22:this is my test string"
        
    [<Fact>]
    let ``Test should encode/decode string`` () =
        shouldEncodeDecode (bstr "this is my test string") 
    
    [<Fact>]
    let ``Test should decode/encode UTF-8 string`` () =
        shouldDecodeEncode "17:ɄɅ ɱ ϴ ЂЃЃ"
        
    [<Fact>]
    let ``Test should encode/decode UTF-8 string`` () =
        shouldEncodeDecode (bstr "17:ɄɅ ɱ ϴ ЂЃЃ")
    
    [<Fact>]
    let ``Test should decode/encode empty string`` () =
        shouldDecodeEncode "0:"
        
    [<Fact>]
    let ``Test should encode/decode empty string`` () =
        shouldEncodeDecode (bstr "")
    
    [<Fact>]
    let ``Test should decode/encode integer`` () =
        shouldDecodeEncode "i12412e"
        
    [<Fact>]
    let ``Test should encode/decode integer`` () =
        shouldEncodeDecode (bint 123412L)
    
    [<Fact>]
    let ``Test should decode/encode negative integer`` () =
        shouldDecodeEncode "i-12412e"
        
    [<Fact>]
    let ``Test should encode/decode negative integer`` () =
        shouldEncodeDecode (bint -123412L)
        
    [<Fact>]
    let ``Test should decode/encode zero integer`` () =
        shouldDecodeEncode "i0e"
        
    [<Fact>]
    let ``Test should encode/decode zero integer`` () =
        shouldEncodeDecode (bint 0L)
    
    [<Fact>]
    let ``Test should decode/encode min integer`` () =
        shouldDecodeEncode "i-9223372036854775808e"
        
    [<Fact>]
    let ``Test should encode/decode min integer`` () =
        shouldEncodeDecode (bint -9223372036854775808L)
        
    [<Fact>]
    let ``Test should decode/encode max integer`` () =
        shouldDecodeEncode "i9223372036854775807e"
        
    [<Fact>]
    let ``Test should encode/decode max integer`` () =
        shouldEncodeDecode (bint 9223372036854775807L)
        
    [<Fact>]
    let ``Test should decode/encode list`` () =
        shouldDecodeEncode "l4:test5:tests6:testede"
        
    [<Fact>]
    let ``Test should encode/decode list`` () =
        shouldEncodeDecode (blist [ (bstr "test")
                                    (bstr "tests")
                                    (bstr "tested") ])
        
    [<Fact>]
    let ``Test should decode/encode empty list`` () =
        shouldDecodeEncode "le"
        
    [<Fact>]
    let ``Test should encode/decode empty list`` () =
        shouldEncodeDecode (blist [])
        
    [<Fact>]
    let ``Test should decode/encode stacked list`` () =
        shouldDecodeEncode "l6:stringl7:stringsl8:stringedei23456eei12345ee"
        
    [<Fact>]
    let ``Test should encode/decode stacked list`` () =
        shouldEncodeDecode (blist [ (bstr "string")
                                    (blist [ (bstr "strings")
                                             (blist [ (bstr "stringed") ])
                                             (bint 23456L) ])
                                    (bint 12345L) ])
        
    [<Fact>]
    let ``Test should decode/encode dictionary`` () =
        shouldDecodeEncode "d4:spaml1:a1:bee"
        
    [<Fact>]
    let ``Test should encode/decode dictionary`` () =
        shouldEncodeDecode (bdict (Map.empty |> Map.add
                                       (bstr "spam")
                                       (blist [ (bstr "a")
                                                (bstr "b") ])))
        
    [<Fact>]
    let ``Test should decode/encode empty dictionary`` () =
        shouldDecodeEncode "de"
        
    [<Fact>]
    let ``Test should encode/decode empty dictionary`` () =
        shouldEncodeDecode (bdict Map.empty)
        
    [<Fact>]
    let ``Test should decode/encode stacked dictionary`` () =
        shouldDecodeEncode "d4:testd5:testsli12345ei12345ee2:tod3:tomi12345eeee"
        
    [<Fact>]
    let ``Test should encode/decode stacked dictionary`` () =
        let d3 = bdict (Map.empty
                        |> Map.add (bstr "tom") (bint 12345L))
        let d2 = bdict (Map.empty
                        |> Map.add (bstr "tests") (blist [ (bint 12345L); (bint 12345L) ])
                        |> Map.add (bstr "to") d3)
        let d1 = bdict (Map.empty |> Map.add (bstr "test") d2)
        shouldEncodeDecode d1

    [<Fact>]
    let ``Test should decode sorted dictionary`` () =
        let value =
            bdict (Map.empty
                   |> Map.add (bstr "a") (bstr "a")
                   |> Map.add (bstr "b") (bstr "b")
                   |> Map.add (bstr "c") (bstr "c"))
        let decoded = BDecode.fromString Encoding.UTF8 "d1:c1:c1:b1:b1:a1:ae"
        Assert.Equal(value, decoded)
        
    [<Fact>]
    let ``Test should encode sorted dictionary`` () =
        let value = "d1:a1:a1:b1:b1:c1:ce"
        let encoded = BEncode.toString Encoding.UTF8 (
                   bdict (Map.empty
                          |> Map.add (bstr "b") (bstr "b")
                          |> Map.add (bstr "a") (bstr "a")
                          |> Map.add (bstr "c") (bstr "c")))
        Assert.Equal(value, encoded)
