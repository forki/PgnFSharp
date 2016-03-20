module PgnFSharp.Tests.Psn

open FsUnit
open NUnit.Framework
open PgnFSharp

let s0 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w"
let s1 = "rnbqkbnr/p1p3p1/4p2p/3p1pP1/Pp6/2N1P2P/1PPP1P2/R1BQKBNR w"
let s2 = "rnbqkbn1/1pppppp1/r7/p6p/4P3/2PP1N2/PP3PPP/RNBQKB1R b"

[<Test>]
let ``Load start to Posn``() = 
    let ans = Psn.FromStr s0
    ans.ToString() |> should equal s0

[<Test>]
let ``Load pos1 to Posn``() = 
    let ans = Psn.FromStr s1
    ans.ToString() |> should equal s1

[<Test>]
let ``Load pos2 to Posn``() = 
    let ans = Psn.FromStr s2
    ans.ToString() |> should equal s2
