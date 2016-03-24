module PgnFSharp.Tests.Psn

open FsUnit
open NUnit.Framework
open PgnFSharp

let s0 = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w"
let s1 = "rnbqkbnr/p1p3p1/4p2p/3p1pP1/Pp6/2N1P2P/1PPP1P2/R1BQKBNR w"
let s2 = "rnbqkbn1/1pppppp1/r7/p6p/4P3/2PP1N2/PP3PPP/RNBQKB1R b"
let s3 = "r1bq1bnr/p1pP1kp1/n3p2p/5pP1/Pp6/2N4P/1PPP1P2/R1BQKBNR w"

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

[<Test>]
let ``Get Castle Kingside`` () =
  let pos = Psn.FromStr s1
  let ans = Psn.GetMv pos "O-O"
  ans.ToString() |> should equal "O-O"
  ans.Mfrom |> should equal 60
  ans.Mto |> should equal 62
  ans.Mtyp.Value |> should equal CasK
  pos.DoMv ans
  pos.ToString()|>should equal "rnbqkbnr/p1p3p1/4p2p/3p1pP1/Pp6/2N1P2P/1PPP1P2/R1BQ1RK1 b"

[<Test>]
let ``Get Castle Queenside`` () =
  let pos = Psn.FromStr s1
  let ans = Psn.GetMv pos "O-O-O"
  ans.ToString() |> should equal "O-O-O"
  ans.Mfrom |> should equal 60
  ans.Mto |> should equal 58
  ans.Mtyp.Value |> should equal CasQ
  pos.DoMv ans
  pos.ToString()|>should equal "rnbqkbnr/p1p3p1/4p2p/3p1pP1/Pp6/2N1P2P/1PPP1P2/2KR1BNR b"

[<Test>]
let ``Get Pawn Move`` () =
  let pos = Psn.FromStr s1
  let ans = Psn.GetMv pos "d4"
  ans.ToString() |> should equal "d4"
  ans.Mfrom |> should equal 51
  ans.Mto |> should equal 35
  ans.Mtyp |> should be Null
  pos.DoMv ans
  pos.ToString()|>should equal "rnbqkbnr/p1p3p1/4p2p/3p1pP1/Pp1P4/2N1P2P/1PP2P2/R1BQKBNR b"

[<Test>]
let ``Get Knight Move`` () =
  let pos = Psn.FromStr s1
  let ans = Psn.GetMv pos "Nf3"
  ans.ToString() |> should equal "Nf3"
  ans.Mfrom |> should equal 62
  ans.Mto |> should equal 45
  ans.Mtyp |> should be Null
  pos.DoMv ans
  pos.ToString()|>should equal "rnbqkbnr/p1p3p1/4p2p/3p1pP1/Pp6/2N1PN1P/1PPP1P2/R1BQKB1R b"

[<Test>]
let ``Get Knight Move 2`` () =
  let pos = Psn.FromStr "rn2k1nr/pp3ppp/1qp5/3p1b2/3P4/1QN1P1P1/PP2BPP1/R3K1NR b"
  let ans = Psn.GetMv pos "Na6"
  ans.ToString() |> should equal "Na6"
  ans.Mfrom |> should equal 1
  ans.Mto |> should equal 16
  ans.Mtyp |> should be Null
  pos.DoMv ans
  pos.ToString()|>should equal "r3k1nr/pp3ppp/nqp5/3p1b2/3P4/1QN1P1P1/PP2BPP1/R3K1NR w"

[<Test>]
let ``Get Rook Move`` () =
  let pos = Psn.FromStr "r6r/1p1b1kp1/p1b1pp1p/q7/2p1PB2/2N3Q1/PPP2PPP/3R1RK1 w"
  let ans = Psn.GetMv pos "Rd2"
  ans.ToString() |> should equal "Rd2"
  ans.Mfrom |> should equal 59
  ans.Mto |> should equal 51
  ans.Mtyp |> should be Null
  pos.DoMv ans
  pos.ToString()|>should equal "r6r/1p1b1kp1/p1b1pp1p/q7/2p1PB2/2N3Q1/PPPR1PPP/5RK1 b"

[<Test>]
let ``Get Knight Capture`` () =
  let pos = Psn.FromStr s1
  let ans = Psn.GetMv pos "Nxd5"
  ans.ToString() |> should equal "Nxd5"
  ans.Mfrom |> should equal 42
  ans.Mto |> should equal 27
  ans.Mtyp |> should be Null
  pos.DoMv ans
  pos.ToString()|>should equal "rnbqkbnr/p1p3p1/4p2p/3N1pP1/Pp6/4P2P/1PPP1P2/R1BQKBNR b"

[<Test>]
let ``Get Rook Capture`` () =
  let pos = Psn.FromStr "3rr3/1p3kp1/p4p1p/q1b1pP2/1bp1P3/2N1B1Q1/PPPR2PP/3R2K1 w"
  let ans = Psn.GetMv pos "Rxd8"
  ans.ToString() |> should equal "Rxd8"
  ans.Mfrom |> should equal 51
  ans.Mto |> should equal 3
  ans.Mtyp |> should be Null
  pos.DoMv ans
  pos.ToString()|>should equal "3Rr3/1p3kp1/p4p1p/q1b1pP2/1bp1P3/2N1B1Q1/PPP3PP/3R2K1 b"

[<Test>]
let ``Get Queen Capture`` () =
  let pos = Psn.FromStr "1Q3b2/3Nq1kp/6p1/3Q1p2/3P4/4P1PP/2r3r1/1R2R1K1 w"
  let ans = Psn.GetMv pos "Qxg2"
  ans.ToString() |> should equal "Qxg2"
  ans.Mfrom |> should equal 27
  ans.Mto |> should equal 54
  ans.Mtyp |> should be Null
  pos.DoMv ans
  pos.ToString()|>should equal "1Q3b2/3Nq1kp/6p1/5p2/3P4/4P1PP/2r3Q1/1R2R1K1 b"

[<Test>]
let ``Get Pawn Capture`` () =
  let pos = Psn.FromStr s1
  let ans = Psn.GetMv pos "gxh6"
  ans.ToString() |> should equal "gxh6"
  ans.Mfrom |> should equal 30
  ans.Mto |> should equal 23
  ans.Mtyp |> should be Null
  pos.DoMv ans
  pos.ToString()|>should equal "rnbqkbnr/p1p3p1/4p2P/3p1p2/Pp6/2N1P2P/1PPP1P2/R1BQKBNR b"

[<Test>]
let ``Get Pawn Capture E.p.`` () =
  let pos = Psn.FromStr s1
  let ans = Psn.GetMv pos "gxf6e.p."
  ans.ToString() |> should equal "gxf6e.p."
  ans.Mfrom |> should equal 30
  ans.Mto |> should equal 21
  ans.Mtyp.Value |> should equal Ep
  pos.DoMv ans
  pos.ToString()|>should equal "rnbqkbnr/p1p3p1/4pP1p/3p4/Pp6/2N1P2P/1PPP1P2/R1BQKBNR b"

[<Test>]
let ``Get ambiguous file`` () =
  let pos = Psn.FromStr s1
  let ans = Psn.GetMv pos "Nge2"
  ans.ToString() |> should equal "Nge2"
  ans.Mfrom |> should equal 62
  ans.Mto |> should equal 52
  ans.Mtyp |> should be Null
  pos.DoMv ans
  pos.ToString()|>should equal "rnbqkbnr/p1p3p1/4p2p/3p1pP1/Pp6/2N1P2P/1PPPNP2/R1BQKB1R b"

[<Test>]
let ``Get ambiguous Rook Move`` () =
  let pos = Psn.FromStr "r3b2r/1p1b1kp1/p3pp1p/q7/2p1PB2/2N3Q1/PPP2PPP/R4RK1 w"
  let ans = Psn.GetMv pos "Rad1"
  ans.ToString() |> should equal "Rad1"
  ans.Mfrom |> should equal 56
  ans.Mto |> should equal 59
  ans.Mtyp |> should be Null
  pos.DoMv ans
  pos.ToString()|>should equal "r3b2r/1p1b1kp1/p3pp1p/q7/2p1PB2/2N3Q1/PPP2PPP/3R1RK1 b"

[<Test>]
let ``Get ambiguous rank`` () =
  let pos = Psn.FromStr s2
  let ans = Psn.GetMv pos "R6a7"
  ans.ToString() |> should equal "R6a7"
  ans.Mfrom |> should equal 16
  ans.Mto |> should equal 8
  ans.Mtyp |> should be Null
  pos.DoMv ans
  pos.ToString()|>should equal "rnbqkbn1/rpppppp1/8/p6p/4P3/2PP1N2/PP3PPP/RNBQKB1R w"

[<Test>]
let ``Get promotion`` () =
  let pos = Psn.FromStr "8/6P1/7K/8/1p6/8/Pk6/8 w"
  let ans = Psn.GetMv pos "g8=Q"
  ans.ToString() |> should equal "g8=Q"
  ans.Mfrom |> should equal 14
  ans.Mto |> should equal 6
  ans.Mtyp.Value |> should equal (Prom('Q'))
  pos.DoMv ans
  pos.ToString()|>should equal "6Q1/8/7K/8/1p6/8/Pk6/8 b"

[<Test>]
let ``Get promotion capture`` () =
  let pos = Psn.FromStr s3
  let ans = Psn.GetMv pos "dxc8=Q"
  ans.ToString() |> should equal "dxc8=Q"
  ans.Mfrom |> should equal 11
  ans.Mto |> should equal 2
  ans.Mtyp.Value |> should equal (Prom('Q'))
  pos.DoMv ans
  pos.ToString()|>should equal "r1Qq1bnr/p1p2kp1/n3p2p/5pP1/Pp6/2N4P/1PPP1P2/R1BQKBNR b"

[<Test>]
let ``Get check`` () =
  let pos = Psn.FromStr s1
  let ans = Psn.GetMv pos "Bb5+"
  ans.ToString() |> should equal "Bb5+"
  ans.Mfrom |> should equal 61
  ans.Mto |> should equal 25
  ans.Mtyp |> should be Null
  pos.DoMv ans
  pos.ToString()|>should equal "rnbqkbnr/p1p3p1/4p2p/1B1p1pP1/Pp6/2N1P2P/1PPP1P2/R1BQK1NR b"

[<Test>]
let ``Get Queen check`` () =
  let pos = Psn.FromStr "8/8/Q7/1r6/5P1P/Pk4P1/6BK/3qq3 b"
  let ans = Psn.GetMv pos "Qg1+"
  ans.ToString() |> should equal "Qg1+"
  ans.Mfrom |> should equal 60
  ans.Mto |> should equal 62
  ans.Mtyp |> should be Null
  pos.DoMv ans
  pos.ToString()|>should equal "8/8/Q7/1r6/5P1P/Pk4P1/6BK/3q2q1 w"

[<Test>]
let ``Get where restricted by check`` () =
  let pos = Psn.FromStr "rnbqk2r/pp1p1ppp/4pn2/2p5/1bPP4/2N1P3/PP3PPP/R1BQKBNR w"
  let ans = Psn.GetMv pos "Ne2"
  ans.ToString() |> should equal "Ne2"
  ans.Mfrom |> should equal 62
  ans.Mto |> should equal 52
  ans.Mtyp |> should be Null
  pos.DoMv ans
  pos.ToString()|>should equal "rnbqk2r/pp1p1ppp/4pn2/2p5/1bPP4/2N1P3/PP2NPPP/R1BQKB1R b"
