module PgnFSharp.Tests.Move

open FsUnit
open NUnit.Framework
open PgnFSharp

let bd = Board.Create2 (FEN.FromStr "rnbqkbnr/p1p3p1/4p2p/3p1pP1/Pp6/2N1P2P/1PPP1P2/R1BQKBNR w KQkq f6 0 7")
let bd2 = Board.Create2 (FEN.FromStr "rnbqkbn1/1pppppp1/r7/p6p/4P3/2PP1N2/PP3PPP/RNBQKB1R b KQq - 0 5")
[<Test>]
let ``Parse Castle Kingside`` () =
  let ans = MoveUtil.Parse bd "O-O"
  ans.ToString() |> should equal "O-O"

[<Test>]
let ``Parse Castle Queenside`` () =
  let ans = MoveUtil.Parse bd "O-O-O"
  ans.ToString() |> should equal "O-O-O"

[<Test>]
let ``Parse Pawn Move`` () =
  let ans = MoveUtil.Parse bd "d4"
  ans.ToString() |> should equal "d4"

[<Test>]
let ``Parse Knight Move`` () =
  let ans = MoveUtil.Parse bd "Nf3"
  ans.ToString() |> should equal "Nf3"

[<Test>]
let ``Parse Knight Capture`` () =
  let ans = MoveUtil.Parse bd "Nxd5"
  ans.ToString() |> should equal "Nxd5"

[<Test>]
let ``Parse Pawn Capture`` () =
  let ans = MoveUtil.Parse bd "gxh6"
  ans.ToString() |> should equal "gxh6"

[<Test>]
let ``Parse Pawn Capture EnPassant`` () =
  let ans = MoveUtil.Parse bd "gxf6e.p."
  ans.ToString() |> should equal "gxf6e.p."

[<Test>]
let ``Parse ambiguous file`` () =
  let ans = MoveUtil.Parse bd "Nge2"
  ans.ToString() |> should equal "Nge2"

[<Test>]
let ``Parse ambiguous rank`` () =
  let ans = MoveUtil.Parse bd2 "R6a7"
  ans.ToString() |> should equal "R6a7"

[<Test>]
let ``Parse promotion`` () =
  let ans = MoveUtil.Parse bd "d7xc8=Q"
  ans.ToString() |> should equal "d7xc8=Q"

[<Test>]
let ``Parse check`` () =
  let ans = MoveUtil.Parse bd "Bb5+"
  ans.ToString() |> should equal "Bb5+"

[<Test>]
let ``Parse annotation`` () =
  let ans = MoveUtil.Parse bd "Bb5!!"
  ans.ToString() |> should equal "Bb5!!"
