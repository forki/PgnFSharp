module PgnFSharp.Tests.MoveEntry

open FsUnit
open NUnit.Framework
open System.IO
open PgnFSharp

let tostrm (s:string) = new StreamReader (new MemoryStream (System.Text.Encoding.ASCII.GetBytes (s)))
[<Test>]
let ``Parse Move Pair`` () =
  let ans = PGN.NextGameRdr("53. Nf3 g6"|>tostrm)
  ans.Value.Moves.Head.ToString() |> should equal "Nf3"
  ans.Value.Moves.Tail.Head.ToString() |> should equal "g6"

[<Test>]
let ``Parse Half Move White`` () =
  let ans = PGN.NextGameRdr("1. e4"|>tostrm)
  ans.Value.Moves.Head.ToString() |> should equal "e4"

[<Test>]
let ``Parse Half Move Black`` () =
  let ans = PGN.NextGameRdr("13... Nf3"|>tostrm)
  ans.Value.Moves.Head.ToString() |> should equal "Nf3"

[<Test>]
let ``Parse Comment`` () =
  let ans = PGN.NextGameRdr("{this is a comment}"|>tostrm)
  ans |> should be Null

[<Test>]
let ``Parse Game End`` () =
  let ans = PGN.NextGameRdr("1-0"|>tostrm)
  ans |> should be Null

[<Test>]
let ``Parse NAG Entry`` () =
  let ans = PGN.NextGameRdr("$6"|>tostrm)
  ans |> should be Null

[<Test>]
let ``Parse RAV Entry`` () =
  let ans = PGN.NextGameRdr("(6. Bd3 cxd4 7. exd4 d5 { - B14 })"|>tostrm)
  ans |> should be Null
