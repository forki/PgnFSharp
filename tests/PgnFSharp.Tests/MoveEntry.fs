module PgnFSharp.Tests.MoveEntry

open FsUnit
open NUnit.Framework
open System.IO
open PgnParsers
open FParsec 

let getres p =
    match p with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

[<Test>]
let ``Parse Move Pair`` () =
  let ans = (run pMoveSeriesEntry "53. Nf7+ Kg6")|>getres
  ans.Value.ToString() |> should equal "53. Nf7+ Kg6"

[<Test>]
let ``Parse Half Move White`` () =
  let ans = (run pMoveSeriesEntry "1. e4")|>getres
  ans.Value.ToString() |> should equal "1. e4"

[<Test>]
let ``Parse Half Move Black`` () =
  let ans = (run pMoveSeriesEntry "13... Ba6")|>getres
  ans.Value.ToString() |> should equal "13... Ba6"

[<Test>]
let ``Parse Comment`` () =
  let ans = (run pMoveSeriesEntry "{this is a comment}")|>getres
  ans |> should be Null

[<Test>]
let ``Parse Game End`` () =
  let ans = (run pMoveSeriesEntry "1-0")|>getres
  ans |> should be Null

[<Test>]
let ``Parse NAG Entry`` () =
  let ans = (run pMoveSeriesEntry "$6")|>getres
  ans |> should be Null

[<Test>]
let ``Parse RAV Entry`` () =
  let ans = (run pMoveSeriesEntry "(6. Bd3 cxd4 7. exd4 d5 { - B14 })")|>getres
  ans |> should be Null
