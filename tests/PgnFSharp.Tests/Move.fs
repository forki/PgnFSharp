module PgnFSharp.Tests.Move

open FsUnit
open NUnit.Framework
open System.IO
open FParsec
open PgnParsers

let getres p =
    match p with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

[<Test>]
let ``Parse Castle Kingside`` () =
  let ans = (Move.appyPMove "O-O")|>getres
  ans.ToString() |> should equal "O-O"

[<Test>]
let ``Parse Castle Queenside`` () =
  let ans = (Move.appyPMove "O-O-O")|>getres
  ans.ToString() |> should equal "O-O-O"

[<Test>]
let ``Parse Pawn Move`` () =
  let ans = (Move.appyPMove "d4")|>getres
  ans.ToString() |> should equal "d4"

[<Test>]
let ``Parse Knight Move`` () =
  let ans = (Move.appyPMove "Nf3")|>getres
  ans.ToString() |> should equal "Nf3"

[<Test>]
let ``Parse Knight Capture`` () =
  let ans = (Move.appyPMove "Nxe5")|>getres
  ans.ToString() |> should equal "Nxe5"

[<Test>]
let ``Parse Pawn Capture`` () =
  let ans = (Move.appyPMove "axb5")|>getres
  ans.ToString() |> should equal "axb5"

[<Test>]
let ``Parse Pawn Capture EnPassant`` () =
  let ans = (Move.appyPMove "axb6e.p.")|>getres
  ans.ToString() |> should equal "axb6e.p."

[<Test>]
let ``Parse ambiguous file`` () =
  let ans = (Move.appyPMove "Nbd2")|>getres
  ans.ToString() |> should equal "Nbd2"

[<Test>]
let ``Parse ambiguous rank`` () =
  let ans = (Move.appyPMove "R5f6")|>getres
  ans.ToString() |> should equal "R5f6"

[<Test>]
let ``Parse promotion`` () =
  let ans = (Move.appyPMove "d7xc8=Q")|>getres
  ans.ToString() |> should equal "d7xc8=Q"

[<Test>]
let ``Parse check`` () =
  let ans = (Move.appyPMove "Bb5+")|>getres
  ans.ToString() |> should equal "Bb5+"

[<Test>]
let ``Parse annotation`` () =
  let ans = (Move.appyPMove "Bb5!!")|>getres
  ans.ToString() |> should equal "Bb5!!"
