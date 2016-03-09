module PgnFSharp.Tests.Database

open PgnFSharp
open FsUnit
open NUnit.Framework
open System.IO

let fol = @"..\..\..\data"
let fl = Path.Combine(fol,"simple-game.pgn")
let db = PgnReader.ReadFromFile fl
let fol2 = @"..\..\..\data\RealFiles"
let fl2 = Path.Combine(fol2,"demoGames.pgn")
let db2 = PgnReader.ReadFromFile fl2

[<Test>]
let ``Simple database to string length`` () =
  let ans = db.ToString()
  ans.Length |> should equal 865

[<Test>]
let ``Simple database count`` () =
  let ans = db.Length
  ans |> should equal 1

[<Test>]
let ``Demo database to string length`` () =
  let ans = db2.ToString()
  ans.Length |> should equal 4131

[<Test>]
let ``Demo database count`` () =
  let ans = db2.Length
  ans |> should equal 2
