module PgnFSharp.Tests.Game

open PgnFSharp
open FsUnit
open NUnit.Framework
open System.IO

let nl = System.Environment.NewLine
let fol = @"..\..\..\data"
let fl = Path.Combine(fol,"simple-game.pgn")
let db = PgnRead.FromFile fl
let gm = db.Head 

[<Test>]
let ``Event to string`` () =
  let ans = gm.Event|>FormatTag "Event"
  ans |> should equal ("[Event \"London Chess Classic\"]" + nl)

[<Test>]
let ``Site to string`` () =
  let ans = gm.Site|>FormatTag "Site"
  ans |> should equal ("[Site \"London\"]" + nl)

[<Test>]
let ``Date to string`` () =
  let ans = gm.DateStr|>FormatTag "Date"
  ans |> should equal ("[Date \"2009.12.13\"]" + nl)

[<Test>]
let ``Round to string`` () =
  let ans = gm.Round|>FormatTag "Round"
  ans |> should equal ("[Round \"5\"]" + nl)

[<Test>]
let ``White to string`` () =
  let ans = gm.White|>FormatTag "White"
  ans |> should equal ("[White \"Howell, David\"]" + nl)

[<Test>]
let ``Black to string`` () =
  let ans = gm.Black|>FormatTag "Black"
  ans |> should equal ("[Black \"Kramnik, Vladimir\"]" + nl)

[<Test>]
let ``Result to string`` () =
  let ans = gm.Result.ToString()|>FormatTag "Result"
  ans |> should equal ("[Result \"1/2-1/2\"]" + nl)

[<Test>]
let ``Game to string length`` () =
  let ans = gm.ToString()
  ans.Length |> should equal 813
