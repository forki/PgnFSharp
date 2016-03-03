module PgnFSharp.Tests

open PgnFSharp.FParsec
open FsUnit
open NUnit.Framework

[<Test>]
let ``Parse piece fail`` () =
  let r1 = Parse.applyPc "A"
  r1.ToString() |> should equal "faile"
