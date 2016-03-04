module PgnFSharp.Tests

open PgnFSharp
open FsUnit
open NUnit.Framework

[<Test>]
let ``Parse piece fail`` () =
  let r1 = "A"
  r1.ToString() |> should equal "faile"
