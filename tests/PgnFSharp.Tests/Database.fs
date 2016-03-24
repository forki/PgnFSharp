module PgnFSharp.Tests.Database

open PgnFSharp
open FsUnit
open NUnit.Framework
open System.IO

let fol = @"..\..\..\data"
let fl1 = Path.Combine(fol, "empty-file.pgn")
let fl2 = Path.Combine(fol, "simple-game.pgn")
let fl3 = Path.Combine(fol, "time-annotated-games.pgn")
let fol2 = @"..\..\..\data\RealFiles"
let fl4 = Path.Combine(fol2, "C.pgn")
let fl5 = Path.Combine(fol2, "chess-informant-sample.pgn")
let fl6 = Path.Combine(fol2, "demoGames.pgn")
let fl7 = Path.Combine(fol2, "tilb98r2.pgn")
let fl8 = Path.Combine(fol2, "endings.pgn")

[<Test>]
let ``Empty database test``() = 
    let db1 = PGN.ReadFromFile fl1
    db1.ToString().Length |> should equal 2
    db1.Length |> should equal 0

[<Test>]
let ``Simple database test``() = 
    let db = PGN.ReadFromFile fl2
    let ans = db|>PGN.WriteToStr
    ans.Length |> should equal 814
    db.Length |> should equal 1

[<Test>]
let ``Time database test``() = 
    let db = PGN.ReadFromFile fl3
    let ans = db|>PGN.WriteToStr
    ans.Length |> should equal 2715
    db.Length |> should equal 4

[<Test>]
let ``C database test``() = 
    let db = PGN.ReadFromFile fl4
    let ans = db|>PGN.WriteToStr
    ans.Length |> should equal 2967
    db.Length |> should equal 4

[<Test>]
let ``Informant database test``() = 
    let db = PGN.ReadFromFile fl5
    let ans = db|>PGN.WriteToStr
    ans.Length |> should equal 3630
    db.Length |> should equal 5

[<Test>]
let ``Demo database test``() = 
    let db = PGN.ReadFromFile fl6
    let ans = db|>PGN.WriteToStr
    ans.Length |> should equal 1395
    db.Length |> should equal 2

[<Test>]
let ``Tilb database test``() = 
    let db = PGN.ReadFromFile fl7
    let ans = db|>PGN.WriteToStr
    ans.Length |> should equal 4049
    db.Length |> should equal 6

[<Test>]
let ``Endings database test``() = 
    let db = PGN.ReadFromFile fl8
    let ans = db|>PGN.WriteToStr
    ans.Length |> should equal 0
    db.Length |> should equal 0
