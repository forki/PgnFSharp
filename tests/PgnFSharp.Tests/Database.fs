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

[<Test>]
let ``Empty database test``() = 
    let db1 = PgnReader.ReadFromFile fl1|>List.ofSeq
    db1.ToString().Length |> should equal 2
    db1.Length |> should equal 0

[<Test>]
let ``Simple database test``() = 
    let db = PgnReader.ReadFromFile fl2|>List.ofSeq
    db.ToString().Length |> should equal 15
    db.Length |> should equal 1

[<Test>]
let ``Time database test``() = 
    let db = PgnReader.ReadFromFile fl3|>List.ofSeq
    db.ToString().Length |> should equal 15
    db.Length |> should equal 4

[<Test>]
let ``C database test``() = 
    let db = PgnReader.ReadFromFile fl4|>List.ofSeq
    db.ToString().Length |> should equal 51
    db.Length |> should equal 4

[<Test>]
let ``Informant database test``() = 
    let db = PgnReader.ReadFromFile fl5|>List.ofSeq
    db.ToString().Length |> should equal 51
    db.Length |> should equal 5

[<Test>]
let ``Demo database test``() = 
    let db = PgnReader.ReadFromFile fl6|>List.ofSeq
    db.ToString().Length |> should equal 30
    db.Length |> should equal 2

[<Test>]
let ``Tilb database test``() = 
    let db = PgnReader.ReadFromFile fl7|>List.ofSeq
    db.ToString().Length |> should equal 51
    db.Length |> should equal 6
