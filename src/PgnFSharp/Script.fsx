#r @"I:\GitHub\PgnFSharp\src\PgnFSharp\bin\Release\FParsecCS.dll"
#r @"I:\GitHub\PgnFSharp\src\PgnFSharp\bin\Release\FParsec.dll"
#r @"I:\GitHub\PgnFSharp\src\PgnFSharp\bin\Release\PgnFSharp.dll"

open PgnFSharp
open System.IO
open FParsec
#time
let fol = @"I:\GitHub\PgnFSharp\tests\data\RealFiles"
//let fl = Path.Combine(fol,"o-deville.pgn")
//let ofl = Path.Combine(fol,"o-deville_copy.pgn")
let fl = Path.Combine(fol,"demoGames.pgn")
//let ofl = Path.Combine(fol,"demoGames_copy.pgn")
let gms = PgnReader.ReadFromFile fl 
//if File.Exists ofl then File.Delete ofl
//PgnWriter.Write gms ofl
let gettypes gm = gm.Moves|>List.map(fun m -> m.Type,m.ToString())
let typs = gms|>List.map gettypes|>List.reduce(@)
let tst = run PgnParsers.Move.pShortMove "a6"
let tst2 = run PgnParsers.Move.pShortMove "Ra6"