#r @"I:\GitHub\PgnFSharp\src\PgnFSharp\bin\Release\PgnFSharp.dll"

open PgnFSharp
open System.IO
#time
let fol = @"I:\GitHub\PgnFSharp\tests\data\RealFiles"
let fl = Path.Combine(fol,"o-deville.pgn")
let ofl = Path.Combine(fol,"o-deville_copy.pgn")
//let fl = Path.Combine(fol,"demoGames.pgn")
//let ofl = Path.Combine(fol,"demoGames_copy.pgn")
let gms = PgnReader.ReadFromFile fl 
if File.Exists ofl then File.Delete ofl
PgnWriter.Write gms ofl