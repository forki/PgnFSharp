open PgnFSharp
open System.IO
open System

[<EntryPoint>]
let main argv = 
    let st = DateTime.Now
    let fol = @"I:\GitHub\PgnFSharp\tests\data\RealFiles"
    let fl = Path.Combine(fol,"o-deville.pgn")
    //let fl = Path.Combine(fol,"demoGames.pgn")
    use sr = new StreamReader(fl)
    let gms = PGN.AllGamesRdr sr|>Seq.toArray

    let nd = DateTime.Now
    let el = (nd-st).TotalSeconds
    printfn "Elasped Time: %f" el
    printfn "Press enter to exit"
    Console.ReadLine()|>ignore
    0

