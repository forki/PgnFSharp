namespace PgnFSharp

open System.IO
open System.Text

/// Functions to write PGN files from a sequence of Games
module PgnWrite = 
    let ToFile db file = 
        use fs = File.CreateText file
        db |> List.iter (fun (gm : Game) -> fs.WriteLine(gm.ToString()))
    
    let ToStr db = 
        let sb = new StringBuilder()
        db |> List.iter (fun (gm : Game) -> sb.AppendLine(gm.ToString()) |> ignore)
        sb.ToString()
