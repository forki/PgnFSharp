namespace PgnFSharp

open System.IO

/// Functions to write PGN files from a sequence of Games
module PgnWriter = 
    let nl = System.Environment.NewLine
    let Write db file = 
        use fs = File.CreateText file
        db|>List.iter(fun (gm:Game) -> fs.WriteLine(gm.ToString()))
