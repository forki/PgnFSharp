﻿namespace PgnFSharp

open System.IO

module PgnWriter = 
    let nl = System.Environment.NewLine
    let Write db file = 
        use fs = File.CreateText file
        db|>List.iter(fun (gm:Game) -> fs.WriteLine(gm.ToString()))
