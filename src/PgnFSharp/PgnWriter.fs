namespace PgnFSharp

open System.IO

module PgnWriter = 
    let Write db file = 
        db|>List.iter(fun gm -> File.AppendAllText(file,gm.ToString()))
