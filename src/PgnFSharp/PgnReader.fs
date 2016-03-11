namespace PgnFSharp

open PgnParsers

/// Functions to Parse PGN files and convert to a sequence of Games
module PgnReader = 
    let ReadFromFile file = 
        let p = new Parser()
        p.ReadFromFile(file)
    
    let ReadFromFileDebug file = 
        let p = new Parser()
        p.ReadFromFileDebug(file)

    let ReadFromStream stream = 
        let p = new Parser()
        p.ReadFromStream(stream)
    
    let ReadFromString s = 
        let p = new Parser()
        p.ReadFromString(s)
    
    let ReadGamesFromFile file = 
        let p = new Parser()
        p.ReadGamesFromFile(file)
    
    let ReadGamesFromStream stream = 
        let p = new Parser()
        p.ReadGamesFromStream(stream)
