namespace PgnFSharp

open ilf.pgn.PgnParsers

module PgnReader = 
    let ReadFromFile file = 
        let p = new Parser()
        p.ReadFromFile(file)
    
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
