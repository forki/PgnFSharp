namespace PgnFSharp

/// Functions to Parse PGN files and convert to a sequence of Games
module PgnReader = 
    let ReadFromFile file = 
        let p = new Parser()
        p.ReadFromFile(file)
    
    let ReadFromStream stream = 
        let p = new Parser()
        p.ReadFromStream(stream)
    
    let ReadGamesFromFile file = 
        let p = new Parser()
        p.ReadGamesFromFile(file)
    
    let ReadGamesFromStream stream = 
        let p = new Parser()
        p.ReadGamesFromStream(stream)
