namespace PgnFSharp

/// Functions to Parse PGN files and convert to a sequence of Games
module PgnRead = 
    let FromFile file = 
        let p = new Parser()
        p.ReadFromFile(file)
    
    let FromStream stream = 
        let p = new Parser()
        p.ReadFromStream(stream)
    
    let GamesFromFile file = 
        let p = new Parser()
        p.ReadGamesFromFile(file)
    
    let GamesFromStream stream = 
        let p = new Parser()
        p.ReadGamesFromStream(stream)
