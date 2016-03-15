namespace PgnFSharp

open System.IO

type Parser() = 
    
    member this.ReadFromFile(file : string) = 
        let stream = new FileStream(file, FileMode.Open)
        let result = this.ReadFromStream(stream)|>Seq.toList
        stream.Close()
        result
    
    member this.ReadFromStream(stream : Stream) = 
        let sr = new StreamReader(stream)
        let db = PGN.AllGamesRdr(sr)
        db
    
    member this.ReadGamesFromStream(stream : Stream) = 
        let sr = new StreamReader(stream)
        seq { 
            while not sr.EndOfStream do
                yield this.ParseGame(sr)
        }
    
    member this.ReadGamesFromFile(file : string) = 
        let sr = new StreamReader(file)
        seq { 
            while not sr.EndOfStream do
                yield this.ParseGame(sr)
        }
    
    member this.ParseGame(sr : StreamReader) = 
        let gm = PGN.NextGameRdr(sr)
        gm
