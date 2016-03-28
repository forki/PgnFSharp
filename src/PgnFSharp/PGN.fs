namespace PgnFSharp

open System.Text
open System.IO

module PGN = 
    type State = 
        | Unknown
        | InHeader
        | InMove
        | InComment of int
        | InRAV of int
        | InNAG
        | InNum
        | FinishedOK
        | Invalid
        | FinishedInvalid
    
    let NextGameRdr(sr : StreamReader) = 
        let pos = Posn.Start()
        
        let rec docomm cl (s : string) = 
            if s = "" then InComment(cl), ""
            else 
                let c = s.[0]
                
                let ncl = 
                    if c = '}' then cl - 1
                    elif c = '{' then cl + 1
                    else cl
                if ncl = 0 then Unknown, s.[1..]
                else docomm ncl s.[1..]
        
        let rec dorav cl (s : string) = 
            if s = "" then InRAV(cl), ""
            else 
                let c = s.[0]
                
                let ncl = 
                    if c = ')' then cl - 1
                    elif c = '(' then cl + 1
                    else cl
                if ncl = 0 then Unknown, s.[1..]
                else dorav ncl s.[1..]
        
        let getwd (s : string) = 
            let eloc = s.IndexOf(" ")
            
            let tok = 
                if eloc = -1 then s
                else s.[..eloc - 1]
            
            let es = 
                if eloc = -1 then ""
                else s.[eloc + 1..]
            
            tok, es
        
        let donag s = 
            let _, es = s |> getwd
            es
        
        let isEnd s = s = "1/2-1/2" || s = "1-0" || s = "0-1"
        
        let donum s = 
            let tok, es = s |> getwd
            if tok |> isEnd then FinishedOK, es
            else Unknown, es
        
        let rec doinv (s : string) = 
            if s = "" then Invalid
            else 
                let tok, es = s |> getwd
                if tok |> isEnd then FinishedInvalid
                else doinv es
        
        let dohdr (gm : Game) (s : string) = 
            let eloc = s.IndexOf("]")
            if eloc = -1 then Invalid, gm, ""
            else 
                let tok = s.[..eloc - 1]
                let es = s.[eloc + 1..]
                let k,v = //gethdr ("[" + tok + "]")
                   match ("[" + tok + "]") with
                     | Header (k,v) -> k,v
                     | _ -> failwith ("not a valid pgn header: " + "[" + tok + "]")
                
                
                if k = "FEN" then Invalid, gm, es
                elif k = "Result" && v = "*" then Invalid, gm, es
                else 
                    let ngm = (k,v) |> gm.AddHdr
                    Unknown, ngm, es
        
        let domv s = 
            let tok, es = s |> getwd
            let mv = tok |> pos.GetMv
            mv |> pos.DoMv
            mv, es
        
        let rec proclin st s gm = 
            if s = "" then st, gm
            else 
                match st with
                | InComment(cl) -> 
                    let nst, ns = docomm cl s
                    proclin nst ns gm
                | InRAV(cl) -> 
                    let nst, ns = dorav cl s
                    proclin nst ns gm
                | InNAG -> 
                    let ns = s |> donag
                    proclin Unknown ns gm
                | InNum -> 
                    let nst, ns = s |> donum
                    proclin nst ns gm
                | Invalid -> 
                    let nst = s |> doinv
                    nst, gm
                | InHeader -> 
                    let nst, ngm, ns = dohdr gm s
                    proclin nst ns ngm
                | InMove -> 
                    let move, ns = domv s
                    proclin Unknown ns { gm with Moves = (move :: gm.Moves) }
                | FinishedOK | FinishedInvalid -> st, gm
                | Unknown -> 
                    let st, ns = 
                        match s.[0] with
                        | '[' -> InHeader, s.[1..]
                        | '{' -> InComment(1), s.[1..]
                        | '(' -> InRAV(1), s.[1..]
                        | '$' -> InNAG, s.[1..]
                        | '*' -> FinishedOK, s.[1..]
                        | c when System.Char.IsNumber(c) || c = '.' -> InNum, s
                        | ' ' -> Unknown, s.[1..]
                        | _ -> InMove, s
                    proclin st ns gm
        
        let rec getgm st gm = 
            let lin = sr.ReadLine()
            if lin |> isNull then { gm with Moves = (gm.Moves |> List.rev) }
            else 
                let nst, ngm = proclin st lin gm
                if nst = FinishedOK then { ngm with Moves = (ngm.Moves |> List.rev) }
                elif nst = FinishedInvalid then Game.Blank
                else getgm nst ngm
        
        let gm = getgm Unknown Game.Blank
        if gm.Moves.Length > 0 then gm |> Some
        else None
    
    let AllGamesRdr(sr : System.IO.StreamReader) = 
        seq { 
            while not sr.EndOfStream do
                let gm = NextGameRdr(sr)
                if gm.IsSome then yield gm.Value
        }
    
    let AllGamesFile(fil : System.IO.FileInfo) = 
        let sr = new System.IO.StreamReader(fil.FullName)
        seq { 
            while not sr.EndOfStream do
                let gm = NextGameRdr(sr)
                if gm.IsSome then yield gm.Value
        }
    
    let NextGameStr(str : string) = 
        let memory = new System.IO.MemoryStream(System.Text.Encoding.UTF8.GetBytes(str))
        let reader = new System.IO.StreamReader(memory)
        NextGameRdr(reader)
    
    let ReadFromStream(stream : Stream) = 
        let sr = new StreamReader(stream)
        let db = AllGamesRdr(sr)
        db
    
    let ReadFromFile(file : string) = 
        let stream = new FileStream(file, FileMode.Open)
        let result = ReadFromStream(stream) |> Seq.toList
        stream.Close()
        result
    
    let ReadGamesFromStream(stream : Stream) = 
        let sr = new StreamReader(stream)
        seq { 
            while not sr.EndOfStream do
                yield NextGameRdr(sr)
        }
    
    let ReadGamesFromFile(file : string) = 
        let sr = new StreamReader(file)
        seq { 
            while not sr.EndOfStream do
                yield NextGameRdr(sr)
        }
    
    let WriteToFile db file = 
        use fs = File.CreateText file
        db |> List.iter (fun (gm : Game) -> fs.WriteLine(gm.ToString()))
    
    let WriteToStr db = 
        let sb = new StringBuilder()
        db |> List.iter (fun (gm : Game) -> sb.AppendLine(gm.ToString()) |> ignore)
        sb.ToString()
