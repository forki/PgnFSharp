namespace PgnFSharp

open System.Text
open System.Text.RegularExpressions
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
        
        let gethdr hl = 
            let pattern = @"\[(?<key>[\w]+)\s+\""(?<value>[\S\s]+)\""\]|\[(?<key>[\w]+)\s+\""\""\]"
            let regex = new Regex(pattern)
            let matches = regex.Matches(hl)
            if matches.Count <> 1 then failwith ("not a valid pgn header: " + hl)
            else 
                let mtch = matches.[0]
                mtch.Groups.["key"].Value, mtch.Groups.["value"].Value
        
        let dohdr (gm:Game) (s : string) = 
            let eloc = s.IndexOf("]")
            if eloc = -1 then Invalid, gm, ""
            else 
                let tok = s.[..eloc - 1]
                let es = s.[eloc + 1..]
                let h = gethdr ("[" + tok + "]")
                let ngm = h |>gm.AddHdr
                if fst h = "FEN" then Invalid, ngm, es
                else Unknown, ngm, es
        
        let domv bd s = 
            let tok, es = s |> getwd
            let move = MoveUtil.Parse bd tok
            let nbd = bd |> Board.MoveApply(move)
            move, nbd, es
        
        let rec proclin st s bd gm = 
            if s = "" then st, bd, gm
            else 
                match st with
                | InComment(cl) -> 
                    let nst, ns = docomm cl s
                    proclin nst ns bd gm
                | InRAV(cl) -> 
                    let nst, ns = dorav cl s
                    proclin nst ns bd gm
                | InNAG -> 
                    let ns = s |> donag
                    proclin Unknown ns bd gm
                | InNum -> 
                    let nst, ns = s |> donum
                    proclin nst ns bd gm
                | Invalid -> 
                    let nst = s |> doinv
                    nst, bd, gm
                | InHeader -> 
                    let nst, ngm, ns = dohdr gm s
                    proclin nst ns bd ngm
                | InMove -> 
                    let move, nbd, ns = domv bd s
                    proclin Unknown ns nbd {gm with Moves = (move :: gm.Moves)}
                | FinishedOK | FinishedInvalid -> st, bd, gm
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
                    proclin st ns bd gm
        
        let rec getgm st bd gm = 
            let lin = sr.ReadLine()
            if lin |> isNull then bd, {gm with Moves = (gm.Moves |> List.rev)}
            else 
                let nst, nbd, ngm = proclin st lin bd gm
                if nst = FinishedOK then nbd, {ngm with Moves = (ngm.Moves |> List.rev)}
                elif nst = FinishedInvalid then nbd, Game.Blank()
                else getgm nst nbd ngm
        
        let _, gm = getgm Unknown (Board.Create2 FEN.Start) (Game.Blank())
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
    
    let Write (writer : System.IO.TextWriter) (pgn : Game) = 
        let maxLineLen = 90
        let nl = System.Environment.NewLine
        let sbMoves = new StringBuilder()
        let sbHeaders = new StringBuilder()
        
        //headers
        //        for header in pgn.Headers do
        //            sbHeaders.AppendLine(sprintf @"[%s ""%s""]" header.Key header.Value) |> ignore
        //        if not (pgn.Headers.ContainsKey("Result")) then 
        //            sbHeaders.Append(sprintf @"[Result ""%s""]" pgn.Headers.["Result"] + nl) |> ignore
        //        sbHeaders.Append(nl) |> ignore
        //moves
        let rec procmvs isw ct (mvl : Move list) bd = 
            if not (List.isEmpty mvl) then 
                let mv = mvl.Head
                if isw then 
                    let ifullmove = (ct / 2) + 1
                    sbMoves.Append(ifullmove.ToString() + ". ") |> ignore
                sbMoves.Append((MoveUtil.DescBd mv bd) + " ") |> ignore
                let nbd = bd |> Board.MoveApply(mv)
                procmvs (not isw) (ct + 1) mvl.Tail nbd
        
        let board = Board.Create2(FEN.Start)
        let mvl = pgn.Moves
        procmvs true 1 mvl board
        //result
        //        if pgn.Result.Value = GameResult.WhiteWins then sbMoves.Append("1-0 ") |> ignore
        //        elif pgn.Result.Value = GameResult.BlackWins then sbMoves.Append("0-1 ") |> ignore
        //        elif pgn.Result.Value = GameResult.Draw then sbMoves.Append("1/2-1/2 ") |> ignore
        //reformat move section to break into lines
        //        else 
        //            if //return result
        //               not pgn.Result.IsSome then sbMoves.Append("* ") |> ignore
        let wda = sbMoves.ToString().Split(' ')
        let sbMoves1 = new StringBuilder()
        
        let rec procwds cl wdl = 
            if List.isEmpty wdl then cl
            else 
                let wd : string = wdl.Head
                
                let ncl = 
                    if cl = "" then wd
                    elif (cl.Length + wd.Length) > maxLineLen then 
                        sbMoves1.Append(cl + nl) |> ignore
                        wd
                    else cl + " " + wd
                procwds ncl wdl.Tail
        
        let cl = procwds "" (wda |> Array.toList)
        sbMoves1.Append(cl) |> ignore
        writer.Write(sbHeaders.ToString())
        writer.Write(sbMoves1.ToString())
        writer.WriteLine()
