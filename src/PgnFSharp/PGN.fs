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
                else s.[..eloc-1]
            
            let es = 
                if eloc = -1 then ""
                else s.[eloc+1..]
            tok,es
        
        let donag s = 
            let _,es = s|>getwd 
            es
        
        let isEnd s = s = "1/2-1/2" || s = "1-0" || s = "0-1"
        
        let donum s = 
            let tok,es = s|>getwd 
            if tok |> isEnd then FinishedOK, es
            else Unknown, es
        
        let doinv (s:string) = 
            let tok,es = s.Trim()|>getwd 
            if tok |> isEnd then FinishedInvalid, es
            else Invalid, es
        
        let gethdr hl = 
            let pattern = @"\[(?<key>[\w]+)\s+\""(?<value>[\S\s]+)\""\]|\[(?<key>[\w]+)\s+\""\""\]"
            let regex = new Regex(pattern)
            let matches = regex.Matches(hl)
            if matches.Count <> 1 then failwith ("not a valid pgn header: " + hl)
            else 
                let mtch = matches.[0]
                mtch.Groups.["key"].Value, mtch.Groups.["value"].Value
        
        let dohdr hl (s : string) = 
            let eloc = s.IndexOf("]")
            if eloc = -1 then Invalid, hl, ""
            else 
                let tok = s.[..eloc-1]
                let es = s.[eloc+1..]
                let h = gethdr ("[" + tok + "]")
                let nhl = h :: hl
                if fst h = "FEN" then Invalid, nhl, es
                else Unknown, nhl, es
        
        let domv bd s = 
            let tok,es = s|>getwd 
            let move = MoveUtil.Parse bd tok
            let nbd = bd |> Board.MoveApply(move)
            move, nbd, es
        
        let rec proclin st s bd mvl hl = 
            if s = "" then st, bd, mvl, hl
            else 
                match st with
                | InComment(cl) -> 
                    let nst, ns = docomm cl s
                    proclin nst ns bd mvl hl
                | InRAV(cl) -> 
                    let nst, ns = dorav cl s
                    proclin nst ns bd mvl hl
                | InNAG -> 
                    let ns = s |> donag
                    proclin Unknown ns bd mvl hl
                | InNum -> 
                    let nst, ns = s |> donum
                    proclin nst ns bd mvl hl
                | Invalid -> 
                    let nst, ns = s |> doinv
                    proclin nst ns bd mvl hl
                | InHeader -> 
                    let nst, nhl, ns = dohdr hl s
                    proclin nst ns bd mvl nhl
                | InMove -> 
                    let move, nbd, ns = domv bd s
                    proclin Unknown ns nbd (move :: mvl) hl
                | FinishedOK | FinishedInvalid -> st, bd, mvl, hl
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
                    proclin st ns bd mvl hl
        
        let rec getgm st bd mvl hl = 
            let lin = sr.ReadLine()
            if lin |> isNull then bd, (mvl |> List.rev), hl
            else 
                let nst, nbd, nmvl, nhl = proclin st lin bd mvl hl
                if nst = FinishedOK then nbd, (nmvl |> List.rev), nhl
                elif nst = FinishedInvalid then nbd, [], []
                else getgm nst nbd nmvl nhl
        
        let _, mvl, hl = getgm Unknown (Board.Create2 FEN.Start) [] []
        if mvl.Length > 0 then Game.Create(hl, mvl) |> Some
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
