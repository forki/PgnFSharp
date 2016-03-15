namespace PgnFSharp

open System.Text
open System.Collections.Generic
open System.Text.RegularExpressions

type ChessPgnHeaders = Dictionary<string, string>

module ChessPGNHeaders = 
    let TryParse headerLine = 
        let pattern = @"\[(?<key>[\w]+)\s+\""(?<value>[\S\s]+)\""\]"
        let regex = new Regex(pattern)
        let matches = regex.Matches(headerLine)
        if matches.Count <> 1 then 
            let header = new KeyValuePair<string, string>()
            false, header
        else 
            let mtch = matches.[0]
            let header = new KeyValuePair<string, string>(mtch.Groups.["key"].Value, mtch.Groups.["value"].Value)
            true, header
    
    let Parse headerLine = 
        let ok, hdr = TryParse headerLine
        if ok then hdr
        else failwith ("not a valid pgn header: " + headerLine)

type Pgn = 
    { Moves : Move list
      Result : GameResult Option
      ResultReason : GameResultReason
      Headers : ChessPgnHeaders
      Comments : Dictionary<int, string> }

module PGN = 
    let Create(headers, moves, result, comments, reason) = 
        { Moves = moves
          Result = result
          ResultReason = reason
          Headers = headers
          Comments = comments }
    
    let StartingPosition(pgn : Pgn) = 
        if pgn.Headers.ContainsKey("FEN") then pgn.Headers.["FEN"]
        else FEN.StartStr
    
    let NextGameRdr(sr : System.IO.StreamReader) = 
        let headers = new ChessPgnHeaders()
        let comments = new Dictionary<int, string>()
        
        let rec getgm gd cl (res, reas, bd, mvl) = 
            let rec proclin ptok hl tok (cl, chl, gd) (res, reas, bd, mvl) = 
                if List.isEmpty chl then gd, cl, (res, reas, bd, (mvl : Move list))
                else 
                    let c = chl.Head
                    
                    let nptok, (nhl, ntok, ncl, nbd, nmvl) = 
                        if c = '[' then ptok, (hl + 1, tok, cl, bd, mvl)
                        elif c = ']' && hl > 0 then 
                            let nhl = hl - 1
                            if nhl = 0 then 
                                let header = ChessPGNHeaders.Parse("[" + tok + "]")
                                headers.Add(header.Key, header.Value)
                                let nbd = 
                                    if header.Key.ToUpper() = "FEN" then 
                                        Board.Create2 (header.Value |> FEN.FromStr)
                                    else bd
                                ptok, (nhl, "", cl, nbd, mvl)
                            else ptok, (nhl, tok, cl, bd, mvl)
                        elif hl <> 0 then ptok, (hl, (tok + c.ToString()), cl, bd, mvl)
                        elif c = '(' || c = '{' then ptok, (hl, tok, cl + 1, bd, mvl)
                        elif c = ')' || c = '}' then 
                            let ncl = cl - 1
                            if ncl = 0 then 
                                let ntok = 
                                    if (comments.ContainsKey(mvl.Length - 1)) then 
                                        let ntok = comments.[mvl.Length - 1] + " " + tok
                                        comments.Remove(mvl.Length - 1) |> ignore
                                        ntok
                                    else tok
                                comments.Add(mvl.Length - 1, ntok)
                            ptok, (hl, "", ncl, bd, mvl)
                        elif cl <> 0 then ptok, (hl, tok + c.ToString(), cl, bd, mvl)
                        elif c = '.' then ptok, (hl, "", cl, bd, mvl)
                        elif c = ' ' then true, (hl, tok, cl, bd, mvl)
                        else ptok, (hl, tok + c.ToString(), cl, bd, mvl)
                    if nptok && ntok = "" then proclin false nhl ntok (ncl, chl.Tail, gd) (res, reas, nbd, mvl)
                    elif nptok then 
                        let nptok = false
                        let okToProcessResults = (headers.Count > 0 || mvl.Length > 0)
                        
                        let ngd, nres, nreas, nmvl, nbd = 
                            if ntok.Trim() = "1/2-1/2" then 
                                if okToProcessResults then 
                                    let nres = GameResult.Draw |> Some
                                    
                                    let nreas = 
                                        if nbd |> MoveGenerate.IsDrawByStalemate then GameResultReason.Stalemate
                                        elif nbd |> Board.IsDrawBy50MoveRule then GameResultReason.FiftyMoveRule
                                        else GameResultReason.MutualAgreement
                                    true, nres, nreas, mvl, nbd
                                else gd, res, reas, mvl, nbd
                            elif ntok.Trim() = "1-0" then 
                                if okToProcessResults then 
                                    let nres = GameResult.WhiteWins |> Some
                                    
                                    let nreas = 
                                        if nbd |> MoveGenerate.IsMate then GameResultReason.Checkmate
                                        else GameResultReason.Resign
                                    true, nres, nreas, mvl, nbd
                                else gd, res, reas, mvl, nbd
                            elif ntok.Trim() = "0-1" then 
                                if okToProcessResults then 
                                    let nres = GameResult.BlackWins |> Some
                                    
                                    let nreas = 
                                        if nbd |> MoveGenerate.IsMate then GameResultReason.Checkmate
                                        else GameResultReason.Resign
                                    true, nres, nreas, mvl, nbd
                                else gd, res, reas, mvl, nbd
                            elif ntok.Trim() = "*" then 
                                if okToProcessResults then true, None, reas, mvl, nbd
                                else gd, res, reas, mvl, nbd
                            else 
                                let move = MoveUtil.Parse nbd ntok
                                let nbd = nbd |> Board.MoveApply(move)
                                let nmvl = move :: mvl
                                if nbd |> MoveGenerate.IsMate then 
                                    let nres = 
                                        if nbd.WhosTurn = Player.White then GameResult.BlackWins |> Some
                                        else GameResult.WhiteWins |> Some
                                    
                                    let nreas = GameResultReason.Checkmate
                                    true, nres, nreas, nmvl, nbd
                                else gd, res, reas, nmvl, nbd
                        proclin nptok nhl "" (ncl, chl.Tail, ngd) (nres, nreas, nbd, nmvl)
                    else proclin nptok nhl ntok (ncl, chl.Tail, gd) (res, reas, nbd, nmvl)
            
            let lin = sr.ReadLine()
            if lin = null then res, reas, bd, (mvl |> List.rev)
            else 
                let line = lin + " "
                let cArray = line.ToCharArray()
                let ngd, ncl, (nres, nreas, nbd, nmvl) = 
                    proclin false 0 "" (cl, (cArray |> Array.toList), gd) (res, reas, bd, mvl)
                if not ngd || ncl <> 0 then getgm ngd ncl (nres, nreas, nbd, nmvl)
                else nres, nreas, nbd, (nmvl |> List.rev)
        
        let res, reas, _, mvl = 
            getgm false 0 (None, GameResultReason.NotDecided, Board.Create2 FEN.Start, [])
        if (mvl.Length > 0 || headers.Count > 0) then Create(headers, mvl, res, comments, reas) |> Some
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
    
    let Write (writer : System.IO.TextWriter) (pgn : Pgn) = 
        let maxLineLen = 90
        let nl = System.Environment.NewLine
        let sbMoves = new StringBuilder()
        let sbHeaders = new StringBuilder()
        //headers
        for header in pgn.Headers do
            sbHeaders.AppendLine(sprintf @"[%s ""%s""]" header.Key header.Value) |> ignore
        if not (pgn.Headers.ContainsKey("Result")) then 
            sbHeaders.Append(sprintf @"[Result ""%s""]" pgn.Headers.["Result"] + nl) |> ignore
        sbHeaders.Append(nl) |> ignore
        //moves
        let rec procmvs isw ct (mvl : Move list) bd = 
            if not (List.isEmpty mvl) then 
                let mv = mvl.Head
                if isw then 
                    let ifullmove = (ct / 2) + 1
                    sbMoves.Append(ifullmove.ToString() + ". ") |> ignore
                sbMoves.Append((MoveUtil.DescBd mv bd) + " ") |> ignore
                let nbd = bd |> Board.MoveApply(mv)
                if pgn.Comments.ContainsKey(ct - 1) then 
                    let comment = pgn.Comments.[ct - 1]
                    sbMoves.Append("{") |> ignore
                    sbMoves.Append(comment) |> ignore
                    sbMoves.Append("} ") |> ignore
                procmvs (not isw) (ct + 1) mvl.Tail nbd
        
        let board = Board.Create2 (StartingPosition pgn |> FEN.FromStr)
        let mvl = pgn.Moves
        procmvs true 1 mvl board
        //result
        if pgn.Result.Value = GameResult.WhiteWins then sbMoves.Append("1-0 ") |> ignore
        elif pgn.Result.Value = GameResult.BlackWins then sbMoves.Append("0-1 ") |> ignore
        elif pgn.Result.Value = GameResult.Draw then sbMoves.Append("1/2-1/2 ") |> ignore
        //reformat move section to break into lines
        else 
            if //return result
               not pgn.Result.IsSome then sbMoves.Append("* ") |> ignore
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
