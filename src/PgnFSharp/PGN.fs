namespace PgnFSharp

open System.Text
open System.Collections.Generic
open System.Text.RegularExpressions
open System.IO

type ChessPgnHeaders = Dictionary<string, string>

module ChessPGNHeaders = 
    let TryParse headerLine = 
        let pattern = @"\[(?<key>[\w]+)\s+\""(?<value>[\S\s]+)\""\]|\[(?<key>[\w]+)\s+\""\""\]"
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
      Headers : ChessPgnHeaders }

module PGN = 
    let Create(headers, moves, result) = 
        { Moves = moves
          Result = result
          Headers = headers }
    
    let StartingPosition(pgn : Pgn) = 
        if pgn.Headers.ContainsKey("FEN") then pgn.Headers.["FEN"]
        else FEN.StartStr
    
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
        let headers = new ChessPgnHeaders()
        
        let rec docomm cl chl = 
            if List.isEmpty chl then InComment(cl), chl
            else 
                let c = chl.Head
                
                let ncl = 
                    if c = '}' then cl - 1
                    elif c = '{' then cl + 1
                    else cl
                if ncl = 0 then Unknown, chl.Tail
                else docomm ncl chl.Tail
        
        let rec dorav cl chl = 
            if List.isEmpty chl then InRAV(cl), chl
            else 
                let c = chl.Head
                
                let ncl = 
                    if c = ')' then cl - 1
                    elif c = '(' then cl + 1
                    else cl
                if ncl = 0 then Unknown, chl.Tail
                else dorav ncl chl.Tail
        
        let rec donag chl = 
            if List.isEmpty chl then Unknown, chl
            else 
                let c = chl.Head
                if c = ' ' then Unknown, chl.Tail
                else donag chl.Tail
        
        let isEnd s = s="1/2-1/2"||s="1-0"||s="0-1"
        let rec donum tok chl = 
            if List.isEmpty chl || chl.Head = ' 'then 
                if tok|>isEnd then FinishedOK, chl
                else Unknown, chl
            else 
                let c = chl.Head
                donum (tok + c.ToString()) chl.Tail
        
        let rec getgm st gd (res, bd, mvl) = 
            let rec proclin st ptok tok (chl, gd) (res, bd, mvl) = 
                if List.isEmpty chl then st, gd, (res, bd, (mvl : Move list))
                else 
                    let c = chl.Head
                    match st with
                    | InComment(cl) -> 
                        let nst, nchl = docomm cl chl
                        proclin nst ptok tok (nchl, gd) (res, bd, mvl)
                    | InRAV(cl) -> 
                        let nst, nchl = dorav cl chl
                        proclin nst ptok tok (nchl, gd) (res, bd, mvl)
                    | InNAG -> 
                        let nst, nchl = donag chl
                        proclin nst ptok tok (nchl, gd) (res, bd, mvl)
                    | InNum -> 
                        let nst, nchl = donum "" chl
                        proclin nst ptok tok (nchl, gd) (res, bd, mvl)
                    | InHeader -> 
                        let nst, (ntok, nbd) = 
                            if c = ']' then 
                                let header = ChessPGNHeaders.Parse("[" + tok + "]")
                                headers.Add(header.Key, header.Value)
                                let nbd = 
                                    if header.Key.ToUpper() = "FEN" then Board.Create2(header.Value |> FEN.FromStr)
                                    else bd
                                Unknown, ("", nbd)
                            else st, (tok + c.ToString(), bd)
                        proclin nst ptok ntok (chl.Tail, gd) (res, nbd, mvl)
                    | InMove -> 
                            let nst, nptok, (ntok, nbd, nmvl) = 
                                if c = ' ' then Unknown, true, (tok, bd, mvl)
                                else st, ptok, (tok + c.ToString(), bd, mvl)
                            if nptok && ntok = "" then proclin st false ntok (chl.Tail, gd) (res, nbd, mvl)
                            elif nptok then 
                                let nptok = false
                                let ngd, nres, nmvl, nbd = 
                                    let move = MoveUtil.Parse nbd ntok
                                    let nbd = nbd |> Board.MoveApply(move)
                                    let nmvl = move :: mvl
                                    if nbd |> MoveGenerate.IsMate then 
                                        let nres = 
                                            if nbd.WhosTurn = Player.White then GameResult.BlackWins |> Some
                                            else GameResult.WhiteWins |> Some
                                        true, nres, nmvl, nbd
                                    else gd, res, nmvl, nbd
                                proclin nst nptok "" (chl.Tail, ngd) (nres, nbd, nmvl)
                            else proclin nst nptok ntok (chl.Tail, gd) (res, nbd, nmvl)
                    | FinishedOK -> 
                            st, true, (res, bd, mvl)
                    | _ -> 
                        if c = '[' then proclin InHeader ptok tok (chl.Tail, gd) (res, bd, mvl)
                        elif c = '{' then proclin (InComment(1)) ptok tok (chl.Tail, gd) (res, bd, mvl)
                        elif c = '(' then proclin (InRAV(1)) ptok tok (chl.Tail, gd) (res, bd, mvl)
                        elif c = '$' then proclin InNAG ptok tok (chl.Tail, gd) (res, bd, mvl)
                        elif c = '*' then proclin FinishedOK ptok tok (chl.Tail, true) (res, bd, mvl)
                        elif System.Char.IsNumber(c)||c = '.' then proclin InNum ptok tok (chl, gd) (res, bd, mvl)
                        elif c = ' ' then proclin Unknown ptok tok (chl.Tail, gd) (res, bd, mvl)
                        else proclin InMove ptok tok (chl, gd) (res, bd, mvl)
            
            let lin = sr.ReadLine()
            if lin = null then res, bd, (mvl |> List.rev)
            else 
                let line = lin + " "
                let cArray = line.ToCharArray()
                let nst, ngd, (nres, nbd, nmvl) = proclin st false "" ((cArray |> Array.toList), gd) (res, bd, mvl)
                if ngd||nst=FinishedOK then nres, nbd, (nmvl |> List.rev)
                else getgm nst ngd (nres, nbd, nmvl)
        
        let res, _, mvl = getgm Unknown false (None, Board.Create2 FEN.Start, [])
        if (mvl.Length > 0 || headers.Count > 0) then Create(headers, mvl, res) |> Some
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
                procmvs (not isw) (ct + 1) mvl.Tail nbd
        
        let board = Board.Create2(StartingPosition pgn |> FEN.FromStr)
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
