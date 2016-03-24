﻿namespace PgnFSharp

open System
open System.Text
open System.Text.RegularExpressions

/// Move type where not a simple move
type MvTyp = 
    | Prom of char
    | CasK
    | CasQ
    | Ep

/// Index of square on the board
type Sq = int

/// Fast type for making moves on board
type Mov = 
    { Mfrom : Sq
      Mto : Sq
      Mtyp : MvTyp option
      Mpgn : string }
    override x.ToString() = x.Mpgn

/// Board state - quick version
type Posn = 
    { Sqs : char []
      mutable IsW : bool }
    
    override x.ToString() = 
        let sb = new StringBuilder()
        
        let rec getstr i e chl = 
            if List.isEmpty chl then 
                if e > 0 then sb.Append(e.ToString()) |> ignore
                sb.ToString() + (if x.IsW then " w"
                                 else " b")
            elif i <> 8 && chl.Head = ' ' then getstr (i + 1) (e + 1) chl.Tail
            else 
                if e > 0 then sb.Append(e.ToString()) |> ignore
                if i = 8 then sb.Append("/") |> ignore
                else sb.Append(chl.Head) |> ignore
                if i = 8 then getstr 0 0 chl
                else getstr (i + 1) 0 chl.Tail
        getstr 0 0 (x.Sqs |> List.ofArray)
    
    member x.Copy() = 
        { Sqs = x.Sqs |> Array.copy
          IsW = x.IsW }
    
    member x.DoMv mv = 
        x.IsW <- not x.IsW
        let c = x.Sqs.[mv.Mfrom]
        x.Sqs.[mv.Mfrom] <- ' '
        x.Sqs.[mv.Mto] <- c
        if mv.Mtyp.IsSome then 
            match mv.Mtyp.Value with
            | Prom(c) -> x.Sqs.[mv.Mto] <- if x.IsW then c|>Char.ToLower else c
            | CasK -> 
                x.Sqs.[mv.Mto - 1] <- x.Sqs.[mv.Mto + 1]
                x.Sqs.[mv.Mto + 1] <- ' '
            | CasQ -> 
                x.Sqs.[mv.Mto + 1] <- x.Sqs.[mv.Mto - 2]
                x.Sqs.[mv.Mto - 2] <- ' '
            | Ep -> 
                if x.IsW then x.Sqs.[mv.Mto - 8] <- ' '
                else x.Sqs.[mv.Mto + 8] <- ' '

module Psn = 
    ///Dictionary of files
    let private fDct = 
        [ 'a'..'h' ]
        |> List.mapi (fun i c -> c, i)
        |> dict
    
    ///Dictionary of ranks
    let rDct = 
        [ 1..8 ]
        |> List.rev
        |> List.mapi (fun i c -> char (c.ToString()), i)
        |> dict
    
    ///Dictionary of squares
    let SqDct = 
        [ for r = 8 downto 1 do
              for f in [ 'a'..'h' ] do
                  yield f.ToString() + r.ToString() ]
        |> List.mapi (fun i s -> s, i)
        |> dict
    
    /// loads Posn given a FEN like string
    let FromStr(s : string) = 
        let b = s.Split(' ')
        let isw = b.[1] = "w"
        let sqs = Array.create 64 ' '
        
        let rec getp i ps = 
            if ps = "" then 
                { Sqs = sqs
                  IsW = isw }
            else 
                match ps.[0] with
                | '/' -> getp i ps.[1..]
                | c -> 
                    let ok, p = Int32.TryParse(c.ToString())
                    if ok then getp (i + p) ps.[1..]
                    else 
                        sqs.[i] <- c
                        getp (i + 1) ps.[1..]
        getp 0 b.[0]
    
    /// Gets initial Posn
    let Start() = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w" |> FromStr
    
    /// Gets Move from string
    let GetMv pos mv = 
        //general failure message
        let fl() = failwith ("not done yet, mv: " + mv + " pos: " + pos.ToString())
        let samef a b = a % 8 = b % 8
        let samer a b = a / 8 = b / 8
        let samefr a b = samef a b || samer a b
        let samedg a b = abs (a % 8 - b % 8) = abs (a / 8 - b / 8)
        let samedgfr a b = samedg a b || samefr a b
        let isnmv a b  =
            let rd= abs(a/8-b/8)
            let fd= abs(a%8-b%8)
            rd=2&&fd=1||fd=2&&rd=1
        
        let strip chars = 
            String.collect (fun c -> 
                if Seq.exists ((=) c) chars then ""
                else string c)
        
        let m = mv |> strip "+x#=!?"
        let m = m.Replace("e.p.", "")
        //simple piece move e.g. Nf3
        if Regex.IsMatch(m, "^[BNRQK][abcdefgh][12345678]$") then 
            let mto = SqDct.[m.[1..]]
            
            let pc = 
                if pos.IsW then m.[0]
                else m.[0] |> Char.ToLower
            
            let mfs = 
                pos.Sqs
                |> Array.mapi (fun i c -> i, c)
                |> Array.filter (fun (_, c) -> c = pc)
                |> Array.map fst
            
            if mfs.Length = 1 then 
                { Mfrom = mfs.[0]
                  Mto = mto
                  Mtyp = None
                  Mpgn = mv }
            else 
                match pc with
                | 'N' | 'n' -> 
                    let ms = mfs |> Array.filter (isnmv mto)
                    if ms.Length = 1 then 
                        { Mfrom = ms.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    //filter out moves that lead to check
                    else 
                        let isok m = 
                            let np = pos.Copy()
                            
                            let mov = 
                                { Mfrom = m
                                  Mto = mto
                                  Mtyp = None
                                  Mpgn = mv }
                            mov |> np.DoMv
                            let kc = 
                                if pos.IsW then 'K'
                                else 'k'
                            
                            let kia = 
                                np.Sqs
                                |> Array.mapi (fun i c -> i, c)
                                |> Array.filter (fun (_, c) -> c = kc)
                                |> Array.map fst
                            
                            let ki = kia.[0]
                            
                            let qc = 
                                if pos.IsW then 'q'
                                else 'Q'
                            
                            let rec chkd stop a rbc i = 
                                if stop i then false
                                else 
                                    let j = i + a
                                    let pc = np.Sqs.[j]
                                    if pc = qc || pc = rbc then true
                                    elif pc = ' ' || pc = kc then chkd stop a rbc j
                                    else false
                            
                            let rc = 
                                if pos.IsW then 'r'
                                else 'R'
                            
                            let bc = 
                                if pos.IsW then 'b'
                                else 'B'
                            
                            let chkn = chkd (fun i -> i / 8 = 0) -8 rc
                            let chke = chkd (fun i -> i % 8 = 7) 1 rc
                            let chks = chkd (fun i -> i / 8 = 7) 8 rc
                            let chkw = chkd (fun i -> i % 8 = 0) -1 rc
                            let chkne = chkd (fun i -> i / 8 = 0 || i % 8 = 7) -7 bc
                            let chkse = chkd (fun i -> i / 8 = 7 || i % 8 = 7) 9 bc
                            let chksw = chkd (fun i -> i / 8 = 7 || i % 8 = 0) 7 bc
                            let chknw = chkd (fun i -> i / 8 = 0 || i % 8 = 0) -9 bc
                            let inchk = 
                                chkn ki || chke ki || chks ki || chkw ki || chkne ki || chkse ki || chksw ki || chknw ki
                            not inchk
                        
                        let nms = ms |> Array.filter isok
                        if nms.Length = 1 then 
                            { Mfrom = nms.[0]
                              Mto = mto
                              Mtyp = None
                              Mpgn = mv }
                        else fl()
                | 'B' | 'b' -> 
                    let fmfs = mfs |> Array.filter (samedg mto)
                    if fmfs.Length = 1 then 
                        { Mfrom = fmfs.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else fl()
                | 'Q' | 'q' -> 
                    let fmfs = mfs |> Array.filter (samedgfr mto)
                    if fmfs.Length = 1 then 
                        { Mfrom = fmfs.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else
                        let rec getval fl = 
                            if List.isEmpty fl then 
                                failwith ("can't find valid move, mv: " + mv + " pos: " + pos.ToString())
                            else 
                                let f = fl.Head
                                if samer mto f then 
                                    let betw = 
                                        if mto < f then pos.Sqs.[mto + 1..f - 1]
                                        else pos.Sqs.[f + 1..mto - 1]
                                    if (betw |> Array.filter (fun c -> c <> ' ')).Length = 0 then f
                                    else getval fl.Tail
                                elif samef mto f then
                                    let betw = 
                                        if mto < f then [ mto + 8..8..f - 8 ] |> List.map (fun i -> pos.Sqs.[i])
                                        else [ f + 8..8..mto - 8 ] |> List.map (fun i -> pos.Sqs.[i])
                                    if (betw |> List.filter (fun c -> c <> ' ')).Length = 0 then f
                                    else getval fl.Tail
                                //onsame diagonal
                                else 
                                    let betw = 
                                        if mto < f && (f-mto)%7=0 then [ mto + 7..7..f - 7 ] |> List.map (fun i -> pos.Sqs.[i])
                                        elif mto < f then [ mto + 9..9..f - 9 ] |> List.map (fun i -> pos.Sqs.[i])
                                        elif (mto-f)%7=0 then [ f + 7..7..mto - 7 ] |> List.map (fun i -> pos.Sqs.[i])
                                        else [ f + 9..9..mto - 9 ] |> List.map (fun i -> pos.Sqs.[i])
                                    if (betw |> List.filter (fun c -> c <> ' ')).Length = 0 then f
                                    else getval fl.Tail
                        
                        let mfrom = 
                            fmfs
                            |> List.ofArray
                            |> getval
                        
                        { Mfrom = mfrom
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                | 'R' | 'r' -> 
                    let fmfs = mfs |> Array.filter (samefr mto)
                    if fmfs.Length = 1 then 
                        { Mfrom = fmfs.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else 
                        let rec getval fl = 
                            if List.isEmpty fl then 
                                failwith ("can't find valid move, mv: " + mv + " pos: " + pos.ToString())
                            else 
                                let f = fl.Head
                                if samer mto f then 
                                    let betw = 
                                        if mto < f then pos.Sqs.[mto + 1..f - 1]
                                        else pos.Sqs.[f + 1..mto - 1]
                                    if (betw |> Array.filter (fun c -> c <> ' ')).Length = 0 then f
                                    else getval fl.Tail
                                else 
                                    let betw = 
                                        if mto < f then [ mto + 8..8..f - 8 ] |> List.map (fun i -> pos.Sqs.[i])
                                        else [ f + 8..8..mto - 8 ] |> List.map (fun i -> pos.Sqs.[i])
                                    if (betw |> List.filter (fun c -> c <> ' ')).Length = 0 then f
                                    else getval fl.Tail
                        
                        let mfrom = 
                            fmfs
                            |> List.ofArray
                            |> getval
                        
                        { Mfrom = mfrom
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                | _ -> fl()
        //simple pawn move e.g. d4
        elif Regex.IsMatch(m, "^[abcdefgh][12345678]$") then 
            let mto = SqDct.[m]
            if pos.IsW then 
                if pos.Sqs.[mto + 8] = 'P' then 
                    { Mfrom = mto + 8
                      Mto = mto
                      Mtyp = None
                      Mpgn = mv }
                else 
                    { Mfrom = mto + 16
                      Mto = mto
                      Mtyp = None
                      Mpgn = mv }
            else if pos.Sqs.[mto - 8] = 'p' then 
                { Mfrom = mto - 8
                  Mto = mto
                  Mtyp = None
                  Mpgn = mv }
            else 
                { Mfrom = mto - 16
                  Mto = mto
                  Mtyp = None
                  Mpgn = mv }
        elif m = "O-O" || m = "0-0" || m = "o-o" then 
            if pos.IsW then 
                { Mfrom = 60
                  Mto = 62
                  Mtyp = CasK |> Some
                  Mpgn = mv }
            else 
                { Mfrom = 4
                  Mto = 6
                  Mtyp = CasK |> Some
                  Mpgn = mv }
        elif m = "O-O-O" || m = "0-0-0" || m = "o-o-o" then 
            if pos.IsW then 
                { Mfrom = 60
                  Mto = 58
                  Mtyp = CasQ |> Some
                  Mpgn = mv }
            else 
                { Mfrom = 4
                  Mto = 2
                  Mtyp = CasQ |> Some
                  Mpgn = mv }
        //pawn capture like exd6
        elif Regex.IsMatch(m, "^[abcdefgh][abcdefgh][12345678]$") then 
            let mto = SqDct.[m.[1..]]
            
            let r = 
                int (m.[2].ToString()) + (if pos.IsW then -1
                                          else 1)
            
            let mtyp = 
                if pos.Sqs.[mto] = ' ' then Ep |> Some
                else None
            
            let mfrom = SqDct.[m.[0].ToString() + r.ToString()]
            { Mfrom = mfrom
              Mto = mto
              Mtyp = mtyp
              Mpgn = mv }
        //ambiguous file like Nge2
        elif Regex.IsMatch(m, "^[BNRQK][abcdefgh][abcdefgh][12345678]$") then 
            let mto = SqDct.[m.[2..]]
            
            let pc = 
                if pos.IsW then m.[0]
                else m.[0] |> Char.ToLower
            
            let fn = fDct.[m.[1]]
            
            let mfs = 
                pos.Sqs
                |> Array.mapi (fun i c -> i, c)
                |> Array.filter (fun (_, c) -> c = pc)
                |> Array.map fst
            if mfs.Length = 1 then 
                { Mfrom = mfs.[0]
                  Mto = mto
                  Mtyp = None
                  Mpgn = mv }
            else 
                match pc with
                | 'N' | 'n' -> 
                    let ms = mfs |> Array.filter (isnmv mto)|>Array.filter(fun f -> f%8=fn)
                    if ms.Length = 1 then 
                        { Mfrom = ms.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else fl()
                | 'R' | 'r' | 'Q' | 'q'  -> 
                    let fmfs = mfs |> Array.filter (fun f -> f % 8 = fn)
                    if fmfs.Length = 1 then 
                        { Mfrom = fmfs.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else fl()
                | _ -> fl()
        //ambiguous rank like R7a6
        elif Regex.IsMatch(m, "^[BNRQK][12345678][abcdefgh][12345678]$") then 
            let mto = SqDct.[m.[2..]]
            
            let pc = 
                if pos.IsW then m.[0]
                else m.[0] |> Char.ToLower
            
            let rn = rDct.[m.[1]]
            
            let mfs = 
                pos.Sqs
                |> Array.mapi (fun i c -> i, c)
                |> Array.filter (fun (_, c) -> c = pc)
                |> Array.map fst
            if mfs.Length = 1 then 
                { Mfrom = mfs.[0]
                  Mto = mto
                  Mtyp = None
                  Mpgn = mv }
            else 
                match pc with
                | 'N' | 'n' -> 
                    let ms = mfs |> Array.filter (isnmv mto)|>Array.filter(fun f -> f/8=rn)
                    if ms.Length = 1 then 
                        { Mfrom = ms.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else fl()
                | 'R' | 'r' | 'Q' | 'q'  -> 
                    let rmfs = mfs |> Array.filter (fun f -> f / 8 = rn)
                    if rmfs.Length = 1 then 
                        { Mfrom = rmfs.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else fl()
                | _ -> fl()
        //pawn promotion like b8=Q
        elif Regex.IsMatch(m, "^[abcdefgh][12345678][BNRQK]$") then 
            let mto = SqDct.[m.[0..1]]
            
            let r = 
                int (m.[1].ToString()) + (if pos.IsW then -1
                                          else 1)
            
            let mfrom = SqDct.[m.[0].ToString() + r.ToString()]
            { Mfrom = mfrom
              Mto = mto
              Mtyp = Prom(m.[2]) |> Some
              Mpgn = mv }
        //pawn promotion capture like a*b8=Q
        elif Regex.IsMatch(m, "^[abcdefgh][abcdefgh][12345678][BNRQK]$") then 
            let mto = SqDct.[m.[1..2]]
            
            let r = 
                int (m.[2].ToString()) + (if pos.IsW then -1
                                          else 1)
            
            let mfrom = SqDct.[m.[0].ToString() + r.ToString()]
            { Mfrom = mfrom
              Mto = mto
              Mtyp = Prom(m.[3]) |> Some
              Mpgn = mv }
        else fl()
