namespace PgnFSharp

open System

module Psn = 
    //utility functions
    let private samef a b = a % 8 = b % 8
    let private samer a b = a / 8 = b / 8
    let private samefr a b = samef a b || samer a b
    let private samedg a b = abs (a % 8 - b % 8) = abs (a / 8 - b / 8)
    let private samedgfr a b = samedg a b || samefr a b
    
    let private isnmv a b = 
        let rd = abs (a / 8 - b / 8)
        let fd = abs (a % 8 - b % 8)
        rd = 2 && fd = 1 || fd = 2 && rd = 1
    
    /// Make a move
    let DoMv mv x = 
        x.IsW <- not x.IsW
        let c = x.Sqs.[mv.Mfrom]
        x.Sqs.[mv.Mfrom] <- ' '
        x.Sqs.[mv.Mto] <- c
        if mv.Mtyp.IsSome then 
            match mv.Mtyp.Value with
            | Prom(c) -> 
                x.Sqs.[mv.Mto] <- if x.IsW then c |> Char.ToLower
                                  else c
            | CasK -> 
                x.Sqs.[mv.Mto - 1] <- x.Sqs.[mv.Mto + 1]
                x.Sqs.[mv.Mto + 1] <- ' '
            | CasQ -> 
                x.Sqs.[mv.Mto + 1] <- x.Sqs.[mv.Mto - 2]
                x.Sqs.[mv.Mto - 2] <- ' '
            | Ep -> 
                if x.IsW then x.Sqs.[mv.Mto - 8] <- ' '
                else x.Sqs.[mv.Mto + 8] <- ' '
    
    /// Gets Move from string
    let GetMv mv x = 
        //general failure message
        let fl() = failwith ("not done yet, mv: " + mv + " pos: " + x.ToString())
        
        let strip chars = 
            String.collect (fun c -> 
                if Seq.exists ((=) c) chars then ""
                else string c)
        
        let m = mv |> strip "+x#=!?"
        let m = m.Replace("e.p.", "")
        match m with
        //simple pawn move e.g. d4
        | SimpleMove('P', sq) -> 
            let mto = SqDct.[sq]
            if x.IsW then 
                if x.Sqs.[mto + 8] = 'P' then 
                    { Mfrom = mto + 8
                      Mto = mto
                      Mtyp = None
                      Mpgn = mv }
                else 
                    { Mfrom = mto + 16
                      Mto = mto
                      Mtyp = None
                      Mpgn = mv }
            else if x.Sqs.[mto - 8] = 'p' then 
                { Mfrom = mto - 8
                  Mto = mto
                  Mtyp = None
                  Mpgn = mv }
            else 
                { Mfrom = mto - 16
                  Mto = mto
                  Mtyp = None
                  Mpgn = mv }
        //simple piece move e.g. Nf3
        | SimpleMove(p, sq) -> 
            let mto = SqDct.[sq]
            
            let pc = 
                if x.IsW then p
                else p |> Char.ToLower
            
            let mfs = 
                x.Sqs
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
                            let np = x.Copy()
                            
                            let mov = 
                                { Mfrom = m
                                  Mto = mto
                                  Mtyp = None
                                  Mpgn = mv }
                            np |> (DoMv mov)
                            let kc = 
                                if x.IsW then 'K'
                                else 'k'
                            
                            let kia = 
                                np.Sqs
                                |> Array.mapi (fun i c -> i, c)
                                |> Array.filter (fun (_, c) -> c = kc)
                                |> Array.map fst
                            
                            let ki = kia.[0]
                            
                            let qc = 
                                if x.IsW then 'q'
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
                                if x.IsW then 'r'
                                else 'R'
                            
                            let bc = 
                                if x.IsW then 'b'
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
                                failwith ("can't find valid move, mv: " + mv + " pos: " + x.ToString())
                            else 
                                let f = fl.Head
                                if samer mto f then 
                                    let betw = 
                                        if mto < f then x.Sqs.[mto + 1..f - 1]
                                        else x.Sqs.[f + 1..mto - 1]
                                    if (betw |> Array.filter (fun c -> c <> ' ')).Length = 0 then f
                                    else getval fl.Tail
                                elif samef mto f then 
                                    let betw = 
                                        if mto < f then [ mto + 8..8..f - 8 ] |> List.map (fun i -> x.Sqs.[i])
                                        else [ f + 8..8..mto - 8 ] |> List.map (fun i -> x.Sqs.[i])
                                    if (betw |> List.filter (fun c -> c <> ' ')).Length = 0 then f
                                    else getval fl.Tail
                                //onsame diagonal
                                else 
                                    let betw = 
                                        if mto < f && (f - mto) % 7 = 0 then 
                                            [ mto + 7..7..f - 7 ] |> List.map (fun i -> x.Sqs.[i])
                                        elif mto < f then [ mto + 9..9..f - 9 ] |> List.map (fun i -> x.Sqs.[i])
                                        elif (mto - f) % 7 = 0 then 
                                            [ f + 7..7..mto - 7 ] |> List.map (fun i -> x.Sqs.[i])
                                        else [ f + 9..9..mto - 9 ] |> List.map (fun i -> x.Sqs.[i])
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
                                failwith ("can't find valid move, mv: " + mv + " pos: " + x.ToString())
                            else 
                                let f = fl.Head
                                if samer mto f then 
                                    let betw = 
                                        if mto < f then x.Sqs.[mto + 1..f - 1]
                                        else x.Sqs.[f + 1..mto - 1]
                                    if (betw |> Array.filter (fun c -> c <> ' ')).Length = 0 then f
                                    else getval fl.Tail
                                else 
                                    let betw = 
                                        if mto < f then [ mto + 8..8..f - 8 ] |> List.map (fun i -> x.Sqs.[i])
                                        else [ f + 8..8..mto - 8 ] |> List.map (fun i -> x.Sqs.[i])
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
        | Castle(c) -> 
            if c = 'K' && x.IsW then 
                { Mfrom = 60
                  Mto = 62
                  Mtyp = CasK |> Some
                  Mpgn = mv }
            elif c = 'K' then 
                { Mfrom = 4
                  Mto = 6
                  Mtyp = CasK |> Some
                  Mpgn = mv }
            elif x.IsW then 
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
        | PawnCapture(f, sq) -> 
            let mto = SqDct.[sq]
            
            let r = 
                int (m.[2].ToString()) + (if x.IsW then -1
                                          else 1)
            
            let mtyp = 
                if x.Sqs.[mto] = ' ' then Ep |> Some
                else None
            
            let mfrom = SqDct.[f.ToString() + r.ToString()]
            { Mfrom = mfrom
              Mto = mto
              Mtyp = mtyp
              Mpgn = mv }
        //ambiguous file like Nge2
        | AbiguousFile(p, f, sq) -> 
            let mto = SqDct.[sq]
            
            let pc = 
                if x.IsW then p
                else p |> Char.ToLower
            
            let fn = fDct.[f]
            
            let mfs = 
                x.Sqs
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
                    let ms = 
                        mfs
                        |> Array.filter (isnmv mto)
                        |> Array.filter (fun f -> f % 8 = fn)
                    if ms.Length = 1 then 
                        { Mfrom = ms.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else fl()
                | 'R' | 'r' | 'Q' | 'q' -> 
                    let fmfs = mfs |> Array.filter (fun f -> f % 8 = fn)
                    if fmfs.Length = 1 then 
                        { Mfrom = fmfs.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else fl()
                | _ -> fl()
        //ambiguous rank like R7a6
        | AbiguousRank(p, r, sq) -> 
            let mto = SqDct.[sq]
            
            let pc = 
                if x.IsW then p
                else p |> Char.ToLower
            
            let rn = rDct.[r]
            
            let mfs = 
                x.Sqs
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
                    let ms = 
                        mfs
                        |> Array.filter (isnmv mto)
                        |> Array.filter (fun f -> f / 8 = rn)
                    if ms.Length = 1 then 
                        { Mfrom = ms.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else fl()
                | 'R' | 'r' | 'Q' | 'q' -> 
                    let rmfs = mfs |> Array.filter (fun f -> f / 8 = rn)
                    if rmfs.Length = 1 then 
                        { Mfrom = rmfs.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else fl()
                | _ -> fl()
        //pawn promotion like b8=Q
        | Promotion(sq, pc) -> 
            let mto = SqDct.[sq]
            
            let r = 
                int (m.[1].ToString()) + (if x.IsW then -1
                                          else 1)
            
            let mfrom = SqDct.[m.[0].ToString() + r.ToString()]
            { Mfrom = mfrom
              Mto = mto
              Mtyp = Prom(pc) |> Some
              Mpgn = mv }
        //pawn promotion capture like a*b8=Q
        | PromCapture(f, sq, pc) -> 
            let mto = SqDct.[sq]
            
            let r = 
                int (m.[2].ToString()) + (if x.IsW then -1
                                          else 1)
            
            let mfrom = SqDct.[f.ToString() + r.ToString()]
            { Mfrom = mfrom
              Mto = mto
              Mtyp = Prom(pc) |> Some
              Mpgn = mv }
    
    /// Gets PGN give from and to
    let GetPgn f t x = 
        let p = x.Sqs.[f]
        
        let psqs() = 
            x.Sqs
            |> Array.mapi (fun i pc -> i, pc)
            |> Array.filter (fun (_, pc) -> p = pc)
            |> Array.map fst
            |> Array.filter (fun i -> i <> f)
        
        let pc = p.ToString().ToUpper()
        match pc with
        | "K" -> 
            if x.IsW && f = 60 && t = 62 then "O-O"
            elif x.IsW && f = 60 && t = 58 then "O-O-O"
            elif f = 4 && t = 6 then "O-O"
            elif f = 4 && t = 2 then "O-O-O"
            else 
                let tsq = SqDctRev.[t]
                if x.Sqs.[t] <> ' ' then pc + "x" + tsq
                else pc + tsq
        | "P" -> 
            let tsq = SqDctRev.[t]
            let ff = SqDctRev.[f].[0]
            if tsq.[0] <> ff then ff.ToString() + "x" + tsq
            else tsq
        | "N" -> 
            let tsq = SqDctRev.[t]
            let fsqs = psqs() |> Array.filter (isnmv t)
            if fsqs.Length > 0 then 
                let fsq = SqDctRev.[f]
                
                let ffs = 
                    fsqs
                    |> Array.map (fun s -> SqDctRev.[s].[0])
                    |> Array.filter (fun fl -> fsq.[0] = fl)
                if ffs.Length > 1 then 
                    if x.Sqs.[t] <> ' ' then pc + fsq.[1].ToString() + "x" + tsq
                    else pc + fsq.[1].ToString() + tsq
                else if x.Sqs.[t] <> ' ' then pc + fsq.[0].ToString() + "x" + tsq
                else pc + fsq.[0].ToString() + tsq
            else if x.Sqs.[t] <> ' ' then pc + "x" + tsq
            else pc + tsq
        | "R" -> 
            let tsq = SqDctRev.[t]
            let fmfs = psqs() |> Array.filter (samefr t)
            if fmfs.Length > 0 then 
                let fsq = SqDctRev.[f]
                
                let rec getp fl = 
                    if List.isEmpty fl then 
                        if x.Sqs.[t] <> ' ' then pc + "x" + tsq
                        else pc + tsq
                    else 
                        let s = fl.Head
                        if samer s t then 
                            let betw = 
                                if t < s then x.Sqs.[t + 1..f - 1]
                                else x.Sqs.[s + 1..t - 1]
                            if (betw |> Array.filter (fun c -> c <> ' ')).Length = 0 then 
                                if x.Sqs.[t] <> ' ' then pc + fsq.[0].ToString() + "x" + tsq
                                else pc + fsq.[0].ToString() + tsq
                            else getp fl.Tail
                        else 
                            let betw = 
                                if t < s then [ t + 8..8..s - 8 ] |> List.map (fun i -> x.Sqs.[i])
                                else [ s + 8..8..t - 8 ] |> List.map (fun i -> x.Sqs.[i])
                            if (betw |> List.filter (fun c -> c <> ' ')).Length = 0 then 
                                if x.Sqs.[t] <> ' ' then pc + fsq.[1].ToString() + "x" + tsq
                                else pc + fsq.[1].ToString() + tsq
                            else getp fl.Tail
                fmfs
                |> List.ofArray
                |> getp
            else if x.Sqs.[t] <> ' ' then pc + "x" + tsq
            else pc + tsq
        | "B" -> 
            let tsq = SqDctRev.[t]
            if x.Sqs.[t] <> ' ' then pc + "x" + tsq
            else pc + tsq
        | "Q" -> 
            let tsq = SqDctRev.[t]
            if x.Sqs.[t] <> ' ' then pc + "x" + tsq
            else pc + tsq
        | _ -> failwith "Invalid piece"
