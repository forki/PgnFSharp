namespace PgnFSharp

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
    
    member x.DoMv mv = 
        x.IsW <- not x.IsW
        let c = x.Sqs.[mv.Mfrom]
        x.Sqs.[mv.Mfrom] <- ' '
        x.Sqs.[mv.Mto] <- c
        if mv.Mtyp.IsSome then 
            match mv.Mtyp.Value with
            | Prom(c) -> x.Sqs.[mv.Mto] <- c
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
                    let dfs = mfs |> Array.map (fun f -> f, (mto - f) |> abs)
                    let nmvs = [| 6; 10; 15; 17 |]
                    let vdfs = dfs |> Array.filter (fun (_, d) -> Array.IndexOf(nmvs, d) <> -1)
                    if vdfs.Length = 1 then 
                        { Mfrom = vdfs.[0] |> fst
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else failwith "Too many Knights!)"
                | 'B' | 'b' -> 
                    let fmfs = mfs |> Array.filter (fun f -> (mto+mto/8)%2=(f+f/8)%2)
                    if fmfs.Length = 1 then 
                        { Mfrom = fmfs.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else failwith "Too many Bishops!)"
                | 'R' | 'r' -> 
                    let fmfs = mfs |> Array.filter (fun f -> (mto - f)%8=0 || mto/8=f/8)
                    if fmfs.Length = 1 then 
                        { Mfrom = fmfs.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else
                        let rec getval fl =
                            if List.isEmpty fl then failwith ("can't find valid move, mv: " + mv + " pos: " + pos.ToString())
                            else
                                let f = fl.Head
                                if mto/8=f/8 then
                                    let betw = if mto<f then pos.Sqs.[mto+1..f-1] else pos.Sqs.[f+1..mto-1]
                                    if (betw|>Array.filter(fun c -> c<> ' ')).Length=0 then f else getval fl.Tail
                                else
                                    let betw = if mto<f then [mto+8..8..f-8]|>List.map(fun i ->pos.Sqs.[i]) else [f+8..8..mto-8]|>List.map(fun i ->pos.Sqs.[i])
                                    if (betw|>List.filter(fun c -> c<> ' ')).Length=0 then f else getval fl.Tail
                        let mfrom = fmfs|>List.ofArray|>getval
                        { Mfrom = mfrom
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }

                | _ -> failwith ("not done yet, mv: " + mv + " pos: " + pos.ToString())
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
                    let dfs = mfs |> Array.map (fun f -> f, (mto - f) |> abs)
                    let nmvs = [| 6; 10; 15; 17 |]
                    let vdfs = dfs |> Array.filter (fun (f, d) -> Array.IndexOf(nmvs, d) <> -1 && f % 8 = fn)
                    if vdfs.Length = 1 then 
                        { Mfrom = vdfs.[0] |> fst
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else failwith "Too many Knights!)"
                | 'R' | 'r' -> 
                    let fmfs = mfs |> Array.filter (fun f -> f % 8 = fn)
                    if fmfs.Length = 1 then 
                        { Mfrom = fmfs.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else failwith "Too many Rooks!)"
                | _ -> failwith ("not done yet, pc:" + pc.ToString())
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
                    let dfs = mfs |> Array.map (fun f -> f, (mto - f) |> abs)
                    let nmvs = [| 6; 10; 15; 17 |]
                    let vdfs = dfs |> Array.filter (fun (f, d) -> Array.IndexOf(nmvs, d) <> -1 && f / 8 = rn)
                    if vdfs.Length = 1 then 
                        { Mfrom = vdfs.[0] |> fst
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else failwith "Too many Knights!)"
                | 'R' | 'r' -> 
                    let rmfs = mfs |> Array.filter (fun f -> f / 8 = rn)
                    if rmfs.Length = 1 then 
                        { Mfrom = rmfs.[0]
                          Mto = mto
                          Mtyp = None
                          Mpgn = mv }
                    else failwith "Too many Rooks!)"
                | _ -> failwith ("not done yet, pc:" + pc.ToString())
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
        else failwith ("Unhandled string for move:" + mv)
