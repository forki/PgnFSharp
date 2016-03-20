namespace PgnFSharp
open System
open System.Text
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
            if List.isEmpty chl then sb.ToString() + (if x.IsW then " w" else " b")
            elif i=8 then 
                if e>0 then sb.Append(e.ToString())|>ignore
                sb.Append("/")|>ignore
                getstr 0 0 chl
            elif chl.Head=' ' then getstr (i+1) (e+1) chl.Tail
            else 
                if e>0 then sb.Append(e.ToString())|>ignore
                sb.Append(chl.Head)|>ignore
                getstr (i+1) 0 chl.Tail 
        getstr 0 0 (x.Sqs|>List.ofArray)
    member x.DoMv mv = 
        x.IsW <- not x.IsW
        let c = x.Sqs.[mv.Mfrom]
        x.Sqs.[mv.Mfrom] <- ' '
        x.Sqs.[mv.Mto] <- c
        if mv.Mtyp.IsSome then
            match mv.Mtyp.Value with
            |Prom(c) -> x.Sqs.[mv.Mto] <- c
            |CasK -> 
                x.Sqs.[mv.Mto-1]<-x.Sqs.[mv.Mto+1]
                x.Sqs.[mv.Mto+1] <- ' '
            |CasQ -> 
                x.Sqs.[mv.Mto+1]<-x.Sqs.[mv.Mto-2]
                x.Sqs.[mv.Mto-2] <- ' '
            |Ep -> 
                if x.IsW then x.Sqs.[mv.Mto-7] <- ' ' else x.Sqs.[mv.Mto+7] <- ' '


module Psn = 
    ///Dictionary of files
    let private fDct = 
        [ 'a'..'h' ]
        |> List.mapi (fun i c -> c, i)
        |> dict
    
    ///Dictionary of ranks
    let private rDct = 
        [ 1..8 ]
        |> List.rev
        |> List.mapi (fun i c -> char (c), i)
        |> dict
    
    ///Dictionary of squares
    let SqDct = 
        [ for r = 8 downto 1 do
              for f in [ 'a'..'h' ] do
                  yield f.ToString() + r.ToString() ]
        |> List.mapi (fun i s -> s, i)
        |> dict
    
    /// loads Posn given a FEN like string
    let FromStr (s:string) =
        let b = s.Split(' ')
        let isw = b.[1] = "w"
        let sqs = Array.create 64 ' '
        let rec getp i ps =
            if ps="" then {Sqs=sqs;IsW=isw}
            else
                match ps.[0] with
                |'/' -> getp i ps.[1..]
                |c -> 
                    let ok,p = Int32.TryParse(c.ToString())
                    if ok then getp (i+p) ps.[1..]
                    else 
                        sqs.[i] <- c
                        getp (i+1) ps.[1..]
        getp 0 b.[0] 
    
    
    /// String for initial position
    let private startStr = "rnbqkbnrpppppppp                                PPPPPPPPRNBQKBNR"
    
    /// Gets initial Posn
    let Start() = 
        { Sqs = startStr.ToCharArray()
          IsW = true }
