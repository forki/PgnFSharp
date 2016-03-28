namespace PgnFSharp

open System.Text.RegularExpressions

[<AutoOpen>]
module Utils = 
    ///Dictionary of files
    let fDct = 
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
    
    //Regular expression active patterns
    // *********************************
    ///Active pattern to parse header string
    let (|Header|_|) s = 
        let m = Regex("\[([\w]+)\s+\"([\s\S]*)\"\]").Match(s)
        if m.Success then Some(m.Groups.[1].Value, m.Groups.[2].Value)
        else None

    ///Active pattern to parse move string
    let (|SimpleMove|Castle|PawnCapture|AbiguousFile|AbiguousRank|Promotion|PromCapture|) s = 
        if Regex.IsMatch(s, "^[BNRQK][a-h][1-8]$") then SimpleMove(s.[0], s.[1..])
        elif Regex.IsMatch(s, "^[a-h][1-8]$") then SimpleMove('P', s)
        elif s = "O-O" then Castle('K')
        elif s = "O-O-O" then Castle('Q')
        elif Regex.IsMatch(s, "^[a-h][a-h][1-8]$") then PawnCapture(s.[0], s.[1..])
        elif Regex.IsMatch(s, "^[BNRQK][a-h][a-h][1-8]$") then AbiguousFile(s.[0], s.[1], s.[2..])
        elif Regex.IsMatch(s, "^[BNRQK][1-8][a-h][1-8]$") then AbiguousRank(s.[0], s.[1], s.[2..])
        elif Regex.IsMatch(s, "^[a-h][1-8][BNRQ]$") then Promotion(s.[0..1], s.[2])
        elif Regex.IsMatch(s, "^[a-h][a-h][1-8][BNRQ]$") then PromCapture(s.[0], s.[1..2], s.[3])
        else failwith ("invalid move: " + s)

