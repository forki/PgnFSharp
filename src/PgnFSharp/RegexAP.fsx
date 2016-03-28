open System.Text.RegularExpressions

let (|Header|_|) s = 
    let m = Regex("\[([\w]+)\s+\"([\s\S]*)\"\]").Match(s)
    if m.Success then Some(m.Groups.[1].Value, m.Groups.[2].Value)
    else None

let gethdr s = 
    match s with
    | Header(k, v) -> k, v
    | _ -> failwith ("not a valid pgn header: " + s)

let a1, b1 = gethdr "[key \"val\"]"
let a2, b2 = gethdr "[key \"\"]"
let a4, b4 = gethdr "[Event \"London Chess Classic\"]"

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

let getmv s = 
    match s with
    | SimpleMove(pc, sq) -> pc, sq
    | Castle(c) -> c, "g1"
    | PawnCapture(f, sq) -> f, sq
    | AbiguousFile(pc, f, sq) -> pc, sq
    | AbiguousRank(pc, r, sq) -> pc, sq
    | Promotion(sq, pc) -> pc, sq
    | PromCapture(f, sq, pc) -> f, sq

let p1, s1 = getmv "Na4"
let p2, s2 = getmv "a4"
let p3, s3 = getmv "O-O"
let p4, s4 = getmv "O-O-O"
let p5, s5 = getmv "ab4"
let p6, s6 = getmv "Nge2"
let p7, s7 = getmv "R1e2"
let p8, s8 = getmv "e8Q"
let p9, s9 = getmv "de8Q"
