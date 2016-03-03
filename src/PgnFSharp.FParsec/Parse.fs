namespace PgnFSharp.FParsec

open FParsec
/// Parser module for PGN files
module Parse = 
  
    type Piece = Pawn|Knight|Bishop|Rook|Queen|King
    let toPiece s  =
        match s with
        |'N' -> Knight
        |'B' -> Bishop
        |'R' -> Rook
        |'Q' -> Queen
        |'K' -> King
        |_ -> failwith "Invalid Piece letter"
    let piece = anyOf ['N';'B';'R';'Q';'K'] |>> toPiece <?> "Piece (N, B, R, Q, K, P)"
    let applyPc p = run piece p
