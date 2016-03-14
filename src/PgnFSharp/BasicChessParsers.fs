[<AutoOpen>]
module internal PgnParsers.BasicChess

open FParsec
open PgnFSharp

let pPiece = 
    (pchar 'P' >>% PieceType.Pawn)
    <|> (pchar 'N' >>% PieceType.Knight)
    <|> (pchar 'B' >>% PieceType.Bishop)
    <|> (pchar 'R' >>% PieceType.Rook)
    <|> (pchar 'Q' >>% PieceType.Queen)
    <|> (pchar 'K' >>% PieceType.King)
    <?> "Piece (N, B, R, Q, K, P)"

let pFile = 
    (pchar 'a' >>% File.A)
    <|> (pchar 'b' >>% File.B)
    <|> (pchar 'c' >>% File.C)
    <|> (pchar 'd' >>% File.D)
    <|> (pchar 'e' >>% File.E)
    <|> (pchar 'f' >>% File.F)
    <|> (pchar 'g' >>% File.G)
    <|> (pchar 'h' >>% File.H)
    <?> "File letter (a..h)"

let pRank = 
    (pchar '1' >>% 1)
    <|> (pchar '2' >>% 2)
    <|> (pchar '3' >>% 3)
    <|> (pchar '4' >>% 4)
    <|> (pchar '5' >>% 5)
    <|> (pchar '6' >>% 6)
    <|> (pchar '7' >>% 7)
    <|> (pchar '8' >>% 8)
    <?> "Rank (1..8)"


let apply p = run (pPiece >>. pFile >>. pRank) p