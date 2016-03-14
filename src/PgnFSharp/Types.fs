namespace PgnFSharp

open System.Text

[<AutoOpen>]
module Types = 
    let nl = System.Environment.NewLine
    
    type File = 
        | A
        | B
        | C
        | D
        | E
        | F
        | G
        | H
        override x.ToString() = 
            match x with
            | A -> "a"
            | B -> "b"
            | C -> "c"
            | D -> "d"
            | E -> "e"
            | F -> "f"
            | G -> "g"
            | H -> "h"
    
    type PieceType = 
        | Pawn
        | Knight
        | Bishop
        | Rook
        | Queen
        | King
        override x.ToString() = 
            match x with
            | Pawn -> "P"
            | Knight -> "N"
            | Bishop -> "B"
            | Rook -> "R"
            | Queen -> "Q"
            | King -> "K"
    
    type GameResult = 
        | WhiteWin
        | BlackWin
        | Draw
        | Open
        
        override x.ToString() = 
            match x with
            | WhiteWin -> "1-0"
            | BlackWin -> "0-1"
            | Draw -> "1/2-1/2"
            | Open -> "*"
        
        member x.ToByte() = 
            match x with
            | WhiteWin -> 0uy
            | BlackWin -> 1uy
            | Draw -> 2uy
            | Open -> 3uy
    
    type Color = 
        | White
        | Black
    
    type Piece = 
        { PcTp : PieceType
          Clr : Color }
    
    let WhitePawn = 
        { PcTp = PieceType.Pawn
          Clr = Color.White }
    
    let BlackPawn = 
        { PcTp = PieceType.Pawn
          Clr = Color.Black }
    
    let WhiteKnight = 
        { PcTp = PieceType.Knight
          Clr = Color.White }
    
    let BlackKnight = 
        { PcTp = PieceType.Knight
          Clr = Color.Black }
    
    let WhiteBishop = 
        { PcTp = PieceType.Bishop
          Clr = Color.White }
    
    let BlackBishop = 
        { PcTp = PieceType.Bishop
          Clr = Color.Black }
    
    let WhiteRook = 
        { PcTp = PieceType.Rook
          Clr = Color.White }
    
    let BlackRook = 
        { PcTp = PieceType.Rook
          Clr = Color.Black }
    
    let WhiteQueen = 
        { PcTp = PieceType.Queen
          Clr = Color.White }
    
    let BlackQueen = 
        { PcTp = PieceType.Queen
          Clr = Color.Black }
    
    let WhiteKing = 
        { PcTp = PieceType.King
          Clr = Color.White }
    
    let BlackKing = 
        { PcTp = PieceType.King
          Clr = Color.Black }
    
    type Square = 
        { Fil : File
          Rank : int }
        override x.ToString() = x.Fil.ToString() + x.Rank.ToString()
    
    type MoveType = 
        | Simple
        | Capture
        | CaptureEnPassant
        | CastleKingSide
        | CastleQueenSide
    
    type Move = 
        { Type : MoveType
          TargetPiece : PieceType option
          TargetSquare : Square option
          TargetFile : File option
          Piece : PieceType option
          OriginSquare : Square option
          OriginFile : File option
          OriginRank : int option
          PromotedPiece : PieceType option
          IsCheck : bool option
          IsDoubleCheck : bool option
          IsCheckMate : bool option }
        override x.ToString() = 
            let mv = 
                match x.Type with
                | CastleKingSide -> "O-O"
                | CastleQueenSide -> "O-O-O"
                | Simple -> 
                    let pc = 
                        if x.Piece.IsNone || x.Piece.Value = Pawn then ""
                        else x.Piece.Value.ToString()
                    
                    let st = 
                        if x.OriginSquare.IsSome then pc + x.OriginSquare.Value.ToString()
                        elif x.OriginFile.IsSome && x.OriginRank.IsSome then 
                            pc + x.OriginFile.Value.ToString() + x.OriginRank.Value.ToString()
                        elif x.OriginFile.IsSome then pc + x.OriginFile.Value.ToString()
                        elif x.OriginRank.IsSome then pc + x.OriginRank.Value.ToString()
                        else pc
                    
                    if x.TargetSquare.IsSome then st + x.TargetSquare.Value.ToString()
                    elif x.TargetFile.IsSome then st + x.TargetFile.Value.ToString()
                    else st
                | Capture | CaptureEnPassant -> 
                    let pc = 
                        if x.Piece.IsNone || x.Piece.Value = Pawn then ""
                        else x.Piece.Value.ToString()
                    
                    let st = 
                        if x.OriginSquare.IsSome then pc + x.OriginSquare.Value.ToString()
                        elif x.OriginFile.IsSome && x.OriginRank.IsSome then 
                            pc + x.OriginFile.Value.ToString() + x.OriginRank.Value.ToString()
                        elif x.OriginFile.IsSome then pc + x.OriginFile.Value.ToString()
                        elif x.OriginRank.IsSome then pc + x.OriginRank.Value.ToString()
                        else pc
                    
                    let tpc = 
                        if x.TargetPiece.IsNone || x.TargetPiece.Value = Pawn then ""
                        else x.TargetPiece.Value.ToString()
                    
                    let nd = 
                        if x.TargetSquare.IsSome then tpc + x.TargetSquare.Value.ToString()
                        elif x.TargetFile.IsSome then tpc + x.TargetFile.Value.ToString()
                        else tpc
                    
                    let ans = 
                        if st = "" then nd + "x"
                        else st + "x" + nd
                    
                    if x.Type = Capture then ans
                    else ans + "e.p."
            
            let prom = 
                if x.PromotedPiece.IsNone then ""
                else "=" + x.PromotedPiece.Value.ToString()
            
            let chk = 
                if x.IsCheckMate.IsSome && x.IsCheckMate.Value then "#"
                elif x.IsDoubleCheck.IsSome && x.IsDoubleCheck.Value then "++"
                elif x.IsCheck.IsSome && x.IsCheck.Value then "+"
                else ""
            
            mv + prom + chk
    
    let BlankMv() = 
        { Type = MoveType.Simple
          TargetPiece = None
          TargetSquare = None
          TargetFile = None
          Piece = None
          OriginSquare = None
          OriginFile = None
          OriginRank = None
          PromotedPiece = None
          IsCheck = None
          IsDoubleCheck = None
          IsCheckMate = None }
    
    let FormatTag name value = "[" + name + " \"" + value + "\"]" + nl
    
    type Game = 
        { Event : string
          Site : string
          Year : int16 option
          Month : byte option
          Day : byte option
          Round : string
          WhitePlayer : string
          BlackPlayer : string
          Result : GameResult
          Moves : Move list }
        
        member x.DateStr = 
            (if x.Year.IsNone then "????"
             else x.Year.Value.ToString("0000")) + "." + (if x.Month.IsNone then "??"
                                                          else x.Month.Value.ToString("00")) + "." 
            + (if x.Day.IsNone then "??"
               else x.Day.Value.ToString("00"))
        
        override x.ToString() = 
            let sb = new StringBuilder()
            
            let rec mvs2txt ct mvl = 
                if List.isEmpty mvl then sb.ToString()
                elif mvl.Length = 1 then 
                    sb.Append(" " + ct.ToString() + ". " + mvl.Head.ToString()) |> ignore
                    sb.ToString()
                else 
                    let w = mvl.Head.ToString()
                    let b = mvl.Tail.Head.ToString()
                    let rest = mvl.Tail.Tail
                    sb.Append(" " + ct.ToString() + ". " + w + " " + b) |> ignore
                    mvs2txt (ct + 1) rest
            (x.Event |> FormatTag "Event") + (x.Site |> FormatTag "Site") + (x.DateStr |> FormatTag "Date") 
            + (x.Round |> FormatTag "Round") + (x.WhitePlayer |> FormatTag "White") 
            + (x.BlackPlayer |> FormatTag "Black") + (x.Result.ToString() |> FormatTag "Result") + nl 
            + (x.Moves |> mvs2txt 1) + " " + x.Result.ToString()
    
    let BlankGm() = 
        { Event = "?"
          Site = "?"
          Year = None
          Month = None
          Day = None
          Round = "?"
          WhitePlayer = "?"
          BlackPlayer = "?"
          Result = Open
          Moves = [] }
    
    type Database = Game list
