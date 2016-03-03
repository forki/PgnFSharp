﻿namespace pgn.Data

[<AutoOpen>]
module Types =
    type File = A|B|C|D|E|F|G|H
    type PieceType = Pawn|Knight|Bishop|Rook|Queen|King
    type GameResult = White|Black|Draw|Open
    type Color = White|Black
    type Piece = {PcTp:PieceType;Clr:Color}
    let WhitePawn = {PcTp=PieceType.Pawn;Clr=Color.White}
    let BlackPawn = {PcTp=PieceType.Pawn;Clr=Color.Black}
    let WhiteKnight = {PcTp=PieceType.Knight;Clr=Color.White}
    let BlackKnight = {PcTp=PieceType.Knight;Clr=Color.Black}
    let WhiteBishop = {PcTp=PieceType.Bishop;Clr=Color.White}
    let BlackBishop = {PcTp=PieceType.Bishop;Clr=Color.Black}
    let WhiteRook = {PcTp=PieceType.Rook;Clr=Color.White}
    let BlackRook = {PcTp=PieceType.Rook;Clr=Color.Black}
    let WhiteQueen = {PcTp=PieceType.Queen;Clr=Color.White}
    let BlackQueen = {PcTp=PieceType.Queen;Clr=Color.Black}
    let WhiteKing = {PcTp=PieceType.King;Clr=Color.White}
    let BlackKing = {PcTp=PieceType.King;Clr=Color.Black}
    type Square = {File:File;Rank:int}
    type BoardSetup = {Board:Piece option [];IsWhiteMove:bool;CanWhiteCastleKingSide:bool;CanWhiteCastleQueenSide:bool;CanBlackCastleKingSide:bool;CanBlackCastleQueenSide:bool;EnPassantSquare:Square option;HalfMoveClock:int;FullMoveCount:int}
    type MoveType = Simple|Capture|CaptureEnPassant|CastleKingSide|CastleQueenSide
    type MoveAnnotation = MindBlowing|Brilliant|Good|Interesting|Dubious|Mistake|Blunder|Abysmal|FascinatingButUnsound|Unclear|WithCompensation|EvenPosition|SlightAdvantageWhite|SlightAdvantageBlack|AdvantageWhite|AdvantageBlack|DecisiveAdvantageWhite|DecisiveAdvantageBlack|Space|Initiative|Development|Counterplay|Countering|Idea|TheoreticalNovelty|UnknownAnnotation    
    type Move = {Type:MoveType;TargetPiece:PieceType option;TargetSquare:Square option;TargetFile:File option;Piece:PieceType option;OriginSquare:Square option;OriginFile:File option;OriginRank:int option;PromotedPiece:PieceType option;IsCheck:bool option;IsDoubleCheck:bool option;IsCheckMate:bool option;Annotation:MoveAnnotation option}
