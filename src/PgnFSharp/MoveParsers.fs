[<AutoOpen>]
module internal PgnParsers.Move

open FParsec
open PgnFSharp

type MoveInfo = 
    { OPc : PieceType option
      OFl : File option
      ORk : int option }

let getSquare (moveInfo : MoveInfo) = 
    match moveInfo.OFl, moveInfo.ORk with
    | x, y when x.IsSome && y.IsSome -> 
        Some { Fil = x.Value
               Rank = y.Value }
    | _, _ -> None

let getMove (originInfo : MoveInfo option, targetInfo : MoveInfo, moveType : MoveType) = 
    match originInfo, targetInfo with
    | None, _ -> 
        { Type = moveType
          TargetPiece = targetInfo.OPc
          TargetSquare = getSquare targetInfo
          TargetFile = targetInfo.OFl
          Piece = targetInfo.OPc
          OriginSquare = None
          OriginFile = None
          OriginRank = None
          PromotedPiece = None
          IsCheck = None
          IsDoubleCheck = None
          IsCheckMate = None }
    | Some(orig), _ -> 
        { Type = moveType
          TargetPiece = targetInfo.OPc
          TargetSquare = getSquare targetInfo
          TargetFile = targetInfo.OFl
          Piece = orig.OPc
          OriginSquare = getSquare orig
          OriginFile = orig.OFl
          OriginRank = orig.ORk
          PromotedPiece = None
          IsCheck = None
          IsDoubleCheck = None
          IsCheckMate = None }

//MOVE MECHANICS

// target square of a move
let pTarget = 
    attempt(pPiece .>>. pFile .>>. pRank) // Qd5
    <|> (pFile .>>. pRank |>> fun (f, r) -> ((PieceType.Pawn, f),r)) //Pawn move, e.g. d5
    |>> fun ((piece, file), rank) ->  {OPc=Some(piece);OFl=Some(file);ORk=Some(rank)}
    <!> "pTarget"

// origin squalre of move (usually for disambiguation)
let pOrigin = 
    attempt(pPiece .>>. opt pFile .>>. opt pRank)
    <|> (pFile .>>. opt pRank |>> fun(f, r) -> ((PieceType.Pawn , Some f),r))
    |>> fun ((piece, file), rank) ->  {OPc=Some(piece);OFl=file;ORk=rank}
    <!> "pOrigin"

let pBasicMove = 
    attempt (pOrigin .>>. pTarget) |>> fun (origin, target) -> getMove(Some(origin), target, MoveType.Simple)
    <|> (pTarget |>> fun target -> getMove(None, target, MoveType.Simple))
    <!!> ("pBasicMove", 1)

// parsers for capturing
let pCapturingSign = (pchar 'x' <|> pchar ':') |>> fun x -> x.ToString()

let pInfixCaptureMove =   // e.g. QxBc5
    pOrigin .>> pCapturingSign .>>. pTarget
    |>> fun (orig, target) -> getMove(Some orig, target, MoveType.Capture)
    <!> "pInfixCaptureMove"

let pSimplifiedPawnCapture =  // e.g. dxe or de
    pFile .>> pCapturingSign .>>. pFile 
    >>= fun(f1, f2) -> 
        match f1 = f2 with //do not allow a6xa7
        | true -> pzero
        | false -> preturn (f1, f2)
    |>> fun (file1, file2) -> {Type=MoveType.Capture;TargetPiece=None;TargetSquare=None;TargetFile=Some(file2);Piece=Some(PieceType.Pawn);OriginSquare=None;OriginFile=Some(file1);OriginRank=None;PromotedPiece=None;IsCheck=None;IsDoubleCheck=None;IsCheckMate=None}
    <!> "pSimplifiedPawnCapture"

let pSuffixCaptureMove = // e.g. Qf4d4x or Qf4:
    pBasicMove .>> pCapturingSign 
    |>> fun move -> {move with Type=MoveType.Capture}
    <!> "pSuffixCaptureMove"

let pBasicCapturingMove = 
    attempt (attempt pInfixCaptureMove <|> pSuffixCaptureMove)
    <|> pSimplifiedPawnCapture
    <!> "pBasicCapturingMove"

// the two most common move types: move and capture
let pCapturingMove = 
    pBasicCapturingMove .>>. opt (strCI "e.p." ) //TODO: e.p. should only be allowed for pawns
    |>> fun (move, enpassant) ->
            match enpassant with
            | None -> move
            | _ -> {move with Type=MoveType.CaptureEnPassant}
    <!!> ("pCapturingMove", 1)

// special moves: pawn promotion and castle (king-side, queen-side)
let pPawnPromotion = 
    (attempt  pBasicCapturingMove <|> pBasicMove)
    .>>. ((str "=" >>. pPiece) <|> (str "(" >>. pPiece .>> str ")"))
    |>> fun (move, piece) -> {move with PromotedPiece=Some(piece)}
    <!> "pPawnPromotion"

let pCasteKingSide = 
    str "O-O" <|> str "O - O" <|> str "0-0"  <|> str "0 - 0" 
    |>> fun _ -> {Type=MoveType.CastleKingSide;TargetPiece=None;TargetSquare=None;TargetFile=None;Piece=None;OriginSquare=None;OriginFile=None;OriginRank=None;PromotedPiece=None;IsCheck=None;IsDoubleCheck=None;IsCheckMate=None}
    <!> "pCastleKingSide"

let pCasteQueenSide = 
    str "O-O-O" <|> str "O - O - O" <|> str "0-0-0"  <|> str "0 - 0 - 0" 
    |>> fun _ -> {Type=MoveType.CastleQueenSide;TargetPiece=None;TargetSquare=None;TargetFile=None;Piece=None;OriginSquare=None;OriginFile=None;OriginRank=None;PromotedPiece=None;IsCheck=None;IsDoubleCheck=None;IsCheckMate=None}
    <!> "pCasteQueenSide"

let pCastle = pCasteQueenSide <|> pCasteKingSide

// indicators
let pCheckIndicator = str "++" <|> str "††" <|> str "dbl ch" <|> str "+" <|> str "†" <|> str "ch" <!> "pCheckIndicator"
let pCheckMateIndicator = str "#" <|> str "‡" <!> "pCheckMateIndicator"
let pIndicator = pCheckIndicator <|> pCheckMateIndicator
let pAnnotation = 
    str "????" <|> str "???"
    <|> str "!!!!" <|> str "!!!" <|> str "?!?" <|> str "!?!" <|> str "??" <|> str "?!"
    <|> str "!!" <|> str "!?" <|> str "?" <|> str "!"
    <|> str "=/∞" <|> str "=/+" <|> str "="
    <|> str "+/=" <|> str "+/-" <|> str "+-" <|> str "-/+" <|> str "-+"
    <|> str "∞" <|> str "○" <|> str "↑↑" <|> str "↑" <|> str "⇄" <|> str "∇" <|> str "Δ"
    <|> str "TN" <|> str "N"
    <!> "pAnnotation"
    <?> "Move annotation (e.g. ! or ??)"

let pAdditionalInfo = pIndicator .>> opt(pAnnotation)

   
let pMove = 
    attempt pPawnPromotion <|>
    attempt pCapturingMove <|>
    attempt pBasicMove <|> 
    pCastle
    .>>. opt(pAdditionalInfo)
    |>> fun (move,indicator) ->
            match indicator with 
            | None -> move|>Some
            | Some(i) ->
                match i with
                | "+"  | "†"  | "ch" -> {move with IsCheck=Some(true)}|>Some
                | "++" | "††" | "dbl ch" -> {move with IsCheck=Some(true);IsDoubleCheck=Some(true)}|>Some
                | "#"  | "‡" -> {move with IsCheckMate=Some(true)}|>Some
                | _ -> move|>Some
    <!!> ("pMove", 2)
    <?> "Move (e.g. Qc4 or e2e4 or 0-0-0 etc.)"

let appyPMove (p: string)= run pMove p
