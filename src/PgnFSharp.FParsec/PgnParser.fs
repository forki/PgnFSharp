namespace LizardChess

open System
open System.IO
open System.Linq
open FParsec

module PgnParser = 
    let toNullable = 
        function 
        | None -> Nullable()
        | Some x -> Nullable(x)
    
    let ws = spaces
    let ws1 = spaces1
    let str = pstring
    let strCI = pstringCI
    let pNotChar c = manySatisfy (fun x -> x <> c)
    let charList2String (lChars : char list) = System.String.Concat(lChars)
    
    let pList (p, list : 'a list) = 
        list
        |> List.map p
        |> choice
    
    let concat (a : string, b) = a + b
    let concat3 ((a : string, b), c) = a + b + c
    let BP (p : Parser<_, _>) stream = p stream // set a breakpoint here
    let NBP (p : Parser<_, _>, name : string) stream = p stream // set a breakpoint here
    
    let D (p : Parser<_, _>, name : string) stream = 
        System.Console.WriteLine(name)
        p stream
    
    let fileSymbol = [ 'a'..'h' ] |> List.map (fun x -> x.ToString())
    
    let findFile (a : string) = 
        match a.ToUpper() with
        | "A" -> 0
        | "B" -> 1
        | "C" -> 2
        | "D" -> 3
        | "E" -> 4
        | "F" -> 5
        | "G" -> 6
        | "H" -> 7
        | _ -> raise <| System.ArgumentException("Invalid file letter " + a)
    
    let rankSymbol = [ 1..8 ] |> List.map (fun x -> x.ToString())
    //NOTE: we allow S (ger. "Springer") for knight was used traditionally and is around in older PGNs
    //NOTE: 'b' is not allowed here as it is reserved for the b file
    let pPiece = 
        (pchar 'p' >>% 1) <|> (pchar 'P' >>% 1) <|> (pchar 'N' >>% 2) <|> (pchar 'n' >>% 2) 
        <|> (pchar 'S' >>% 2) <|> (pchar 's' >>% 2) <|> (pchar 'B' >>% 3) <|> (pchar 'R' >>% 4) 
        <|> (pchar 'r' >>% 4) <|> (pchar 'Q' >>% 5) <|> (pchar 'q' >>% 5) <|> (pchar 'K' >>% 6) 
        <|> (pchar 'k' >>% 6) <?> "Piece (N, B, R, Q, K, P, n, r, q, k, p)"
    let pFile = pList (strCI, fileSymbol) |>> findFile <?> "File letter (A..H)"
    let pRank = pList (strCI, rankSymbol) |>> (fun r -> System.Convert.ToInt32(r) - 1) <?> "Rank (1..8)"
    let apply p = run (pPiece >>. pFile >>. pRank) p
    let pTag = 
        (ws .>> pchar '[' .>> ws >>. many (noneOf "\"") .>>. skipChar '"' 
        .>>. many (noneOf "]") .>> ws .>> pchar ']' .>> ws 
         |>> fun (tagName, tagValue) -> 
                    charList2String(fst tagName).Trim(), charList2String(tagValue).Trim([| ' '; '"' |])) 
         <?> "Tag (e.g [Date \"2013.10.02\"])"
    let pTagList = ws >>. sepEndBy pTag ws
    let applyPTag p = run pTag p
    
    let bmvpgn = 
        { Mtyp = NullMove
          MtoWimg = -1
          Mto = -1
          Mfto = -1
          MWimg = -1
          Mfrom = -1
          Mffrom = -1
          Mrfrom = -1
          Mischk = false
          Misdchk = false
          Mischkmt = false
          Mannot = UnknownAnnotation }
    
    let getMove (originInfo : int * int * int, targetInfo : int * int * int, moveType : OMoveName) = 
        let orp, orf, orr = originInfo
        let tgp, tgf, tgr = targetInfo
        { bmvpgn with Mtyp = moveType
                      MWimg = 
                          if orp = -1 then tgp
                          else orp
                      Mfrom = 
                          if orf = -1 || orr = -1 then -1
                          else Ref.GetOrdFR(orf, orr)
                      Mffrom = orf
                      Mrfrom = 7 - orr
                      MtoWimg = tgp
                      Mto = 
                          if tgf = -1 || tgr = -1 then -1
                          else Ref.GetOrdFR(tgf, tgr)
                      Mfto = tgf }
    
    //MOVE MECHANICS
    // target square of a move
    let pTarget = 
        attempt (pPiece .>>. pFile .>>. pRank) // Qd5
        <|> (pFile .>>. pRank |>> fun (f, r) -> ((1, f), r)) //Pawn move, e.g. d5
        |>> fun ((piece, file), rank) -> piece, file, rank
    
    // origin square of move (usually for disambiguation)
    let pOrigin = 
        opt pPiece .>>. opt pFile .>>. opt pRank 
        |>> fun ((piece, file), rank) -> 
            (if piece.IsSome then piece.Value else -1), 
            (if file.IsSome then file.Value else -1), 
            (if rank.IsSome then rank.Value else -1)
    
    let pBasicMove = 
        attempt (pOrigin .>>. pTarget) 
        |>> fun (origin, target) -> getMove (origin, target, Standard) 
        <|> (pTarget 
        |>> fun target -> getMove ((-1, -1, -1), target, Standard))
    // parsers for capturing
    let pCapturingSign = (pchar 'x' <|> pchar ':') |>> fun x -> x.ToString()
    let pInfixCaptureMove = // e.g. QxBc5
        pOrigin .>> pCapturingSign .>>. pTarget |>> fun (orig, target) -> getMove (orig, target, Standard)
    
    let pSimplifiedPawnCapture = // e.g. dxe or de
        let ff2p (f1, f2) = 
            match f1 = f2 with //do not allow a6xa7
            | true -> pzero
            | false -> preturn (f1, f2)
        
        let ff2mv (file1, file2) = 
            { bmvpgn with Mtyp = Standard
                          MWimg = 0
                          Mffrom = file1
                          Mfto = file2 }
        
        pFile .>> pCapturingSign .>>. pFile >>= ff2p |>> ff2mv
    
    let pSuffixCaptureMove = // e.g. Qf4d4x or Qf4:
        pBasicMove .>> pCapturingSign |>> fun move -> { move with Mtyp = Standard }
    let pBasicCapturingMove = attempt (attempt pInfixCaptureMove <|> pSuffixCaptureMove) <|> pSimplifiedPawnCapture
    
    // the two most common move types: move and capture
    let pCapturingMove = 
        pBasicCapturingMove .>>. opt (strCI "e.p.") //TODO: e.p. should only be allowed for pawns
                                                    |>> fun (move, enpassant) -> 
            match enpassant with
            | None -> move
            | _ -> { move with Mtyp = EnPassent }
    
    // special moves: pawn promotion and castle (king-side, queen-side)
    // TODO: this parser allows to much, e.g. Qxd5(R). 
    //       It should be asserted, that the moved piece is a pawn.
    //       If rank is set, then only rank 8 is allowed
    let pPawnPromotion = 
        (attempt pBasicCapturingMove <|> pBasicMove) .>>. ((str "=" >>. pPiece) <|> (str "(" >>. pPiece .>> str ")")) 
        |>> fun (move, piece) -> 
            { move with Mtyp = 
                            if piece = 5 then PromQ
                            elif piece = 3 then PromR
                            elif piece = 2 then PromN
                            else PromB }
    
    let pCasteKingSide = 
        str "O-O" <|> str "O - O" <|> str "0-0" <|> str "0 - 0" |>> fun _ -> 
            { bmvpgn with Mtyp = CastleK
                          MWimg = 4 }
    
    let pCasteQueenSide = 
        str "O-O-O" <|> str "O - O - O" <|> str "0-0-0" <|> str "0 - 0 - 0" |>> fun _ -> 
            { bmvpgn with Mtyp = CastleQ
                          MWimg = 4 }
    
    let pCastle = pCasteQueenSide <|> pCasteKingSide
    // indicators
    let pCheckIndicator = str "++" <|> str "††" <|> str "dbl ch" <|> str "+" <|> str "†" <|> str "ch"
    let pCheckMateIndicator = str "#" <|> str "‡"
    let pIndicator = pCheckIndicator <|> pCheckMateIndicator
    
    let pAnnotation = 
        let getannot annotation = 
            match annotation with
            | "!!!" | "!!!!" -> MindBlowing
            | "!!" -> Brilliant
            | "!" -> Good
            | "!?" -> Interesting
            | "?!" -> Dubious
            | "?" -> Mistake
            | "??" -> Blunder
            | "???" | "????" -> Abysmal
            | "!?!" | "?!?" -> FascinatingButUnsound
            | "∞" -> Unclear
            | "=/∞" -> WithCompensation
            | "=" -> EvenPosition
            | "+/=" -> SlightAdvantageWhite
            | "=/+" -> SlightAdvantageBlack
            | "+/-" -> AdvantageWhite
            | "-/+" -> AdvantageBlack
            | "+-" -> DecisiveAdvantageWhite
            | "-+" -> DecisiveAdvantageBlack
            | "○" -> Space
            | "↑" -> Initiative
            | "↑↑" -> Development
            | "⇄" -> Counterplay
            | "∇" -> Countering
            | "Δ" -> Idea
            | "TN" | "N" -> TheoreticalNovelty
            | _ -> UnknownAnnotation
        str "????" <|> str "???" <|> str "!!!!" <|> str "!!!" <|> str "?!?" <|> str "!?!" <|> str "??" 
        <|> str "?!" <|> str "!!" <|> str "!?" <|> str "?" <|> str "!" <|> str "=/∞" <|> str "=/+" 
        <|> str "=" <|> str "+/=" <|> str "+/-" <|> str "+-" <|> str "-/+" <|> str "-+" <|> str "∞" 
        <|> str "○" <|> str "↑↑" <|> str "↑" <|> str "⇄" <|> str "∇" <|> str "Δ" <|> str "TN" <|> str "N" 
        |>> getannot <?> "Move annotation (e.g. ! or ??)"
    
    let pAdditionalInfo = 
        (attempt (pIndicator .>>. pAnnotation) 
        |>> fun (i, a) -> Some(i), Some(a)) 
        <|> (attempt (pAnnotation) 
        |>> fun a -> None, Some(a)) 
        <|> (pIndicator 
        |>> fun i -> Some(i), None)
    
    let pMove = 
        let procmv (move, addInfo) = 
            let indicator, annotation = 
                match addInfo with
                | None -> None, None
                | Some(x) -> x
            
            let annt = 
                if annotation.IsSome then annotation.Value
                else UnknownAnnotation
            
            match indicator with
            | None -> { move with Mannot = annt }
            | Some(i) -> 
                match i with
                | "+" | "†" | "ch" -> 
                    { move with Mannot = annt
                                Mischk = true }
                | "++" | "††" | "dbl ch" -> 
                    { move with Mannot = annt
                                Mischk = true
                                Misdchk = true }
                | "#" | "‡" -> 
                    { move with Mannot = annt
                                Mischkmt = true }
                | _ -> move
        attempt pPawnPromotion <|> attempt pCapturingMove <|> attempt pBasicMove 
        <|> pCastle .>>. opt (pAdditionalInfo) 
        |>> procmv <?> "Move (e.g. Qc4 or e2e4 or 0-0-0 etc.)"
    
    let appyPMove (p : string) = run pMove p
    
    let getpgnmv mv = 
        let pr = appyPMove mv
        match pr with
        | Success(result, _, _) -> result
        | Failure(errorMsg, _, _) -> failwith (errorMsg)
    
    let pPeriods = 
        (str ".." .>> manyChars (pchar '.') >>% true) //two or more dots => Continued move pair
        <|> (str "…" >>% true) // => Continued move pair
        <|> (pchar '.' >>% false) // => Non-Continued move pair (move start)
    let pMoveNumberIndicator = 
        attempt (pint32 .>> ws .>>. pPeriods 
        |>> fun (num, contd) -> (num, contd)) 
        <|> preturn (-1, false) 
        <?> "Move number indicator (e.g. 5. or 13...)"
    
    let pFullMoveTextEntry = 
        pMoveNumberIndicator .>> ws .>>. pMove .>> ws1 .>>. pMove |>> fun (((moveNum, contd), moveWhite), moveBlack) -> 
            MovePair({ Mvnum = moveNum
                       MvW = moveWhite
                       MvB = moveBlack })
    
    let pSplitMoveTextEntry = 
        pMoveNumberIndicator .>> ws .>>. pMove |>> fun ((moveNum, contd), move) -> 
            SingleMove({ Mvnumh = moveNum
                         Mv = move
                         IsCont = contd })
    
    let pCommentary = 
        between (str "{") (str "}") (many (noneOf "}")) 
        <|> between (str ";") newline (many (noneOf "\n")) //to end of line comment
        |>> charList2String 
        |>> fun text -> Comment({ Cmt = text }) 
        <?> "Comment ( {...} or ;... )"
    let pOneHalf = str "1/2" <|> str "½"
    let pDraw = pOneHalf .>> ws .>> str "-" .>> ws .>> pOneHalf |>> fun _ -> Draw
    let pWhiteWin = str "1" .>> ws .>> str "-" .>> ws .>> str "0" |>> fun _ -> Wwin
    let pBlackWin = str "0" .>> ws .>> str "-" .>> ws .>> str "1" |>> fun _ -> Bwin
    let pEndOpen = str "*" |>> fun _ -> Unknown
    let pEndOfGame = 
        pDraw <|> pWhiteWin <|> pBlackWin <|> pEndOpen 
        |>> fun endType -> GameEnd({ GRes = endType }) 
        <?> "Game termination marker (1/2-1/2 or 1-0 or 0-1 or *)"
    let pNAG = 
        pchar '$' >>. pint32 
        |>> fun code -> NAG({ NCode = code }) 
        <?> "NAG ($<num> e.g. $6 or $32)"
    let pMoveSeries, pMoveSeriesImpl = createParserForwardedToRef()
    let pRAV = 
        pchar '(' .>> ws >>. pMoveSeries .>> ws .>> pchar ')' 
        |>> fun moveSeries -> RAV(moveSeries) 
        <?> "RAV e.g. \"(6. Bd3)\""
    let pMoveSeriesEntry = 
        pCommentary <|> pNAG <|> pRAV <|> attempt (pFullMoveTextEntry) 
        <|> attempt (pSplitMoveTextEntry) <|> attempt (pEndOfGame)
    
    do pMoveSeriesImpl := (sepEndBy1 pMoveSeriesEntry ws)
    
    let setTag (game : Pgngm, (tag, tagval)) = 
        match tag with
        | "Event" -> { game with Phdr = { game.Phdr with Event = tagval } }
        | "Site" -> { game with Phdr = { game.Phdr with Site = tagval } }
        | "Date" -> { game with Phdr = { game.Phdr with Date = tagval } }
        | "Round" -> { game with Phdr = { game.Phdr with Round = tagval } }
        | "White" -> { game with Phdr = { game.Phdr with White = tagval } }
        | "Black" -> { game with Phdr = { game.Phdr with Black = tagval } }
        | "Result" -> { game with Phdr = { game.Phdr with Result = tagval |> str2gmres } }
        | _ -> game
    
    let makeGame (tagList : (string * string) list, moveTextList : Movtxt list) = 
        let bgame = 
            { Phdr = blankhdr
              MoveText = [] }
        
        let rec updgm tl gm = 
            if List.isEmpty tl then gm
            else updgm tl.Tail (setTag (gm, tl.Head))
        
        let game = updgm tagList bgame
        { game with MoveText = moveTextList }
    
    let pGame = pTagList .>> ws .>>. pMoveSeries |>> makeGame
    let pDatabase = ws >>. sepEndBy pGame ws .>> eof
    
    let ReadFromStream(stream : System.IO.Stream) = 
        let parserResult = runParserOnStream pDatabase () "pgn" stream System.Text.Encoding.UTF8
        
        let db = 
            match parserResult with
            | Success(result, _, _) -> result
            | Failure(errorMsg, _, _) -> failwith (errorMsg)
        db
    
    let ReadFromFile(file : string) = 
        let stream = new FileStream(file, FileMode.Open)
        let result = ReadFromStream(stream)
        stream.Close()
        result
    
    let ReadFromString(input : string) = 
        let parserResult = run pDatabase input
        
        let db = 
            match parserResult with
            | Success(result, _, _) -> result
            | Failure(errorMsg, _, _) -> failwith (errorMsg)
        db