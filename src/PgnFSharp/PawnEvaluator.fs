namespace Lizard.Engine

type PawnEval = 
    { DoubledPawnValueStart : int
      DoubledPawnValueEnd : int
      IsolatedPawnValueStart : int
      IsolatedPawnValueEnd : int
      UnconnectedPawnStart : int
      UnconnectedPawnEnd : int
      CandidatePct : float
      PassedDistancePct : float []
      PawnHash : PawnRes option []
      PassedPawnMinScore : int
      EndScore : int []
      Factors : int []
      StartScore : int [] }

module PawnEvaluator = 
    let _telestop = 
        let gettel p =
            p
            |> Position.ToBitboard
            |> Bitboard.Flood(Direction.DirN)
            &&& ~~~(p |> Position.ToBitboard)
        Position.AllPositions|>Array.map gettel
    
    let _attackMaskW = 
        let getmask p =
            let north = 
                p
                |> Position.ToBitboard
                |> Bitboard.Flood(Direction.DirN)
                &&& ~~~(p |> Position.ToBitboard)
            (north |> Bitboard.ShiftDirW) ||| (north |> Bitboard.ShiftDirE)
        Position.AllPositions|>Array.map getmask

    let _attackMaskB = 
        let getmask p =
            let south = 
                p
                |> Position.ToBitboard
                |> Bitboard.Flood(Direction.DirS)
                &&& ~~~(p |> Position.ToBitboard)
            (south |> Bitboard.ShiftDirW) ||| (south |> Bitboard.ShiftDirE)
        Position.AllPositions|>Array.map getmask
    
    let Create (settings : Settngs) hashSize = 
        let getpd i =
            let min = 3
            let max = 7
            if i <= min then settings.PawnPassedClosePct
            else 
                let p = float (i - min) / float (max - min)
                let d = settings.PawnPassedFarPct - settings.PawnPassedClosePct
                settings.PawnPassedClosePct + (p * d)
        let pDistancePct = [|0..7|]|>Array.map getpd
        let pcts = [|0..7|]|>Array.map(fun i -> settings.PawnPassedRankReduction ** (float i))
        let es0 = pcts|>Array.map(fun p -> int (float (settings.PawnPassed8thRankScore) * p))
        let factors = es0|>Array.map(fun e -> int (float(e) * settings.PawnPassedDangerPct))
        let endScore = es0|>Array.map(fun e -> e + settings.PawnPassedMinScore)
        let startScore = endScore|>Array.map(fun e -> int (float(e) * settings.PawnPassedOpeningPct))
        { DoubledPawnValueStart = settings.PawnDoubled.[Opening]
          DoubledPawnValueEnd = settings.PawnDoubled.[Endgame]
          IsolatedPawnValueStart = settings.PawnIsolated.[Opening]
          IsolatedPawnValueEnd = settings.PawnIsolated.[Endgame]
          UnconnectedPawnStart = settings.PawnUnconnected.[Opening]
          UnconnectedPawnEnd = settings.PawnUnconnected.[Endgame]
          CandidatePct = settings.PawnCandidatePct
          PassedDistancePct = pDistancePct
          PawnHash = Array.zeroCreate hashSize
          PassedPawnMinScore = settings.PawnPassedMinScore
          EndScore = endScore
          Factors = factors
          StartScore = startScore }
    
    let _shelterFactor = [| 7; 0; 2; 5; 6; 7; 7; 7 |]
    
    let CalcKingShelterPenaltyFactorBlackPerspective kingFile myPawns = 
        let kfil = 
            if kingFile = File.FileA then File.FileB
            elif kingFile = File.FileH then File.FileG
            else kingFile
        
        let ikfil = int (kfil)
        
        let rec getrv f ct7 rv = 
            if f = ikfil + 2 then ct7, rv
            else 
                let bbFile = Fl(f) |> File.ToBitboard
                let bbPawnFile = bbFile &&& myPawns
                
                let pawnRank = 
                    if bbPawnFile = Bitboard.Empty then Rank.Rank1
                    else 
                        bbPawnFile
                        |> Bitboard.NorthMostPosition
                        |> Position.ToRank
                
                let nrv = rv + _shelterFactor.[int (pawnRank)]
                if pawnRank = Rank.Rank7 then getrv (f + 1) (ct7 + 1) nrv
                else getrv (f + 1) ct7 nrv
        
        let ct7, rv = getrv (ikfil - 1) 0 0
        if ct7 >= 2 then rv - 1
        else rv
    
    let EvalShelterCalc whiteKingFile blackKingFile castleFlags (pr : PawnRes) = 
        let wpRev = pr.WhitePawns |> Bitboard.Reverse
        
        let lowestWhitePenalty = 
            let lwp = CalcKingShelterPenaltyFactorBlackPerspective whiteKingFile wpRev
            
            let lwp = 
                if int (castleFlags &&& CstlFlgs.WhiteShort) <> 0 then 
                    let castlePenalty = CalcKingShelterPenaltyFactorBlackPerspective File.FileG wpRev + 2
                    if castlePenalty < lwp then castlePenalty
                    else lwp
                else lwp
            if int (castleFlags &&& CstlFlgs.WhiteLong) <> 0 then 
                let castlePenalty = CalcKingShelterPenaltyFactorBlackPerspective File.FileC wpRev + 2
                if castlePenalty < lwp then castlePenalty
                else lwp
            else lwp
        
        let lowestBlackPenalty = 
            let lbp = CalcKingShelterPenaltyFactorBlackPerspective blackKingFile pr.BlackPawns
            
            let lbp = 
                if int (castleFlags &&& CstlFlgs.BlackShort) <> 0 then 
                    let castlePenalty = CalcKingShelterPenaltyFactorBlackPerspective File.FileG pr.BlackPawns + 2
                    if castlePenalty < lbp then castlePenalty
                    else lbp
                else lbp
            if int (castleFlags &&& CstlFlgs.BlackLong) <> 0 then 
                let castlePenalty = CalcKingShelterPenaltyFactorBlackPerspective File.FileC pr.BlackPawns + 2
                if castlePenalty < lbp then castlePenalty
                else lbp
            else lbp
        
        lowestBlackPenalty - lowestWhitePenalty
    
    let EvalShelter whiteKingFile blackKingFile castleFlags (pr : PawnRes) = 
        let key = int (whiteKingFile) ||| (int (blackKingFile) <<< 8) ||| (int (castleFlags) <<< 20)
        if key = pr.PshelterCacheKey then pr.PshelterCacheValue
        else EvalShelterCalc whiteKingFile blackKingFile castleFlags pr
    
    let IsPassedUnstoppable (board : Brd) (pawnPlayer : Player) (pawnPos : Position) = 
        let kingPlayer = pawnPlayer |> Player.PlayerOther
        let pawnFile = pawnPos |> Position.ToFile
        let pawnRank = pawnPos |> Position.ToRank
        let pawnRank8 = pawnPlayer |> Player.MyRank(Rank.Rank8)
        let queenSq = pawnRank8 |> Rank.ToPosition(pawnFile)
        let kingPosition = if kingPlayer=Player.White then board.WtKingPos else board.BkKingPos
        let pawnDist = abs (int (pawnRank8) - int (pawnRank))
        let kingDist = kingPosition |> Position.DistanceTo(queenSq)
        
        let plysToPromote = 
            (pawnDist * 2) - (if board.WhosTurn = pawnPlayer then 1
                              else 0)
        
        let plysToCapture = 
            (kingDist * 2) - (if board.WhosTurn = kingPlayer then 1
                              else 0)
        
        let path = (pawnPos |> Position.Between(queenSq)) ||| (queenSq |> Position.ToBitboard)
        if path = (path &&& Attacks.KingAttacks(if pawnPlayer=Player.White then board.WtKingPos else board.BkKingPos)) then true, plysToPromote
        else 
            let plysToPromote = 
                if (path &&& board.PieceLocationsAll) <> Bitboard.Empty then 
                    plysToPromote + ((path &&& board.PieceLocationsAll) |> Bitboard.BitCount) * 2
                else plysToPromote
            if plysToCapture <= plysToPromote + 1 then false, plysToPromote
            else (plysToCapture > (plysToPromote + 1)), plysToPromote
    
    let EvalUnstoppablePawns (board : Brd) passed candidates = 
        let plysToPromoteWhite = 
            if board.BkPrBds = ((board.BkPrBds &&& board.PieceTypes.[int (PieceType.King)]) ||| (board.BkPrBds &&& board.PieceTypes.[int (PieceType.Pawn)])) then 
                let rec getptpw ptpw pp = 
                    if pp = Bitboard.Empty then ptpw
                    else 
                        let pawnPos, npp = Bitboard.PopFirst(pp)
                        let ok, plys = IsPassedUnstoppable board Player.White pawnPos
                        
                        let nptpw = 
                            if ok then min ptpw plys
                            else ptpw
                        getptpw nptpw npp
                
                let playerPassed = passed &&& board.WtPrBds
                getptpw 99 playerPassed
            else 99
        
        let plysToPromoteBlack = 
            if board.WtPrBds = ((board.WtPrBds &&& board.PieceTypes.[int (PieceType.King)]) ||| (board.WtPrBds &&& board.PieceTypes.[int (PieceType.Pawn)])) then 
                let rec getptpb ptpb pp = 
                    if pp = Bitboard.Empty then ptpb
                    else 
                        let pawnPos, npp = Bitboard.PopFirst(pp)
                        let ok, plys = IsPassedUnstoppable board Player.Black pawnPos
                        
                        let nptpb = 
                            if ok then min ptpb plys
                            else ptpb
                        getptpb nptpb npp
                
                let playerPassed = passed &&& board.BkPrBds
                getptpb 99 playerPassed
            else 99
        
        if plysToPromoteWhite <> plysToPromoteBlack && abs (plysToPromoteWhite - plysToPromoteBlack) > 2 then 
            if plysToPromoteWhite < plysToPromoteBlack then PhasedScore.Create 0 (750 - (plysToPromoteWhite * 20))
            else PhasedScore.Create 0 (-750 + (plysToPromoteBlack * 20))
        else PhasedScore.Create 0 0
    
    let EvalPassedPawn (me, p : Position, myKing : Position, hisKing : Position, allPieces : Bitboard) 
        (myPawnAttacks : Bitboard, myAttacks : Bitboard, hisAttacks : Bitboard) (attackingTrailer, supportingTrailer) 
        (pneval : PawnEval) = 
        let rank = 
            if me = Player.Black then 7 - int (p |> Position.ToRank)
            else int (p |> Position.ToRank)
        
        let mbonus = pneval.StartScore.[rank]
        let ebonus = pneval.EndScore.[rank]
        let dangerFactor = pneval.Factors.[rank]
        
        let blockSq = 
            p |> Position.PositionInDirection(if me = Player.White then Direction.DirN
                                              else Direction.DirS)
        
        let k = 
            if rank <= int (Rank.Rank5) then 
                let k = (hisKing |> Position.DistanceTo(blockSq)) - (myKing |> Position.DistanceTo(blockSq))
                
                let k = 
                    if not (allPieces |> Bitboard.ContainsPos(blockSq)) then k + 2
                    else k
                
                let k = 
                    if hisAttacks |> Bitboard.ContainsPos(blockSq) then k - 1
                    else k
                
                if not (myAttacks |> Bitboard.ContainsPos(blockSq)) then k + 1
                else k
            else 0
        
        let k = 
            if attackingTrailer then k - 2
            else k
        
        let k = 
            if supportingTrailer then k + 2
            else k
        
        let ebonus = 
            if supportingTrailer then ebonus + pneval.PassedPawnMinScore
            else ebonus
        
        let k = 
            if myPawnAttacks
               |> Bitboard.ContainsPos(blockSq)
               || myPawnAttacks |> Bitboard.ContainsPos(p) then k + 2
            else k
        
        let ebonus = 
            if myPawnAttacks
               |> Bitboard.ContainsPos(blockSq)
               || myPawnAttacks |> Bitboard.ContainsPos(p) then ebonus + pneval.PassedPawnMinScore
            else ebonus
        
        let ebonus = ebonus + k * dangerFactor
        mbonus, ebonus
    
    let EvalPassedPawnsSide (me : Player) (board : Brd) (plyData : PlDt) 
        (passedPawns, candidatePawns : Bitboard, workspace) (pneval : PawnEval) = 
        let him = me |> Player.PlayerOther
        let myAttackInfo = plyData |> EvalResults.AttacksForPldt me
        let hisAttackInfo = plyData |> EvalResults.AttacksForPldt him
        let myKing = if me=Player.White then board.WtKingPos else board.BkKingPos
        let hisKing = if him=Player.White then board.WtKingPos else board.BkKingPos
        let allPieces = board.PieceLocationsAll
        let myPawnAttacks = myAttackInfo.Apawn
        let myAttacks = myAttackInfo.Acount1
        let hisAttacks = hisAttackInfo.Acount1
        let positions = passedPawns ||| candidatePawns
        
        let mySouth = 
            if me = Player.White then Direction.DirS
            else Direction.DirN
        
        let workspace = Array.create 8 0
        
        let rec getscr tsscr bescr befil psns = 
            if psns = Bitboard.Empty then tsscr, befil
            else 
                let ppos, npsns = Bitboard.PopFirst(psns)
                let tPiece, tpos = board |> Board.PieceInDirection ppos mySouth
                let isCan = candidatePawns |> Bitboard.ContainsPos(ppos)
                let attackingTrailer = (tPiece |> Piece.PieceIsSliderRook) && (tPiece |> Piece.PieceToPlayer) = him
                let supportingTrailer = (tPiece |> Piece.PieceIsSliderRook) && (tPiece |> Piece.PieceToPlayer) = me
                let sscr, escr = 
                    EvalPassedPawn (me, ppos, myKing, hisKing, allPieces) (myPawnAttacks, myAttacks, hisAttacks) 
                        (attackingTrailer, supportingTrailer) pneval
                let sscr = max 0 sscr
                let escr = max 0 escr
                let iFile = int (ppos |> Position.ToFile)
                
                let sscr, escr = 
                    if isCan then 0, int (float (escr) * pneval.CandidatePct)
                    else sscr, escr
                
                let ntsscr = tsscr + sscr
                let escr = escr &&& ~~~1
                workspace.[iFile] <- escr//OK
                let nbescr, nbefil = 
                    if escr > bescr then escr, iFile
                    else bescr, befil
                getscr ntsscr nbescr nbefil npsns
        
        let tsscr, befil = getscr 0 -1 -1 positions
        
        let dows i ws = 
            if i <> befil && workspace.[i] <> 0 then 
                let fileDiff = abs (i - befil)
                int (float (ws) * pneval.PassedDistancePct.[fileDiff])
            else ws
        
        let tescr = 
            workspace
            |> Array.mapi dows
            |> Array.sum
        
        PhasedScore.Create tsscr tescr
    
    let EvalPassedPawns (board : Brd) plyData (passedPawns, candidatePawns, workspace) (pneval : PawnEval) = 
        let rv0 = PhasedScore.Create 0 0
        let psns1 = (passedPawns ||| candidatePawns) &&& board.WtPrBds
        
        let rv1 = 
            if psns1 <> Bitboard.Empty then 
                let white = 
                    EvalPassedPawnsSide Player.White board plyData 
                        (passedPawns &&& board.WtPrBds, 
                         candidatePawns &&& board.WtPrBds, workspace) pneval
                rv0 + white
            else rv0
        
        let psns2 = (passedPawns ||| candidatePawns) &&& board.BkPrBds
        if psns2 <> Bitboard.Empty then 
            let black = 
                EvalPassedPawnsSide Player.Black board plyData 
                    (passedPawns &&& board.BkPrBds, 
                     candidatePawns &&& board.BkPrBds, workspace) pneval
            rv1 - black
        else rv1
    
    let IsCandidate (pos : Position) white black (pneval : PawnEval) = 
        if (_telestop.[int (pos)] &&& black) <> Bitboard.Empty then false
        elif not 
                 ((Bitboard.Rank4 ||| Bitboard.Rank5 ||| Bitboard.Rank6 ||| Bitboard.Rank7) |> Bitboard.ContainsPos(pos)) then 
            false
        else 
            let blockers = _attackMaskW.[int (pos)] &&& black
            let helpers = _attackMaskB.[int (pos |> Position.PositionInDirectionUnsafe(Direction.DirN))] &&& white
            if helpers = Bitboard.Empty then false
            else (helpers |> Bitboard.BitCount) >= (blockers |> Bitboard.BitCount)
    
    let EvalWhitePawns whitePawns blackPawns (pneval : PawnEval) = 
        let rec getans sval nval (pas, dbl, iso, unc, can) psns = 
            if psns = Bitboard.Empty then sval, nval, (pas, dbl, iso, unc, can)
            else 
                let pos, npsns = Bitboard.PopFirst(psns)
                let f = pos |> Position.ToFile
                let r = pos |> Position.ToRank
                
                let bbFile = 
                    pos
                    |> Position.ToFile
                    |> File.ToBitboard
                
                let bbFile2E = bbFile |> Bitboard.ShiftDirE
                let bbFile2W = bbFile |> Bitboard.ShiftDirW
                let telestop = _telestop.[int (pos)]
                
                let nsval, nnval, ndbl, niso, nunc = 
                    if Bitboard.IsEmpty(bbFile2E &&& whitePawns) && Bitboard.IsEmpty(bbFile2W &&& whitePawns) then 
                        let nsval = sval - pneval.IsolatedPawnValueStart
                        let nnval = nval - pneval.IsolatedPawnValueEnd
                        let niso = iso ||| (pos |> Position.ToBitboard)
                        nsval, nnval, dbl, niso, unc
                    elif not (Bitboard.IsEmpty(telestop &&& whitePawns)) then 
                        let nsval = sval - pneval.DoubledPawnValueStart
                        let nnval = nval - pneval.DoubledPawnValueEnd
                        let ndbl = dbl ||| (pos |> Position.ToBitboard)
                        nsval, nnval, ndbl, iso, unc
                    else 
                        let bbRank = r |> Rank.ToBitboard
                        let connectedRanks = 
                            bbRank ||| (bbRank |> Bitboard.ShiftDirN) ||| (bbRank |> Bitboard.ShiftDirS)
                        let connectedPos = connectedRanks &&& (bbFile2E ||| bbFile2W)
                        if Bitboard.IsEmpty(whitePawns &&& connectedPos) then 
                            let nsval = sval - pneval.UnconnectedPawnStart
                            let nnval = nval - pneval.UnconnectedPawnEnd
                            let nunc = unc ||| (pos |> Position.ToBitboard)
                            nsval, nnval, dbl, iso, nunc
                        else sval, nval, dbl, iso, unc
                
                let npas, ncan = 
                    if Bitboard.IsEmpty(telestop &&& whitePawns) then 
                        let blockPositions = _attackMaskW.[int (pos)] ||| _telestop.[int (pos)]
                        if Bitboard.IsEmpty(blockPositions &&& blackPawns) then 
                            let npas = pas ||| (pos |> Position.ToBitboard)
                            npas, can
                        elif IsCandidate pos whitePawns blackPawns pneval then 
                            let ncan = can ||| (pos |> Position.ToBitboard)
                            pas, ncan
                        else pas, can
                    else pas, can
                
                getans nsval nnval (npas, ndbl, niso, nunc, ncan) npsns
        getans 0 0 (Bitboard.Empty, Bitboard.Empty, Bitboard.Empty, Bitboard.Empty, Bitboard.Empty) whitePawns
    
    let EvalAllPawns whitePawns blackPawns pawnZobrist (pneval : PawnEval) = 
        let WStartVal, WEndVal, (wpassed, wdoubled, wisolated, wunconnected, wcandidates) = 
            EvalWhitePawns whitePawns blackPawns pneval
        let blackRev = blackPawns |> Bitboard.Reverse
        let whiteRev = whitePawns |> Bitboard.Reverse
        let BStartVal, BEndVal, (bpassed, bdoubled, bisolated, bunconnected, bcandidates) = 
            EvalWhitePawns blackRev whiteRev pneval
        let passed = wpassed ||| (bpassed |> Bitboard.Reverse)
        let doubled = wdoubled ||| (bdoubled |> Bitboard.Reverse)
        let isolated = wisolated ||| (bisolated |> Bitboard.Reverse)
        let unconnected = wunconnected ||| (bunconnected |> Bitboard.Reverse)
        let candidates = wcandidates ||| (bcandidates |> Bitboard.Reverse)
        let startVal = WStartVal - BStartVal
        let endVal = WEndVal - BEndVal
        { PawnZobrist = pawnZobrist
          WhitePawns = whitePawns
          BlackPawns = blackPawns
          Value = PhasedScore.Create startVal endVal
          PassedPawns = passed
          Doubled = doubled
          Isolated = isolated
          Unconnected = unconnected
          Candidates = candidates
          PshelterCacheKey = 0x3a184743
          PshelterCacheValue = 0 }
        |> Some
    
    let PawnEval (board : Brd) (pneval : PawnEval) = 
        let oidx : int64 = board.ZobPawn % int64 (pneval.PawnHash.GetUpperBound(0))
        
        let idx = 
            if oidx < 0L then -oidx
            else oidx
        
        let rv = pneval.PawnHash.[int (idx)]
        if rv <> None && rv.Value.PawnZobrist = board.ZobPawn then rv
        else 
            let rv = 
                EvalAllPawns (board.PieceTypes.[int (PieceType.Pawn)] &&& board.WtPrBds) 
                    (board.PieceTypes.[int (PieceType.Pawn)] &&& board.BkPrBds) board.ZobPawn 
                    pneval
            pneval.PawnHash.[int (idx)] <- rv//OK
            rv
