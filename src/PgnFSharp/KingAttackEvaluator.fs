namespace Lizard.Engine

type KingAttackEval = 
    { KingAttackCountValue : int
      KingAttackWeightValue : int
      KingAttackWeightCutoff : int
      KingRingAttack : int
      KingRingAttackControlBonus : int
      KingQueenTropismFactor : int [] }

module KingAttackEvaluator = 
    let _kingSafetyRegion = 
        let getk p =
            let rank = p |> Position.ToRank
            
            let rank = 
                if rank = Rank.Rank1 then Rank.Rank2
                else rank
            
            let rank = 
                if rank = Rank.Rank8 then Rank.Rank7
                else rank
            
            let file = p |> Position.ToFile
            
            let file = 
                if file = File.FileA then File.FileB
                else file
            
            let file = 
                if file = File.FileH then File.FileG
                else file
            
            let adjusted = rank |> Rank.ToPosition(file)
            Attacks.KingAttacks(adjusted) ||| (adjusted |> Position.ToBitboard)
        Position.AllPositions|>Array.map getk
    
    let _kingAttackerWeight = [|0;1;2;2;3;4;1|]
    
    let Create(settings : KASet) = 
        let kaCountValue = settings.KingAttackCountValue
        let kaWeightValue = settings.KingAttackWeightValue
        let kaWeightCutoff = settings.KingAttackWeightCutoff
        let kRingAttack = settings.KingRingAttack
        let kRingAttackControlBonus = settings.KingRingAttackControlBonus
        let getkqtf d =
            let fd = float (d)
            let mini = 4.0
            let maxi = 12.0
            let minFactor = settings.KingAttackFactor
            let maxFactor = minFactor + settings.KingAttackFactorQueenTropismBonus
            if fd >= maxi then int (round (100.0 * minFactor))
            elif fd <= mini then int (round (100.0 * maxFactor))
            else 
                let each = (1.0 / (maxi - mini)) * (maxFactor - minFactor)
                let thisFactor = minFactor + (each * (maxi - fd))
                let rnd = int (round (100.0 * thisFactor))
                rnd
        let kqTropismFactor = [|0..24|]|>Array.map getkqtf
        { KingAttackCountValue = kaCountValue
          KingAttackWeightValue = kaWeightValue
          KingAttackWeightCutoff = kaWeightCutoff
          KingRingAttack = kRingAttack
          KingRingAttackControlBonus = kRingAttackControlBonus
          KingQueenTropismFactor = kqTropismFactor }
    
    let KingRegion(pos : Position) = _kingSafetyRegion.[int (pos)]
    
    let EvaluateMyKingAttack (board : Brd, me : Player) info (plyData : PlDt) myInvolvedPieces (kae : KingAttackEval) = 
        let him = me |> Player.PlayerOther
        let myAttackInfo = plyData |> EvalResults.AttacksForPldt me
        let hisAttackInfo = plyData |> EvalResults.AttacksForPldt him
        let hisKingPosition = if him=Player.White then board.WtKingPos else board.BkKingPos
        let hisKingZone = _kingSafetyRegion.[int (hisKingPosition)]
        
        let rec getctwt kac kaw mip = 
            if mip <> Bitboard.Empty then 
                let pos, nmip = Bitboard.PopFirst(mip)
                let piece = board.PieceAt.[int (pos)]
                let nkac = kac + 1
                let nkaw = kaw + _kingAttackerWeight.[int (piece |> Piece.ToPieceType)]
                getctwt nkac nkaw nmip
            else kac, kaw
        
        let kingAttackerCount, kingAttackerWeight = getctwt 0 0 myInvolvedPieces
        
        let rec getkqt kqt mip = 
            if mip <> Bitboard.Empty then 
                let pos, nmip = Bitboard.PopFirst(mip)
                let nkqt = 
                    min kqt 
                        ((hisKingPosition |> Position.DistanceTo(pos)) 
                         + (hisKingPosition |> Position.DistanceToNoDiag(pos)))
                getkqt nkqt nmip
            else kqt
        
        let myInvolvedPieces = (if me=Player.White then board.WtPrBds else board.BkPrBds) &&& board.PieceTypes.[int (PieceType.Queen)]
        let kingQueenTropism = getkqt 24 myInvolvedPieces
        
        let retval, kac = 
            if kingAttackerCount >= 2 && kingAttackerWeight >= kae.KingAttackWeightCutoff then 
                let myPawns = (if me=Player.White then board.WtPrBds else board.BkPrBds) &&& board.PieceTypes.[int (PieceType.Pawn)]
                
                let myInvolvedPawns = 
                    if me = Player.White then 
                        Bitboard.Empty ||| (hisKingZone |> Bitboard.ShiftDirSE) &&& myPawns 
                        ||| (hisKingZone |> Bitboard.ShiftDirSW) &&& myPawns
                    else 
                        Bitboard.Empty ||| (hisKingZone |> Bitboard.ShiftDirNE) &&& myPawns 
                        ||| (hisKingZone |> Bitboard.ShiftDirNW) &&& myPawns
                
                let kingAttackerCount, kingAttackerWeight = 
                    if myInvolvedPawns <> Bitboard.Empty then 
                        let c = myInvolvedPawns |> Bitboard.BitCount
                        (kingAttackerCount + c), (kingAttackerWeight + c * _kingAttackerWeight.[int (PieceType.Pawn)])
                    else kingAttackerCount, kingAttackerWeight
                
                let kingAttackerCount, kingAttackerWeight = 
                    if ((Attacks.KingAttacks(if me=Player.White then board.WtKingPos else board.BkKingPos) &&& hisKingZone) <> Bitboard.Empty) then 
                        kingAttackerCount + 1, kingAttackerWeight + _kingAttackerWeight.[int (PieceType.King)]
                    else kingAttackerCount, kingAttackerWeight
                
                let retval = (kingAttackerWeight - kae.KingAttackWeightCutoff) * kae.KingAttackWeightValue
                
                let rec getrv ka rv = 
                    if ka <> Bitboard.Empty then 
                        let pos, nka = Bitboard.PopFirst(ka)
                        let posBB = pos |> Position.ToBitboard
                        
                        let nrv = 
                            if ((posBB &&& myAttackInfo.Acount1) <> Bitboard.Empty) then 
                                let nrv = rv + kae.KingRingAttack
                                let myCount = myAttackInfo |> AttackInfo.AttackCountTo(pos)
                                let hisCount = hisAttackInfo |> AttackInfo.AttackCountTo(pos)
                                
                                let nrv = 
                                    if myCount > hisCount then 
                                        nrv + kae.KingRingAttackControlBonus * (myCount - hisCount)
                                    else nrv
                                nrv
                            else rv
                        getrv nka nrv
                    else rv
                
                let retval = getrv (Attacks.KingAttacks(if him=Player.White then board.WtKingPos else board.BkKingPos)) retval
                retval, kingAttackerCount
            else 0, kingAttackerCount
        
        let retval2 = retval + kae.KingAttackCountValue * kac
        let retval3 = (retval2 * kae.KingQueenTropismFactor.[kingQueenTropism]) / 100
        retval3
