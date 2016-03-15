namespace Lizard.Engine

type GmStg = 
    | Opening
    | Endgame

type KASet = 
    { KingAttackCountValue : int
      KingAttackWeightValue : int
      KingAttackWeightCutoff : int
      KingRingAttack : int
      KingRingAttackControlBonus : int
      KingAttackFactor : float
      KingAttackFactorQueenTropismBonus : float }

type GmStgIntDict = System.Collections.Generic.IDictionary<GmStg, int>

type Mblty = 
    { Amplitude : int
      BezControlPct : Pnt
      ExpectedAttacksAvailable : int }

type GmStgMbDict = System.Collections.Generic.IDictionary<GmStg, Mblty>

type PcGmStgMbDict = System.Collections.Generic.IDictionary<PieceType, GmStgMbDict>

type MbSet = 
    { MbDict : PcGmStgMbDict
      RookFileOpen : int }

type PcSettngs = 
    { BaseValue : int
      PairBonus : int }

type PnSettngs = 
    { First : int
      Eighth : int
      Curve : Pnt }

type MtSettngs = 
    { Pawn : PnSettngs
      Knight : PcSettngs
      Bishop : PcSettngs
      Rook : PcSettngs
      Queen : PcSettngs
      RookMinorExchange : int
      PawnMinorExchange : int
      QueenRookExchange : int }

type MtSetDict = System.Collections.Generic.IDictionary<GmStg, MtSettngs>

type SqDict = 
    { mutable Offset : int //OK
      Rank1 : int
      Rank2 : int
      Rank3 : int
      Rank4 : int
      Rank5 : int
      Rank6 : int
      Rank7 : int
      Rank8 : int
      FileAH : int
      FileBG : int
      FileCF : int
      FileDE : int
      Center4 : int
      CenterBorder : int
      OutsideEdge : int
      BbCenter4 : Bitboard
      BbCenterBorder : Bitboard
      BbOutsiteEdge : Bitboard }

type GmStgSqDict = System.Collections.Generic.IDictionary<GmStg, SqDict>

type PcGmStgSqDict = System.Collections.Generic.IDictionary<PieceType, GmStgSqDict>

type Settngs = 
    { PcSqTables : PcGmStgSqDict
      MaterialValues : MtSetDict
      PawnDoubled : GmStgIntDict
      PawnIsolated : GmStgIntDict
      PawnUnconnected : GmStgIntDict
      Mobility : MbSet
      KingAttack : KASet
      PawnPassed8thRankScore : int
      PawnPassedRankReduction : double
      PawnPassedMinScore : int
      PawnPassedDangerPct : double
      PawnPassedOpeningPct : double
      PawnCandidatePct : double
      PawnPassedClosePct : double
      PawnPassedFarPct : double
      PawnShelterFactor : int }

module Settings = 
    let GetPnVals pns = 
        let vals = QuadraticBezier.GetIntegerValues 7.0 (float (pns.First)) (float (pns.Eighth)) pns.Curve.X pns.Curve.Y
        vals |> Array.map (fun v -> int (v))
    
    let GetSd (pos : Position) sd = 
        let rank = pos |> Position.ToRank
        let file = pos |> Position.ToFile
        
        let radd = 
            match rank with
            | Rank.Rank8 -> sd.Rank8
            | Rank.Rank7 -> sd.Rank7
            | Rank.Rank6 -> sd.Rank6
            | Rank.Rank5 -> sd.Rank5
            | Rank.Rank4 -> sd.Rank4
            | Rank.Rank3 -> sd.Rank3
            | Rank.Rank2 -> sd.Rank2
            | Rank.Rank1 -> sd.Rank1
            | _ -> failwith "invalid rank"
        
        let fadd = 
            match file with
            | File.FileA | File.FileH -> sd.FileAH
            | File.FileB | File.FileG -> sd.FileBG
            | File.FileC | File.FileF -> sd.FileCF
            | File.FileD | File.FileE -> sd.FileDE
            | _ -> failwith "invalid file"
        
        let oadd1 = 
            if sd.BbCenter4 |> Bitboard.ContainsPos(pos) then sd.Center4
            else 0
        
        let oadd2 = 
            if sd.BbCenterBorder |> Bitboard.ContainsPos(pos) then sd.CenterBorder
            else 0
        
        let oadd3 = 
            if sd.BbOutsiteEdge |> Bitboard.ContainsPos(pos) then sd.OutsideEdge
            else 0
        
        radd + fadd + oadd1 + oadd2 + oadd3 + sd.Offset
    
    let CreateSd(r1, r2, r3, r4, r5, r6, r7, r8, fAH, fBG, fCF, fDE, c4, cB, oE) = 
        let bbC4 = Bitboard.D4 ||| Bitboard.D5 ||| Bitboard.E4 ||| Bitboard.E5
        let bbCB = 
            Bitboard.C6 ||| Bitboard.D6 ||| Bitboard.E6 ||| Bitboard.F6 ||| Bitboard.C5 ||| Bitboard.F5 ||| Bitboard.C4 
            ||| Bitboard.F4 ||| Bitboard.C3 ||| Bitboard.D3 ||| Bitboard.E3 ||| Bitboard.F3
        let bbOE = Bitboard.Rank1 ||| Bitboard.Rank8 ||| Bitboard.FileA ||| Bitboard.FileH
        { Offset = 0
          Rank1 = r1
          Rank2 = r2
          Rank3 = r3
          Rank4 = r4
          Rank5 = r5
          Rank6 = r6
          Rank7 = r7
          Rank8 = r8
          FileAH = fAH
          FileBG = fBG
          FileCF = fCF
          FileDE = fDE
          Center4 = c4
          CenterBorder = cB
          OutsideEdge = oE
          BbCenter4 = bbC4
          BbCenterBorder = bbCB
          BbOutsiteEdge = bbOE }
    
    let Default() = 
        let pcSqTabs = 
            let p = 
                [ Opening, CreateSd(0, -9, 0, 6, 18, 72, 100, 0, -16, 7, 3, 2, 7, 0, 0)
                  Endgame, CreateSd(0, 2, 3, 6, 9, 11, 4, 0, -10, 9, 12, 12, -20, -10, 0) ]
                |> dict
            
            let n = 
                [ Opening, CreateSd(-9, 0, 3, 15, 18, 36, 18, -60, -12, -9, 0, 12, -5, 6, -6)
                  Endgame, CreateSd(-3, 0, 0, 16, 22, 14, 9, 7, 1, 6, 6, 10, 6, 4, 6) ]
                |> dict
            
            let b = 
                [ Opening, CreateSd(9, 18, 18, 0, -6, 15, -27, -33, 3, 3, -6, -12, 30, 4, -9)
                  Endgame, CreateSd(-3, 3, 6, 3, 6, 9, 9, -6, -6, 0, 0, 3, 8, 6, -8) ]
                |> dict
            
            let r = 
                [ Opening, CreateSd(6, -9, -6, -3, 27, 45, 40, 45, -6, 9, 22, 28, -9, -6, 3)
                  Endgame, CreateSd(-9, -12, -6, 0, 6, 15, 21, 15, 0, 0, -3, -6, 6, 0, 0) ]
                |> dict
            
            let q = 
                [ Opening, CreateSd(6, 21, 12, 3, -15, 3, -24, -33, -9, 0, 3, 6, -10, 1, 11)
                  Endgame, CreateSd(-36, -15, 12, 27, 51, 39, 54, 54, -6, 3, 0, 3, 2, 4, -15) ]
                |> dict
            
            let k = 
                [ Opening, CreateSd(0, -5, -35, -64, -80, -80, -80, -80, 15, 26, 5, -3, 0, 0, 10)
                  Endgame, CreateSd(-33, -9, 0, 18, 36, 60, 45, 9, -12, 3, 3, 6, 1, 7, 0) ]
                |> dict
            
            [ PieceType.Pawn, p
              PieceType.Knight, n
              PieceType.Bishop, b
              PieceType.Rook, r
              PieceType.Queen, q
              PieceType.King, k ]
            |> dict
        
        let matVals = 
            let oset = 
                { Pawn = 
                      { First = 100
                        Eighth = 100
                        Curve = 
                            { X = 0.5
                              Y = 0.5 } }
                  Knight = 
                      { BaseValue = 315
                        PairBonus = 0 }
                  Bishop = 
                      { BaseValue = 310
                        PairBonus = 25 }
                  Rook = 
                      { BaseValue = 500
                        PairBonus = 0 }
                  Queen = 
                      { BaseValue = 1100
                        PairBonus = 0 }
                  RookMinorExchange = 60
                  PawnMinorExchange = 75
                  QueenRookExchange = 30 }
            
            let eset = 
                { Pawn = 
                      { First = 120
                        Eighth = 75
                        Curve = 
                            { X = 0.3
                              Y = 0.7 } }
                  Knight = 
                      { BaseValue = 305
                        PairBonus = 0 }
                  Bishop = 
                      { BaseValue = 325
                        PairBonus = 75 }
                  Rook = 
                      { BaseValue = 550
                        PairBonus = 0 }
                  Queen = 
                      { BaseValue = 1100
                        PairBonus = 0 }
                  RookMinorExchange = -30
                  PawnMinorExchange = 55
                  QueenRookExchange = 60 }
            
            [ Opening, oset
              Endgame, eset ]
            |> dict
        
        let pDoubled = 
            [ Opening, 22
              Endgame, 10 ]
            |> dict
        
        let pIsolated = 
            [ Opening, 9
              Endgame, 13 ]
            |> dict
        
        let pUnconnected = 
            [ Opening, 11
              Endgame, 2 ]
            |> dict
        
        let mob = 
            let n = 
                [ Opening, 
                  { ExpectedAttacksAvailable = 3
                    Amplitude = 50
                    BezControlPct = 
                        { X = 0.2
                          Y = 0.8 } }
                  Endgame, 
                  { ExpectedAttacksAvailable = 4
                    Amplitude = 80
                    BezControlPct = 
                        { X = 0.2
                          Y = 0.8 } } ]
                |> dict
            
            let b = 
                [ Opening, 
                  { ExpectedAttacksAvailable = 4
                    Amplitude = 44
                    BezControlPct = 
                        { X = 0.2
                          Y = 0.8 } }
                  Endgame, 
                  { ExpectedAttacksAvailable = 7
                    Amplitude = 52
                    BezControlPct = 
                        { X = 0.2
                          Y = 0.8 } } ]
                |> dict
            
            let r = 
                [ Opening, 
                  { ExpectedAttacksAvailable = 5
                    Amplitude = 50
                    BezControlPct = 
                        { X = 0.2
                          Y = 0.8 } }
                  Endgame, 
                  { ExpectedAttacksAvailable = 8
                    Amplitude = 90
                    BezControlPct = 
                        { X = 0.2
                          Y = 0.7 } } ]
                |> dict
            
            let q = 
                [ Opening, 
                  { ExpectedAttacksAvailable = 8
                    Amplitude = 30
                    BezControlPct = 
                        { X = 0.2
                          Y = 0.8 } }
                  Endgame, 
                  { ExpectedAttacksAvailable = 12
                    Amplitude = 120
                    BezControlPct = 
                        { X = 0.2
                          Y = 0.8 } } ]
                |> dict
            
            let mb = 
                [ PieceType.Knight, n
                  PieceType.Bishop, b
                  PieceType.Rook, r
                  PieceType.Queen, q ]
                |> dict
            
            { MbDict = mb
              RookFileOpen = 20 }
        
        let kAttack = 
            { KingAttackCountValue = 7
              KingAttackWeightValue = 6
              KingAttackWeightCutoff = 5
              KingRingAttack = 4
              KingRingAttackControlBonus = 25
              KingAttackFactor = 0.53
              KingAttackFactorQueenTropismBonus = 2.15 }
        
        let pp8thRankScore = 407
        let ppRankReduction = 0.54
        let ppMinScore = 0
        let ppDangerPct = 0.18
        let ppOpeningPct = 0.20
        let pCandidatePct = 0.35
        let ppClosePct = 0.90
        let ppFarPct = 1.15
        let pShelterFactor = 6
        { PcSqTables = pcSqTabs
          MaterialValues = matVals
          PawnDoubled = pDoubled
          PawnIsolated = pIsolated
          PawnUnconnected = pUnconnected
          Mobility = mob
          KingAttack = kAttack
          PawnPassed8thRankScore = pp8thRankScore
          PawnPassedRankReduction = ppRankReduction
          PawnPassedMinScore = ppMinScore
          PawnPassedDangerPct = ppDangerPct
          PawnPassedOpeningPct = ppOpeningPct
          PawnCandidatePct = pCandidatePct
          PawnPassedClosePct = ppClosePct
          PawnPassedFarPct = ppFarPct
          PawnShelterFactor = pShelterFactor }
