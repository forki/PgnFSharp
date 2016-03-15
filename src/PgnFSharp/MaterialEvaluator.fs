namespace Lizard.Engine

type SidePiecCount = 
    { P : int
      N : int
      B : int
      R : int
      Q : int }

type MaterialEval = 
    { Msettings : MtSetDict
      Mhash : MaterialRes option []
      MpawnValuesOpening : int []
      MpawnValuesEndGame : int [] }

module MaterialEvaluator = 
    let CreateSpc(p, n, b, r, q) = 
        { P = p
          N = n
          B = b
          R = r
          Q = q }
    
    let Create(settings : MtSetDict) = 
        { Msettings = settings
          Mhash = Array.zeroCreate 64
          MpawnValuesOpening = Settings.GetPnVals(settings.[Opening].Pawn)
          MpawnValuesEndGame = Settings.GetPnVals(settings.[Endgame].Pawn) }
    
    let DoShelter(mat : MaterialRes) = (mat.StartWeight / 256.0) > 0.4
    
    let CalcWeight startWeight startScore endScore = 
        let startWeight = min 1.0 (max 0.0 startWeight)
        (startScore * startWeight) + (endScore * (1.0 - startWeight))
    
    let MyMaterial (totalPct, pawnPct, minorPct) my his (mteval : MaterialEval) = 
        let myMinor : int = my.N + my.B
        let hisMinor : int = his.N + his.B
        let minorDiff : int = myMinor - hisMinor
        let pawnDiff : int = my.P - his.P
        let rookDiff : int = my.R - his.R
        let queenDiff : int = my.Q - his.Q
        
        let pawnsOpening = 
            mteval.MpawnValuesOpening
            |> Seq.take my.P
            |> Seq.sum
        
        let pawnsEndgame = 
            mteval.MpawnValuesEndGame
            |> Seq.take my.P
            |> Seq.sum
        
        let knightVal = 
            CalcWeight pawnPct (float (mteval.Msettings.[Opening].Knight.BaseValue)) 
                (float (mteval.Msettings.[Endgame].Knight.BaseValue))
        let bishopVal = 
            CalcWeight pawnPct (float (mteval.Msettings.[Opening].Bishop.BaseValue)) 
                (float (mteval.Msettings.[Endgame].Bishop.BaseValue))
        let rookVal = 
            CalcWeight pawnPct (float (mteval.Msettings.[Opening].Rook.BaseValue)) 
                (float (mteval.Msettings.[Endgame].Rook.BaseValue))
        let queenVal = 
            CalcWeight minorPct (float (mteval.Msettings.[Opening].Queen.BaseValue)) 
                (float (mteval.Msettings.[Endgame].Queen.BaseValue))
        let bishopPairValue = 
            CalcWeight pawnPct (float (mteval.Msettings.[Opening].Bishop.PairBonus)) 
                (float (mteval.Msettings.[Endgame].Bishop.PairBonus))
        let rv = 
            CalcWeight totalPct (float (pawnsOpening)) (float (pawnsEndgame)) + (float (my.N) * knightVal) 
            + (float (my.B) * bishopVal) + (float (my.R) * rookVal) + (float (my.Q) * queenVal)
        
        let rv = 
            if minorDiff >= 1 && rookDiff < 0 && pawnDiff < 0 then 
                let rookMinorExchange = 
                    CalcWeight pawnPct (float (mteval.Msettings.[Opening].RookMinorExchange)) 
                        (float (mteval.Msettings.[Endgame].RookMinorExchange))
                rv + rookMinorExchange
            elif minorDiff > 0 && pawnDiff <= -2 then 
                let pawnMinorExchange = 
                    CalcWeight pawnPct (float (mteval.Msettings.[Opening].PawnMinorExchange)) 
                        (float (mteval.Msettings.[Endgame].PawnMinorExchange))
                rv + pawnMinorExchange
            else rv
        
        let rv = 
            if rookDiff >= 2 && queenDiff < 0 then 
                let queenRookExchange = 
                    CalcWeight pawnPct (float (mteval.Msettings.[Opening].QueenRookExchange)) 
                        (float (mteval.Msettings.[Endgame].QueenRookExchange))
                rv + queenRookExchange
            else rv
        
        let rv = 
            if my.N > 1 then 
                rv 
                + (CalcWeight pawnPct (float (mteval.Msettings.[Opening].Knight.PairBonus)) 
                       (float (mteval.Msettings.[Endgame].Knight.PairBonus)))
            else rv
        
        let rv = 
            if my.B > 1 then 
                rv 
                + (CalcWeight pawnPct (float (mteval.Msettings.[Opening].Bishop.PairBonus)) 
                       (float (mteval.Msettings.[Endgame].Bishop.PairBonus)))
            else rv
        
        let rv = 
            if my.R > 1 then 
                rv 
                + (CalcWeight pawnPct (float (mteval.Msettings.[Opening].Rook.PairBonus)) 
                       (float (mteval.Msettings.[Endgame].Rook.PairBonus)))
            else rv
        
        if my.Q > 1 then 
            rv 
            + (CalcWeight minorPct (float (mteval.Msettings.[Opening].Queen.PairBonus)) 
                   (float (mteval.Msettings.[Endgame].Queen.PairBonus)))
        else rv
    
    let ScaleFactorPlayer my his = 
        if my.P = 0 then 
            let myMaj = my.Q * 2 + my.R
            let myMin = my.B + my.N
            let myTot = myMaj * 2 + myMin
            let hisMaj = his.Q * 2 + his.R
            let hisMin = his.B + his.N
            let hisTot = hisMaj * 2 + hisMin
            if myTot = 1 then 0.0
            elif myTot = 2 && my.N = 2 then 
                if hisTot <> 0 || his.P = 0 then 0.0
                else 0.1
            else if myTot = 2 && my.B = 2 && hisTot = 1 && his.N = 1 then 0.5
            elif myTot - hisTot <= 1 && myMaj <= 2 then 0.15
            else 1.0
        elif my.P = 1 then 
            let w_maj = my.Q * 2 + my.R
            let w_min = my.B + my.N
            let w_tot = w_maj * 2 + w_min
            let b_maj = his.Q * 2 + his.R
            let b_min = his.B + his.N
            let b_tot = b_maj * 2 + b_min
            if b_min = 0 && his.R = 0 then 1.0
            elif b_min <> 0 then 
                let b_tot = b_tot - 1
                if w_tot = 1 then 0.25
                elif w_tot = 2 && my.N = 2 then 0.25
                elif w_tot - b_tot <= 1 && w_maj <= 2 then 0.5
                else 1.0
            else 
                let b_tot = b_tot - 2
                if w_tot = 1 then 0.25
                elif w_tot = 2 && my.N = 2 then 0.25
                elif w_tot - b_tot <= 1 && w_maj <= 2 then 0.5
                else 1.0
        else 1.0
    
    let CalcStartWeight w b = 
        let basicMaterialCount = 
            (w.N * 3) + (b.N * 3) + (w.B * 3) + (b.B * 3) + (w.R * 5) + (b.R * 5) + (w.Q * 9) + (b.Q * 9)
        if basicMaterialCount >= 56 then 256.0
        elif basicMaterialCount <= 10 then 0.0
        else 
            let rem = basicMaterialCount - 10
            let retval = float (rem) / 46.0
            256.0 * retval
    
    let EvalMaterial zob w b (mteval : MaterialEval) = 
        let startWeight = CalcStartWeight w b
        let pctPawns = float (w.P + b.P) / 16.0
        let pctMinors = float (w.N + b.N + w.B + b.B) / 8.0
        let totalPct = (startWeight / 256.0 + pctPawns) / 2.0
        let white = MyMaterial (totalPct, pctPawns, pctMinors) w b mteval
        let black = MyMaterial (totalPct, pctPawns, pctMinors) b w mteval
        let score = white - black
        let scaleWhite = 256.0 * (ScaleFactorPlayer w b)
        let scaleBlack = 256.0 * (ScaleFactorPlayer b w)
        EvalResults.CreateRes zob startWeight (int (score)) scaleWhite scaleBlack
    
    let EvalMaterialHash (board : Brd) (mteval : MaterialEval) = 
        let oidx : int64 = (board.ZobMaterial) % int64 (mteval.Mhash.GetUpperBound(0))
        
        let idx = 
            if oidx < 0L then -oidx
            else oidx
        
        let rv = mteval.Mhash.[int (idx)]
        if rv.IsSome && rv.Value.ZobristMaterial = board.ZobMaterial then rv
        else 
            let rv = 
                let w = 
                    CreateSpc
                        (board.WtPcCnt.[int (PieceType.Pawn)], 
                         board.WtPcCnt.[int (PieceType.Knight)], 
                         board.WtPcCnt.[int (PieceType.Bishop)], 
                         board.WtPcCnt.[int (PieceType.Rook)], 
                         board.WtPcCnt.[int (PieceType.Queen)])
                let b = 
                    CreateSpc
                        (board.BkPcCnt.[int (PieceType.Pawn)], 
                         board.BkPcCnt.[int (PieceType.Knight)], 
                         board.BkPcCnt.[int (PieceType.Bishop)], 
                         board.BkPcCnt.[int (PieceType.Rook)], 
                         board.BkPcCnt.[int (PieceType.Queen)])
                EvalMaterial board.ZobMaterial w b mteval
            mteval.Mhash.[int (idx)] <- Some(rv)//OK
            Some(rv)
