namespace Lizard.Engine

type MobilityEval = 
    { RookFileOpen : PhsdScr
      RookFileHalfOpen : PhsdScr
      MobilityPieceTypeCount : PhsdScr [] [] }

module MobilityEvaluator = 
    let GetMob (maxMobility : int) mb = 
        let start = 
            { X = 0.0
              Y = 0.0 }
        
        let nd = 
            { X = float (maxMobility)
              Y = float (mb.Amplitude) }
        
        let control = 
            { X = float (maxMobility) * mb.BezControlPct.X
              Y = float (mb.Amplitude) * mb.BezControlPct.Y }
        
        let curve = 
            { Start = start
              End = nd
              Gravity = control }
        
        let xCoordsFloat = [| 0..(maxMobility + 1) |] |> Array.map (fun i -> float (i))
        
        let yCoords = 
            QuadraticBezier.Select xCoordsFloat curve
            |> Seq.toArray
            |> Array.map (fun p -> int (p.Y))
        
        let offset = yCoords.[mb.ExpectedAttacksAvailable]
        let rv = yCoords |> Array.map (fun y -> y - offset)
        rv
    
    let Create(settings : MbSet) = 
        let mbtc = Array.zeroCreate PieceType.LookupArrayLength
        let rfo = PhasedScore.Create settings.RookFileOpen (settings.RookFileOpen / 2)
        let rfho = PhasedScore.Create (settings.RookFileOpen / 2) (settings.RookFileOpen / 4)
        for pieceType in [| PieceType.Knight; PieceType.Bishop; PieceType.Rook; PieceType.Queen |] do
            let pieceSettings = settings.MbDict.[pieceType]
            let openingVals = pieceSettings.[Opening] |> GetMob(pieceType |> PieceType.MaximumMoves)
            let endgameVals = pieceSettings.[Endgame] |> GetMob(pieceType |> PieceType.MaximumMoves)
            let combined = (PhasedScore.Combine openingVals endgameVals) |> Seq.toArray
            mbtc.[int (pieceType)] <- combined //OK
        { RookFileOpen = rfo
          RookFileHalfOpen = rfho
          MobilityPieceTypeCount = mbtc }
    
