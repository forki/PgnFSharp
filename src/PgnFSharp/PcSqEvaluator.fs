namespace Lizard.Engine

type PcsqPcPs = PhsdScr [] []

module PcSqEvaluator = 
    let Emp() : PcsqPcPs = Array.zeroCreate Piece.LookupArrayLength
    
    let Create(settings : Settngs) = 
        let actionNormalizePcSq (data : SqDict) = 
            let sum = 
                Position.AllPositions
                |> Array.map (fun p -> Settings.GetSd p data)
                |> Array.sum
            
            let per = sum / 64
            data.Offset <- data.Offset - per //OK
        for pieceType in PieceType.AllPieceTypes do
            actionNormalizePcSq (settings.PcSqTables.[pieceType].[GmStg.Opening])
            actionNormalizePcSq (settings.PcSqTables.[pieceType].[GmStg.Endgame])
        let getp pc sq = 
            if pc = Piece.EMPTY then PhasedScore.Create 0 0
            elif (pc |> Piece.PieceToPlayer) = Player.White then 
                PhasedScore.Create (settings.PcSqTables.[pc |> Piece.ToPieceType].[GmStg.Opening] |> Settings.GetSd sq) 
                    (settings.PcSqTables.[pc |> Piece.ToPieceType].[GmStg.Endgame] |> Settings.GetSd sq)
            else 
                PhasedScore.Create 
                    (-(settings.PcSqTables.[pc |> Piece.ToPieceType].[GmStg.Opening] 
                       |> Settings.GetSd(sq |> Position.Reverse))) 
                    (-(settings.PcSqTables.[pc |> Piece.ToPieceType].[GmStg.Endgame] 
                       |> Settings.GetSd(sq |> Position.Reverse)))
        
        let getps pc = Position.AllPositions |> Array.map (getp pc)
        Piece.AllPieces2 |> Array.map getps
    
    let PcSqValuesAdd (piece : Piece) (pos : Position) (pPP : PcsqPcPs) (vlue : PhsdScr) = 
        vlue + (pPP.[int (piece)].[int (pos)])
    let PcSqValuesRemove (piece : Piece) (pos : Position) (pPP : PcsqPcPs) (vlue : PhsdScr) = 
        vlue - (pPP.[int (piece)].[int (pos)])
