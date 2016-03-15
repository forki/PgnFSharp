namespace Lizard.Engine

type EvalRes = 
    { Workspace : int []
      Zobrist : int64
      DrawScore : int
      Material : int
      PcSq : PhsdScr
      Pawns : PhsdScr
      Mobility : PhsdScr
      KingAttack : int
      StageStartWeight : float
      ScaleWhite : float
      ScaleBlack : float
      PassedPawns : Bitboard
      CandidatePawns : Bitboard
      LazyAge : int
      PawnsPassed : PhsdScr
      ShelterStorm : PhsdScr }

type PlDt = 
    { AttacksWhite : AttInf
      AttacksBlack : AttInf
      ChecksWhite : ChkInf
      ChecksBlack : ChkInf
      EvalResults : EvalRes }

type PawnRes = 
    { PawnZobrist : int64
      WhitePawns : Bitboard
      BlackPawns : Bitboard
      Value : PhsdScr
      PassedPawns : Bitboard
      Doubled : Bitboard
      Isolated : Bitboard
      Unconnected : Bitboard
      Candidates : Bitboard
      PshelterCacheKey : int
      PshelterCacheValue : int }

type MaterialRes = 
    { ZobristMaterial : int64
      Score : int
      StartWeight : float
      ScaleWhite : float
      ScaleBlack : float }

module EvalResults = 
    let AttacksForPldt player pldt = 
        if player = Player.White then pldt.AttacksWhite
        else pldt.AttacksBlack
    
    let CreateRes zobristMaterial startWeight score scaleWhite scaleBlack = 
        { ZobristMaterial = zobristMaterial
          Score = score
          StartWeight = startWeight
          ScaleWhite = scaleWhite
          ScaleBlack = scaleBlack }
    
    let Create() = 
        { Workspace = (Array.create 64 0)
          Zobrist = 0L
          DrawScore = 0
          Material = 0
          PcSq = PhasedScore.Create 0 0
          Pawns = PhasedScore.Create 0 0
          Mobility = PhasedScore.Create 0 0
          KingAttack = 0
          StageStartWeight = 256.0
          ScaleWhite = 256.0
          ScaleBlack = 256.0
          PassedPawns = Bitboard.Empty
          CandidatePawns = Bitboard.Empty
          LazyAge = -1
          PawnsPassed = PhasedScore.Create 0 0
          ShelterStorm = PhasedScore.Create 0 0 }
    
    let CreatePldt() = 
        { AttacksWhite = AttackInfo.Create(Player.White)
          AttacksBlack = AttackInfo.Create(Player.Black)
          ChecksWhite = CheckInfo.CreatePlr(Player.White)
          ChecksBlack = CheckInfo.CreatePlr(Player.Black)
          EvalResults = Create() }
    
    let Reset(er : EvalRes) = 
        { er with Material = 0
                  PcSq = PhasedScore.Create 0 0
                  Pawns = PhasedScore.Create 0 0
                  PawnsPassed = PhasedScore.Create 0 0
                  Mobility = PhasedScore.Create 0 0
                  KingAttack = 0
                  ShelterStorm = PhasedScore.Create 0 0
                  StageStartWeight = 256.0
                  ScaleWhite = 256.0
                  ScaleBlack = 256.0
                  DrawScore = 0
                  PassedPawns = Bitboard.Empty
                  CandidatePawns = Bitboard.Empty
                  LazyAge = -1 }
    
    let MaterialPawnsApply (board : Brd) (material : MaterialRes) (pawns : PawnRes) drawScore (er : EvalRes) = 
        { er with Zobrist = board.ZobristBoard
                  DrawScore = drawScore
                  PcSq = board.PcSq
                  Material = material.Score
                  StageStartWeight = material.StartWeight
                  ScaleWhite = material.ScaleWhite
                  ScaleBlack = material.ScaleBlack
                  Pawns = pawns.Value
                  PassedPawns = pawns.PassedPawns
                  CandidatePawns = pawns.Candidates }
    
    let ApplyPreviousEval (prev : EvalRes) (er : EvalRes) = 
        { er with LazyAge = prev.LazyAge + 1
                  Mobility = prev.Mobility
                  KingAttack = prev.KingAttack
                  PawnsPassed = prev.PawnsPassed
                  ShelterStorm = prev.ShelterStorm }
    
    let PawnsPassedPhased(er : EvalRes) = er.PawnsPassed |> PhasedScore.ApplyScaleFactor(er.StageStartWeight)
    let PawnsPhased(er : EvalRes) = er.Pawns |> PhasedScore.ApplyScaleFactor(er.StageStartWeight)
    let MobilityPhased(er : EvalRes) = er.Mobility |> PhasedScore.ApplyScaleFactor(er.StageStartWeight)
    let PcSqPhased(er : EvalRes) = er.PcSq |> PhasedScore.ApplyScaleFactor(er.StageStartWeight)
    
    let Score(er : EvalRes) = 
        let nonScaled = 
            ((er.PcSq + er.Pawns + er.PawnsPassed + er.ShelterStorm + er.Mobility) 
             |> PhasedScore.ApplyScaleFactor(er.StageStartWeight)) + er.Material + er.KingAttack
        if nonScaled > er.DrawScore && er.ScaleWhite < 256.0 then 
            (((nonScaled - er.DrawScore) * int (er.ScaleWhite)) >>> 8) + er.DrawScore
        elif nonScaled < er.DrawScore && er.ScaleBlack < 256.0 then 
            (((nonScaled - er.DrawScore) * int (er.ScaleBlack)) >>> 8) + er.DrawScore
        else nonScaled
    
    let PositionalScore(er : EvalRes) = (er |> Score) - er.Material
    
    let LazyLow(er : EvalRes) = 
        let margin = er.LazyAge * 50
        (er |> Score) - margin
    
    let LazyHigh(er : EvalRes) = 
        let margin = er.LazyAge * 50
        (er |> Score) + margin
