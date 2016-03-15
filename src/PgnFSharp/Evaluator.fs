namespace Lizard.Engine

type Eval = 
    { EevalPawns : PawnEval
      EevalMaterial : MaterialEval
      EevalPcSq : PcsqPcPs
      EevalKing : KingAttackEval
      EevalMobility : MobilityEval
      Esettings : Settngs
      DrawScore : int }

module Evaluator = 
    let Create(esettings : Settngs) = 
        { EevalPawns = PawnEvaluator.Create esettings 10000
          EevalMaterial = MaterialEvaluator.Create(esettings.MaterialValues)
          EevalPcSq = PcSqEvaluator.Create(esettings)
          EevalKing = KingAttackEvaluator.Create(esettings.KingAttack)
          EevalMobility = MobilityEvaluator.Create(esettings.Mobility)
          Esettings = esettings
          DrawScore = 0 }
    
    let CreateDef() = Create(Settings.Default())
    let Default = CreateDef()
    let PcSqDef = Default.EevalPcSq
