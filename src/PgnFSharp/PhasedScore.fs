namespace Lizard.Engine

type PhsdScr = int64

module PhasedScore = 
    let Create opening endgame : PhsdScr = (int64 (opening) <<< 32) + int64 (endgame)
    let Opening(phasedScore : PhsdScr) = int (((int64 (phasedScore) + 0x80000000L) &&& ~~~0xffffffffL) / 0x100000000L)
    let Endgame(phasedScore : PhsdScr) = int (int64 (phasedScore) &&& 0xffffffffL)
    
    let ApplyScaleFactor (startWeight : float) (phasedScore : PhsdScr) = 
        let endWeight = 256 - int (startWeight)
        let op = ((phasedScore |> Opening) * int (startWeight)) >>> 8
        let nd = ((phasedScore |> Endgame) * int (endWeight)) >>> 8
        op + nd
    
    let Combine openingScores endgameScores = Seq.map2 Create openingScores endgameScores
