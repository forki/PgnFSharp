namespace PgnFSharp

module Direction = 
    let AllDirectionsKnight = 
        [| Direction.DirNNE; Direction.DirEEN; Direction.DirEES; Direction.DirSSE; Direction.DirSSW; Direction.DirWWS; 
           Direction.DirWWN; Direction.DirNNW |]
    let AllDirectionsRook = [| Direction.DirN; Direction.DirE; Direction.DirS; Direction.DirW |]
    let AllDirectionsBishop = [| Direction.DirNE; Direction.DirSE; Direction.DirSW; Direction.DirNW |]
    let AllDirectionsQueen = 
        [| Direction.DirN; Direction.DirE; Direction.DirS; Direction.DirW; Direction.DirNE; Direction.DirSE; 
           Direction.DirSW; Direction.DirNW |]
    let Opposite(dir : Direction) = -int (dir) |> Dirn
