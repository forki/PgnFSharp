namespace Lizard.Engine

open System

module Zobrist = 
    let private rand64(rand : Random) = 
        let bytes : byte [] = Array.zeroCreate 8
        rand.NextBytes(bytes)
        [| 0..7 |]
        |> Array.map (fun i -> int64 (bytes.[i]) <<< (i * 8))
        |> Array.reduce (|||)
    
    let private rand = new Random(12345)
    let PlayerKey = rand64(rand)
    let CastleBL = rand64(rand)
    let CastleBS = rand64(rand)
    let CastleWL = rand64(rand)
    let CastleWS = rand64(rand)
    
    let private enpassant = 
        [| for i in 0..63 -> rand64(rand) |]
    
    let private piecepos = 
        [| for i in 0..Piece.LookupArrayLength - 1 -> 
               [| for j in 0..63 -> rand64(rand) |] |]
    
    let Enpassant(pos : Position) = enpassant.[int (pos)]
    let PiecePosition (piece : Piece) (pos : Position) = piecepos.[int (piece)].[int (pos)]
    let Material (piece : Piece) (pieceCountBesidesThis : int) = piecepos.[int (piece)].[pieceCountBesidesThis]
