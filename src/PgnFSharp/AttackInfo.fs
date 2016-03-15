namespace Lizard.Engine

type AttInf = 
    { Azobrist : int64
      Aplayer : Player
      Apawn : Bitboard
      AlessThan : Bitboard []
      Acount1 : Bitboard
      Acount2 : Bitboard
      Acount3 : Bitboard
      Acount4 : Bitboard
      BySliderBishop : Bitboard
      BySliderRook : Bitboard }

module AttackInfo = 
    let Create player = 
        { Azobrist = 0L
          Aplayer = player
          Apawn = Bitboard.Empty
          AlessThan = Array.create PieceType.LookupArrayLength Bitboard.Empty
          Acount1 = Bitboard.Empty
          Acount2 = Bitboard.Empty
          Acount3 = Bitboard.Empty
          Acount4 = Bitboard.Empty
          BySliderBishop = Bitboard.Empty
          BySliderRook = Bitboard.Empty }
    
    let Initialize (board : Brd) (attcknf : AttInf) = 
        if attcknf.Azobrist <> board.ZobristBoard then 
            let allPieces = board.PieceLocationsAll
            let myPieces = if attcknf.Aplayer=Player.White then board.WtPrBds else board.BkPrBds
            let myBishopSliders = myPieces &&& (board|>Board.BishopSliders)
            let myRookSliders = myPieces &&& (board|>Board.RookSliders)
            
            //knights
            let rec getCnts bbpc bbpas (ct1, ct2, ct3, ct4) = 
                if bbpc = Bitboard.Empty then bbpas, ct1, ct2, ct3, ct4
                else 
                    let pos,nbbpc = Bitboard.PopFirst(bbpc)
                    let pattacks = Attacks.KnightAttacks(pos)
                    let nbbpas = bbpas ||| pattacks
                    let nct4 = ct4 ||| (ct3 &&& pattacks)
                    let nct3 = ct3 ||| (ct2 &&& pattacks)
                    let nct2 = ct2 ||| (ct1 &&& pattacks)
                    let nct1 = ct1 ||| pattacks
                    getCnts nbbpc nbbpas (nct1, nct2, nct3, nct4)
            
            let bbPiece = myPieces &&& board.PieceTypes.[int(PieceType.Knight)]
            let nPieceAttacks, count1, count2, count3, count4 = 
                getCnts bbPiece Bitboard.Empty (Bitboard.Empty, Bitboard.Empty, Bitboard.Empty, Bitboard.Empty)
            //bishops
            let rec getCnts bbpc bbpas (ct1, ct2, ct3, ct4) = 
                if bbpc = Bitboard.Empty then bbpas, ct1, ct2, ct3, ct4
                else 
                    let pos,nbbpc = Bitboard.PopFirst(bbpc)
                    let pattacks = Attacks.BishopAttacks pos allPieces
                    let nbbpas = bbpas ||| pattacks
                    let pattacks = Attacks.BishopAttacks pos (allPieces ^^^ myBishopSliders)
                    let nct4 = ct4 ||| (ct3 &&& pattacks)
                    let nct3 = ct3 ||| (ct2 &&& pattacks)
                    let nct2 = ct2 ||| (ct1 &&& pattacks)
                    let nct1 = ct1 ||| pattacks
                    getCnts nbbpc nbbpas (nct1, nct2, nct3, nct4)
            
            let bbPiece = myPieces &&& board.PieceTypes.[int(PieceType.Bishop)]
            let bPieceAttacks, count1, count2, count3, count4 = 
                getCnts bbPiece Bitboard.Empty (count1, count2, count3, count4)
            //rooks
            let rec getCnts bbpc bbpas (ct1, ct2, ct3, ct4) = 
                if bbpc = Bitboard.Empty then bbpas, ct1, ct2, ct3, ct4
                else 
                    let pos,nbbpc = Bitboard.PopFirst(bbpc)
                    let pattacks = Attacks.RookAttacks pos allPieces
                    let nbbpas = bbpas ||| pattacks
                    let pattacks = Attacks.RookAttacks pos (allPieces ^^^ myRookSliders)
                    let nct4 = ct4 ||| (ct3 &&& pattacks)
                    let nct3 = ct3 ||| (ct2 &&& pattacks)
                    let nct2 = ct2 ||| (ct1 &&& pattacks)
                    let nct1 = ct1 ||| pattacks
                    getCnts nbbpc nbbpas (nct1, nct2, nct3, nct4)
            
            let bbPiece = myPieces &&& board.PieceTypes.[int(PieceType.Rook)]
            let rPieceAttacks, count1, count2, count3, count4 = 
                getCnts bbPiece Bitboard.Empty (count1, count2, count3, count4)
            //queen
            let rec getCnts bbpc bbpas (ct1, ct2, ct3, ct4) = 
                if bbpc = Bitboard.Empty then bbpas, ct1, ct2, ct3, ct4
                else 
                    let pos,nbbpc = Bitboard.PopFirst(bbpc)
                    let pattacks = Attacks.QueenAttacks pos allPieces
                    let nbbpas = bbpas ||| pattacks
                    let pattacks = 
                        (Attacks.BishopAttacks pos (allPieces ^^^ myBishopSliders))
                        ||| (Attacks.RookAttacks pos (allPieces ^^^ myRookSliders))
                    let nct4 = ct4 ||| (ct3 &&& pattacks)
                    let nct3 = ct3 ||| (ct2 &&& pattacks)
                    let nct2 = ct2 ||| (ct1 &&& pattacks)
                    let nct1 = ct1 ||| pattacks
                    getCnts nbbpc nbbpas (nct1, nct2, nct3, nct4)
            
            let bbPiece = myPieces &&& board.PieceTypes.[int(PieceType.Queen)]
            let qPieceAttacks, count1, count2, count3, count4 = 
                getCnts bbPiece Bitboard.Empty (count1, count2, count3, count4)
            //add king attacks
            let kattacks = Attacks.KingAttacks(if attcknf.Aplayer=Player.White then board.WtKingPos else board.BkKingPos)
            let count4 = count4 ||| (count3 &&& kattacks)
            let count3 = count3 ||| (count2 &&& kattacks)
            let count2 = count2 ||| (count1 &&& kattacks)
            let count1 = count1 ||| kattacks
            //add pawn attacks
            let pPieceAttacks = Bitboard.Empty
            let bbPiece = myPieces &&& board.PieceTypes.[int(PieceType.Pawn)]
            
            let bbPiece = 
                if attcknf.Aplayer = Player.White then bbPiece|>Bitboard.ShiftDirN
                else bbPiece|>Bitboard.ShiftDirS
            
            //pawn attacks west;
            let pattacks = bbPiece|>Bitboard.ShiftDirW
            let pPieceAttacks = pPieceAttacks ||| pattacks
            let count4 = count4 ||| (count3 &&& pattacks)
            let count3 = count3 ||| (count2 &&& pattacks)
            let count2 = count2 ||| (count1 &&& pattacks)
            let count1 = count1 ||| pattacks
            //pawn attacks east;
            let pattacks = bbPiece|>Bitboard.ShiftDirE
            let pPieceAttacks = pPieceAttacks ||| pattacks
            let count4 = count4 ||| (count3 &&& pattacks)
            let count3 = count3 ||| (count2 &&& pattacks)
            let count2 = count2 ||| (count1 &&& pattacks)
            let count1 = count1 ||| pattacks
            //setup less than attack arrays
            let rookL = pPieceAttacks ||| nPieceAttacks ||| bPieceAttacks
            let queenL = rookL ||| rPieceAttacks
            let al = [|Bitboard.Empty;Bitboard.Empty;pPieceAttacks;pPieceAttacks;rookL;queenL;count1|]
            let bySliderBishop = 
                bPieceAttacks ||| qPieceAttacks
            let bySliderRook = 
                rPieceAttacks ||| qPieceAttacks
            { attcknf with Azobrist = board.ZobristBoard
                           BySliderBishop = bySliderBishop
                           BySliderRook = bySliderRook
                           Apawn = pPieceAttacks
                           AlessThan = al
                           Acount1 = count1
                           Acount2 = count2
                           Acount3 = count3
                           Acount4 = count4 }
        else attcknf
    
    let AttackCountTo (pos : Position) (attcknf : AttInf) = 
        let bb = pos|>Position.ToBitboard
        int (uint64 (attcknf.Acount1 &&& bb >>> int (pos))) 
        + int (uint64 (attcknf.Acount2 &&& bb >>> int (pos))) 
        + int (uint64 (attcknf.Acount3 &&& bb >>> int (pos))) 
        + int (uint64 (attcknf.Acount4 &&& bb >>> int (pos)))
    
    let IsInitialized (board : Brd) (attcknf : AttInf) = attcknf.Azobrist = board.ZobristBoard
