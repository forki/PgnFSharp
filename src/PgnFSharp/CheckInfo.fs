namespace Lizard.Engine

type ChkInf = 
    { Zobrist : int64
      Player : Player
      KingPosition : Position
      PawnDirect : Bitboard
      KnightDirect : Bitboard
      BishopDirect : Bitboard
      RookDirect : Bitboard
      DirectAll : Bitboard
      PinnedOrDiscovered : Bitboard
      Checkers : Bitboard }

module CheckInfo = 
    let Create() = 
        { Zobrist = 0L
          Player = Player.White
          KingPosition = Position.OUTOFBOUNDS
          PawnDirect = Bitboard.Empty
          KnightDirect = Bitboard.Empty
          BishopDirect = Bitboard.Empty
          RookDirect = Bitboard.Empty
          DirectAll = Bitboard.Empty
          PinnedOrDiscovered = Bitboard.Empty
          Checkers = Bitboard.Empty }
    
    let CreatePlr plr = 
        let ans = Create()
        { ans with Player = plr }
    
    let IsCheck(chknf : ChkInf) = chknf.Checkers <> Bitboard.Empty
    
    let Initialize (board : Brd) (chknf : ChkInf) = 
        if chknf.Zobrist <> board.ZobristBoard then 
            let zobrist = board.ZobristBoard
            let kingPos = if chknf.Player=Player.White then board.WtKingPos else board.BkKingPos
            let all = board.PieceLocationsAll
            let them = if (Player.PlayerOther(chknf.Player))=Player.White then board.WtPrBds else board.BkPrBds
            let pDirect = Attacks.PawnAttacks kingPos chknf.Player
            let nDirect = Attacks.KnightAttacks(kingPos)
            let bDirect = Attacks.BishopAttacks kingPos all
            let rDirect = Attacks.RookAttacks kingPos all
            let dAll = pDirect ||| nDirect ||| bDirect ||| rDirect
            let checkers = Bitboard.Empty
            let checkers = checkers ||| (pDirect &&& them &&& board.PieceTypes.[int(PieceType.Pawn)])
            let checkers = checkers ||| (nDirect &&& them &&& board.PieceTypes.[int(PieceType.Knight)])
            let checkers = checkers ||| (bDirect &&& them &&& (board|>Board.BishopSliders))
            let checkers = checkers ||| (rDirect &&& them &&& (board|>Board.RookSliders))
            let chkrs = checkers
            let theirRookSliders = them &&& (board|>Board.RookSliders)
            let theirBishopSliders = them &&& (board|>Board.BishopSliders)
            
            let rec getPinDisc xray pindisc = 
                if xray = Bitboard.Empty then pindisc
                else 
                    let xrayAttacker,nxray = Bitboard.PopFirst(xray)
                    let between = (xrayAttacker|>Position.Between(kingPos)) &&& all
                    if between <> Bitboard.Empty then 
                        let blocker,nbetween = Bitboard.PopFirst(between)
                        if nbetween = Bitboard.Empty then 
                            let npindisc = pindisc ||| (blocker|>Position.ToBitboard)
                            getPinDisc nxray npindisc
                        else getPinDisc nxray pindisc
                    else getPinDisc nxray pindisc
            
            let xray = (Attacks.RookAttacks kingPos theirRookSliders) &&& theirRookSliders
            let xray = xray ||| ((Attacks.BishopAttacks kingPos theirBishopSliders) &&& theirBishopSliders)
            let pDiscovered = getPinDisc xray Bitboard.Empty
            { chknf with Zobrist = zobrist
                         KingPosition = kingPos
                         PawnDirect = pDirect
                         KnightDirect = nDirect
                         BishopDirect = bDirect
                         RookDirect = rDirect
                         DirectAll = dAll
                         PinnedOrDiscovered = pDiscovered
                         Checkers = chkrs }
        else chknf
