namespace Lizard.Engine

open System.Text
open System.Text.RegularExpressions

module MoveUtil = 
    let ParseFilter (board : Brd) attackto piece file rank = 
        let rec getfits posl fitl = 
            if List.isEmpty posl then fitl
            else 
                let pos:Position = posl.Head
                if piece <> Piece.EMPTY && piece <> board.PieceAt.[int(pos)] then getfits posl.Tail fitl
                elif rank <> Rank.EMPTY && rank <> (pos|>Position.ToRank) then getfits posl.Tail fitl
                elif file <> File.EMPTY && file <> (pos|>Position.ToFile) then getfits posl.Tail fitl
                else getfits posl.Tail (pos :: fitl)
        
        let attacksTo = board|>Board.AttacksTo2 attackto board.WhosTurn
        let fits = getfits (attacksTo|>Bitboard.ToPositions) []
        if fits.Length <> 1 then 
            let rec getfits (mvl : Move list) fitl = 
                if List.isEmpty mvl then fitl
                else 
                    let mv = mvl.Head
                    if mv|>Board.To <> attackto then getfits mvl.Tail fitl
                    elif board.PieceAt.[int(mv|>Board.From)] <> piece then getfits mvl.Tail fitl
                    elif file <> File.EMPTY && ((mv|>Board.From)|>Position.ToFile) <> file then getfits mvl.Tail fitl
                    elif rank <> Rank.EMPTY && (mv|>Board.From|>Position.ToRank) <> rank then getfits mvl.Tail fitl
                    else getfits mvl.Tail ((mv|>Board.From) :: fitl)
            
            let fits = getfits (MoveGenerate.GenMovesLegal(board) |> Seq.toList) []
            if fits.Length = 1 then fits.Head
            else failwith "invalid move input"
        else fits.Head
    
    let Parse (board : Brd) (movetext : string) = 
        let promote = Piece.EMPTY
        let mFrom = (Position.OUTOFBOUNDS)
        let mTo = (Position.OUTOFBOUNDS)
        let regex = new Regex("")
        let movetext = movetext.Replace("+", "")
        let movetext = movetext.Replace("x", "")
        let movetext = movetext.Replace("#", "")
        let movetext = movetext.Replace("=", "")
        let me = board.WhosTurn
        
        let mypawn = 
            if board.WhosTurn = Player.White then Piece.WPawn
            else Piece.BPawn
        
        let myknight = 
            if board.WhosTurn = Player.White then Piece.WKnight
            else Piece.BKnight
        
        let mybishop = 
            if board.WhosTurn = Player.White then Piece.WBishop
            else Piece.BBishop
        
        let myrook = 
            if board.WhosTurn = Player.White then Piece.WRook
            else Piece.BRook
        
        let myqueen = 
            if board.WhosTurn = Player.White then Piece.WQueen
            else Piece.BQueen
        
        let myking = 
            if board.WhosTurn = Player.White then Piece.WKing
            else Piece.BKing
        
        let mynorth = 
            if board.WhosTurn = Player.White then Direction.DirN
            else Direction.DirS
        
        let mysouth = 
            if board.WhosTurn = Player.White then Direction.DirS
            else Direction.DirN
        
        let myrank4 = 
            if board.WhosTurn = Player.White then Rank.Rank4
            else Rank.Rank5
        
        if Regex.IsMatch(movetext, "^[abcdefgh][12345678][abcdefgh][12345678]$", RegexOptions.IgnoreCase) then 
            let mFrom = Position.Parse(movetext.Substring(0, 2))
            let mTo = Position.Parse(movetext.Substring(2, 2))
            MoveGenerate.Create mFrom mTo board.PieceAt.[int(mFrom)] board.PieceAt.[int(mTo)]
        elif Regex.IsMatch(movetext, "^[abcdefgh][12345678][abcdefgh][12345678][BNRQK]$", RegexOptions.IgnoreCase) then 
            let mFrom = Position.Parse(movetext.Substring(0, 2))
            let mTo = Position.Parse(movetext.Substring(2, 2))
            let promote = movetext.[4]|>Piece.ParseAsPiece(me)
            MoveGenerate.CreateProm mFrom mTo board.PieceAt.[int(mFrom)] board.PieceAt.[int(mTo)] (promote|>Piece.ToPieceType)
        elif movetext = "0-0" || movetext = "O-O" || movetext = "o-o" then 
            if me = Player.White then 
                let mFrom = Position.E1
                let mTo = Position.G1
                MoveGenerate.Create mFrom mTo board.PieceAt.[int(mFrom)] board.PieceAt.[int(mTo)]
            else 
                let mFrom = Position.E8
                let mTo = Position.G8
                MoveGenerate.Create mFrom mTo board.PieceAt.[int(mFrom)] board.PieceAt.[int(mTo)]
        elif movetext = "0-0-0" || movetext = "O-O-O" || movetext = "o-o-o" then 
            if me = Player.White then 
                let mFrom = Position.E1
                let mTo = Position.C1
                MoveGenerate.Create mFrom mTo board.PieceAt.[int(mFrom)] board.PieceAt.[int(mTo)]
            else 
                let mFrom = Position.E8
                let mTo = Position.C8
                MoveGenerate.Create mFrom mTo board.PieceAt.[int(mFrom)] board.PieceAt.[int(mTo)]
        elif Regex.IsMatch(movetext, "^[abcdefgh][12345678]$") then 
            let mTo = Position.Parse(movetext)
            let tmppos = mTo|>Position.PositionInDirection(mysouth)
            if board.PieceAt.[int(tmppos)] = mypawn then 
                let mFrom = tmppos
                MoveGenerate.Create mFrom mTo board.PieceAt.[int(mFrom)] board.PieceAt.[int(mTo)]
            elif board.PieceAt.[int(tmppos)] = Piece.EMPTY && (mTo|>Position.ToRank) = myrank4 then 
                let tmppos = tmppos|>Position.PositionInDirection(mysouth)
                if board.PieceAt.[int(tmppos)] = mypawn then 
                    let mFrom = tmppos
                    MoveGenerate.Create mFrom mTo board.PieceAt.[int(mFrom)] board.PieceAt.[int(mTo)]
                else failwith ("no pawn can move to " + movetext)
            else failwith ("no pawn can move to " + movetext)
        elif Regex.IsMatch(movetext, "^[abcdefgh][12345678][BNRQK]$") then 
            let mTo = Position.Parse(movetext.Substring(0, 2))
            let tmppos = mTo|>Position.PositionInDirection(mysouth)
            if board.PieceAt.[int(tmppos)] = mypawn then 
                let mFrom = tmppos
                let promote = movetext.[2]|>Piece.ParseAsPiece(me)
                MoveGenerate.CreateProm mFrom mTo board.PieceAt.[int(mFrom)] 
                                        board.PieceAt.[int(mTo)] (promote|>Piece.ToPieceType)
            else failwith ("no pawn can promoted to " + movetext.Substring(0, 2))
        elif Regex.IsMatch(movetext, "^[abcdefgh][abcdefgh][12345678]$") then 
            let mTo = Position.Parse(movetext.Substring(1, 2))
            let tmpfile = File.Parse(movetext.[0])
            let mFrom = ParseFilter board mTo mypawn tmpfile Rank.EMPTY
            MoveGenerate.Create mFrom mTo board.PieceAt.[int(mFrom)] board.PieceAt.[int(mTo)]
        elif Regex.IsMatch(movetext, "^[abcdefgh][abcdefgh][12345678][BNRQK]$") then 
            let mTo = Position.Parse(movetext.Substring(1, 2))
            let tmpfile = File.Parse(movetext.[0])
            let mFrom = ParseFilter board mTo mypawn tmpfile Rank.EMPTY
            let promote = movetext.[3]|>Piece.ParseAsPiece(me)
            MoveGenerate.CreateProm mFrom mTo board.PieceAt.[int(mFrom)] board.PieceAt.[int(mTo)] (promote|>Piece.ToPieceType)
        elif Regex.IsMatch(movetext, "^[BNRQK][abcdefgh][12345678]$") then 
            let mTo = Position.Parse(movetext.Substring(1, 2))
            let tmppiece = movetext.[0]|>Piece.ParseAsPiece(me)
            let mFrom = ParseFilter board mTo tmppiece File.EMPTY Rank.EMPTY
            MoveGenerate.Create mFrom mTo board.PieceAt.[int(mFrom)] board.PieceAt.[int(mTo)]
        elif Regex.IsMatch(movetext, "^[BNRQK][abcdefgh][abcdefgh][12345678]$") then 
            let mTo = Position.Parse(movetext.Substring(2, 2))
            let tmppiece = movetext.[0]|>Piece.ParseAsPiece(me)
            let tmpfile = File.Parse(movetext.[1])
            let mFrom = ParseFilter board mTo tmppiece tmpfile Rank.EMPTY
            MoveGenerate.Create mFrom mTo board.PieceAt.[int(mFrom)] board.PieceAt.[int(mTo)]
        elif Regex.IsMatch(movetext, "^[BNRQK][12345678][abcdefgh][12345678]$") then 
            let mTo = Position.Parse(movetext.Substring(2, 2))
            let tmppiece = movetext.[0]|>Piece.ParseAsPiece(me)
            let tmprank = Rank.Parse(movetext.[1])
            let mFrom = ParseFilter board mTo tmppiece File.EMPTY tmprank
            MoveGenerate.Create mFrom mTo board.PieceAt.[int(mFrom)] board.PieceAt.[int(mTo)]
        elif Regex.IsMatch(movetext, "^[BNRQK][abcdefgh][12345678][abcdefgh][12345678]$") then 
            let mTo = Position.Parse(movetext.Substring(3, 2))
            let tmppiece = movetext.[0]|>Piece.ParseAsPiece(me)
            let tmpfile = File.Parse(movetext.[1])
            let tmprank = Rank.Parse(movetext.[2])
            let mFrom = ParseFilter board mTo tmppiece tmpfile tmprank
            MoveGenerate.Create mFrom mTo board.PieceAt.[int(mFrom)] board.PieceAt.[int(mTo)]
        else failwith "invalid move format"
    
    let DescBd (move : Move) (board : Brd) = 
        let sb = new StringBuilder()
        let piece = board.PieceAt.[int(move|>Board.From)]
        let fromrank = move|>Board.From|>Position.ToRank
        let fromfile = move|>Board.From|>Position.ToFile
        let isprom = move|>Board.Promote <> Piece.EMPTY
        let sTo = move|>Board.To|>Position.Name
        let sPiece = (piece|>Piece.PieceToString).ToUpper()
        let sRank = (fromrank|>Rank.RankToString).ToLower()
        let sFile = (fromfile|>File.FileToString).ToLower()
        
        let iscap = 
            if (move|>Board.To = board.EnPassant && (piece = Piece.WPawn || piece = Piece.BPawn)) then true
            else board.PieceAt.[int(move|>Board.To)] <> Piece.EMPTY
        
        let sProm = 
            if isprom then ((move|>Board.Promote)|>Piece.PieceToString).ToUpper()
            else ""
        
        if piece = Piece.WPawn || piece = Piece.BPawn then 
            if iscap then sb.Append(sFile + "x") |> ignore
            sb.Append(sTo) |> ignore
            if isprom then sb.Append(sProm) |> ignore
        elif piece = Piece.WKing && (move|>Board.From) = Position.E1 && (move|>Board.To) = Position.G1 then 
            sb.Append("O-O") |> ignore
        elif piece = Piece.BKing && (move|>Board.From) = Position.E8 && (move|>Board.To) = Position.G8 then 
            sb.Append("O-O") |> ignore
        elif piece = Piece.WKing && (move|>Board.From) = Position.E1 && (move|>Board.To) = Position.C1 then 
            sb.Append("O-O-O") |> ignore
        elif piece = Piece.BKing && (move|>Board.From) = Position.E8 && (move|>Board.To) = Position.C8 then 
            sb.Append("O-O-O") |> ignore
        else 
            let rec getuniqs pu fu ru attl = 
                if List.isEmpty attl then pu, fu, ru
                else 
                    let att = attl.Head
                    if att = (move|>Board.From) then getuniqs pu fu ru attl.Tail
                    else 
                        let otherpiece = board.PieceAt.[int(att)]
                        if otherpiece = piece then 
                            let npu = false
                            
                            let nru = 
                                if (att|>Position.ToRank) = fromrank then false
                                else ru
                            
                            let nfu = 
                                if (att|>Position.ToFile) = fromfile then false
                                else fu
                            
                            getuniqs npu nfu nru attl.Tail
                        else getuniqs pu fu ru attl.Tail
            
            let pu, fu, ru = 
                getuniqs true true true 
                    ((board|>Board.AttacksTo2 (move|>Board.To) (piece|>Piece.PieceToPlayer))|>Bitboard.ToPositions)
            sb.Append(sPiece) |> ignore
            if pu then ()
            elif fu then sb.Append(sFile) |> ignore
            elif ru then sb.Append(sRank) |> ignore
            else sb.Append(sFile + sRank) |> ignore
            if iscap then sb.Append("x") |> ignore
            sb.Append(sTo) |> ignore
        let board = board|>Board.MoveApply(move)
        if board|>Board.IsChk then 
            if MoveGenerate.GenMovesLegal(board) |> Seq.isEmpty then sb.Append("#") |> ignore
            else sb.Append("+") |> ignore
        sb.ToString()
