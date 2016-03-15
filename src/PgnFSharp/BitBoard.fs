﻿namespace PgnFSharp

open System

module Bitboard = 
    let DebrujinPositions = 
        [| 0; 1; 28; 2; 29; 14; 24; 3; 30; 22; 20; 15; 25; 17; 4; 8; 31; 27; 13; 23; 21; 19; 16; 7; 26; 12; 18; 6; 11; 5; 
           10; 9 |]
    
    let DebrujinLSB num = 
        let ind1 = uint32 (num &&& -num) * 0x077CB531u
        let ind2 = ind1 >>> 27
        DebrujinPositions.[int (ind2)]
    
    let private byteBitcount = 
        [| 0; 1; 1; 2; 1; 2; 2; 3; 1; 2; 2; 3; 2; 3; 3; 4; 1; 2; 2; 3; 2; 3; 3; 4; 2; 3; 3; 4; 3; 4; 4; 5; 1; 2; 2; 
            3; 2; 3; 3; 4; 2; 3; 3; 4; 3; 4; 4; 5; 2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6; 1; 2; 2; 3; 2; 3; 
            3; 4; 2; 3; 3; 4; 3; 4; 4; 5; 2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6; 2; 3; 3; 4; 3; 4; 4; 5; 3; 
            4; 4; 5; 4; 5; 5; 6; 3; 4; 4; 5; 4; 5; 5; 6; 4; 5; 5; 6; 5; 6; 6; 7; 1; 2; 2; 3; 2; 3; 3; 4; 2; 3; 3; 4; 
            3; 4; 4; 5; 2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6; 2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 
            6; 3; 4; 4; 5; 4; 5; 5; 6; 4; 5; 5; 6; 5; 6; 6; 7; 2; 3; 3; 4; 3; 4; 4; 5; 3; 4; 4; 5; 4; 5; 5; 6; 3; 4; 
            4; 5; 4; 5; 5; 6; 4; 5; 5; 6; 5; 6; 6; 7; 3; 4; 4; 5; 4; 5; 5; 6; 4; 5; 5; 6; 5; 6; 6; 7; 4; 5; 5; 6; 5; 
            6; 6; 7; 5; 6; 6; 7; 6; 7; 7; 8 |]

    let BitCount(ibb : Bitboard) = 
        let rec getv vl rv = 
            if vl = 0UL then rv
            else 
                let nrv = rv + byteBitcount.[int (vl &&& 255UL)]
                let nvl = vl >>> 8
                getv nvl nrv
        getv (uint64 (ibb)) 0
    
    let NorthMostPosition(ibb : Bitboard) = 
        if (uint64 (ibb) &&& 0xFFFFFFFFUL) <> 0UL then 
            let x = uint64 (ibb) &&& 0xFFFFFFFFUL
            DebrujinLSB(int (x)) |> Pos
        else 
            let x = uint64 (ibb) >>> 32
            (DebrujinLSB(int (x)) + 32) |> Pos
    
    let ShiftDirN(ibb : Bitboard) = (uint64 (ibb &&& ~~~Bitboard.Rank8) >>> 8) |> BitB
    let ShiftDirE(ibb : Bitboard) = (uint64 (ibb &&& ~~~Bitboard.FileH) <<< 1) |> BitB
    let ShiftDirS(ibb : Bitboard) = (uint64 (ibb &&& ~~~Bitboard.Rank1) <<< 8) |> BitB
    let ShiftDirW(ibb : Bitboard) = (uint64 (ibb &&& ~~~Bitboard.FileA) >>> 1) |> BitB
    let ShiftDirNE(ibb : Bitboard) = (uint64 (ibb &&& ~~~Bitboard.Rank8 &&& ~~~Bitboard.FileH) >>> 7) |> BitB
    let ShiftDirSE(ibb : Bitboard) = (uint64 (ibb &&& ~~~Bitboard.Rank1 &&& ~~~Bitboard.FileH) <<< 9) |> BitB
    let ShiftDirSW(ibb : Bitboard) = (uint64 (ibb &&& ~~~Bitboard.Rank1 &&& ~~~Bitboard.FileA) <<< 7) |> BitB
    let ShiftDirNW(ibb : Bitboard) = (uint64 (ibb &&& ~~~Bitboard.Rank8 &&& ~~~Bitboard.FileA) >>> 9) |> BitB
    
    let Shift dir (ibb : Bitboard) = 
        match dir with
        | Direction.DirN -> ibb |> ShiftDirN
        | Direction.DirE -> ibb |> ShiftDirE
        | Direction.DirS -> ibb |> ShiftDirS
        | Direction.DirW -> ibb |> ShiftDirW
        | Direction.DirNE -> ibb |> ShiftDirNE
        | Direction.DirSE -> ibb |> ShiftDirSE
        | Direction.DirSW -> ibb |> ShiftDirSW
        | Direction.DirNW -> ibb |> ShiftDirNW
        | Direction.DirNNE -> 
            ibb
            |> ShiftDirNE
            |> ShiftDirN
        | Direction.DirEEN -> 
            ibb
            |> ShiftDirNE
            |> ShiftDirE
        | Direction.DirEES -> 
            ibb
            |> ShiftDirSE
            |> ShiftDirE
        | Direction.DirSSE -> 
            ibb
            |> ShiftDirSE
            |> ShiftDirS
        | Direction.DirSSW -> 
            ibb
            |> ShiftDirSW
            |> ShiftDirS
        | Direction.DirWWS -> 
            ibb
            |> ShiftDirSW
            |> ShiftDirW
        | Direction.DirWWN -> 
            ibb
            |> ShiftDirNW
            |> ShiftDirW
        | Direction.DirNNW -> 
            ibb
            |> ShiftDirNW
            |> ShiftDirN
        | _ -> failwith "invalid dir"
    
    let Flood dir (ibb : Bitboard) = 
        let rec getb (bb : Bitboard) = 
            let shift = bb |> Shift(dir)
            if (shift &&& bb) = shift then bb
            else getb (shift ||| bb)
        getb ibb
    
    let ContainsPos (pos : Position) ibb = (ibb &&& (pos |> Position.ToBitboard)) <> Bitboard.Empty
    
    let ToPositions(ibb : Bitboard) = 
        let rec getp (bb : Bitboard) ol = 
            if bb = Bitboard.Empty then ol |> List.rev
            else 
                let first = bb |> NorthMostPosition
                let nbb = bb &&& ~~~(first |> Position.ToBitboard)
                getp nbb (first :: ol)
        getp ibb []
    
    let GetFirstPos ibb =
        let num = uint64 (ibb) &&& 0xFFFFFFFFUL
        if num <> 0UL then 
            let number = int (num)
            DebrujinPositions.[int ((uint32 (number &&& -number) * 0x077CB531u) >>> 27)] |> Pos
        else 
            let number = int (uint64 (ibb) >>> 32)
            (DebrujinPositions.[int ((uint32 (number &&& -number) * 0x077CB531u) >>> 27)] + 32) |> Pos

    let GetRemainPos first ibb = ibb &&& ~~~(first |> Position.ToBitboard)
    
    let PopFirst ibb = 
        let first = GetFirstPos ibb
        let bb = GetRemainPos first ibb
        first, bb

    