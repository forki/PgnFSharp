namespace Pgn2Sql

[<AutoOpen>]
module Types =
    type Mv = {Mfrom:int;Mto:int;Typ:char;Txt:string}
    type MvTxt = 
        |Cmmnt of string
        |MvPr of int option * Mv *Mv


