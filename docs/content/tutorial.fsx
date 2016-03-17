(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/PgnFSharp"

(**
Simple Example
========================

This section sets out how to generate a subset of a PGN file limited to games of a particular player.

*)
#r "PgnFSharp.dll"
open PgnFSharp
open System.IO

let fl = Path.Combine(__SOURCE_DIRECTORY__,"o-deville.pgn")
let gms = PgnRead.FromFile fl

(**
You can then filter the games where the name is "Adams, Michael":
*)
let filt = gms|>List.filter(fun gm -> gm.White="Adams, Michael"||gm.Black="Adams, Michael")

(** You can then save this as a new PGN: *)

let ofl = Path.Combine(__SOURCE_DIRECTORY__,"Adams.pgn")
PgnWrite.ToFile filt ofl

(** This has saved 8 games out of 11,586 *)

// [fsi:val gms.Length : int = 11586]
// [fsi:val filt.Length : int = 8]
