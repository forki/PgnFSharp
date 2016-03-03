#I @"I:\GitHub\PgnFSharp\src\PgnFSharp.FParsec\bin\Debug"
#r "PgnFSharp.FParsec.dll"
#r "FParsec.dll"

open PgnFSharp.FParsec
open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let tst1 = test Parse.piece "A"
let r1 = run Parse.piece "A"
let a1 = Parse.applyPc "A"

let tst2 = test Parse.piece "N"
let r2 = run Parse.piece "N"

let tst3 = test Parse.piece ""
let r3 = run Parse.piece ""
