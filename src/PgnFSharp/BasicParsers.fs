[<AutoOpen>]
module internal PgnParsers.BasicCommons

open FParsec

let ws = spaces
let ws1 = spaces1
let str = pstring
let strCI = pstringCI
let pNotChar c = manySatisfy (fun x -> x <> c)
let charList2String (lChars : char list) = System.String.Concat(lChars)

let pList (p, list : 'a list) = 
    list
    |> List.map p
    |> choice

let BP (p : Parser<_, _>) stream = p stream // set a breakpoint here
let NBP (p : Parser<_, _>, name : string) stream = p stream // set a breakpoint here

let D (p : Parser<_, _>, name : string) stream = 
    System.Console.WriteLine(name)
    p stream
#if DEBUG

let (<!!>) (p : Parser<_, _>) (label, depth) : Parser<_, _> = 
    fun stream -> 
        // UNCOMMENT TO GET TRACE MESSAGES
        //        let startTime = System.DateTime.Now
        //        printfn "%A: %sEntering %s. \"%s\""  stream.Position ("->".PadLeft(2*depth)) label (stream.PeekString(5))
        let reply = p stream
        //        let duration = System.DateTime.Now - startTime
        //        printfn "%A: %sLeaving %s (%A) (%f)"  stream.Position ("->".PadLeft(2*depth)) label reply.Status duration.TotalMilliseconds
        reply
#else
let (<!!>) (p: Parser<_,_>) (label, depth) : Parser<_,_> =
    fun stream ->
        p stream
#endif


let (<!>) (p : Parser<_, _>) label : Parser<_, _> = p <!!> (label, 0)
