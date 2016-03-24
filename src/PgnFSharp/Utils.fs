namespace PgnFSharp

[<AutoOpen>]
module Utils = 
    ///Dictionary of files
    let fDct = 
        [ 'a'..'h' ]
        |> List.mapi (fun i c -> c, i)
        |> dict
    
    ///Dictionary of ranks
    let rDct = 
        [ 1..8 ]
        |> List.rev
        |> List.mapi (fun i c -> char (c.ToString()), i)
        |> dict
    
    ///Dictionary of squares
    let SqDct = 
        [ for r = 8 downto 1 do
              for f in [ 'a'..'h' ] do
                  yield f.ToString() + r.ToString() ]
        |> List.mapi (fun i s -> s, i)
        |> dict
