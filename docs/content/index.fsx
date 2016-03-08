(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin/PgnFSharp"

(**
PgnFSharp
======================

Example
-------

This example demonstrates using a function defined in this sample library.

We can load a pgn file into an F# list of game records:

*)
#r "PgnFSharp.dll"
open PgnFSharp
open System.IO

let fl = Path.Combine(__SOURCE_DIRECTORY__,"simple-game.pgn")
let gms = PgnReader.ReadFromFile fl

(**
You can then load the first game and get the player of the white pieces:
*)
let gm = gms.Head
let white = gm.WhitePlayer

(** And the variable `white` has the following value: *)

// [fsi:val white : string = "Howell, David"]

(**

Samples & documentation
-----------------------

The library comes with some limited documentation: 

 * [Simple Example](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. 

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/PgnFSharp/tree/master/docs/content
  [gh]: https://github.com/fsprojects/PgnFSharp
  [issues]: https://github.com/fsprojects/PgnFSharp/issues
  [readme]: https://github.com/fsprojects/PgnFSharp/blob/master/README.md
  [license]: https://github.com/fsprojects/PgnFSharp/blob/master/LICENSE.txt
*)
