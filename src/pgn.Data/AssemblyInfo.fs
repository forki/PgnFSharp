namespace System
open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitleAttribute("pgn.Data")>]
[<assembly: AssemblyProductAttribute("PgnFSharp")>]
[<assembly: AssemblyDescriptionAttribute("PGN tools using F#")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
[<assembly: InternalsVisibleToAttribute("PgnFSharp.Tests")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
