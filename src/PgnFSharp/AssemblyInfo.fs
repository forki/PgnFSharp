namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("PgnFSharp")>]
[<assembly: AssemblyProductAttribute("PgnFSharp")>]
[<assembly: AssemblyDescriptionAttribute("PGN tools using F#")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
