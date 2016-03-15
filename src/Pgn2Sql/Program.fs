open System.IO
open PgnFSharp
open System.Data
open System.Data.SqlClient
open Nessos.FsPickler
open FSharp.Data
open Microsoft.FSharp.Linq


[<Literal>]
let conn = "Server=CHILL\SQLEXPRESS;Integrated security=SSPI;database=ChessDb"
type GameDb = SqlProgrammabilityProvider<conn>
[<Literal>]
let str =
    "IF EXISTS (SELECT 1 FROM Players WHERE Player=@p) 
    BEGIN
    SELECT PlayerID FROM Players WHERE Player=@p
    END
    ELSE
    BEGIN
    INSERT INTO Players(Player) SELECT @p
    SELECT MAX(PlayerID) From Players AS PlayerID
    END"

[<EntryPoint>]
let main argv = 
    // Get path if provided as argument
    let pth = 
        if argv.Length>0 then 
            printfn "Args provided: %A" argv
            argv.[0]
        else
            let fol = @"I:\GitHub\PgnFSharp\tests\data\RealFiles"
            let fl = "demoGames.pgn"
            Path.Combine(fol,fl)
    // load games from file
//    let gms:Game list = PgnReader.ReadFromFile(pth)
//    let prs = 
//        let bs = gms|>List.map(fun gm -> gm.BlackPlayer)|>Set.ofList
//        let ws = gms|>List.map(fun gm -> gm.WhitePlayer)|>Set.ofList
//        bs+ws
//    let dbprs = new GameDb.dbo.Tables.Players()
//    let addpr pr =
//        let newrow = dbprs.NewRow(pr)
//        dbprs.Rows.Add newrow
//    prs|>Set.iter addpr
//    dbprs.BulkCopy(copyOptions = System.Data.SqlClient.SqlBulkCopyOptions.TableLock)

    let dbgms = new GameDb.dbo.Tables.Games()
    let ser = FsPickler.CreateBinarySerializer()
    
    
//    use addp = new SqlCommandProvider<str, conn, SingleRow = true>()
//    let addgm gm =
//        let wid = addp.Execute(gm.WhitePlayer)
//        let bid = addp.Execute(gm.BlackPlayer)
//        //let newrow = dbgms.NewRow(gm.Event,gm.Site,gm.Round,gm.WhitePlayer,gm.BlackPlayer,gm.Result.ToByte(),gm.AdditionalInfo|>ser.Pickle,gm.Tags|>ser.Pickle,gm.MoveText|>ser.Pickle,Year=gm.Year,Month=gm.Month,Day=gm.Day)
//        //dbgms.Rows.Add newrow
//        wid,bid
//    let tst = gms|>List.map addgm
    //gms|>List.iter addgm
    //let recordsAffected = dbgms.Update()
    //dbgms.BulkCopy(copyOptions = System.Data.SqlClient.SqlBulkCopyOptions.TableLock)
    
    
    0

