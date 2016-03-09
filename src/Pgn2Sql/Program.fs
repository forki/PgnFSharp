open System.IO
open PgnFSharp
open System.Data
open System.Data.SqlClient
open Nessos.FsPickler
open FSharp.Data

[<Literal>]
let conn = "Server=CHILL\SQLEXPRESS;Integrated security=SSPI;database=DemoGames"
type GameTempDb = SqlProgrammabilityProvider<conn>

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    // Create Database
    let myConn = new SqlConnection ("Server=CHILL\SQLEXPRESS;Integrated security=SSPI;database=master")
    let str = 
        "CREATE DATABASE DemoGames ON PRIMARY " +
        "(NAME = DemoGames, " +
        "FILENAME = 'I:\\SqlServer\\Data\\DemoGames.mdf', " +
        "SIZE = 5MB, MAXSIZE = UNLIMITED, FILEGROWTH = 1MB) " +
        "LOG ON (NAME = DemoGames_Log, " +
        "FILENAME = 'I:\\SqlServer\\Data\\DemoGames_log.ldf', " +
        "SIZE = 1MB, " +
        "MAXSIZE = 2048GB, " +
        "FILEGROWTH = 10%)"
    let myCommand = new SqlCommand(str, myConn)
    try
        try
            myConn.Open()|>ignore
            //myCommand.ExecuteNonQuery()|>ignore
        with
        | e -> failwith e.Message
    finally
        if myConn.State = ConnectionState.Open then myConn.Close()
    // Create Temp Table
    let str = 
        "USE DemoGames " +
        "CREATE TABLE GamesTemp (" +
        "GameId int IDENTITY(1,1) NOT NULL," +
        "Event nvarchar(32) NOT NULL," +
        "Site nvarchar(32) NOT NULL," +
        "Year smallint NULL," + 
        "Month tinyint NULL," +
        "Day tinyint NULL," +
        "Round nvarchar(32) NOT NULL," +
        "WhitePlayer nvarchar(32) NOT NULL," +
        "BlackPlayer nvarchar(32) NOT NULL," +
        "Result tinyint NOT NULL," +
        "AdditionalInfo varbinary(MAX)," +
        "Tags varbinary(MAX)," +
        "MoveText varbinary(MAX)," +
        "BoardSetup varbinary(MAX))"

    let myCommand = new SqlCommand(str, myConn)
    try
        try
            myConn.Open()|>ignore
//            myCommand.ExecuteNonQuery()|>ignore
        with
        | e -> failwith e.Message
    finally
        if myConn.State = ConnectionState.Open then myConn.Close()

    // Write games to table
    let fol = @"I:\GitHub\PgnFSharp\tests\data\RealFiles"
    let fl = "demoGames.pgn"
    let pth = Path.Combine(fol,fl)
    let gms:Game list = PgnReader.ReadFromFile(pth)
    let ser = FsPickler.CreateBinarySerializer()
//    let dt = new DataTable()
//    dt.Columns.Add("GameId",typeof<int>)|>ignore
//    dt.Columns.Add("Event",typeof<string>)|>ignore
//    dt.Columns.Add("Site",typeof<string>)|>ignore
//    dt.Columns.Add("Year",typeof<int option>)|>ignore
//    dt.Columns.Add("Month",typeof<int option>)|>ignore
//    dt.Columns.Add("Day",typeof<int option>)|>ignore
//    dt.Columns.Add("Round",typeof<string>)|>ignore
//    dt.Columns.Add("WhitePlayer",typeof<string>)|>ignore
//    dt.Columns.Add("BlackPlayer",typeof<string>)|>ignore
//    dt.Columns.Add("Result",typeof<int>)|>ignore
//    dt.Columns.Add("AdditionalInfo",typeof<byte[]>)|>ignore
//    dt.Columns.Add("Tags",typeof<byte[]>)|>ignore
//    dt.Columns.Add("MoveText",typeof<byte[]>)|>ignore
//    dt.Columns.Add("BoardSetup",typeof<byte[]>)|>ignore
//    let addgm i (gm:Game) =
//        let r = dt.NewRow()
//        r.["GameId"] <- i
//        r.["Event"] <- gm.Event
//        r.["Site"] <- gm.Site
//        r.["Year"] <- gm.Year
//        r.["Month"] <- gm.Month
//        r.["Day"] <- gm.Day
//        r.["Round"] <- gm.Round
//        r.["WhitePlayer"] <- gm.WhitePlayer
//        r.["BlackPlayer"] <- gm.BlackPlayer
//        r.["Result"] <- gm.Result.ToByte()
//        r.["AdditionalInfo"] <- gm.AdditionalInfo|>ser.Pickle
//        r.["Tags"] <- gm.Tags|>ser.Pickle
//        r.["MoveText"] <- gm.MoveText|>ser.Pickle
//        r.["BoardSetup"] <- gm.BoardSetup|>ser.Pickle
//        dt.Rows.Add(r)
//    gms|>List.iteri addgm
    let dbgms = new GameTempDb.dbo.Tables.GamesTemp()
    let gm = gms.Head
    let newrow = dbgms.NewRow(Event=gm.Event,Site=gm.Site,Round=gm.Round,WhitePlayer=gm.WhitePlayer,BlackPlayer=gm.BlackPlayer,Result=gm.Result.ToByte())
    dbgms.Rows.Add newrow
    //let recordsAffected = dbgms.Update()
    dbgms.BulkCopy(copyOptions = System.Data.SqlClient.SqlBulkCopyOptions.TableLock)
    
    
    0

