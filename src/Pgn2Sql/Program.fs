open System.IO
open ilf.pgn
open System.Data
open System.Data.SqlClient
open Nessos.FsPickler

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let fol = @"I:\GitHub\pgn.net\test\pgn.NET.Test\Test Files\Real Files"
    let fl = "demoGames.pgn"
    let pth = Path.Combine(fol,fl)
    let parser = new PgnReader()
    let db = parser.ReadFromFile(pth)
    let cnt = db.Games.Count
    let gm = db.Games.[0]
    let ser = FsPickler.CreateBinarySerializer()
    let addi = gm.AdditionalInfo|>Seq.map(fun a -> a.Name,a.Value)|>Seq.toArray
    let seraddi = ser.Pickle addi
    let addi2 = ser.UnPickle<(string*string)[]> seraddi
    let mvs = gm.MoveText
    let mv0 = mvs.[0]
    let mv1 = mvs.[1]
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
            myCommand.ExecuteNonQuery()|>ignore
        with
        | e -> failwith e.Message
    finally
        if myConn.State = ConnectionState.Open then myConn.Close()

    let str = 
        "USE DemoGames " +
        "CREATE TABLE GamesTemp (" +
        "GameId int IDENTITY(1,1) NOT NULL," +
        "Event nvarchar(32) NULL," +
        "Site nvarchar(32) NULL," +
        "Round nvarchar(32) NULL," +
        "WhitePlayer nvarchar(32) NULL," +
        "BlackPlayer nvarchar(32) NULL," +
        "Result tinyint NOT NULL," +
        "Day tinyint NULL," +
        "Month tinyint NULL," +
        "Year tinyint NULL)" 
    let myCommand = new SqlCommand(str, myConn)
    try
        try
            myConn.Open()|>ignore
            myCommand.ExecuteNonQuery()|>ignore
        with
        | e -> failwith e.Message
    finally
        if myConn.State = ConnectionState.Open then myConn.Close()

    cnt

