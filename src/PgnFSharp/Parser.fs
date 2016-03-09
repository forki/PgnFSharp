﻿namespace PgnFSharp

open FParsec
open System
open System.IO
open PgnParsers.Game
open PgnParsers.BasicCommons

type Parser() =
    member this.ReadFromFile(file:string) =  
        let stream = new FileStream(file, FileMode.Open)
        let result = this.ReadFromStream(stream)
        stream.Close()

        result

    member this.ReadFromFileDebug(file:string) =  
        let stream = new FileStream(file, FileMode.Open)
        let result = this.ReadFromStreamDebug(stream)
        stream.Close()

        result

    member this.ReadFromStream(stream: System.IO.Stream) =
        let parserResult = runParserOnStream pDatabase () "pgn" stream System.Text.Encoding.UTF8

        let db =
            match parserResult with
            | Success(result, _, _)   -> result
            | Failure(errorMsg, _, _) -> raise (FormatException errorMsg)

        db

    member this.ReadFromStreamDebug(stream: System.IO.Stream) =
        let parserResult = runParserOnStream (BP pDatabase) () "pgn" stream System.Text.Encoding.UTF8

        let db =
            match parserResult with
            | Success(result, _, _)   -> result
            | Failure(errorMsg, _, _) -> raise (FormatException errorMsg)

        db
    member this.ReadFromString(input: string) =
        let parserResult = run pDatabase input

        let db =
            match parserResult with
            | Success(result, _, _)   -> result
            | Failure(errorMsg, _, _) -> raise (FormatException errorMsg)

        db

    member this.ReadFromStringDebug(input: string) =
        let parserResult = run (BP pDatabase) input

        let db =
            match parserResult with
            | Success(result, _, _)   -> result
            | Failure(errorMsg, _, _) -> raise (FormatException errorMsg)

        db

    member this.ReadGamesFromStream(stream: System.IO.Stream) =
        seq {
            let charStream = new CharStream<Unit>(stream, true, System.Text.Encoding.UTF8);
            while not charStream.IsEndOfStream do
                 yield this.ParseGame(charStream)
            }

    member this.ReadGamesFromFile(file: string) =
        seq {
            let charStream = new CharStream<Unit>(file, System.Text.Encoding.UTF8);
            while not charStream.IsEndOfStream do
                 yield this.ParseGame(charStream)
            }

    member this.ParseGame(charStream: CharStream<unit>) =

        let parserResult = pGame(charStream)
        let game  =
            match parserResult.Status with
            | ReplyStatus.Ok   -> parserResult.Result
            | _ -> raise (FormatException (ErrorMessageList.ToSortedArray(parserResult.Error) |> Array.map(fun e -> e.ToString()) |> String.concat "\n" ));

        game