namespace SudokuSolver

open System
open System.IO
open System.Diagnostics
open Sudoku

module Main =

    let tryParseRow (str : string) =
        let squares =
            str.ToCharArray()
            |> Array.choose (fun c ->
                match Int32.TryParse(string c) with
                | true, n when n > 0 -> Some (Some n)
                | _ when c = ' ' -> Some None  
                | _ -> None)
        if squares.Length >= 9 then
            Some (Array.take 9 squares)
        else
            None


    let parseSudoku (str : string) =
        let rows =
            str.Split("\n")
            |> Array.choose tryParseRow
        if rows.Length = 9 then
            rows
        else
            failwith "Invalid input: There should be nine lines with nine numbers or spaces"


    let formatSolvedSudoku (sudoku : int array array) =
        sudoku
        |> Array.map (fun row ->
            Array.map string row
            |> Array.chunkBySize 3
            |> Array.map (String.concat "")
            |> String.concat "|")
        |> Array.chunkBySize 3
        |> Array.map (String.concat "\n")
        |> String.concat "\n---+---+---\n"


    [<EntryPoint>]
    let main argv =
        let sudoku = parseSudoku (File.ReadAllText argv.[0])
        printfn "Solving..."
        let stopWatch = Stopwatch.StartNew()
        match solveSudoku sudoku with
        | None ->
            printfn "No solution found"
        | Some solvedSudoku ->
            stopWatch.Stop()
            printfn "Solved in %ims" stopWatch.ElapsedMilliseconds 
            formatSolvedSudoku solvedSudoku
            |> printfn "%s"
        0
