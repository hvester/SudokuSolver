open System.IO
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running
open SudokuSolver
open Sudoku
open Main

type SudokuBenchmark () =

    let mutable sudoku = [||]

    [<GlobalSetup>]
    member __.Setup() =
        let sudokuStr =
            [ "   | 87|45 " 
              "5  |4  |   "
              " 1 |  5|89 "
              "---+---+---"
              "134|25 |   "
              "28 | 4 | 35"
              "   | 63|124"
              "---+---+---"
              " 23|5  | 8 "
              "   |  6|  3"
              " 57|31 |   " ]
            |> String.concat "\n"
        sudoku <- parseSudoku sudokuStr

    [<Benchmark>]
    member __.SolveSudoku() =
        solveSudoku sudoku |> ignore


module Program =

    [<EntryPoint>]
    let main argv =
        let benchmarks = [| typeof<SudokuBenchmark> |]
        BenchmarkSwitcher(benchmarks).Run argv |> ignore
        0 // return an integer exit code
