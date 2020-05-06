namespace SudokuSolver

open Coloring

module Sudoku =

    let squareToVertex i j = i * 9 + j

    let vertexToSquare n = n / 9, n % 9

    let areConnected (i1, j1) (i2, j2) =
        i1 = i2 ||
        j1 = j2 ||
        (i1 / 3 = i2 / 3 && j1 / 3 = j2 / 3)
        

    let createSudokuGraph () =
        let graph = UndirectedAdjacencyMatrix(9 * 9)
        for i1 in 0 .. 8 do
            for j1 in 0 .. 8 do
                for i2 in 0 .. 8 do
                    for j2 in 0 .. 8 do
                        if (i1 <> i2 || j1 <> j2) && areConnected (i1, j1) (i2, j2) then
                            graph.AddEdge(squareToVertex i1 j1, squareToVertex i2 j2)
        graph


    let solutionToSudoku solution =
        solution
        |> Array.sortBy fst
        |> Array.map snd
        |> Array.chunkBySize 9


    let solveSudoku (sudoku : int option array array) =
        let graph = createSudokuGraph ()
        let colors = [| 1 .. 9 |]
        let initialColors =
            [ for i, row in Array.indexed sudoku do
                for j, square in Array.indexed row do
                    match square with
                    | None -> ()
                    | Some n -> yield (squareToVertex i j, n) ]
        runColoring colors initialColors graph
        |> Option.map (List.toArray >> solutionToSudoku)
