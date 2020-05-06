module SudokuTests

open Expecto
open SudokuSolver
open Sudoku

[<Tests>]
let tests =
    testList "Sudoku tests" [
        
        test "Sudoku graph has all connections" {
            let graph = createSudokuGraph ()
            for v in 0 .. 80 do
                let actualAdjacenciens = List.length (graph.GetAdjacentVertices(v))
                Expect.equal actualAdjacenciens 20 "Wrong number of adjacent vertices"
        }
    ]
