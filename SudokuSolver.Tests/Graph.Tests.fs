module GraphTests

open Expecto
open SudokuSolver
open Coloring

[<Tests>]
let tests =
    testList "Graph tests" [
        
        test "Coloring works" {
            let g = UndirectedAdjacencyMatrix(4)
            g.AddEdge(0, 1)
            g.AddEdge(1, 2)
            g.AddEdge(2, 3)
            g.AddEdge(3, 0)
            let expectedSolution =
                [ (0, "blue"); (1, "red"); (2, "blue"); (3, "red") ]
            match runColoring [| "red"; "blue" |] [ (0, "red")] g with
            | Some solution ->
                Expect.equal solution expectedSolution "Wrong solution"
            | None ->
                failwith "No solution found"
        }
    ]
