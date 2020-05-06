module GraphTests

open System.Collections.Generic
open Expecto
open SudokuSolver
open Coloring

[<Tests>]
let tests =
    testList "Graph tests" [
        
        test "Added edges are returned" {
            let g = UndirectedAdjacencyMatrix(20)
            let edges =
                [ for v in 3 .. 12 do yield (5, v)
                  for v in 12 .. 18 do yield (v, v + 1) ]
            for v1, v2 in edges do
                g.AddEdge(v1, v2)
            let expectedAdjacencies =
                [ for v1, v2 in edges do
                    yield (v1, v2)
                    yield (v2, v1) ]
                |> HashSet
            for v1 in 0 .. 19 do
                for v2 in 0 .. 19 do
                    if expectedAdjacencies.Contains(v1, v2) then
                        Expect.isTrue (g.HasEdge(v1, v2)) "Edge missing"
                    else
                        Expect.isFalse (g.HasEdge(v1, v2)) "Edge should not exist"
        }

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
