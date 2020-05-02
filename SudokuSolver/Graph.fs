namespace SudokuSolver

type UndirectedAdjacencyMatrix(nVertices) =
    let maxNumberOfEdges = nVertices * (nVertices + 1) / 2 
    let adjacencies =
        (maxNumberOfEdges - 1) / 8 + 1
        |> Array.zeroCreate<byte>

    let getArrayIndexAndMask i j =
        let n, m = if i < j then i, j else j, i
        if n < 0 || m >= nVertices then failwith "Vertex index out of bounds"
        let logicalIndex = n * (n + 1) / 2 + m
        let arrayIndex = logicalIndex / 8
        let mask = 1uy <<< (logicalIndex - 8 * arrayIndex)
        arrayIndex, mask

    member __.Vertices = [ 0 .. nVertices - 1 ]

    member __.AddEdge(i, j) =
        let arrayIndex, mask = getArrayIndexAndMask i j
        let currentValue = adjacencies.[arrayIndex]
        adjacencies.[arrayIndex] <- currentValue ||| mask

    member __.RemoveEdge(i, j) =
        let arrayIndex, mask = getArrayIndexAndMask i j
        let currentValue = adjacencies.[arrayIndex]
        adjacencies.[arrayIndex] <- currentValue &&& ~~~mask

    member __.HasEdge(i, j) =
        let arrayIndex, mask = getArrayIndexAndMask i j
        adjacencies.[arrayIndex] &&& mask <> 0uy

    member this.GetAdjacentVertices(i) =
        [ for j in 0 .. nVertices - 1 do
            if this.HasEdge(i, j) then
                yield j ]



module Coloring =
    open System.Collections.Generic

    let runColoring (colors : 'Color array) initialColors (graph : UndirectedAdjacencyMatrix) =
        let state = Dictionary<_,_>()
        initialColors |> List.iter state.Add
        let vertices = List.toArray graph.Vertices

        let canSetColor vertex color =
            if state.ContainsKey vertex then
                false
            else
                graph.GetAdjacentVertices(vertex)
                |> List.forall (fun adjacentVertex ->
                    match state.TryGetValue adjacentVertex with
                    | false, _  -> true
                    | true, adjacentColor ->
                        color <> adjacentColor)

        let moveIndices (vertexIndex, colorIndex) =
            let newColorIndex = (colorIndex + 1) % colors.Length
            let newVertexIndex = if newColorIndex = 0 then vertexIndex + 1 else vertexIndex
            (newVertexIndex, newColorIndex) 

        let rec loop stack =
            match stack with
            | [] -> None
            | ((vertexIndex, colorIndex), prevVertex) :: remainingStack ->
                if vertexIndex >= vertices.Length then
                    prevVertex |> Option.iter (state.Remove >> ignore)
                    loop remainingStack
                else
                    let nextVertex = vertices.[vertexIndex]
                    let nextColor = colors.[colorIndex]
                    let updatedStack = (moveIndices (vertexIndex, colorIndex), prevVertex) :: remainingStack
                    if canSetColor nextVertex nextColor then
                        state.Add(nextVertex, nextColor)
                        if state.Count = vertices.Length then
                            [ for vertex in vertices do yield (vertex, state.[vertex]) ]
                            |> Some
                        else
                            loop (((0, 0), Some nextVertex) :: updatedStack)
                    else
                        loop updatedStack

        loop [ ((0, 0), None) ]
