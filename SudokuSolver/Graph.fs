namespace SudokuSolver

type UndirectedAdjacencyMatrix(nVertices) =
    let maxNumberOfEdges = nVertices * (nVertices + 1) / 2

    let adjacencies =
        let nBytes = (maxNumberOfEdges - 1) / 8 + 1
        Array.zeroCreate<byte> nBytes

    let getArrayIndexAndMask i j =
        let n, m = if i < j then i, j else j, i

        if n < 0 || m >= nVertices then
            failwithf "Vertex index out of bounds: %i, %i" i j

        let logicalIndex = m * (m + 1) / 2 + n
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
        [
            for j in 0 .. nVertices - 1 do
                if this.HasEdge(i, j) then
                    yield j
        ]



module Coloring =
    open System.Collections.Generic

    let runColoring (colors : 'Color array) initialColors (graph : UndirectedAdjacencyMatrix) =
        let state = Dictionary<_, _>()
        initialColors |> List.iter state.Add

        let verticesToColor =
            graph.Vertices
            |> List.filter (state.ContainsKey >> not)
            |> List.toArray

        let canSetColor vertex color =
            graph.GetAdjacentVertices(vertex)
            |> List.forall (fun adjacentVertex ->
                match state.TryGetValue adjacentVertex with
                | false, _ -> true
                | true, adjacentColor -> color <> adjacentColor)

        let rec loop vertexIndex colorStack =
            match colorStack with
            | [] -> None
            | _ when vertexIndex >= verticesToColor.Length ->
                [
                    for vertex in graph.Vertices do
                        yield (vertex, state.[vertex])
                ]
                |> Some
            | colorIndex :: tail when colorIndex >= colors.Length ->
                if vertexIndex > 0 then
                    state.Remove(verticesToColor.[vertexIndex - 1])
                    |> ignore

                loop (vertexIndex - 1) tail
            | colorIndex :: tail ->
                let vertex = verticesToColor.[vertexIndex]
                let color = colors.[colorIndex]

                if canSetColor vertex color then
                    state.Add(vertex, color)
                    loop (vertexIndex + 1) (0 :: (colorIndex + 1) :: tail)
                else
                    loop vertexIndex ((colorIndex + 1) :: tail)

        loop 0 [ 0 ]
