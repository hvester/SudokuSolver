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
