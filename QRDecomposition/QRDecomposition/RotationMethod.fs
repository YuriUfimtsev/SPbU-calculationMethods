module QRDecomposition.RotationMethod

open FsAlg.Generic

let getRotationMatrix i j (sourceVector : Vector<float>) =
    let denominator = sqrt ((Vector.get sourceVector i) ** 2.0 + (Vector.get sourceVector j) ** 2.0)
    let angle'sCos = (/) (Vector.get sourceVector i) denominator
    let angle'sSin = -1.0 * ((/) (Vector.get sourceVector j) denominator)
    Matrix.init (Vector.length sourceVector) (Vector.length sourceVector)
        (fun localI localJ ->
            match localI, localJ with
            | x, y when x = y ->
                match x with
                | index when index = i || index = j -> angle'sCos
                | _ -> 1.0
            | x, y when x = i && y = j -> (-1.0 * angle'sSin)
            | x, y when x = j && y = i -> angle'sSin
            | _ -> 0.0)

let getSingleDiagonalMatrix dimension =
    Matrix.init dimension dimension
        (fun i j ->
        match i, j with
        | localI, localJ when localI = localJ -> 1.0
        | _ -> 0.0
        )

let getRotationTable matrix =
    if Matrix.rows matrix <> Matrix.cols matrix then invalidArg "matrix" "Expected the square matrix"
    
    let matrixCols = matrix |> Matrix.toCols |> Seq.map Matrix.toVector |> Seq.toArray
    Array2D.init (Matrix.rows matrix) (Matrix.rows matrix)
        (fun i j ->
        match i, j with
        | localI, localJ when localI < localJ ->
            getRotationMatrix localI localJ (matrixCols[localI])
        | _ -> Matrix.init (Matrix.rows matrix) (Matrix.rows matrix) (fun x y -> 0.0))

let getRMatrix matrix =
    let rotationTable = matrix |> getRotationTable
    let rec rowMultiplicationLoop acc i =
        let rec elementsMultiplicationLoop innerAcc j =
            if i = j then innerAcc
            else
                elementsMultiplicationLoop (innerAcc * rotationTable[i, j]) (j - 1)
            
        if i = rotationTable.Length - 2 then acc
        else
            let newAcc = elementsMultiplicationLoop
                             (rotationTable |> Array2D.base2 |> getSingleDiagonalMatrix)
                             (Array2D.base2 rotationTable)
            rowMultiplicationLoop (newAcc * acc) (i + 1)

    let multiplicationResult = rowMultiplicationLoop
                                   (rotationTable |> Array2D.base1 |> getSingleDiagonalMatrix)
                                   0
    multiplicationResult * matrix

let inversion'sRotationTable = getRotationTable >> Array2D.map Matrix.inverse

let getQMatrix matrix =
    let inversion'sRotationTable = matrix |> getRotationTable |> Array2D.map Matrix.inverse
    
    let rec rowMultiplicationLoop acc i =
        let rec elementsMultiplicationLoop innerAcc j =
            if j = (inversion'sRotationTable |> Array2D.base2 |> (+) 1) then innerAcc
            else
                elementsMultiplicationLoop (innerAcc * inversion'sRotationTable[i, j]) (j + 1)
            
        if i = inversion'sRotationTable.Length - 2 then acc
        else
            let newAcc = elementsMultiplicationLoop
                             (inversion'sRotationTable |> Array2D.base2 |> getSingleDiagonalMatrix)
                             (i + 1)
            rowMultiplicationLoop (acc * newAcc) (i + 1)

    rowMultiplicationLoop (inversion'sRotationTable |> Array2D.base1 |> getSingleDiagonalMatrix) 0


     
let solveSystemUsingQRDecomposition matrix rightVector =
    let qMatrix = getQMatrix matrix
    let rMatrix = getRMatrix matrix
    Matrix.solve rMatrix ((Matrix.inverse qMatrix) * rightVector)