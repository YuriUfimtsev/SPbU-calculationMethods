module QRDecomposition.AuxiliaryOperations

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

// Next code from ConditionNumbers project
type ConditionNumbers =
        { Spectral: float;
          Volume: float;
          Angular: float }

let calculateEuclideanNorm = Matrix.toArray >> Seq.map (fun x -> x * x) >> Seq.fold (+) 0.0 >> sqrt

let calculateSpectralNumber matrix =
    [ matrix; matrix |> Matrix.inverse ]
    |> Seq.map calculateEuclideanNorm
    |> Seq.fold (*) 1.0

let calculateVolumeNumber matrix =
    let calculateNumerator =
        Matrix.toRows
        >> Seq.map Matrix.toVector
        >> Seq.map Vector.l2norm
        >> Seq.fold (*) 1.0
    let calculateDenominator = Matrix.det >> abs
    (/) (calculateNumerator matrix) (calculateDenominator matrix)

let calculateAngularNumber (matrix : float Matrix) =
    Seq.init
        (matrix |> Matrix.rows)
        (fun n -> (matrix |> Matrix.row n |> Matrix.toVector |> Vector.l2norm)
                  |> (*) (matrix |> Matrix.inverse |> Matrix.col n |> Matrix.toVector |> Vector.l2norm))
    |> Seq.max

let getConditionNumbers matrix = { Spectral = matrix |> calculateSpectralNumber
                                   Volume = matrix |> calculateVolumeNumber;
                                   Angular = matrix |> calculateAngularNumber }