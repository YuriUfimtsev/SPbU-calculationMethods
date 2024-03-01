namespace ConditionNumbers

open FsAlg.Generic

module AuxiliaryOperations =
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
    
    let addNumberToAllMatrixElements matrix number =
        matrix |> Matrix.toRows |> Seq.map (Matrix.toVector >> Vector.map (fun x -> (+) number x)) |> Matrix.ofRows
    
    let getMatrixVariationErrors matrix vector (pureSolution : float Vector) =
        Seq.map (addNumberToAllMatrixElements matrix)
        >> Seq.map (fun x -> Matrix.solve x vector)
        >> Seq.map ((-) pureSolution)
        >> Seq.map Vector.l2norm
        >> Seq.toList
    
    let getVectorVariationErrors matrix vector (pureSolution : float Vector) =
        Seq.map (fun variationValue -> Vector.map (fun vectorElement -> (+) vectorElement variationValue) vector)
                 >> Seq.map (Matrix.solve matrix)
                 >> Seq.map ((-) pureSolution)
                 >> Seq.map Vector.l2norm
                 >> Seq.toList