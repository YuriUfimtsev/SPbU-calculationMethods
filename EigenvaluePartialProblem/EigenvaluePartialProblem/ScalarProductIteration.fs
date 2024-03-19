module EigenvaluePartialProblem.ScalarProductIteration

open FsAlg.Generic
open PowerIteration

type TemporaryPair =
    { Vector_x : float Vector
      Vector_y : float Vector }

let performStep (matrix : float Matrix) (transposedMatrix : float Matrix) (previous_pair : TemporaryPair) =
    { Vector_x = matrix * previous_pair.Vector_x
      Vector_y = transposedMatrix * previous_pair.Vector_y }
    
let calculatePosteriorError previousVector vector (eigenValue : float) =
    let numerator = vector - (eigenValue * previousVector)
                    |> Vector.l2norm
    let denominator = previousVector |> Vector.l2norm
    numerator / denominator

let calculateEigenNumber (previous_pair : TemporaryPair) (vector_pair : TemporaryPair) =
    let numerator = vector_pair.Vector_x |> (*) vector_pair.Vector_y
    let denominator = previous_pair.Vector_x |> (*) vector_pair.Vector_y
    numerator / denominator

let perform (matrix : float Matrix) vectorPair targetError =
    let transposedMatrix = Matrix.transpose matrix
    
    let rec loop stepsCounter previous_pair =
        let vector_pair = performStep matrix transposedMatrix previous_pair
        let eigenNumber = calculateEigenNumber previous_pair vector_pair
        let error = calculatePosteriorError previous_pair.Vector_x vector_pair.Vector_x eigenNumber
        if error.CompareTo(targetError) <= 0 then
            { Eigenvalue = eigenNumber
              Eigenvector = vector_pair.Vector_x
              PosteriorError = error
              StepsCount = stepsCounter + 1 }
        else
            loop (stepsCounter + 1) vector_pair

    loop 0 { Vector_x = vectorPair.Vector_x
             Vector_y = vectorPair.Vector_y }