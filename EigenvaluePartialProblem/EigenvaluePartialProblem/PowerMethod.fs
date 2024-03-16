module EigenvaluePartialProblem.PowerMethod

open FsAlg.Generic

type EigenCalculationReport =
    { Eigenvalue : float
      Eigenvector : float Vector
      PosteriorError : float
      StepsCount : int }

let performStep matrix previousVector =
    let newVector = matrix * previousVector
    let newVectorNorm = newVector |> Vector.l2norm
    newVector // / newVectorNorm
    
let calculatePosteriorError previousVector vector (eigenValue : float) =
    let numerator = vector - (eigenValue * previousVector)
                    |> Vector.l2norm
    let denominator = previousVector |> Vector.l2norm
    numerator / denominator

let calculateEigenNumber previousVector vector =
    let numerator = vector |> (*) vector
    let denominator = previousVector |> (*) previousVector
    numerator / denominator |> sqrt

let getEigenPair (matrix : float Matrix) initialVector targetError =
    let rec loop stepsCounter x_previous =
        let x = performStep matrix x_previous
        let eigenNumber = calculateEigenNumber x_previous x
        let error = calculatePosteriorError x_previous x eigenNumber
        if error.CompareTo(targetError) <= 0 then
            { Eigenvalue = eigenNumber
              Eigenvector = x
              PosteriorError = error
              StepsCount = stepsCounter + 1 }
        else
            loop (stepsCounter + 1) x

    loop 0 initialVector