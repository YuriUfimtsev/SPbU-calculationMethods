module IterativeMethods.SimpleIterationMethod

open FsAlg.Generic
open AuxiliaryOperations

let performStep matrix_B x_k rightVector_c : float Vector =
    matrix_B * x_k + rightVector_c
    
let calculatePosteriorError matrix_B x_previous x_last =
    let euclideanNorm = matrix_B |> calculateEuclideanNorm
    let coefficient = euclideanNorm / ((-) 1.0 euclideanNorm)
    x_previous |> (-) x_last |> Vector.l2norm |> (*) coefficient
    
let performMethod matrix_B x_0 rightVector_c targetError =
    let rec loop stepsCounter x_previous =
        let x = performStep matrix_B x_previous rightVector_c
        let error = calculatePosteriorError matrix_B x_previous x
        if error.CompareTo(targetError) <= 0 then
            { StepsCount = stepsCounter + 1
              Solution = x
              PosteriorError = error }
        else
            loop (stepsCounter + 1) x
    
    loop 0 x_0