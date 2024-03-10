module IterativeMethods.Seidel_sMethod

open FsAlg.Generic
open AuxiliaryOperations
open Microsoft.FSharp.Core

let expandMatrix matrix =
    { LMatrix = matrix |> getLMatrix
      DMatrix = matrix |> getDMatrix
      RMatrix = matrix |> getRMatrix }

let performStep (expandedMatrix : RegularMatrix)
    (x_previous : float Vector) (vector_b : float Vector) =
        let baseMatrix = expandedMatrix.LMatrix
                        |> (+) expandedMatrix.DMatrix
                        |> Matrix.inverse
        (baseMatrix |> convertToNegative) * expandedMatrix.RMatrix * x_previous
        + baseMatrix * vector_b
    
let calculatePosteriorError expandedMatrix x_previous x_last =
    let temporaryMatrix = expandedMatrix.LMatrix
                        |> (+) expandedMatrix.DMatrix
                        |> Matrix.inverse
    let baseMatrix = (temporaryMatrix |> convertToNegative) * expandedMatrix.RMatrix
    SimpleIterationMethod.calculatePosteriorError baseMatrix x_previous x_last
    
let performMethod matrix_A x_0 rightVector_b targetError =
    let expandedMatrix = matrix_A |> expandMatrix
    
    let rec loop stepsCounter x_previous =
        let x = performStep expandedMatrix x_previous rightVector_b
        let error = calculatePosteriorError expandedMatrix x_previous x |> abs
        if error.CompareTo(targetError) <= 0 then
            { StepsCount = stepsCounter + 1
              Solution = x
              PosteriorError = error }
        else
            loop (stepsCounter + 1) x
    
    loop 0 x_0