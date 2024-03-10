module IterativeMethods.RelaxationMethod

open FsAlg.Generic
open AuxiliaryOperations

let getResiduals (previousSolution : float Vector) (currentSolution : float List) (matrix_A : float Matrix) vector_b =
    let residualCalculator j =
        let firstSummand =
            Seq.init
                 (currentSolution |> List.length)
                 (fun index -> (*) matrix_A[j, index] currentSolution[index])
            |> Seq.sum
        let secondSummand =
            Seq.init
                (vector_b |> Vector.length)
                (fun index ->
                if index >= (currentSolution |> List.length)
                then matrix_A[j, index] * previousSolution[j]
                else 0.0)
            |> Seq.sum
        firstSummand + secondSummand - vector_b[j]
        |> abs
            
    Vector.init
        (vector_b |> Vector.length)
        residualCalculator

let getSolutionComponent (previousSolution : float Vector) (currentSolution : float List) (matrix_A : float Matrix) vector_b equationNumber =
    let firstSummand =
        Seq.init
             (currentSolution |> List.length)
             (fun index -> (*) matrix_A[equationNumber, index] currentSolution[index])
        |> Seq.sum
    let secondSummand =
        Seq.init
            (vector_b |> Vector.length)
            (fun index ->
            if index > (currentSolution |> List.length)
            then matrix_A[equationNumber, index] * previousSolution[index]
            else 0.0)
        |> Seq.sum
    (/)
        (vector_b[equationNumber] - firstSummand - secondSummand)
        (matrix_A[equationNumber, (currentSolution |> List.length)])

let performStep matrix_A rightVector_b x_previous =
    
    let rec loop usedEquations resultVector =
        if (resultVector |> List.length) = (rightVector_b |> Vector.length)
        then resultVector
        else
            let residuals = getResiduals x_previous resultVector matrix_A rightVector_b
            let correctResiduals =
                Seq.init
                    (residuals |> Vector.length)
                    (fun i -> if (usedEquations |> List.contains i)
                                then 0.0
                                else residuals[i])
                |> Vector.ofSeq // not essential. Check how works without this step
            let equationNumber = correctResiduals |> Vector.findIndex ((=) (correctResiduals |> Vector.max))
            let solutionComponent = getSolutionComponent x_previous resultVector matrix_A rightVector_b equationNumber
            loop (equationNumber :: usedEquations) (resultVector @ [solutionComponent])
    
    loop [] []
    |> Vector.ofSeq

let getPosteriorError (solution : float Vector) (matrix_A : float Matrix) rightVector_b =
    let firstResidualCalculator i =
        Seq.init
            (rightVector_b |> Vector.length)
            (fun index ->
            matrix_A[i, index] * solution[index])
        |> Seq.sum
        |> (-) rightVector_b[i]
        |> abs
        
    Vector.init
        (rightVector_b |> Vector.length)
        firstResidualCalculator
    |> Vector.l2norm


let performMethod matrix_A rightVector_b startSolution targetError =
    let rec loop stepsCounter x_previous =
        let currentSolution = performStep matrix_A rightVector_b x_previous
        let error = getPosteriorError currentSolution matrix_A rightVector_b
        if error.CompareTo(targetError) <= 0 then
            { StepsCount = stepsCounter + 1
              Solution = currentSolution
              PosteriorError = error }
        else
            loop (stepsCounter + 1) currentSolution
    
    loop 0 startSolution