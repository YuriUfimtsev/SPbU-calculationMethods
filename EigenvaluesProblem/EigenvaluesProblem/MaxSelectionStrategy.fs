module EigenvaluesProblem.MaxSelectionStrategy

open JacobiMethod
open FsAlg.Generic
open AuxiliaryOperations

let getMaxElementPosition symmetricMatrix =
    let matrixDimension = symmetricMatrix |> Matrix.rows
    let seqPositionIndex =
        symmetricMatrix
        |> Matrix.toSeq
        |> Seq.mapi (fun index element ->
           if index % (matrixDimension + 1) = 0
           then 0.0
           else abs(element))
        |> maxIndex
    seqPositionIndex / matrixDimension, seqPositionIndex % matrixDimension

let perform (symmetricMatrix : float Matrix) targetError =
    let rec loop previousMatrix rowSumVector stepsCounter =
        if (rowSumVector |> isFinalStep targetError)
        then
            { Eigenvalues = previousMatrix |> Matrix.diagonal
              StepsCount = stepsCounter + 1 }
        else
            let maxPosition = getMaxElementPosition previousMatrix
            let rotationMatrix = getRotationMatrix maxPosition previousMatrix
            let newMatrix = (rotationMatrix |> Matrix.transpose) * previousMatrix * rotationMatrix
              
            loop
                newMatrix
                (newMatrix |> getRowSumVector)
                (stepsCounter + 1)

    loop symmetricMatrix (symmetricMatrix |> getRowSumVector) 0
