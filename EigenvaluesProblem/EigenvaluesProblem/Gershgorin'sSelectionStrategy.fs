module EigenvaluesProblem.Gershgorin'sSelectionStrategy

open JacobiMethod
open FsAlg.Generic
open AuxiliaryOperations

let getMaxGershgorinElementPosition symmetricMatrix (rowSumVector : float Vector) =
    let rowNumber = rowSumVector |> Vector.toSeq |> maxIndex
    let columnNumber =
       symmetricMatrix
       |> Matrix.row rowNumber
       |> Matrix.toSeq
       |> Seq.mapi (fun index element ->
           if index = rowNumber
           then 0.0
           else abs(element))
       |> maxIndex
    rowNumber, columnNumber

let perform (symmetricMatrix : float Matrix) targetError =
    let rec loop previousMatrix rowSumVector stepsCounter =
        if (rowSumVector |> isFinalStep targetError)
        then
            { Eigenvalues = previousMatrix |> Matrix.diagonal
              StepsCount = stepsCounter + 1 }
        else
            let maxElementPosition = getMaxGershgorinElementPosition previousMatrix rowSumVector
            let rotationMatrix = getRotationMatrix maxElementPosition previousMatrix
            let newMatrix = (rotationMatrix |> Matrix.transpose) * previousMatrix * rotationMatrix
              
            loop
                newMatrix
                (newMatrix |> getRowSumVector)
                (stepsCounter + 1)

    loop symmetricMatrix (symmetricMatrix |> getRowSumVector) 0