module EigenvaluesProblem.JacobiMethod

open System.Linq.Expressions
open FsAlg.Generic
open Matrices
open System
open AuxiliaryOperations

type JacobiResult =
    { Eigenvalues : float Vector
      StepsCount : int }

let calculate_x position symmetricMatrix =
    let i, j = position
    -2.0 * symmetricMatrix[i, j]
    
let calculate_y position symmetricMatrix =
    let i, j = position
    symmetricMatrix[i, i] - symmetricMatrix [j, j]
    
let calculateCos position symmetricMatrix =
    let x = calculate_x position symmetricMatrix
    let y = calculate_y position symmetricMatrix
    if (y = 0) then 1.0 / sqrt(2.0)
    else
        y
        |> abs
        |> (/) (x ** 2 + y ** 2 |> sqrt)
        |> (+) 1.0
        |> (*) 0.5
        |> sqrt

let calculateSin position symmetricMatrix =
    let x = calculate_x position symmetricMatrix
    let y = calculate_y position symmetricMatrix
    if (y = 0) then 1.0 / sqrt(2.0)
    else
        (sign (x * y) |> float) * (x |> abs)
        |> (/) (2.0 * (x ** 2 + y ** 2 |> sqrt) * calculateCos position symmetricMatrix)

let performStep position matrix =
    

let updateRowSumVector position matrix previousVector =


let initializeRowSumVector matrix =


let isFinalStep targetError (rowSumVector : float Vector) =
    rowSumVector
    |> Vector.exists (fun rowSum -> rowSum.CompareTo(targetError) < 0)    

let getMaxElementPosition symmetricMatrix (rowSumVector : float Vector) =
    let rowNumber = rowSumVector |> Vector.toSeq |> maxIndex
    let columnNumber = symmetricMatrix |> Matrix.row rowNumber |> Matrix.toSeq |> maxIndex
    if (rowNumber < columnNumber)
    then rowNumber, columnNumber
    else columnNumber, rowNumber

let performMaxElementStrategy (symmetricMatrix : float Matrix) targetError =
    let rec loop previousMatrix rowSumVector stepsCounter =
        if (rowSumVector |> isFinalStep targetError)
        then
            { Eigenvalues = previousMatrix |> Matrix.diagonal
              StepsCount = stepsCounter + 1 }
        else
            let maxElementPosition = getMaxElementPosition previousMatrix rowSumVector
            let newMatrix = previousMatrix |> performStep maxElementPosition
            loop
                newMatrix
                (updateRowSumVector maxElementPosition newMatrix rowSumVector)
                (stepsCounter + 1)

    loop symmetricMatrix (symmetricMatrix |> initializeRowSumVector) 0