module EigenvaluesProblem.JacobiMethod

open System.Linq.Expressions
open FsAlg.Generic
open Matrices
open System
open AuxiliaryOperations

type JacobiResult =
    { Eigenvalues : float Vector
      StepsCount : int }

let calculate_x position (symmetricMatrix : float Matrix) =
    let i, j = position
    -2.0 * symmetricMatrix[i, j]
    
let calculate_y position (symmetricMatrix : float Matrix) =
    let i, j = position
    symmetricMatrix[i, i] - symmetricMatrix[j, j]
    
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

let calculateSin position (symmetricMatrix : float Matrix) =
    let x = calculate_x position symmetricMatrix
    let y = calculate_y position symmetricMatrix
    if (y = 0) then 1.0 / sqrt(2.0)
    else
        (sign (x * y) |> float) * (x |> abs)
        |> (/) (2.0 * (x ** 2 + y ** 2 |> sqrt) * calculateCos position symmetricMatrix)

let performStep position (symmetricMatrix : float Matrix) =
    let pos_i, pos_j = position
    let sin = calculateSin position symmetricMatrix
    let cos = calculateCos position symmetricMatrix
    
    Matrix.initSymmetric (symmetricMatrix |> Matrix.rows)
        (fun i j ->
        if i = pos_i && j = pos_j then 0.0
        elif i = pos_i && j = pos_i then
            (cos ** 2) * symmetricMatrix[i, i]
            + 2.0 * cos * sin * symmetricMatrix[i, j]
            + (sin ** 2) * symmetricMatrix[j, j]
        elif i = pos_j && j = pos_j then
            (sin ** 2) * symmetricMatrix[i, i]
            - 2.0 * cos * sin * symmetricMatrix[i, j]
            + (cos ** 2) * symmetricMatrix[j, j]
        elif i = pos_j then
            - 1.0 * sin * symmetricMatrix[j, pos_i] + cos * symmetricMatrix[j, pos_j]
        elif j = pos_j then
            - 1.0 * sin * symmetricMatrix[i, pos_i] + cos * symmetricMatrix[i, pos_j]
        elif i = pos_i then
            cos * symmetricMatrix[j, pos_i] + sin * symmetricMatrix[j, pos_j]
        elif j = pos_i then
            cos * symmetricMatrix[i, pos_i] + sin * symmetricMatrix[i, pos_j]
        else
            symmetricMatrix[i, j])

let getRowSumVector matrix =
    Vector.init (matrix |> Matrix.rows)
        (fun i -> matrix |> Matrix.row i |> Matrix.toVector |> Vector.sum |> (-) matrix[i, i])

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
                (newMatrix |> getRowSumVector)
                (stepsCounter + 1)

    loop symmetricMatrix (symmetricMatrix |> getRowSumVector) 0