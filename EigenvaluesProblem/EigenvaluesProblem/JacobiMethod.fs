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
        ((((y |> abs)
        /
        ((x ** 2 + y ** 2) |> sqrt))
        |> (+) 1.0)
        |> (*) 0.5)
        |> sqrt

let calculateSin position (symmetricMatrix : float Matrix) =
    let x = calculate_x position symmetricMatrix
    let y = calculate_y position symmetricMatrix
    if (y = 0) then 1.0 / sqrt(2.0)
    else
        (((sign (x * y)) |> float) * (x |> abs))
        /
        (2.0 * ((x ** 2 + y ** 2) |> sqrt) * (calculateCos position symmetricMatrix))
        
let getRotationMatrix position (symmetricMatrix : float Matrix) =
    let i, j = position
    let cos = calculateCos position symmetricMatrix
    let sin = calculateSin position symmetricMatrix
    
    Matrix.init (symmetricMatrix |> Matrix.rows) (symmetricMatrix |> Matrix.rows)
        (fun localI localJ ->
            match localI, localJ with
            | x, y when x = y ->
                match x with
                | index when index = i || index = j -> cos
                | _ -> 1.0
            | x, y when x = i && y = j -> (-1.0 * sin)
            | x, y when x = j && y = i -> sin
            | _ -> 0.0)

let getRowSumVector matrix =
    Vector.init (matrix |> Matrix.rows)
        (fun i ->
        matrix[i, i]
        |> abs
        |> (-) (matrix |> Matrix.row i |> Matrix.toVector |> Vector.map abs |> Vector.sum))


let isFinalStep targetError (rowSumVector : float Vector) =
    rowSumVector
    |> Vector.exists (fun rowSum -> rowSum.CompareTo(targetError) > 0)
    |> not

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

let performMaxGershgorinElementStrategy (symmetricMatrix : float Matrix) targetError =
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


let performMaxSelectionStrategy (symmetricMatrix : float Matrix) targetError =
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
