module QRDecomposition.RotationMethod

open FsAlg.Generic
open AuxiliaryOperations

let getRMatrix matrix =
    let rotationTable = matrix |> getRotationTable
    let rec rowMultiplicationLoop acc i =
        let rec elementsMultiplicationLoop innerAcc j =
            if i = j then innerAcc
            else
                elementsMultiplicationLoop (innerAcc * rotationTable[i, j]) (j - 1)
            
        if i = Array2D.length1 rotationTable - 1 then acc
        else
            let newAcc = elementsMultiplicationLoop
                             (rotationTable |> Array2D.length2 |> getSingleDiagonalMatrix)
                             (Array2D.length2 rotationTable - 1)
            rowMultiplicationLoop (newAcc * acc) (i + 1)

    let multiplicationResult = rowMultiplicationLoop
                                   (rotationTable |> Array2D.length1 |> getSingleDiagonalMatrix)
                                   0
    multiplicationResult * matrix

let getQMatrix matrix =
    let inversion'sRotationTable = matrix |> getRotationTable |> Array2D.map Matrix.inverse
    
    let rec rowMultiplicationLoop acc i =
        let rec elementsMultiplicationLoop innerAcc j =
            if j >= (inversion'sRotationTable |> Array2D.length2) then innerAcc
            else
                elementsMultiplicationLoop (innerAcc * inversion'sRotationTable[i, j]) (j + 1)
            
        if i = inversion'sRotationTable.Length - 2 then acc
        else
            let newAcc = elementsMultiplicationLoop
                             (inversion'sRotationTable |> Array2D.length2 |> getSingleDiagonalMatrix)
                             (i + 1)
            rowMultiplicationLoop (acc * newAcc) (i + 1)
    
    rowMultiplicationLoop
        (inversion'sRotationTable |> Array2D.length1 |> getSingleDiagonalMatrix)
        0

let solveSystemUsingQRDecomposition matrix rightVector =
    let qMatrix = getQMatrix matrix
    let rMatrix = getRMatrix matrix
    Matrix.solve rMatrix ((qMatrix |> Matrix.inverse) * rightVector)