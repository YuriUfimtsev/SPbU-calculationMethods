module IterativeMethods.AuxiliaryOperations

open FsAlg.Generic

type ComparisonResult =
    | StrictlyMore = 0
    | Equals = 1
    | StrictlyLess = 2
    
type RegularMatrix =
    { LMatrix : float Matrix;
      DMatrix : float Matrix;
      RMatrix : float Matrix }

let calculateEuclideanNorm =
    Matrix.toArray
    >> Seq.map (fun x -> x * x)
    >> Seq.fold (+) 0.0
    >> sqrt

let checkForDiagonalDominance matrix =
    if Matrix.length1 matrix <> Matrix.length2 matrix
    then invalidArg "matrix" "Expected the square matrix"
    
    let dimension = matrix |> Matrix.length1
    let checkRowDominance rowNumber =
        let diagonalElement = matrix[rowNumber, rowNumber] |> abs
        let rowSum = matrix
                     |> Matrix.row rowNumber
                     |> Matrix.map abs
                     |> Matrix.toSeq
                     |> Seq.sum
                     |> (-) diagonalElement
                     |> abs
        match diagonalElement - rowSum with
        | number when number > 0.0 -> ComparisonResult.StrictlyMore
        | number when number = 0.0 -> ComparisonResult.Equals
        | _ -> ComparisonResult.StrictlyLess
        
    let rec checkDominanceLoop acc  i =
        if i = dimension then acc
        else
            checkDominanceLoop ((checkRowDominance i) :: acc) (i + 1)
    
    let comparisonResults = checkDominanceLoop [] 0
    if List.contains ComparisonResult.StrictlyLess comparisonResults then false
    elif not (List.contains ComparisonResult.StrictlyMore comparisonResults) then false
    else true
    

let convertSystem matrix_A rightVector_b =
    if matrix_A |> checkForDiagonalDominance |> not
    then invalidArg "matrix" "Expected the diagonal dominance matrix"
    
    let matrix_B = Matrix.init (Matrix.length1 matrix_A) (Matrix.length1 matrix_A)
                       (fun i j ->
                        if i <> j then -1.0 * matrix_A[i, j] / matrix_A[i, i]
                        else 0.0)
    
    let vector_c = Vector.init (Vector.length rightVector_b)
                    (fun i -> rightVector_b[i] / matrix_A[i, i])
    
    (matrix_B, vector_c)

let getLMatrix matrix =
    Matrix.init
      (matrix |> Matrix.length1)
      (matrix |> Matrix.length2)
      (fun i j -> if i < j then matrix[i, j] else 0.0)
      
let getDMatrix matrix =
    Matrix.init
      (matrix |> Matrix.length1)
      (matrix |> Matrix.length2)
      (fun i j -> if i = j then matrix[i, j] else 0.0)
      
let getRMatrix matrix =
    Matrix.init
      (matrix |> Matrix.length1)
      (matrix |> Matrix.length2)
      (fun i j -> if i > j then matrix[i, j] else 0.0)
      
let convertToNegative matrix =
    Matrix.init
        (matrix |> Matrix.length1)
        (matrix |> Matrix.length2)
        (fun i j -> -1.0 * matrix[i, j])
