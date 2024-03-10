module IterativeMethods.Matrices

open System
open FsAlg.Generic

let getRandomNumbersMatrix dimension =
    let random = Random();
    Matrix.init dimension dimension (fun _ _ -> random.NextDouble() * 1000.0)

let getRandomDiagonalMatrix dimension =
    Matrix.init dimension dimension (fun _ _ -> Random().NextDouble() * 1000.0)

let getSingleDiagonalMatrix dimension =
    Matrix.init dimension dimension (fun i j -> if i = j then 1.0 else 0.0)

let getRandomDiagonalDominanceMatrix dimension =
    let random = Random()
    let diagonalElements = Seq.init dimension
                                (fun _ -> random.NextDouble() * 10000.0)
                           |> Seq.toArray
    Matrix.init dimension dimension
        (fun i j ->
        if i = j
        then diagonalElements[i]
        else (/) diagonalElements[i] (float(dimension)))
    
let getRandomNumbersVector dimension =
    let random = Random();
    Vector.init dimension (fun _ -> random.NextDouble() * 1000.0)


// Matrix from Lebedeva&Pakulina document, variant 5
let floatThreeDimensionalMatrix () = matrix [[8.673134; 1.041039; -2.677712];
                                          [1.041039; 6.586211; 0.623016];
                                          [-2.677712; 0.623016; 5.225935]]

// Vector from Lebedeva&Pakulina document, variant 5
let floatThreeDimensionalVector () = vector [-1.289879; 4.020225; 5.269671]

let cubesVector dimension = Vector.init dimension (fun i -> float(i) ** 3)
