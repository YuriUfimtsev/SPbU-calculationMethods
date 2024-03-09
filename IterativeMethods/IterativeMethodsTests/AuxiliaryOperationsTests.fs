module IterativeMethodsTests

open IterativeMethods
open NUnit.Framework
open FsUnit
open FsAlg.Generic
open IterativeMethods.AuxiliaryOperations
open IterativeMethods.Seidel_sMethod

let areFloatCollectionsEqual zipCollection =
    for pair in zipCollection do
        pair |> fst |> should (equalWithin 0.1) (pair |> snd) 


[<Test>]
let ``Check for diagonal dominance on nonDiagonalDominanceMatrix should be false`` () =
    let nonDiagonalDominanceMatrix = matrix [[1.0; 5.0; 0.0];
                                            [5.0; 1.0; 1.0];
                                            [2.0; 1.0; 1.0]]
    nonDiagonalDominanceMatrix |> checkForDiagonalDominance |> should be False
    
[<Test>]
let ``Check for diagonal dominance on specialNonDiagonalDominanceMatrix should be false`` () =
    let specialNonDiagonalDominanceMatrix = matrix [[4.0; 2.0; 2.0];
                                                  [6.0; 6.0; 0.0];
                                                  [4.0; 4.0; 8.0]]
    specialNonDiagonalDominanceMatrix |> checkForDiagonalDominance |> should be False
    
[<Test>]
let ``Check for diagonal dominance on diagonalDominanceMatrix should be true`` () =
    let diagonalDominanceMatrix = matrix [[5.0; 2.0; 2.0];
                                         [6.0; 6.0; 0.0];
                                         [4.0; 4.0; 8.0]]
    diagonalDominanceMatrix |> checkForDiagonalDominance |> should be True
    
[<Test>]
let ``Sum of the LDR-decomposition components for arbitrary matrix should be equal to arbitrary matrix`` () =
    let arbitraryMatrix = matrix [[5.0; 2.0; 2.0];
                                  [6.0; 6.0; 0.0];
                                  [4.0; 4.0; 8.0]]
    let decomposition = arbitraryMatrix |> expandMatrix
    decomposition.LMatrix + decomposition.DMatrix + decomposition.RMatrix
    |> Matrix.toSeq
    |> Seq.zip (arbitraryMatrix |> Matrix.toSeq)
    |> areFloatCollectionsEqual