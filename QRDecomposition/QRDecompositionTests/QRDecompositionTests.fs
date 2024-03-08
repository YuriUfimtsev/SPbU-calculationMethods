module QRDecompositionTests

open QRDecomposition
open RotationMethod
open Matrices

open NUnit.Framework
open FsUnit
open FsAlg.Generic

let areFloatCollectionsEqual zipCollection =
    for pair in zipCollection do
        pair |> fst |> should (equalWithin 0.1) (pair |> snd) 

[<Test>]
let ``Solution of the system with diagonal matrix should be equal to the right vector`` () =
    let matrix = getSingleDiagonalMatrix 10
    let vector = getRandomNumbersVector 10
    solveSystemUsingQRDecomposition matrix vector
      |> Vector.toSeq
      |> Seq.zip (vector |> Vector.toSeq)
      |> areFloatCollectionsEqual

[<Test>]
let ``Solution of the system with tridiagonal matrix should be equal to the FsAlg solution`` () =
    let matrix = getTridiagonalMatrix 10
    let vector = getRandomNumbersVector 10
    let fsAlg'sSolution = Matrix.solve matrix vector
    solveSystemUsingQRDecomposition matrix vector
      |> Vector.toSeq
      |> Seq.zip (fsAlg'sSolution |> Vector.toSeq)
      |> areFloatCollectionsEqual

// Test passes on the small Hilbert matrices.
// As the size increases, the error increases due to the bad conditioning of the Hilbert matrices.
[<Test>]
let ``Solution of the system with hilbert matrix should be equal to the FsAlg solution`` () =
    let matrix = getHilbertMatrix 5
    let vector = getRandomNumbersVector 5
    let fsAlg'sSolution = Matrix.solve matrix vector
    solveSystemUsingQRDecomposition matrix vector
      |> Vector.toSeq
      |> Seq.zip (fsAlg'sSolution |> Vector.toSeq)
      |> areFloatCollectionsEqual
      
[<Test>]
let ``Solution of the arbitrary should be equal to the FsAlg solution`` () =
    let matrix = getRandomNumbersMatrix 10
    let vector = getRandomNumbersVector 10
    let fsAlg'sSolution = Matrix.solve matrix vector
    solveSystemUsingQRDecomposition matrix vector
      |> Vector.toSeq
      |> Seq.zip (fsAlg'sSolution |> Vector.toSeq)
      |> areFloatCollectionsEqual