module Tests

open EigenvaluesProblem
open JacobiMethod
open NUnit.Framework
open FsUnit
open Matrices
open FsAlg.Generic

let areFloatCollectionsEqual targetError zipCollection =
    for pair in zipCollection do
        pair |> fst |> should (equalWithin targetError) (pair |> snd)

[<Test>]
let ``Method based on max Gershgorin selection strategy should converge`` () =
    let matrix =  getRandomSymmetricMatrix 10
    let targetError = 0.1
    performMaxGershgorinElementStrategy matrix targetError
    |> ignore

[<Test>]
let ``Method's (max Gershgorin selection) eigenvalue should be equal to the max FsAlg's eigenvalue within target error`` () =
    let matrix = getRandomSymmetricMatrix 5

    let sortedFsAlg'sEigenvalues =
        matrix
        |> Matrix.eigenvalues
        |> Vector.toSeq
        |> Seq.sort

    let targetError = 0.01
    let actualResult = performMaxGershgorinElementStrategy matrix targetError
    let sortedActualEigenvalues =
        actualResult.Eigenvalues
        |> Vector.toSeq
        |> Seq.sort

    sortedFsAlg'sEigenvalues
    |> Seq.zip sortedActualEigenvalues
    |> areFloatCollectionsEqual 0.1
    

[<Test>]
let ``Method based on max selection strategy should converge`` () =
    let matrix = getRandomSymmetricMatrix 10
    let targetError = 0.1
    performMaxSelectionStrategy matrix targetError
    |> ignore

[<Test>]
let ``Method's (max strategy) eigenvalue should be equal to the max FsAlg's eigenvalue within target error`` () =
    let matrix = getRandomSymmetricMatrix 5

    let sortedFsAlg'sEigenvalues =
        matrix
        |> Matrix.eigenvalues
        |> Vector.toSeq
        |> Seq.sort

    let targetError = 0.01
    let actualResult = performMaxSelectionStrategy matrix targetError
    let sortedActualEigenvalues =
        actualResult.Eigenvalues
        |> Vector.toSeq
        |> Seq.sort

    sortedFsAlg'sEigenvalues
    |> Seq.zip sortedActualEigenvalues
    |> areFloatCollectionsEqual 0.1