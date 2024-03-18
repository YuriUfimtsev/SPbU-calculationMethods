module Tests.JacobiMaxSelectionTests

open EigenvaluesProblem
open JacobiMethod
open NUnit.Framework
open MaxSelectionStrategy
open Matrices
open JacobiTests
open FsAlg.Generic

[<Test>]
let ``Method based on max selection strategy should converge`` () =
    let matrix = getRandomSymmetricMatrix 10
    let targetError = 0.1
    perform matrix targetError
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
    let actualResult = perform matrix targetError
    let sortedActualEigenvalues =
        actualResult.Eigenvalues
        |> Vector.toSeq
        |> Seq.sort

    sortedFsAlg'sEigenvalues
    |> Seq.zip sortedActualEigenvalues
    |> areFloatCollectionsEqual 0.1