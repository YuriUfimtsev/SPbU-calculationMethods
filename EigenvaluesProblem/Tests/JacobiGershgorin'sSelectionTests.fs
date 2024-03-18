module Tests.JacobiGershgorin_sSelectionTests

open EigenvaluesProblem
open JacobiMethod
open NUnit.Framework
open Matrices
open Gershgorin'sSelectionStrategy
open JacobiTests
open FsAlg.Generic
open FsUnit

[<Test>]
let ``Method based on max Gershgorin selection strategy should converge`` () =
    let matrix =  getRandomSymmetricMatrix 10
    let targetError = 0.1
    perform matrix targetError
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
    let actualResult = perform matrix targetError
    let sortedActualEigenvalues =
        actualResult.Eigenvalues
        |> Vector.toSeq
        |> Seq.sort

    sortedFsAlg'sEigenvalues
    |> Seq.zip sortedActualEigenvalues
    |> areFloatCollectionsEqual targetError
    
[<Test>]
let ``Method's eigenvalues should be in Gershgorin's circles`` () =
    let matrix = getRandomSymmetricMatrix 5
    
    let targetError = 0.01
    let actualResult = perform matrix targetError
    let resultEigenvalues = actualResult.Eigenvalues
    
    let Gershgorin'sField = matrix |> getGershgorin'sField
    
    resultEigenvalues
    |> Vector.exists (doesBelongToGershgorin'sField Gershgorin'sField >> not)
    |> should be False