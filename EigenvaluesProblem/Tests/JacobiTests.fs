module Tests

open EigenvaluesProblem
open JacobiMethod
open NUnit.Framework
open FsUnit
open Matrices
open FsAlg.Generic

[<Test>]
let ``Method should converge`` () =
    let matrix =  getRandomSymmetricMatrix 5
    let fsAlg'sEigenvalues = matrix |> Matrix.eigenvalues
    let targetError = 0.1
    performMaxElementStrategy matrix targetError
    |> ignore

[<Test>]
let ``Method's eigenvalue should be equal to the max FsAlg's eigenvalue within target error`` () =
    //let matrix = getRandomSymmetricMatrix 15

    let matrix = matrix [[4.0; -2 ;2]
                         [-2; 5; 0]
                         [2; 0; 6]]
    let fsAlg'sEigenvalues = matrix |> Matrix.eigenvalues
    let fsAlg'sEigenvaluesNorm = fsAlg'sEigenvalues |> Vector.l2norm

    let targetError = 0.1
    let actualResult = performMaxElementStrategy matrix targetError

    actualResult.Eigenvalues |> Vector.l2norm |> should (equalWithin targetError) fsAlg'sEigenvaluesNorm

// [<Test>]
// let ``Method's error should be less than target error`` () =
//     let matrix = getRandomNumbersMatrix 15
//
//     let initialVector = getRandomNumbersVector 15
//     let targetError = 0.001
//     let actualResult = PowerIteration.perform matrix initialVector targetError
//
//     actualResult.PosteriorError |> should lessThan targetError
