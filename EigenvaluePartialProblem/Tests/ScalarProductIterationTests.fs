module Tests.ScalarProductIterationTests

open EigenvaluePartialProblem
open FsAlg.Generic
open NUnit.Framework
open FsUnit
open Matrices
open ScalarProductIteration

[<Test>]
let ``Method should converge`` () =
    let matrix = getRandomNumbersMatrix 10
    let initialPair =
        { Vector_x = getRandomNumbersVector 10
          Vector_y = getRandomNumbersVector 10 }
    let targetError = 0.1
    
    ScalarProductIteration.getEigenPair matrix initialPair targetError
    |> ignore

[<Test>]
let ``Method's eigenvalue should be equal to the max FsAlg's eigenvalue within target error`` () =
    let matrix = getRandomNumbersMatrix 15
    
    let fsAlg'sEigenValue = matrix |> Matrix.eigenvalues |> Vector.max

    let initialPair =
        { Vector_x = getRandomNumbersVector 15
          Vector_y = getRandomNumbersVector 15 }
    let targetError = 0.001
    
    let actualResult = ScalarProductIteration.getEigenPair matrix initialPair targetError
    actualResult.Eigenvalue |> should (equalWithin targetError) fsAlg'sEigenValue

[<Test>]
let ``Method's error should be less than target error`` () =
    let matrix = getRandomNumbersMatrix 15
    let initialPair =
        { Vector_x = getRandomNumbersVector 15
          Vector_y = getRandomNumbersVector 15 }
    let targetError = 0.001
    
    let actualResult = ScalarProductIteration.getEigenPair matrix initialPair targetError
    actualResult.PosteriorError |> should lessThan targetError
