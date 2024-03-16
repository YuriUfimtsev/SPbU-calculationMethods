module Tests

open EigenvaluePartialProblem
open FsAlg.Generic
open NUnit.Framework
open FsUnit
open Matrices

[<Test>]
let ``Method should converge on a matrix ...`` () =
    let matrix = getRandomNumbersMatrix 10
    let initialVector = getRandomNumbersVector 10
    let targetError = 0.1
    PowerMethod.getEigenPair matrix initialVector targetError
    |> ignore

[<Test>]
let ``Method's eigenvalue should be equal to the max FsAlg's eigenvalue within target error`` () =
    let matrix = getRandomNumbersMatrix 15
    
    let fsAlg'sEigenValue = matrix |> Matrix.eigenvalues |> Vector.max

    let initialVector = getRandomNumbersVector 15
    let targetError = 0.001
    let actualResult = PowerMethod.getEigenPair matrix initialVector targetError

    actualResult.Eigenvalue |> should (equalWithin targetError) fsAlg'sEigenValue

[<Test>]
let ``Method's error should be less than target error`` () =
    let matrix = getRandomNumbersMatrix 15

    let initialVector = getRandomNumbersVector 15
    let targetError = 0.001
    let actualResult = PowerMethod.getEigenPair matrix initialVector targetError

    actualResult.PosteriorError |> should lessThan targetError
