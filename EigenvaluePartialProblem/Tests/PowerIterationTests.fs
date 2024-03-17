module Tests.PowerIterationTests

open EigenvaluePartialProblem
open FsAlg.Generic
open NUnit.Framework
open FsUnit
open Matrices

let areFloatCollectionsEqual targetError zipCollection =
    for pair in zipCollection do
        pair |> fst |> should (equalWithin targetError) (pair |> snd) 

[<Test>]
let ``Method should converge`` () =
    let matrix = getRandomNumbersMatrix 10
    let initialVector = getRandomNumbersVector 10
    let targetError = 0.1
    PowerIteration.perform matrix initialVector targetError
    |> ignore

[<Test>]
let ``Method's eigenvalue should be equal to the max FsAlg's eigenvalue within target error`` () =
    let matrix = getRandomNumbersMatrix 15
    
    let fsAlg'sEigenValue = matrix |> Matrix.eigenvalues |> Vector.max

    let initialVector = getRandomNumbersVector 15
    let targetError = 0.001
    let actualResult = PowerIteration.perform matrix initialVector targetError

    actualResult.Eigenvalue |> should (equalWithin targetError) fsAlg'sEigenValue

[<Test>]
let ``Method's error should be less than target error`` () =
    let matrix = getRandomNumbersMatrix 15

    let initialVector = getRandomNumbersVector 15
    let targetError = 0.001
    let actualResult = PowerIteration.perform matrix initialVector targetError

    actualResult.PosteriorError |> should lessThan targetError

[<Test>]
let ``Method's eigenvector should be correct within error (substitution check)`` () =
    let matrix = getRandomNumbersMatrix 15

    let initialVector = getRandomNumbersVector 15
    let targetError = 0.001
    let actualResult = PowerIteration.perform matrix initialVector targetError
    
    let rightVector = actualResult.Eigenvector * actualResult.Eigenvalue
    let leftVector = matrix * actualResult.Eigenvector
    let numerator = rightVector - leftVector |> Vector.l2norm
    let denominator = leftVector |> Vector.l2norm
    
    numerator / denominator |> should lessThan 0.01
