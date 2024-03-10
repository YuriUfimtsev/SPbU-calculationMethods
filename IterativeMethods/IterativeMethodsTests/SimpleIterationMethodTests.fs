module IterativeMethodsTests.SimpleIterationMethodTests

open IterativeMethods
open NUnit.Framework
open FsUnit
open FsAlg.Generic
open Matrices
open AuxiliaryOperations
open AuxiliaryOperationsTests

[<Test>]
let ``Method should converge on a matrix with diagonal dominance`` () =
    let matrix_A = getRandomDiagonalDominanceMatrix 10
    let rightVector_b = getRandomNumbersVector 10
    let matrix_B, vector_c = convertSystem matrix_A rightVector_b
    let solution_0 = getRandomNumbersVector 10
    let targetError = 0.1
    SimpleIterationMethod.performMethod matrix_B solution_0 vector_c targetError
    |> ignore
    

[<Test>]
let ``Method's answer should be equal to the FsAlg solution within target error`` () =
    let matrix_A = getRandomDiagonalDominanceMatrix 15
    let rightVector_b = getRandomNumbersVector 15
    let fsAlg'sSolution = Matrix.solve matrix_A rightVector_b
    
    let matrix_B, vector_c = convertSystem matrix_A rightVector_b
    let solution_0 = getRandomNumbersVector 15
    let targetError = 0.001
    let actualResult = SimpleIterationMethod.performMethod matrix_B solution_0 vector_c targetError
    
    actualResult.Solution
    |> Vector.toSeq
    |> Seq.zip (fsAlg'sSolution |> Vector.toSeq)
    |> areFloatCollectionsEqual targetError
    
[<Test>]
let ``Method's error should be less than target error`` () =
    let matrix_A = getRandomDiagonalDominanceMatrix 15
    let rightVector_b = getRandomNumbersVector 15
    
    let matrix_B, vector_c = convertSystem matrix_A rightVector_b
    let solution_0 = getRandomNumbersVector 15
    let targetError = 0.001
    let actualResult = SimpleIterationMethod.performMethod matrix_B solution_0 vector_c targetError
    
    actualResult.PosteriorError |> should lessThan targetError    