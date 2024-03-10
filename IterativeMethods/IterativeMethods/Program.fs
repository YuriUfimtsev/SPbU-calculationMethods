module IterativeMethods.Program

open Plotly.NET
open Matrices
open AuxiliaryOperations
open System

let targetErrors = [1e-1; 1e-2; 1e-3; 1e-4; 1e-8; 1e-12; 1e-13;]

let header = [ "Method's name" ] @ List.map (fun x -> $"ε = {x}") targetErrors

let emptyRow length = Seq.init length (fun _ -> String.Empty) |> Seq.toList

let simpleIterationRow matrix rightVector start_solution title =
    let matrix_B, vector_c = convertSystem matrix rightVector
    let stepsCountList = Seq.init
                            (targetErrors |> List.length)
                            (fun i ->
                                targetErrors[i]|> SimpleIterationMethod.performMethod matrix_B start_solution vector_c)
                                |> Seq.map (fun iterationResult -> $"{iterationResult.StepsCount}")
                                |> Seq.toList
    title :: stepsCountList

let Seidel'sRow matrix rightVector start_solution title =
    let stepsCountList = Seq.init
                            (targetErrors |> List.length)
                            (fun i ->
                                targetErrors[i]|> Seidel_sMethod.performMethod matrix start_solution rightVector)
                                |> Seq.map (fun iterationResult -> $"{iterationResult.StepsCount}")
                                |> Seq.toList
    title :: stepsCountList

let standardMatrix = getRandomDiagonalDominanceMatrix 15
let standardVector = getRandomNumbersVector 15
let start_solution = getRandomNumbersVector 15

let complexMatrix = getSymmetricSparseDiagonalDominanceMatrix 300
let complexVector = getRandomNumbersVector 300
let complexSolution = getRandomNumbersVector 300

let rows =
    [ simpleIterationRow standardMatrix standardVector start_solution "Simple iteration method steps (15x15)"
      Seidel'sRow standardMatrix standardVector start_solution "Seidel's method  steps (15x15)"
      emptyRow header.Length
      simpleIterationRow complexMatrix complexVector complexSolution "Simple iteration method steps (300x300)"
      Seidel'sRow complexMatrix complexVector complexSolution "Seidel's method  steps (300x300)"
      ] 

Chart.Table(header, rows) |> Chart.withSize (1000, 1500) |> Chart.show
