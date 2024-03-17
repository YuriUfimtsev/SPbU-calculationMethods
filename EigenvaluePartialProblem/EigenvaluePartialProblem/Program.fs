module EigenvaluePartialProblem.Program

open Plotly.NET
open Matrices
open System

let targetErrors = [1e-1; 1e-2; 1e-3; 1e-4; 1e-8; 1e-12; 1e-15; 1e-20; 1e-25]

let header = [ "Method's name" ] @ List.map (fun x -> $"ε = {x}") targetErrors

let emptyRow length = Seq.init length (fun _ -> String.Empty) |> Seq.toList

let scalarProductRow matrix initialPair title =
    let stepsCountList =
        Seq.init
            (targetErrors |> List.length)
            (fun i ->
                targetErrors[i] |> ScalarProductIteration.perform matrix initialPair)
                |> Seq.map (fun iterationResult -> $"{iterationResult.StepsCount}")
                |> Seq.toList
    title :: stepsCountList

let powerIterationRow matrix initialVector title =
    let stepsCountList =
        Seq.init
            (targetErrors |> List.length)
            (fun i ->
                targetErrors[i] |> PowerIteration.perform matrix initialVector)
                |> Seq.map (fun iterationResult -> $"{iterationResult.StepsCount}")
                |> Seq.toList
    title :: stepsCountList

let randomMatrix = getRandomNumbersMatrix 100
let randomLargeMatrix = getRandomNumbersMatrix 300
let sparseMatrix = getSparseMatrix 300 30
let hilbertMatrix = getHilbertMatrix 15

let rows =
    [ powerIterationRow randomMatrix (getRandomNumbersVector 100) "Power's method steps. Random matrix (100x100)"
      scalarProductRow randomMatrix { Vector_x = getRandomNumbersVector 100 ; Vector_y = (getRandomNumbersVector 100) }
          "Scalar method steps. Random matrix (100x100)"
      emptyRow header.Length
      powerIterationRow randomLargeMatrix (getRandomNumbersVector 300) "Power's method steps. Random matrix (300x300)"
      scalarProductRow randomLargeMatrix { Vector_x = getRandomNumbersVector 300 ; Vector_y = (getRandomNumbersVector 300) }
          "Scalar method steps. Random matrix (300x300)"
      emptyRow header.Length
      powerIterationRow hilbertMatrix (getRandomNumbersVector 15) "Power's method steps. Hilbert matrix (15x15)"
      scalarProductRow hilbertMatrix { Vector_x = (getRandomNumbersVector 15); Vector_y = (getRandomNumbersVector 15) }
          "Scalar method steps. Hilbert matrix (15x15)"
      emptyRow header.Length
      powerIterationRow sparseMatrix (getRandomNumbersVector 300) "Power's method steps. Sparse matrix (300x300)"
      scalarProductRow sparseMatrix { Vector_x = (getRandomNumbersVector 300); Vector_y = (getRandomNumbersVector 300) }
          "Scalar method steps. Sparse matrix (300x300)"
      ] 

Chart.Table(header, rows) |> Chart.withSize (1300, 1500) |> Chart.show
