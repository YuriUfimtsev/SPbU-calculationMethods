module EigenvaluesProblem.Program

open Plotly.NET
open Matrices
open System

let targetErrors = [1e-1; 1e-2; 1e-3]

let header = [ "Method's name" ] @ List.map (fun x -> $"ε = {x}") targetErrors

let emptyRow length = Seq.init length (fun _ -> String.Empty) |> Seq.toList

let maxStrategyRow symmetricMatrix title =
    let stepsCountList =
        Seq.init
            (targetErrors |> List.length)
            (fun i ->
                targetErrors[i] |> MaxSelectionStrategy.perform symmetricMatrix)
                |> Seq.map (fun jacobiResult -> $"{jacobiResult.StepsCount}")
                |> Seq.toList
    title :: stepsCountList

let Gershgorin'sStrategyRow symmetricMatrix title =
    let stepsCountList =
        Seq.init
            (targetErrors |> List.length)
            (fun i ->
                targetErrors[i] |> Gershgorin'sSelectionStrategy.perform symmetricMatrix)
                |> Seq.map (fun jacobiResult -> $"{jacobiResult.StepsCount}")
                |> Seq.toList
    title :: stepsCountList

let randomMatrix = getRandomSymmetricMatrix 10
let randomSparseMatrix = getSymmetricSparseDiagonalDominanceMatrix 10
let hilbertMatrix = getHilbertMatrix 10

let rows =
    [ Gershgorin'sStrategyRow randomMatrix "Gershgorin's strategy steps. Random symmetric matrix (10x10)"
      maxStrategyRow randomMatrix "Max strategy steps. Random symmetric matrix (10x10)"
      emptyRow header.Length
      Gershgorin'sStrategyRow hilbertMatrix "Gershgorin's strategy steps. Hilbert matrix (10x10)"
      maxStrategyRow hilbertMatrix "Max strategy steps. Hilbert matrix (10x10)"
      emptyRow header.Length
      Gershgorin'sStrategyRow randomSparseMatrix "Gershgorin's strategy steps. Sparse matrix (10x10)"
      maxStrategyRow randomSparseMatrix "Max strategy steps. Sparse matrix (10x10)"
      ] 

Chart.Table(header, rows) |> Chart.withSize (800, 1500) |> Chart.show
