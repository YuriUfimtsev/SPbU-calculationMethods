module ConditionNumbers.Main

open Plotly.NET
open FsAlg.Generic
open System
open SLAE
open Matrices

let header = [ "Matrix name"; "Spectral number"; "Volume number"; "Angular number"; ]
                     @ List.map (fun x -> $"{x} matrix var.") variationValues
                     @ List.map (fun x -> $"{x} vector var.") variationValues

let row matrix rightVector matrixName =
    let conditionNumbers = getConditionNumbers matrix
    let variationError = getVariationError matrix rightVector
    [matrixName
     $"{Math.Round(conditionNumbers.Spectral, 6)}"
     $"{Math.Round(conditionNumbers.Volume, 6)}"
     $"{Math.Round(conditionNumbers.Angular, 6)}"
     ]
    @ List.map (fun (x : float) -> $"{Math.Round(x, 6)}") variationError.MatrixVariation
    @ List.map (fun (x : float) -> $"{Math.Round(x, 6)}") variationError.RightVectorVariation
    
let emptyRow length = Seq.init length (fun _ -> String.Empty) |> Seq.toList

let simpleMatrix = matrix [[ 1.0; 2.0 ]
                           [3.0; 4.0]]

let rows =
    [ row (getHilbertMatrix 10) (cubesVector 10) "Hilbert matrix (10x10)"
      row (getHilbertMatrix 5) (cubesVector 5) "Hilbert matrix (5x5)";
      row (getHilbertMatrix 3) (cubesVector 3) "Hilbert matrix (3x3)";
      emptyRow header.Length    
      row (getDiagonalMatrix 10) (cubesVector 10) "Diagonal matrix (10x10)";
      row (getDiagonalMatrix 5) (cubesVector 5) "Diagonal matrix (5x5)";
      row (getDiagonalMatrix 3) (cubesVector 3) "Diagonal matrix (3x3)"
      emptyRow header.Length
      row (getTridiagonalMatrix 10) (cubesVector 10) "Tridiagonal matrix (10x10)"
      row (getTridiagonalMatrix 5) (cubesVector 5) "Tridiagonal matrix (5x5)"
      row (getTridiagonalMatrix 3) (cubesVector 3) "Tridiagonal matrix (3x3)"
      emptyRow header.Length
      row floatThreeDimensionalMatrix floatThreeDimensionalVector "Float matrix (3x3)" ]

Chart.Table(header, rows) |> Chart.withSize (2500, 1500) |> Chart.show