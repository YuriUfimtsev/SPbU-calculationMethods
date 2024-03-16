module QRDecomposition.Program

open Plotly.NET
open System
open FsAlg.Generic

open QRDecomposition
open AuxiliaryOperations
open RotationMethod
open Matrices

let spectralHeader = [ "Matrix name"; "Spectral number"; "R spectral number"; "Q spectral number"; "Error" ]
let volumeHeader = [ "Matrix name"; "Volume number"; "R volume number"; "Q volume number"; "Error" ]
let angularHeader = [ "Matrix name"; "Angular number"; "R angular number"; "Q angular number"; "Error" ]


let volumeRow matrix rightVector matrixName =
    let accurateSolution = Matrix.solve matrix rightVector
    let rMatrix = getRMatrix matrix
    let qMatrix = getQMatrix matrix
    let rotationMethodSolution = solveSystemUsingQRDecomposition matrix rightVector
    let error = rotationMethodSolution |> (-) accurateSolution |> Vector.l2norm
    
    let baseMatrixVolumeNumber = (getConditionNumbers matrix).Volume
    let rMatrixVolumeNumbers = (getConditionNumbers rMatrix).Volume
    let qMatrixVolumeNumbers = (getConditionNumbers qMatrix).Volume

    [matrixName;
    $"{Math.Round(baseMatrixVolumeNumber, 6)}";
    $"{Math.Round(rMatrixVolumeNumbers, 6)}";
    $"{Math.Round(qMatrixVolumeNumbers, 6)}";
    $"{error}";
    ]

let spectralRow matrix rightVector matrixName =
    let accurateSolution = Matrix.solve matrix rightVector
    let rMatrix = getRMatrix matrix
    let qMatrix = getQMatrix matrix
    let rotationMethodSolution = solveSystemUsingQRDecomposition matrix rightVector
    let error = rotationMethodSolution |> (-) accurateSolution |> Vector.l2norm
    
    let baseMatrixVolumeNumber = (getConditionNumbers matrix).Spectral
    let rMatrixVolumeNumbers = (getConditionNumbers rMatrix).Spectral
    let qMatrixVolumeNumbers = (getConditionNumbers qMatrix).Spectral

    [matrixName;
    $"{Math.Round(baseMatrixVolumeNumber, 6)}";
    $"{Math.Round(rMatrixVolumeNumbers, 6)}";
    $"{Math.Round(qMatrixVolumeNumbers, 6)}";
    $"{error}";
    ]
    
let angularRow matrix rightVector matrixName =
    let accurateSolution = Matrix.solve matrix rightVector
    let rMatrix = getRMatrix matrix
    let qMatrix = getQMatrix matrix
    let rotationMethodSolution = solveSystemUsingQRDecomposition matrix rightVector
    let error = rotationMethodSolution |> (-) accurateSolution |> Vector.l2norm
    
    let baseMatrixVolumeNumber = (getConditionNumbers matrix).Angular
    let rMatrixVolumeNumber = (getConditionNumbers rMatrix).Angular
    let qMatrixVolumeNumber = (getConditionNumbers qMatrix).Angular

    [matrixName;
    $"{Math.Round(baseMatrixVolumeNumber, 6)}";
    $"{Math.Round(rMatrixVolumeNumber, 6)}";
    $"{Math.Round(qMatrixVolumeNumber, 6)}";
    $"{error}";
    ]

let emptyRow length = Seq.init length (fun _ -> String.Empty) |> Seq.toList

let volumeRows =
    [ volumeRow (getHilbertMatrix 10) (cubesVector 10) "Hilbert matrix (10x10)"
      volumeRow (getHilbertMatrix 5) (cubesVector 5) "Hilbert matrix (5x5)"
      volumeRow (getHilbertMatrix 3) (cubesVector 3) "Hilbert matrix (3x3)";
      emptyRow volumeHeader.Length    
      volumeRow (getRandomDiagonalMatrix 10) (cubesVector 10) "Diagonal matrix (10x10)";
      volumeRow (getRandomDiagonalMatrix 5) (cubesVector 5) "Diagonal matrix (5x5)";
      volumeRow (getRandomDiagonalMatrix 3) (cubesVector 3) "Diagonal matrix (3x3)"
      emptyRow volumeHeader.Length
      volumeRow (getTridiagonalMatrix 10) (cubesVector 10) "Tridiagonal matrix (10x10)"
      volumeRow (getTridiagonalMatrix 5) (cubesVector 5) "Tridiagonal matrix (5x5)"
      volumeRow (getTridiagonalMatrix 3) (cubesVector 3) "Tridiagonal matrix (3x3)"
      emptyRow volumeHeader.Length
      volumeRow floatThreeDimensionalMatrix floatThreeDimensionalVector "Float matrix (3x3)" ]

let spectralRows =
    [ spectralRow (getHilbertMatrix 10) (cubesVector 10) "Hilbert matrix (10x10)"
      spectralRow (getHilbertMatrix 5) (cubesVector 5) "Hilbert matrix (5x5)"
      spectralRow (getHilbertMatrix 3) (cubesVector 3) "Hilbert matrix (3x3)";
      emptyRow spectralHeader.Length    
      spectralRow (getRandomDiagonalMatrix 10) (cubesVector 10) "Diagonal matrix (10x10)";
      spectralRow (getRandomDiagonalMatrix 5) (cubesVector 5) "Diagonal matrix (5x5)";
      spectralRow (getRandomDiagonalMatrix 3) (cubesVector 3) "Diagonal matrix (3x3)"
      emptyRow spectralHeader.Length
      spectralRow (getTridiagonalMatrix 10) (cubesVector 10) "Tridiagonal matrix (10x10)"
      spectralRow (getTridiagonalMatrix 5) (cubesVector 5) "Tridiagonal matrix (5x5)"
      spectralRow (getTridiagonalMatrix 3) (cubesVector 3) "Tridiagonal matrix (3x3)"
      emptyRow spectralHeader.Length
      spectralRow floatThreeDimensionalMatrix floatThreeDimensionalVector "Float matrix (3x3)" ]

let angularRows =
    [ angularRow (getHilbertMatrix 10) (cubesVector 10) "Hilbert matrix (10x10)"
      angularRow (getHilbertMatrix 5) (cubesVector 5) "Hilbert matrix (5x5)"
      angularRow (getHilbertMatrix 3) (cubesVector 3) "Hilbert matrix (3x3)";
      emptyRow angularHeader.Length
      angularRow (getRandomDiagonalMatrix 10) (cubesVector 10) "Diagonal matrix (10x10)";
      angularRow (getRandomDiagonalMatrix 5) (cubesVector 5) "Diagonal matrix (5x5)";
      angularRow (getRandomDiagonalMatrix 3) (cubesVector 3) "Diagonal matrix (3x3)"
      emptyRow angularHeader.Length
      angularRow (getTridiagonalMatrix 10) (cubesVector 10) "Tridiagonal matrix (10x10)"
      angularRow (getTridiagonalMatrix 5) (cubesVector 5) "Tridiagonal matrix (5x5)"
      angularRow (getTridiagonalMatrix 3) (cubesVector 3) "Tridiagonal matrix (3x3)"
      emptyRow angularHeader.Length
      angularRow floatThreeDimensionalMatrix floatThreeDimensionalVector "Float matrix (3x3)" ]


Chart.Table(spectralHeader, spectralRows) |> Chart.withSize (1400, 1500) |> Chart.show
Chart.Table(volumeHeader, volumeRows) |> Chart.withSize (1400, 1500) |> Chart.show
Chart.Table(angularHeader, angularRows) |> Chart.withSize (1400, 1500) |> Chart.show