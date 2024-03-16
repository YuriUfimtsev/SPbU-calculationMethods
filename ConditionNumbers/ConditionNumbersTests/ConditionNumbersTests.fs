module ConditionNumbersTests

open ConditionNumbers
open NUnit.Framework
open FsUnit
open FsAlg.Generic
open SLAE

// Euclid norm: sqrt(30)
// det = -2
let simpleMatrix = matrix [[ 1.0; 2.0 ]
                           [3.0; 4.0]]

// Euclid norm: sqrt(7,5)
let inverseSimpleMatrix = matrix [[ -2.0; 1.0 ]
                                  [1.5; -0.5]]

[<Test>]
let ``Should spectral number of simple matrix equal to sqrt(30)*sqrt(7,5)`` () =
    (getConditionNumbers simpleMatrix).Spectral |> should (equalWithin 0.1) (sqrt 30.0 * sqrt 7.5)

[<Test>]
let ``Should volume number of simple matrix equal to 5*sqrt(5)/2`` () =
    (getConditionNumbers simpleMatrix).Volume |> should (equalWithin 0.1) (5.0 * sqrt 5.0 / 2.0)
    
[<Test>]
let ``Should angular number of simple matrix equal to sqrt(31,25)`` () =
    (getConditionNumbers simpleMatrix).Angular |> should (equalWithin 0.1) (sqrt 31.25)