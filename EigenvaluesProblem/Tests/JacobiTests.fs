module Tests.JacobiTests

open FsUnit

let areFloatCollectionsEqual targetError zipCollection =
    for pair in zipCollection do
        pair |> fst |> should (equalWithin targetError) (pair |> snd)