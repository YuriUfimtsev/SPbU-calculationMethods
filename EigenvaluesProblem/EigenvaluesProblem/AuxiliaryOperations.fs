module EigenvaluesProblem.AuxiliaryOperations

open FsAlg.Generic

let maxIndex seq =
    fst (Seq.maxBy snd (Seq.mapi (fun i x -> i, x) seq))
