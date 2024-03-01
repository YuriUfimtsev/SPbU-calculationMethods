namespace ConditionNumbers

open AuxiliaryOperations
open FsAlg.Generic

module SLAE =
    type ConditionNumbers =
        { Spectral: float;
          Volume: float;
          Angular: float }
        
    type VariationError =
        { MatrixVariation: float List;
          RightVectorVariation: float list }
        
    let variationValues = [ 1.E-2; 1.E-4; 1.E-6; 1.E-8; 1.E-10 ]
    
    let getConditionNumbers matrix = { Spectral = matrix |> calculateSpectralNumber
                                       Volume = matrix |> calculateVolumeNumber;
                                       Angular = matrix |> calculateAngularNumber }
    let getVariationError matrix rightVector =
        let correctSolution = Matrix.solve matrix rightVector
        let matrixVariationErrors = variationValues |> (getMatrixVariationErrors matrix rightVector correctSolution)
        let rightVectorVariationErrors = variationValues |> (getVectorVariationErrors matrix rightVector correctSolution)
        { MatrixVariation = matrixVariationErrors
          RightVectorVariation = rightVectorVariationErrors }        