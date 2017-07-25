namespace Render

module Utils = 
    
    module HaltonSequence = 
        open FShade
        open Aardvark.Base
        open Aardvark.Base.Incremental
        open Aardvark.SceneGraph
        
        
        type UniformScope with
            member uniform.HaltonSamples : Arr<N<Config.NUM_SAMPLES>, V2d> = uniform?HaltonSamples
        
        // Generates a 2d halton sequence of length Config.NUM_SAMPlES with the bases 2 and 3
        let private genHaltonSequence = 

            let h = V2d.Zero |> Array.create Config.NUM_SAMPLES

            h.[0] <- V2d(0.5, 1.0/3.0)

            for i in 1 .. Config.NUM_SAMPLES - 1 do

                h.[i] <- V2d(
                    Quasi.QuasiHaltonWithIndex(0, h.[i - 1].X),
                    Quasi.QuasiHaltonWithIndex(1, h.[i - 1].Y))
                
            h |> Mod.constant
            
        let addSequenceToSg sg = sg |> Sg.uniform "HaltonSamples" genHaltonSequence
     





