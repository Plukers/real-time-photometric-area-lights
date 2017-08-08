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
        let init = 

            let h = V2d.Zero |> Array.create Config.NUM_SAMPLES

            h.[0] <- V2d(0.5, 1.0/3.0)

            for i in 1 .. Config.NUM_SAMPLES - 1 do

                h.[i] <- V2d(
                    Quasi.QuasiHaltonWithIndex(0, h.[i - 1].X),
                    Quasi.QuasiHaltonWithIndex(1, h.[i - 1].Y))
              
            h
            // h |> Mod.constant

        // Generates a 2d halton sequence of length Config.NUM_SAMPlES with the bases 2 and 3
        // The seed is used as a startpoint for the seqauence
        let next (seed : V2d) = 

            let h = V2d.Zero |> Array.create (Config.NUM_SAMPLES + 1)

            h.[0] <- seed

            for i in 1 .. Config.NUM_SAMPLES do

                h.[i] <- V2d(
                    Quasi.QuasiHaltonWithIndex(0, h.[i - 1].X),
                    Quasi.QuasiHaltonWithIndex(1, h.[i - 1].Y))
              
            h.[1..]
            
        let addSequenceToSg (sequence : ModRef<V2d[]>) sg = sg |> Sg.uniform "HaltonSamples" (Mod.map (fun sequence ->  sequence) sequence)


     





