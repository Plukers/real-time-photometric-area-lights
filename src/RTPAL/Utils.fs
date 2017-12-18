namespace Render

module Utils = 

    module Sg =
        
        open Aardvark.Base
        open Aardvark.Base.Incremental
        open Aardvark.Base.Incremental.Operators        
        open Aardvark.Base.Rendering
        open Aardvark.UI

        let fullscreenQuad size =
            Sg.draw IndexedGeometryMode.TriangleStrip
                |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant [|V3f(-1.0,-1.0,0.0); V3f(1.0,-1.0,0.0); V3f(-1.0,1.0,0.0);V3f(1.0,1.0,0.0) |])
                |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates (Mod.constant [|V2f.OO; V2f.IO; V2f.OI; V2f.II|])
                |> Sg.depthTest ~~DepthTestMode.None
                |> Sg.uniform "ViewportSize" size

    module Assimp =
        open Aardvark.Base
        open Aardvark.UI

        let loadFromFile zUp f =  
            f 
                |> Aardvark.SceneGraph.IO.Loader.Assimp.load 
                |> Sg.adapter 
                |> Sg.noEvents
                |> Sg.transform (if zUp then Trafo3d.FromOrthoNormalBasis(V3d.IOO, V3d.OOI, -V3d.OIO) else Trafo3d.Identity)

    module PoissonSequence = 

        open Aardvark.Base
        open Aardvark.UI.Static

        let private genRandomNumbers count =
            let rnd = System.Random()
            List.init count (fun _ -> rnd.Next ())

        
                
        (*
            Returns sample points for a given triangle of size  Config.NUM_SS_LIGHT_SAMPLES
        *)
        let computeSequenceForTriangle p1 p2 p3 (discard : V3d -> bool )= 
        
            let u = p2 - p1
            let v = p3 - p1
            
            let seed = genRandomNumbers 2
            let seed = V2d(seed.[0] * u, seed.[1] * v)

            let samplePoints = seed :: List.empty<V2d>

            for i in 1 .. Config.NUM_SS_LIGHT_SAMPLES - 1 do

                let mutable maxDistance = 0.0
            
                for sampleCandidate in genRandomNumbers (500 * samplePoints.Length) do

                    

                    ()

                ()





            for i in 1 .. Config.NUM_SS_LIGHT_SAMPLES - 1 do

                let sampleCandidates = genRandomNumbers (500 * samplePoints.Length)
                let mutable sample = sampleCandidates.[0]
                let mutable sampleDist = 0.0
                


                ()



            ()

        let computeSequenceForSquare = 


            ()

        ()
    
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
            
        let addSequenceToSg (sequence : IMod<V2d[]>) sg = sg |> Sg.uniform "HaltonSamples" sequence