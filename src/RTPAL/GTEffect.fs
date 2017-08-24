﻿namespace Render

(*
    Ground Truth Rendering Effect
    Single Bounce Path Tracing
*)
module GTEffect = 
    open System

    open FShade
    open FShade.Imperative
    open Aardvark.Base
    open Aardvark.Base.Rendering

    open Utils.HaltonSequence
    open Light.Effect
    open EffectUtils

    

    type GTVertex = {
        [<Position>]        pos     : V4d
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
        [<FragCoord>]       fc      : V4d
    }        
        
    let groundTruthLighting (v : GTVertex) = 
        fragment {

            let P = v.wp.XYZ
            let worldV = ( uniform.CameraLocation - P) |> Vec.normalize

            let w2t = v.n |> Vec.normalize |> basisFrisvad |> Mat.transpose
            // let t2w = w2t |> Mat.inverse
            
            // Transform view vector into tangent space
            let o = w2t * worldV
            
            // Compute a jitter
            let jitter = (fast32Hash v.fc.XYZ).XY              

            let mutable illumination = V4d.Zero
            
            // Iterate over Samples
            for sIdx in 0 .. Config.NUM_SAMPLES - 1 do
                   
                let u1 = 
                    let x = jitter.X + uniform.HaltonSamples.[sIdx].X
                    x - Math.Floor(x)

                let u2 = 
                    let x = jitter.Y + uniform.HaltonSamples.[sIdx].Y
                    x - Math.Floor(x)

                // let m = BRDF_GGX.sampleGGX u1 u2 alpha                
                // let i = sampleHemisphere u1 u2
                let i = cosineSampleHemisphere u1 u2   

                let pdf = i.Z / PI
                
                // Check if i hits a light
                // If it does, compute the illumination
                for addr in 0 .. (Config.NUM_LIGHTS - 1) do 
                    match uniform.Lights.[addr] with
                    | -1 -> ()
                    |  _ ->
                        
                        let vAddr = addr * Config.VERT_PER_LIGHT
                        let iAddr = addr * Config.MAX_IDX_BUFFER_SIZE_PER_LIGHT

                        for iIdx in iAddr .. 3 .. (iAddr + uniform.LNumIndices.[addr] - 1) do
                            
                            let v0Addr = uniform.LIndices.[iIdx + 0] + vAddr
                            let v0 = w2t * (uniform.LVertices.[v0Addr] - P)
                           
                            let v1Addr = uniform.LIndices.[iIdx + 1] + vAddr
                            let v1 = w2t * (uniform.LVertices.[v1Addr] - P)
                           
                            let v2Addr = uniform.LIndices.[iIdx + 2] + vAddr
                            let v2 = w2t * (uniform.LVertices.[v2Addr] - P)                       

                            let t = rayTriangleIntersaction V3d.Zero i v0 v1 v2

                            if t > 1e-8 then
                                let irr = uniform.LIntensities.[addr]

                                illumination <-
                                    let brdf = v.c / PI
                                    illumination + irr * brdf / pdf * i.Z                               
                                ()                            
                            ()  
                        ()  
                ()

            illumination <- 2.0 * illumination / V4d(Config.NUM_SAMPLES);

            let alpha = 1.0 / (float)(uniform.FrameCount + 1)

            return V4d(illumination.XYZ, alpha)
            }


       
    let debugOutput = 
   
        let config =
            EffectConfig.ofList [ 
                Intrinsics.Color, typeof<V4d>, 0
            ]

        let cModule = 
            Effect.ofFunction groundTruthLighting
               |> Effect.toModule config
               |> ModuleCompiler.compile glsl410

        let glsl = 
            cModule
                |> GLSL.Assembler.assemble glsl410

        printfn "+-------- Ground Truth Lighting --------+"
        printfn "+--------- Start Shader Output ---------+"
        printfn "%A" glsl.builtIns
        printfn "%s" glsl.code
        printfn "+---------- End Shader Output ----------+"
        
    
