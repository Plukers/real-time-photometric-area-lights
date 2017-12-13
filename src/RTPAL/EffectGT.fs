namespace Render

(*
    Ground Truth Rendering Effect
    Single Bounce Path Tracing
*)
module EffectGT = 
    open System

    open FShade
    open FShade.Imperative
    open Aardvark.Base
    open Aardvark.Base.Rendering

    open Utils.HaltonSequence
    open Light.Effect
    open EffectUtils
    open PhotometricLight
    open Light
    
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

            let t2w = v.n |> Vec.normalize |> basisFrisvad 
            let w2t = t2w |> Mat.transpose
            
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

                for addr in 0 .. (Config.NUM_LIGHTS - 1) do 
                    match uniform.Lights.[addr] with
                    | -1 -> ()
                    |  _ ->                        
                        let vAddr = addr * Config.VERT_PER_LIGHT
                        let iAddr = addr * Config.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT
                        
                        for iIdx in iAddr .. Config.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT .. (iAddr + uniform.LNumPatchIndices.[addr] - 1) do

                            let mutable vt = Arr<N<Config.MAX_PATCH_SIZE>, V3d>() 
                            
                            for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                                let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                                vt.[vtc] <- w2t * (uniform.LVertices.[vtcAddr] - P)
     
                            ////////////////////////////////////////////////////////
                            
                            let hitLight = 
                                match uniform.LBaseComponents.[addr] with
                                | bt when bt = Light.LIGHT_BASE_TYPE_TRIANGLE ->                                 
                                    let t = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[1] vt.[2]
                                    t > 1e-8
                                | bt when bt = Light.LIGHT_BASE_TYPE_SQUARE -> 
                                    let t1 = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[1] vt.[2]
                                    let t2 = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[2] vt.[3]
                                    t1 > 1e-8 || t2 > 1e-8
                                | _ -> false         
                                    
                            if hitLight then
                                
                                let worldI = t2w * -i

                                let dotOut = max 1e-5 (abs (Vec.dot worldI uniform.LForwards.[addr]))
                                
                                let irr = (getPhotometricIntensity worldI uniform.LForwards.[addr]  uniform.LUps.[addr]) / (uniform.LAreas.[addr] * dotOut)

                                if irr > 0.0 then 

                                    illumination <-
                                        let brdf = v.c / PI 
                                        illumination + irr * v.c//(brdf / pdf) * i.Z                            
                                    ()                            
                             
                            ////////////////////////////////////////////////////////
                        ()       
                ()

            illumination <- illumination / V4d(Config.NUM_SAMPLES);

            let alpha = 1.0 / (float)(uniform.FrameCount)

            return V4d(illumination.XYZ, alpha)
            }
                