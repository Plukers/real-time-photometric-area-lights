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
                
                // Check if i hits a light
                // If it does, compute the illumination
                (*
                areaLightMap P w2t (fun addr v0 v1 v2 ->


                    )
*)
                for addr in 0 .. (Config.NUM_LIGHTS - 1) do 
                    match uniform.Lights.[addr] with
                    | -1 -> ()
                    |  _ ->                        
                        let vAddr = addr * Config.VERT_PER_LIGHT
                        let iAddr = addr * Config.MAX_EVAL_IDX_BUFFER_SIZE_PER_LIGHT

                        let patchSize = 
                            match uniform.LBaseComponents.[addr] with
                            | LightBaseComponent.Square   -> 4
                            | _ -> 3 // Triangle

                        for iIdx in iAddr .. patchSize .. (iAddr + uniform.LNumEvalIndices.[addr] - 1) do

                            let vt = Arr<N<4>, V3d>([| V3d.Zero; V3d.Zero; V3d.Zero; V3d.Zero|])

                            for vtc in 0 .. patchSize - 1 do
                                let vtcAddr = uniform.LEvalIndices.[iIdx + vtc] + vAddr
                                vt.[vtc] <- w2t * (uniform.LVertices.[vtcAddr] - P)
                            (*
                            let v0Addr = uniform.LEvalIndices.[iIdx + 0] + vAddr
                            let v0 = w2t * (uniform.LVertices.[v0Addr] - P)
                           
                            let v1Addr = uniform.LEvalIndices.[iIdx + 1] + vAddr
                            let v1 = w2t * (uniform.LVertices.[v1Addr] - P)
                           
                            let v2Addr = uniform.LEvalIndices.[iIdx + 2] + vAddr
                            let v2 = w2t * (uniform.LVertices.[v2Addr] - P)                       
                            *)
                            ////////////////////////////////////////////////////////
                            
                            let t = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[1] vt.[2]

                            let hitLight = 
                                match uniform.LBaseComponents.[addr] with
                                | LightBaseComponent.Square -> 
                                    let t1 = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[1] vt.[2]
                                    let t2 = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[2] vt.[3]
                                    t1 > 1e-8 || t2 > 1e-8
                                | _ -> // Triangle 
                                    let t = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[1] vt.[2]
                                    t > 1e-8
                                    
                            if hitLight then
                                
                                let worldI = t2w * -i

                                let dotOut = max 1e-5 (abs (Vec.dot worldI uniform.LForwards.[addr]))
                                
                                let irr = (getPhotometricIntensity worldI uniform.LForwards.[addr]  uniform.LUps.[addr]) / (uniform.LAreas.[addr] * dotOut)

                                if irr > 0.0 then 

                                    illumination <-
                                        let brdf = v.c / PI 
                                        illumination + irr * (brdf / pdf) * i.Z                            
                                    ()                            
                             
                            ////////////////////////////////////////////////////////
                        ()       
                ()

            illumination <- illumination / V4d(Config.NUM_SAMPLES);

            let alpha = 1.0 / (float)(uniform.FrameCount)

            return V4d(illumination.XYZ, alpha)
            }
                