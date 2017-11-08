namespace Render

module EffectApPoint =
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open FShade

    open Light.Effect
    open EffectUtils
    open PhotometricLight

    type Vertex = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
    }  


    let centerPointApprox (v : Vertex) = 
        fragment {

            let P = v.wp.XYZ

            let t2w = v.n |> Vec.normalize |> basisFrisvad 
            let w2t = t2w |> Mat.transpose

            let mutable illumination = V4d.Zero


            // blinn-phong
            (*
            let V = (uniform.CameraLocation - v.wp.XYZ) |> Vec.normalize
            *)
            // blinn-phong

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

                            ////////////////////////////////////////////////////////

                            let (clippedVa, clippedVc) = clipTriangle(V3d.Zero, V3d.OOI, Arr<N<3>, V3d>([| v0; v1; v2|]))

                            if clippedVc <> 0 then

                                // compute barycenter
                                let mutable barycenter = V3d.Zero
                                for l in 0 .. clippedVc - 1 do
                                    barycenter <- barycenter + clippedVa.[l]
                                    ()
                                let i = barycenter / (float clippedVc)

                                

                                // blinn-phong
                                (*
                                let L = (barycenter - P) |> Vec.normalize
                                let H = (L + V) |> Vec.normalize
                    
                                let NdotH = Vec.dot V3d.OOI H |> max 0.0
                                let NdotL = Vec.dot V3d.OOI L |> max 0.0
                                
                                let White = V4d(1.0)

                                illumination <- illumination 
                                        + v.c * White 
                                        + (
                                            White * NdotL 
                                            + White * NdotH
                                            )
                                *)
                                // blinn-phong
                                
                                let d = Vec.length i
                                let i = i |> Vec.normalize
                                
                                let irr = getPhotometricIntensity -(t2w * i) uniform.LForwards.[vAddr]  uniform.LUps.[vAddr]

                                if irr > 0.0 then 
                                    
                                    illumination <-
                                        let irr = irr / (d * d)

                                        let brdf = v.c / PI 

                                        illumination + irr * brdf * i.Z                            
                                    ()  
                                    
                                ()
                                                                
                            ////////////////////////////////////////////////////////
                        ()

            return V4d(illumination.XYZ, v.c.W)
        }

