namespace Render

module EffectApSolidAngle =
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
    
    let solidAngleApprox (v : Vertex) = 
        fragment {

            ////////////////////////////////////////////////////////

            let P = v.wp.XYZ

            let t2w = v.n |> Vec.normalize |> basisFrisvad 
            let w2t = t2w |> Mat.transpose

            let brdf = v.c / PI 

            let mutable illumination = V4d.Zero

            ////////////////////////////////////////////////////////

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
                                
                                // compute solid angle
                                let solidAngle = 
                                    if clippedVc = 3 then
                                        computeSolidAngle clippedVa.[0] clippedVa.[1] clippedVa.[2]
                                    else
                                        let sa1 = computeSolidAngle clippedVa.[0] clippedVa.[1] clippedVa.[2]
                                        let sa2 = computeSolidAngle clippedVa.[0] clippedVa.[2] clippedVa.[3]
                                        sa1 + sa2

                                let mutable barycenter = V3d.Zero
                                for l in 0 .. clippedVc - 1 do
                                    barycenter <- barycenter + clippedVa.[l]
                                    
                                let i = barycenter / (float clippedVc)
                                
                                let i = i |> Vec.normalize

                                let worldI = t2w * -i

                                let dotOut = max 1e-5 (abs (Vec.dot worldI uniform.LForwards.[addr]))
                                
                                let irr = getPhotometricIntensity worldI uniform.LForwards.[addr] uniform.LUps.[addr]

                                if irr > 0.0 then 
                                    illumination <- illumination + irr * brdf * solidAngle / dotOut              
                                    
                                ()
                                                                
                            ////////////////////////////////////////////////////////
                        ()

            return V4d(illumination.XYZ, v.c.W)
        }

