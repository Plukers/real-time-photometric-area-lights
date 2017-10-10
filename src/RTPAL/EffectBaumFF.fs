namespace Render

(*
    Baum Form Factor Effect
*)
module EffectBaumFF = 
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open FShade

    open Light.Effect
    open EffectUtils

    type FFVertex = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
    }  

    let formFactorLighting (v : FFVertex) = 
        fragment {


            let P = v.wp.XYZ

            let w2t = v.n |> Vec.normalize |> basisFrisvad |> Mat.transpose

            let mutable illumination = V4d.Zero

            for addr in 0 .. (Config.NUM_LIGHTS - 1) do 
                    match uniform.Lights.[addr] with
                    | Some _ ->
                        let vAddr = addr * Config.VERT_PER_LIGHT
                        let iAddr = addr * Config.MAX_IDX_BUFFER_SIZE_PER_LIGHT

                        for iIdx in iAddr .. 3 .. (iAddr + uniform.LNumIndices.[addr] - 1) do
                            
                            let v0Addr = uniform.LIndices.[iIdx + 0] + vAddr
                            let v0 = w2t * (uniform.LVertices.[v0Addr] - P)
                           
                            let v1Addr = uniform.LIndices.[iIdx + 1] + vAddr
                            let v1 = w2t * (uniform.LVertices.[v1Addr] - P)
                           
                            let v2Addr = uniform.LIndices.[iIdx + 2] + vAddr
                            let v2 = w2t * (uniform.LVertices.[v2Addr] - P) 
                            
                            let (clippedVa, clippedVc) = clipTriangle(V3d.Zero, V3d.OOI, Arr<N<3>, V3d>([| v0; v1; v2|]))

                            if clippedVc <> 0 then                            
                                // Project polygon light onto sphere
                                for l in 0 .. clippedVc - 1 do
                                    clippedVa.[l] <- Vec.normalize clippedVa.[l]
                                    ()

                                let irr = uniform.LIntensities.[addr]
                                
                                illumination <-                                        

                                    let I = baumFormFactor(clippedVa, clippedVc)
                                    
                                    let I = match uniform.LTwoSided.[addr] with
                                            | true  -> abs I
                                            | false -> I |> clamp 0.0 1.0   
                                            
                                    illumination + irr * I
                                    
                                ()

                        ()
                    | None -> ()

            illumination <- v.c * illumination / (2.0 * PI)

            return V4d(illumination.XYZ, v.c.W)
        }

