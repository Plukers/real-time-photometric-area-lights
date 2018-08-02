namespace Render

module EffectApBaumFF =
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
    
    let baumFFApprox (v : Vertex) = 
        fragment {

            ////////////////////////////////////////////////////////

            let P = v.wp.XYZ

            let t2w = v.n |> Vec.normalize |> basisFrisvad 
            let w2t = t2w |> Mat.transpose

            let brdf = v.c / PI 

            let mutable illumination = V4d.Zero * (uniform.dT * 1e-256 * 0.0)

            ////////////////////////////////////////////////////////
            
            for addr in 0 .. (Config.Light.NUM_LIGHTS - 1) do 
                match uniform.Lights.[addr] with
                    | -1 -> ()
                    |  _ ->    
                        let vAddr = addr * Config.Light.VERT_PER_LIGHT
                        let iAddr = addr * Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT

                        for iIdx in iAddr .. Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT .. (iAddr + uniform.LNumPatchIndices.[addr] - 1) do
                            
                            let mutable vt = Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>() 
                            
                            for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                                let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                                vt.[vtc] <- w2t * (uniform.LVertices.[vtcAddr] - P)

                            ////////////////////////////////////////////////////////

                            let (clippedVa, clippedVc) = clipPatch(V3d.Zero, V3d.OOI, vt, uniform.LBaseComponents.[addr])

                            if clippedVc <> 0 then      
                                
                                let mutable barycenter = V3d.Zero
                                for l in 0 .. clippedVc - 1 do
                                    barycenter <- barycenter + clippedVa.[l]

                                    // Project polygon light onto sphere
                                    clippedVa.[l] <- Vec.normalize clippedVa.[l]
                                    

                                let i = barycenter / (float clippedVc) 

                                let i = i |> Vec.normalize

                                let worldI = t2w * i

                                let dotOut = max 1e-9 (abs (Vec.dot (-worldI) uniform.LForwards.[addr]))
                                                
                                let L = getPhotometricIntensity (-worldI) uniform.LForwards.[addr] uniform.LUps.[addr] / (uniform.LAreas.[addr] * dotOut)
                                
                                illumination <-    
                                    
                                    let I = abs (baumFormFactor(clippedVa, clippedVc)) / (2.0) // should be divided by 2 PI, but PI is already in the brdf
                                    illumination + L * brdf * I //* i.Z

                                ()
                                                              
                            ////////////////////////////////////////////////////////
                        ()

            return V4d(illumination.XYZ, v.c.W)
        }

    module Rendering =

        open Aardvark.SceneGraph

        open RenderInterop
        open Utils
        open Utils.Sg

        let baumFFApproxRenderTask (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            sceneSg
                |> setupFbEffects [ 
                        baumFFApprox |> toEffect 
                        EffectUtils.effectClearNaN |> toEffect
                    ]                    
                |> Light.Sg.addLightCollectionSg (data.lights) (data.lightData)
                |> Light.Sg.setLightCollectionUniforms data.lights
                |> setupPhotometricData data.photometricData
                |> setupCamera data.view data.projTrafo data.viewportSize 
                |> setUniformDT data.dt
                |> setUniformUsePhotometry data.lightData.usePhotometry
                |> setUniformDiffuseExitance data.lightData.diffuseExitance
                |> Sg.compile data.runtime signature

        let baumFFApproxFb (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            baumFFApproxRenderTask data signature sceneSg
            |> RenderTask.renderToColor data.viewportSize