namespace Render

module EffectSolidAngle =
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open FShade

    open Light.Effect
    open EffectUtils
    open PhotometricLight

    
    type UniformScope with
        member uniform.compMethod : SolidAngleCompMethod  = uniform?compMethod 

    type Vertex = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
    }  
    
    let solidAngle (v : Vertex) = 
        fragment {

            ////////////////////////////////////////////////////////

            let P = v.wp.XYZ

            let t2w = v.n |> Vec.normalize |> basisFrisvad 
            let w2t = t2w |> Mat.transpose

            let brdf = v.c / PI 

            let mutable illumination = V4d.Zero

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
                                if uniform.compMethod = SolidAngleCompMethod.Triangle then                                    
                                    vt.[vtc] <- w2t * (uniform.LVertices.[vtcAddr] - P)
                                if uniform.compMethod = SolidAngleCompMethod.Square then
                                    vt.[vtc] <- uniform.LVertices.[vtcAddr]
                                

                            ////////////////////////////////////////////////////////

                            let (clippedVa, clippedVc) = clipPatch(V3d.Zero, V3d.OOI, vt, uniform.LBaseComponents.[addr])

                            if clippedVc <> 0 then
                                
                                let mutable solidAngle = 0.0

                                if uniform.compMethod = SolidAngleCompMethod.Triangle then
                                    solidAngle <-
                                        if clippedVc = 3 then
                                            computeSolidAngle clippedVa.[0] clippedVa.[1] clippedVa.[2]
                                        else
                                            let sa1 = computeSolidAngle clippedVa.[0] clippedVa.[1] clippedVa.[2]
                                            let sa2 = computeSolidAngle clippedVa.[0] clippedVa.[2] clippedVa.[3]
                                            sa1 + sa2

                                if uniform.compMethod = SolidAngleCompMethod.Square then
                                    solidAngle <-
                                        if clippedVc = 3 then
                                            // Not supported
                                            0.0 // computeSolidAngle clippedVa.[0] clippedVa.[1] clippedVa.[2]
                                        else
                                            let ex = vt.[1] - vt.[0]
                                            let ey = vt.[3] - vt.[0]
                                            let squad = SphericalQuad.sphQuadInit vt.[0] ex ey P
                                            squad.S

                                    

                                
                                illumination <- illumination + solidAngle         
                                    
                                ()
                                                                
                            ////////////////////////////////////////////////////////
                        ()

            return V4d(illumination.XYZ, v.c.W)
        }

    module Rendering =

        open Aardvark.SceneGraph
        open Aardvark.Base.Incremental

        open RenderInterop
        open Utils
        open Utils.Sg

        type SolidAngleData = {
            compMethod : IMod<SolidAngleCompMethod>
        }

        let initSolidAngleData  (m : MRenderState) = {
            compMethod = m.solidAngleCompMethod
        }

        let initSolidAngleData' compMethod = {
            compMethod = compMethod
        }


        let solidAngleRenderTask (data : RenderData) (saData : SolidAngleData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            sceneSg
                |> setupFbEffects [ 
                        solidAngle |> toEffect 
                        EffectUtils.effectClearNaN |> toEffect
                    ]
                |> Light.Sg.setLightCollectionUniforms data.lights
                |> Sg.uniform "compMethod" (saData.compMethod |> Mod.map (fun cm -> cm |> int))
                |> setupPhotometricData data.photometricData
                |> setupCamera data.view data.projTrafo data.viewportSize 
                |> Sg.compile data.runtime signature

        let solidAngleFb (data : RenderData) (saData : SolidAngleData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            solidAngleRenderTask data saData signature sceneSg
            |> RenderTask.renderToColor data.viewportSize


