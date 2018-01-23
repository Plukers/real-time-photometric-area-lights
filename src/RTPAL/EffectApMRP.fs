﻿namespace Render

module EffectApMRP =
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open FShade

    open Light.Effect
    open EffectUtils
    open PhotometricLight

    
    type UniformScope with
        member uniform.mrpWeights : V3d = uniform?mrpWeights // closest, normal, barycenter

    type Vertex = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
    }  
    
    let mostRepresentativePointApprox (v : Vertex) = 
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
                        let iAddr = addr * Config.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT

                        ////////////////////////////////////////////////////////

                        let l2w = M33dFromCols  (V3d.Cross((uniform.LUps.[addr]), (uniform.LForwards.[addr]))) uniform.LUps.[addr] uniform.LForwards.[addr]
                            
                        let w2l = l2w |> Mat.transpose

                        let t2l = w2l * t2w

                        ////////////////////////////////////////////////////////

                        for iIdx in iAddr .. Config.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT .. (iAddr + uniform.LNumPatchIndices.[addr] - 1) do
                            
                            let mutable vt = Arr<N<Config.MAX_PATCH_SIZE>, V3d>() 
                            
                            for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                                let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                                vt.[vtc] <- w2t * (uniform.LVertices.[vtcAddr] - P)

                            ////////////////////////////////////////////////////////

                            let (clippedVa, clippedVc) = clipPatch(V3d.Zero, V3d.OOI, vt, uniform.LBaseComponents.[addr])

                            if clippedVc <> 0 then

                                let eps = 1e-9
                                let epb = 1e-3
                                                    
                                let lightPlaneN = w2t * uniform.LForwards.[addr] |> Vec.normalize                                

                                // find closest point limited to upper hemisphere
                                let t = (- clippedVa.[0]) |> Vec.dot lightPlaneN
                                let mutable closestPoint = t * (-lightPlaneN)
                                                    
                                if (Vec.dot closestPoint V3d.OOI) < 0.0 then
                                    let newDir = V3d(closestPoint.X, closestPoint.Y, 0.0) |> Vec.normalize
                                    closestPoint <- linePlaneIntersection V3d.Zero newDir (clippedVa.[0]) lightPlaneN
                                    
                                let insideLightPlane = (Vec.length closestPoint) < eps
                                
                                if not insideLightPlane then
                                    
                                    let closestPointDir = closestPoint |> Vec.normalize

                                    // intersect normal with plane
                                    let mutable up = V3d.OOI
                                
                                    if abs(Vec.dot up lightPlaneN) < eps then
                                        up <- up + (epb * closestPointDir) |> Vec.normalize     
                                    else
                                        let abovePlane = if (Vec.dot V3d.OOI closestPoint) < 0.0 && (Vec.dot closestPoint lightPlaneN) < 0.0 then false else true
                                        if abovePlane then
                                            if (Vec.dot up lightPlaneN) > 0.0 then
                                                up <- up + (abs(Vec.dot up lightPlaneN) + epb) * (-lightPlaneN) |> Vec.normalize
                                    
                                    
                                    let normPlaneP = linePlaneIntersection V3d.Zero up (clippedVa.[0]) lightPlaneN // tangent space
                                    
                                    let (closestPointDir, normPlanePDir) = 
                                        
                                        let closestPoint = clampPointToPolygon clippedVa clippedVc closestPoint t2l
                                        let normPlaneP =   clampPointToPolygon clippedVa clippedVc normPlaneP t2l 
     
                                        (closestPoint |> Vec.normalize, normPlaneP |> Vec.normalize)
                                    
                                    let mutable barycenter = V3d.Zero
                                    for l in 0 .. clippedVc - 1 do
                                        barycenter <- barycenter + clippedVa.[l]
                                    
                                    let barycenter = barycenter / (float clippedVc)
                                    
                                    let mrpDir = uniform.mrpWeights.X * closestPointDir + uniform.mrpWeights.Y * normPlanePDir + uniform.mrpWeights.Z * barycenter |> Vec.normalize
                                            
                                    let i =  mrpDir                                
                                    let dotOut = max 1e-5 (abs (Vec.dot -(t2w * i) uniform.LForwards.[addr]))
                                    let L = getPhotometricIntensity -(t2w * i) uniform.LForwards.[addr]  uniform.LUps.[addr] / (uniform.LAreas.[addr] * dotOut)

                                    if L > 0.0 then 

                                        for l in 0 .. clippedVc - 1 do
                                            // Project polygon light onto sphere
                                            clippedVa.[l] <- Vec.normalize clippedVa.[l]

                                        let I = abs (baumFormFactor(clippedVa, clippedVc)) / (2.0) // should be divided by 2 PI, but PI is already in the brdf
                                        
                                        illumination <- illumination + L * brdf * I // * i.Z  
                                    
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

        type MRPData = {
            mrpWeights : IMod<V3d>
        }

        let initMRPData (m : MRenderState) = 
            {
                mrpWeights = m.mrpWeights
            }

        let initMRPData' (weights : IMod<V3d>) =
            {
                mrpWeights = weights
            }

        let mrpApproxRenderTask (data : RenderData) (mrpData : MRPData) (signature : IFramebufferSignature) (sceneSg : ISg) = 

            //let sceneSg = MRPApproxDebug.sceneSg data.lights

            sceneSg
                |> setupFbEffects [ 
                        mostRepresentativePointApprox |> toEffect
                        EffectUtils.effectClearNaN |> toEffect
                    ]
                |> Sg.uniform "mrpWeights" mrpData.mrpWeights
                |> Sg.compile data.runtime signature

        let mrpApproxFb (data : RenderData) (mrpData : MRPData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            mrpApproxRenderTask data mrpData signature sceneSg
            |> RenderTask.renderToColor data.viewportSize
