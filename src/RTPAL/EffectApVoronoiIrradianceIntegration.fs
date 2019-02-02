module EffectApVoronoiIrradianceIntegration

open Aardvark.Base
open Aardvark.Base.Rendering

    
open Aardvark.Base.ShaderReflection
open FShade
open FShade.Imperative

open Light.Effect
open EffectUtils
open PhotometricLight

open RenderState


type Vertex = {
    [<WorldPosition>]   wp      : V4d
    [<Normal>]          n       : V3d
    [<Color>]           c       : V4d
    [<FragCoord>]       fc      : V4d
}  

let private voronoiTexCorners =
    sampler2d {
        texture uniform?voronoiTexCorners
        filter Filter.MinMagLinear
    }

let private voronoiTexCenterCustom =
    sampler2d {
        texture uniform?voronoiTexCenterCustom
        filter Filter.MinMagLinear
    }

[<ReflectedDefinition>]
let private sampleIrr (t2w : M33d) (voronoiArea : float) (addr : int) (p : V3d) = 
    
    let i = p |> Vec.normalize  
    let iw = t2w * -i
 
    let dotOut = max 1e-9 (abs (Vec.dot iw uniform.LForwards.[addr]))
    let invDistSquared = 1.0 / (Vec.lengthSquared p + 1e-9)

    // let irr = getPhotometricIntensity iw uniform.LForwards.[addr]  uniform.LUps.[addr] / (uniform.LAreas.[addr] * dotOut)

    // let areaToHemisphere = uniform.LAreas.[addr] * invDistSquared * dotOut * i.Z //voronoiArea * dotOut * invDistSquared

    // let weight = areaToHemisphere


    // simplified
    let irr = getPhotometricIntensity iw uniform.LForwards.[addr]  uniform.LUps.[addr]
    let weight = voronoiArea * i.Z * invDistSquared  
        
    (irr * weight, weight * dotOut)

let voronoiIrrIntegration (v : Vertex) = 
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

                    ////////////////////////////////////////////////////////

                    let l2w = M33dFromCols  (V3d.Cross((uniform.LUps.[addr]), (uniform.LForwards.[addr]))) uniform.LUps.[addr] uniform.LForwards.[addr]
                            
                    let w2l = l2w |> Mat.transpose

                    let t2l = w2l * t2w

                    let l2t = l2w * w2t

                    ////////////////////////////////////////////////////////

                    for iIdx in iAddr .. Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT .. (iAddr + uniform.LNumPatchIndices.[addr] - 1) do
                            
                        // let vt = Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>() 
                        let vt = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_ONE>, V3d>() 
                            
                        for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                            let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                            vt.[vtc] <- w2t * (uniform.LVertices.[vtcAddr] - P)

                        ////////////////////////////////////////////////////////

                        // let (clippedVa, clippedVc) = clipPatch(V3d.Zero, V3d.OOI, vt, uniform.LBaseComponents.[addr])

                        let (clippedVa, clippedVc) = (vt, 4)

                        if clippedVc <> 0 then

                            let eps = 1e-9

                            let lightPlaneN = w2t * uniform.LForwards.[addr] |> Vec.normalize   

                            // find closest point limited to upper hemisphere
                            let t = (- clippedVa.[0]) |> Vec.dot lightPlaneN
                            let mutable closestPoint = t * (-lightPlaneN)
                                                    
                            if (Vec.dot closestPoint V3d.OOI) < 0.0 then
                                let newDir = V3d(closestPoint.X, closestPoint.Y, 0.0) |> Vec.normalize
                                closestPoint <- linePlaneIntersection V3d.Zero newDir (clippedVa.[0]) lightPlaneN
                                    
                            let insideLightPlane = (Vec.length closestPoint) < eps
                                
                            if not insideLightPlane then

                                let mutable barycenter = V3d.Zero
                                for l in 0 .. clippedVc - 1 do
                                    barycenter <- barycenter + clippedVa.[l]
                                    
                                let barycenter = barycenter / (float clippedVc)

                                let (closestPoint, _, _, _) = clampPointToPolygon clippedVa clippedVc closestPoint t2l
                                let closestProjected = (t2l * (closestPoint - clippedVa.[0]))
                                let closestPoint2d = V2d(closestProjected.X, closestProjected.Y)

                                let weights = 
                                    let wCorners = voronoiTexCorners.Sample(closestPoint2d)
                                    let wCenterCustom = voronoiTexCenterCustom.Sample(closestPoint2d)

                                    let weights = Arr<N<6>, float>()
                                    weights.[0] <- wCorners.X
                                    weights.[1] <- wCorners.Y
                                    weights.[2] <- wCorners.Z
                                    weights.[3] <- wCorners.W
                                    weights.[4] <- wCenterCustom.X
                                    weights.[5] <- wCenterCustom.Y
                                        
                                    weights

                                    
                                let mutable patchIllumination = 0.0
                                let mutable weightSum = 0.0

                                for i in 0 .. 3 do
                                    let (irr, weight) = sampleIrr t2w weights.[i] addr clippedVa.[i]
                                    patchIllumination <- patchIllumination + irr
                                    weightSum <- weightSum + weight

                                let (irr, weight) = sampleIrr t2w weights.[4] addr barycenter
                                patchIllumination <- patchIllumination + irr
                                weightSum <- weightSum + weight

                                let (irr, weight) = sampleIrr t2w weights.[5] addr closestPoint
                                patchIllumination <- patchIllumination + irr
                                weightSum <- weightSum + weight
                                    
                                let L =
                                    if weightSum > 0.0 then
                                        patchIllumination / weightSum
                                    else 
                                        0.0

                                    
                                for l in 0 .. Config.Light.MAX_PATCH_SIZE_PLUS_ONE - 1 do
                                        if l < clippedVc then
                                            // Project polygon light onto sphere
                                            clippedVa.[l] <- Vec.normalize clippedVa.[l]
                                                
                                let I = abs (baumFormFactor(clippedVa, clippedVc)) / (2.0) // should be divided by 2 PI, but PI is already in the brdf
                                        
                                illumination <- illumination + L * brdf * I //* scale // * i.Z  

                                ()
                           
                        ////////////////////////////////////////////////////////
                        

        return V4d(illumination.XYZ, v.c.W)

    }


module Rendering =

    open Aardvark.SceneGraph
    open Aardvark.Base.Incremental

    open RenderInterop
    open Utils
    open Utils.Sg

    let voronoiIrrIntApproxRenderTask (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 


        let voronoiTexA = FileTexture(Path.combine [__SOURCE_DIRECTORY__;"misc";"voronoiTexA.exr"], TextureParams.empty)
        let voronoiTexB = FileTexture(Path.combine [__SOURCE_DIRECTORY__;"misc";"voronoiTexB.exr"], TextureParams.empty)

        sceneSg
        |> setupFbEffects [ 
                voronoiIrrIntegration |> toEffect
            ]
        |> Light.Sg.addLightCollectionSg (data.lights) (data.lightData)
        |> Light.Sg.setLightCollectionUniforms data.lights
        |> setupPhotometricData data.photometricData
        |> setupCamera data.view data.projTrafo data.viewportSize 
        |> setUniformDT data.dt
        |> setUniformUsePhotometry data.lightData.usePhotometry
        |> setUniformDiffuseExitance data.lightData.diffuseExitance
        |> Sg.texture (Sym.ofString "voronoiTexCorners")       ((voronoiTexA :> ITexture) |> Mod.constant)    
        |> Sg.texture (Sym.ofString "voronoiTexCenterCustom")  ((voronoiTexB :> ITexture) |> Mod.constant)    
        |> Sg.compile data.runtime signature



    let voronoiIrrIntApproxFb (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
        voronoiIrrIntApproxRenderTask data signature sceneSg
        |> RenderTask.renderToColor data.viewportSize
