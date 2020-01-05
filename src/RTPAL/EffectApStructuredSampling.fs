module EffectApStructuredSampling

open Aardvark.Base
open Aardvark.Base.Rendering
open FShade

open Light.Effect
open EffectUtils
open PhotometricLight
open RenderState

type UniformScope with
    member uniform.sampleCorners            : bool  = uniform?sampleCorners     
    member uniform.sampleBarycenter         : bool  = uniform?sampleBarycenter  
    member uniform.sampleClosest            : bool  = uniform?sampleClosest     
    member uniform.sampleNorm               : bool  = uniform?sampleNorm 
    member uniform.sampleMRP                : bool  = uniform?sampleMRP 
    member uniform.sampleRandom             : bool  = uniform?sampleRandom
    member uniform.sampleLight              : bool  = uniform?sampleLight
    member uniform.blendSamples             : bool  = uniform?blendSamples
    member uniform.blendEasing              : bool  = uniform?blendEasing
    member uniform.blendDistance            : float = uniform?blendDistance
    member uniform.numSRSamples             : int   = uniform?numSRSamples
    member uniform.weightScaleSRSamples     : float = uniform?weightScaleSRSamples
    member uniform.tangentApproxDist        : float = uniform?tangentApproxDist
    member uniform.weightScaleSRSamplesIrr  : float = uniform?weightScaleSRSamplesIrr
    member uniform.tangentApproxDistIrr     : float = uniform?tangentApproxDistIrr
    member uniform.combinedLerpValue        : float = uniform?combinedLerpValue
    
type Vertex = {
    [<WorldPosition>]   wp      : V4d
    [<Normal>]          n       : V3d
    [<Color>]           c       : V4d
}  

                          
[<Literal>]
let MAX_SAMPLE_NUM_WO_RANDOM = 9
    
[<Literal>]
let MIN_WEIGHT_SCALE_FACTOR_3 = 0.6299605249474366 // = (1/4)^(1/3)

[<Literal>]
let MIN_WEIGHT_SCALE_FACTOR_2 = 0.5773502691896257 // = (1/3)^(1/2)

[<Literal>]
let MIN_WEIGHT_SCALE_FACTOR_1 = 0.5 // = (1/2)^(1/1)

[<ReflectedDefinition>]
let sampleAlreadyExisting (samples : Arr<N<9>, V3d>) (sampleIdx : int) (sampleCandidate : V3d) =
    
    if sampleIdx > 0 then 
        let mutable minDist = 1e16
        for i in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
            if i < sampleIdx then
                let dist = Vec.length (sampleCandidate - samples.[i])
                if dist < minDist then minDist <- dist

        if minDist > 1e-16 then
            false
        else
            true
    else 
        false
         
    
[<ReflectedDefinition>]
let private sample (t2w : M33d) (S : float) (addr : int) (p : V3d) = 

    let i = p |> Vec.normalize  
    let iw = t2w * -i


    //if uniform.sampleLight then
            
    let dotOut = max 1e-9 (abs (Vec.dot iw uniform.LForwards.[addr]))

    let irr = getPhotometricIntensity iw uniform.LForwards.[addr]  uniform.LUps.[addr] / (uniform.LAreas.[addr] * dotOut)

    i.Z * irr * S
    //else
    //    let irr = getPhotometricIntensity iw uniform.LForwards.[addr]  uniform.LUps.[addr] 

    //    irr * i.Z / (Vec.lengthSquared p + 1e-9)

let structuredSampling (v : Vertex) = 
    fragment {

        ////////////////////////////////////////////////////////

        let P = v.wp.XYZ

        let t2w = v.n |> Vec.normalize |> basisFrisvad 
        let w2t = t2w |> Mat.transpose
            
        let mutable illumination = V4d.Zero * (uniform.dT * 1e-256 * 0.0)
            
        ////////////////////////////////////////////////////////

        for addr in 0 .. (Config.Light.NUM_LIGHTS - 1) do 
            match uniform.Lights.[addr] with
                | -1 -> ()
                |  _ ->    
                        
                    let vAddr = addr * Config.Light.VERT_PER_LIGHT
                    let iAddr = addr * Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT
                    let sAddr = addr * Config.Light.SS_LIGHT_SAMPLES_PER_LIGHT

                    ////////////////////////////////////////////////////////

                    let l2w = M33dFromCols  (V3d.Cross((uniform.LUps.[addr]), (uniform.LForwards.[addr]))) uniform.LUps.[addr] uniform.LForwards.[addr]
                            
                    let w2l = l2w |> Mat.transpose

                    let t2l = w2l * t2w

                    ////////////////////////////////////////////////////////


                    for iIdx in iAddr .. Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT .. (iAddr + uniform.LNumPatchIndices.[addr] - 1) do
                            
                        let dotOut = Vec.dot (uniform.LForwards.[addr]) ((P - uniform.LCenters.[addr])  |> Vec.normalize) |> clamp -1.0 1.0

                        let mutable vt = Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>() 
                            
                        if dotOut > 0.0 then
                            for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                                let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                                vt.[vtc] <- uniform.LVertices.[vtcAddr]
                        else
                            for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                                let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                                vt.[vtc] <- uniform.LVerticesInverse.[vtcAddr]

                        let squad =  
                            let ex = vt.[1] - vt.[0]
                            let ey = vt.[3] - vt.[0]
                            let squad = SphericalQuad.sphQuadInit vt.[0] ex ey P
                            squad

                        //for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                        //    let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                        //    vt.[vtc] <- w2t * (vt.[vtc] - P)

                        ////////////////////////////////////////////////////////

                        // let (clippedVa, clippedVc) = clipPatch(V3d.Zero, V3d.OOI, vt, uniform.LBaseComponents.[addr])
                        let (clippedVa, clippedVc) = clipPatchTS( ( if dotOut > 0.0 then uniform.LVertices else uniform.LVerticesInverse) , uniform.LBaseComponents.[addr], P, w2t)

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
                                    
                                    
                                let normPlanePoint = linePlaneIntersection V3d.Zero up (clippedVa.[0]) lightPlaneN // tangent space
                                    
                                let (closestPoint, normPlanePoint) = 
                                        
                                    let (closestPoint, _, _, _) = clampPointToPolygon clippedVa clippedVc closestPoint t2l
                                    let (normPlanePoint, _, _, _) =   clampPointToPolygon clippedVa clippedVc normPlanePoint t2l 
     
                                    (closestPoint, normPlanePoint)
                                    
                                let mutable barycenter = V3d.Zero
                                for l in 0 .. clippedVc - 1 do
                                    barycenter <- barycenter + clippedVa.[l]
                                    
                                let barycenter = barycenter / (float clippedVc)

                                let mrpDir = ((closestPoint |> Vec.normalize) + (normPlanePoint |> Vec.normalize)) |> Vec.normalize
                                let mrp = linePlaneIntersection V3d.Zero mrpDir (clippedVa.[0]) lightPlaneN

                                    

                                let mutable sampleCount = 0        
                                let mutable patchIllumination = 0.0

                                let mutable sampleIdx = 0
                                let samples = Arr<N<MAX_SAMPLE_NUM_WO_RANDOM>, V3d>() // all samples except random samples
                                    
                                if uniform.sampleCorners then   
                                    for l in 0 .. Config.Light.MAX_PATCH_SIZE_PLUS_ONE - 1 do
                                        if l < clippedVc then
                                            //if not (sampleAlreadyExisting samples sampleIdx clippedVa.[l]) then
                                            samples.[sampleIdx] <- V3d(clippedVa.[l])
                                            sampleIdx <- sampleIdx + 1
                                            sampleCount <- sampleIdx
                                            
                                if uniform.sampleBarycenter (* && not (sampleAlreadyExisting samples sampleIdx barycenter)      *) then
                                    samples.[sampleIdx] <- V3d(barycenter)
                                    sampleIdx <- sampleIdx + 1
                                    sampleCount <- sampleIdx

                                if uniform.sampleNorm       (* && not (sampleAlreadyExisting samples sampleIdx normPlanePoint)  *) then
                                    samples.[sampleIdx] <- V3d(normPlanePoint)
                                    sampleIdx <- sampleIdx + 1
                                    sampleCount <- sampleIdx

                                if uniform.sampleMRP        (* && not (sampleAlreadyExisting samples sampleIdx mrp)             *) then
                                    samples.[sampleIdx] <- V3d(mrp)
                                    sampleIdx <- sampleIdx + 1
                                    sampleCount <- sampleIdx

                                if uniform.sampleClosest    (* && not (sampleAlreadyExisting samples sampleIdx closestPoint)    *) then
                                    samples.[sampleIdx] <- closestPoint
                                    sampleIdx <- sampleIdx + 1
                                    sampleCount <- sampleIdx

                                if uniform.sampleRandom then        
                                    sampleCount <- sampleCount + uniform.numSRSamples
                                                                          

                                if sampleIdx > 0 then
                                                                                                            
                                    for l in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
                                        if l < sampleIdx then 

                                            // let scale = uniform.weightScaleSRSamples * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr samples.[l]
                                                
                                            //let irr = sample t2w samplesWeightScale.[l] addr samples.[l]

                                            let irr = sample t2w squad.S addr samples.[l]
                                            patchIllumination <- patchIllumination + irr

                                if uniform.sampleRandom && uniform.numSRSamples > 0 then
                                    if uniform.sampleLight then
                                        for l in 0 .. uniform.numSRSamples - 1 do
                                            let uvSamplePoint = uniform.LUVSamplePoints.[l]

                                            let samplePoint = (SphericalQuad.sphQuadSample squad uvSamplePoint.X uvSamplePoint.Y) - P

                                            if samplePoint.Z >= eps then
                                                let irr = sample t2w squad.S addr samplePoint
                                                patchIllumination <- patchIllumination + irr

                                    else
                                        for l in 0 .. uniform.numSRSamples - 1 do
                                            let samplePoint = w2t * (uniform.LSamplePoints.[l] - P)

                                            if samplePoint.Z >= eps then
                                                // let scale = uniform.weightScaleSRSamples * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr samplePoint
                                                let irr = sample t2w squad.S addr samplePoint
                                                patchIllumination <- patchIllumination + irr

                                            

                                if sampleCount > 0 then

                                    //let clampedDist = (clamp 0.0 uniform.scaleSRSampleDist (Vec.length closestPoint)) / uniform.scaleSRSampleDist
                                    //let scale = clampedDist * 1.0 + (1.0 - clampedDist) * uniform.weightScaleSRSamples
                                           
                                    illumination <- illumination + (1.0 / float(sampleCount)) * (v.c / PI) * patchIllumination //* scale  
                            ()
                                                                
                        ////////////////////////////////////////////////////////
                    ()

        return V4d(illumination.XYZ, v.c.W)
    }

[<ReflectedDefinition>]
let private samplePointRandom (t2w : M33d) (S : float) (addr : int) (p : V3d) = 

    let i = p |> Vec.normalize  
    let iw = t2w * -i


    if uniform.sampleLight then
            
        let dotOut = max 1e-9 (abs (Vec.dot iw uniform.LForwards.[addr]))

        let irr = getPhotometricIntensity iw uniform.LForwards.[addr] uniform.LUps.[addr] / (uniform.LAreas.[addr] * dotOut)

        i.Z * irr * S
    else
        let irr = getPhotometricIntensity iw uniform.LForwards.[addr] uniform.LUps.[addr] 

        irr * i.Z / (Vec.lengthSquared p + 1e-9)


let structuredSamplingRandom (v : Vertex) = 
    fragment {

        ////////////////////////////////////////////////////////

        let P = v.wp.XYZ

        let t2w = v.n |> Vec.normalize |> basisFrisvad 
        let w2t = t2w |> Mat.transpose
            
        let mutable illumination = V4d.Zero * (uniform.dT * 1e-256 * 0.0)
            
        ////////////////////////////////////////////////////////

        for addr in 0 .. (Config.Light.NUM_LIGHTS - 1) do 
            match uniform.Lights.[addr] with
                | -1 -> ()
                |  _ ->    
                        
                    let vAddr = addr * Config.Light.VERT_PER_LIGHT
                    let iAddr = addr * Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT
                    let sAddr = addr * Config.Light.SS_LIGHT_SAMPLES_PER_LIGHT

                    ////////////////////////////////////////////////////////

                    let l2w = M33dFromCols  (V3d.Cross((uniform.LUps.[addr]), (uniform.LForwards.[addr]))) uniform.LUps.[addr] uniform.LForwards.[addr]
                            
                    let w2l = l2w |> Mat.transpose

                    let t2l = w2l * t2w

                    ////////////////////////////////////////////////////////


                    for iIdx in iAddr .. Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT .. (iAddr + uniform.LNumPatchIndices.[addr] - 1) do
                            
                        let dotOut = Vec.dot (uniform.LForwards.[addr]) ((P - uniform.LCenters.[addr])  |> Vec.normalize) |> clamp -1.0 1.0

                        let mutable vt = Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>() 
                        let mutable vtN = Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>() 
                            
                        if dotOut > 0.0 then
                            for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                                let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                                vt.[vtc] <- uniform.LVertices.[vtcAddr]
                                vtN.[vtc] <- w2t * (uniform.LVertices.[vtcAddr] - P)
                        else
                            for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                                let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                                vt.[vtc] <- uniform.LVerticesInverse.[vtcAddr]
                                vtN.[vtc] <- w2t * (uniform.LVertices.[vtcAddr] - P)

                        let squad =  
                            let ex = vt.[1] - vt.[0]
                            let ey = vt.[3] - vt.[0]
                            let squad = SphericalQuad.sphQuadInit vt.[0] ex ey P
                            squad
                            
                        let (clippedVa, clippedVc) = clipPatchTS( ( if dotOut > 0.0 then uniform.LVertices else uniform.LVerticesInverse) , uniform.LBaseComponents.[addr], P, w2t)

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
                                                                                                  

                                let mutable sampleCount = uniform.numSRSamples      
                                let mutable patchIllumination = 0.0


                                if uniform.numSRSamples > 0 then
                                    if uniform.sampleLight then
                                        for l in 0 .. uniform.numSRSamples - 1 do
                                            let uvSamplePoint = uniform.LUVSamplePoints.[l]

                                            let samplePoint = (SphericalQuad.sphQuadSample squad uvSamplePoint.X uvSamplePoint.Y) - P

                                            if samplePoint.Z >= eps then
                                                let irr = samplePointRandom t2w squad.S addr samplePoint
                                                patchIllumination <- patchIllumination + irr

                                    else
                                        let ex = vt.[1] - vt.[0]
                                        let ey = vt.[3] - vt.[0]
                                        for l in 0 .. uniform.numSRSamples - 1 do
                                            let uv = uniform.LUVSamplePoints.[l]
                                            let sampleOnLight = uv.X * ex + uv.Y * ey + vt.[0]
                                            let samplePoint = w2t * (sampleOnLight - P)

                                            if samplePoint.Z >= eps then
                                                // let scale = uniform.weightScaleSRSamples * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr samplePoint
                                                let irr = samplePointRandom t2w squad.S addr samplePoint
                                                patchIllumination <- patchIllumination + irr

                                            

                                if sampleCount > 0 then

                                    //let clampedDist = (clamp 0.0 uniform.scaleSRSampleDist (Vec.length closestPoint)) / uniform.scaleSRSampleDist
                                    //let scale = clampedDist * 1.0 + (1.0 - clampedDist) * uniform.weightScaleSRSamples
                                           
                                    illumination <- illumination + (1.0 / float(sampleCount)) * (v.c / PI) * patchIllumination //* scale  
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

    type SSData = {
        sampleCorners        : IMod<bool>
        sampleBarycenter     : IMod<bool>
        sampleClosest        : IMod<bool>
        sampleNorm           : IMod<bool>
        sampleMRP            : IMod<bool>
        sampleRandom         : IMod<bool>
        sampleLight          : IMod<bool>
        blendSamples         : IMod<bool>
        blendEasing          : IMod<bool>
        blendDistance        : IMod<float>
        numSRSamples         : IMod<int>
        SRSWeightScale       : IMod<float>
        TangentApproxDist    : IMod<float>
        SRSWeightScaleIrr    : IMod<float>
        TangentApproxDistIrr : IMod<float>
        CombinedLerpValue    : IMod<float>
    }

    let initSSData (m : MRenderState) = 
        {
            sampleCorners        = m.sampleCorners
            sampleBarycenter     = m.sampleBarycenter
            sampleClosest        = m.sampleClosest
            sampleNorm           = m.sampleNorm
            sampleMRP            = m.sampleMRP
            sampleRandom         = m.sampleRandom
            sampleLight          = m.sampleLight
            blendSamples         = m.blendSamples
            blendEasing          = m.blendEasing
            blendDistance        = m.blendDistance.value
            numSRSamples         = m.numOfSRSamples.value |> Mod.map (fun numSRS -> (int)(ceil numSRS))
            SRSWeightScale       = m.SRSWeightScale.value
            TangentApproxDist    = m.TangentApproxDist.value
            SRSWeightScaleIrr    = m.SRSWeightScaleIrr.value
            TangentApproxDistIrr = m.TangentApproxDistIrr.value
            CombinedLerpValue    = m.CombinedSSWeight.value
        }
        
    let private pointSg color trafo = 
        IndexedGeometryPrimitives.solidSubdivisionSphere (Sphere3d(V3d.Zero, 0.04)) 6 color
        |> Sg.ofIndexedGeometry
        |> Sg.trafo trafo
        |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.vertexColor |> toEffect
            ]

    //// closestPointTrafo, normPlanePointTrafo, mrpTrafo
    //let private getSamplePointSg (lc : Light.LightCollection) (ssData : SSData) (point : V3d * V3d) = 

    //    let M33dFromCols (c1 : V3d) (c2 : V3d) (c3 : V3d) =
    //        M33d(c1.X, c2.X, c3.X, c1.Y, c2.Y, c3.Y, c1.Z, c2.Z, c3.Z)

    //    let basisFrisvad (n : V3d) = 
    //        let c1 = V3d(
    //                    1.0 - (n.X  * n.X) / (1.0 + n.Z),
    //                    (-n.X * n.Y) / (1.0 + n.Z),
    //                    -n.X
    //                    )

    //        let c2 = V3d(
    //                    (-n.X * n.Y) / (1.0 + n.Z),
    //                    1.0 - (n.Y  * n.Y) / (1.0 + n.Z),
    //                    -n.Y
    //                    )

    //        let c3 = n
            
    //        M33dFromCols c1 c2 c3
            
    //    let (P, n) = point

    //    let t2w = n |> Vec.normalize |> basisFrisvad 
    //    let w2t = t2w |> Mat.transpose

    //    let samplePointPositions =
    //        adaptive {

    //            let! lights = lc.Lights
                
    //            ////////////////////////////////////////////////////////

    //            let addr = 0

    //            let vAddr = addr * Config.Light.VERT_PER_LIGHT
    //            let iAddr = addr * Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT

    //            ////////////////////////////////////////////////////////

    //            let! lUps = lc.Ups
    //            let! lForwards = lc.Forwards

    //            let l2w = M33dFromCols (V3d.Cross((lUps.[addr]), (lForwards.[addr]))) lUps.[addr] lForwards.[addr]
                            
    //            let w2l = l2w |> Mat.transpose

    //            let t2l = w2l * t2w
                        
    //            ////////////////////////////////////////////////////////
                
    //            let! lPatchIndices = lc.PatchIndices
    //            let! lVertices = lc.Vertices
                
    //            let! lBaseComponents = lc.BaseComponents
    //            let! lForwards = lc.Forwards
                                   
    //            let! blendDistance = ssData.blendDistance
                
    //            let computeLightData iIdx = 
                            
    //                let mutable vt = Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>() 
                            
    //                for vtc in 0 .. lBaseComponents.[addr] - 1 do
    //                    let vtcAddr = lPatchIndices.[iIdx + vtc] + vAddr
    //                    vt.[vtc] <- w2t * (lVertices.[vtcAddr] - P)

    //                ////////////////////////////////////////////////////////

    //                let (clippedVa, clippedVc) = clipPatch(V3d.Zero, V3d.OOI, vt, lBaseComponents.[addr])

    //                if clippedVc <> 0 then

    //                    let eps = 1e-9
    //                    let epb = 1e-3

    //                    let mutable barycenter = V3d.Zero
    //                    for l in 0 .. clippedVc - 1 do
    //                        barycenter <- barycenter + clippedVa.[l]
                                    
    //                    let barycenter = barycenter / (float clippedVc)
               
    //                    let lightPlaneN = w2t * lForwards.[addr] |> Vec.normalize                                

    //                    // find closest point limited to upper hemisphere
    //                    let t = (- clippedVa.[0]) |> Vec.dot lightPlaneN
    //                    let mutable closestPoint = t * (-lightPlaneN)
                                                    
    //                    if (Vec.dot closestPoint V3d.OOI) < 0.0 then
    //                        let newDir = V3d(closestPoint.X, closestPoint.Y, 0.0) |> Vec.normalize
    //                        closestPoint <- linePlaneIntersection V3d.Zero newDir (clippedVa.[0]) lightPlaneN
                                    
    //                    let insideLightPlane = (Vec.length closestPoint) < eps
                                
    //                    if not insideLightPlane then
                                    
    //                        let closestPointDir = closestPoint |> Vec.normalize

    //                        // intersect normal with plane
    //                        let mutable up = V3d.OOI
                                
    //                        if abs(Vec.dot up lightPlaneN) < eps then
    //                            up <- up + (epb * closestPointDir) |> Vec.normalize     
    //                        else
    //                            let abovePlane = if (Vec.dot V3d.OOI closestPoint) < 0.0 && (Vec.dot closestPoint lightPlaneN) < 0.0 then false else true
    //                            if abovePlane then
    //                                if (Vec.dot up lightPlaneN) > 0.0 then
    //                                    up <- up + (abs(Vec.dot up lightPlaneN) + epb) * (-lightPlaneN) |> Vec.normalize
                                    
                                    
    //                        let normPlanePoint = linePlaneIntersection V3d.Zero up (clippedVa.[0]) lightPlaneN // tangent space
                                    
    //                        let (closestPoint, normPlanePoint) = 
                                        
    //                            let (closestPoint, _, _, _) = clampPointToPolygon clippedVa clippedVc closestPoint t2l
    //                            let (normPlanePoint, _, _, _) =   clampPointToPolygon clippedVa clippedVc normPlanePoint t2l 
     
    //                            (closestPoint, normPlanePoint)
                                    
    //                        let mrpDir = ((closestPoint |> Vec.normalize) + (normPlanePoint |> Vec.normalize)) |> Vec.normalize
    //                        let mrp = linePlaneIntersection V3d.Zero mrpDir (clippedVa.[0]) lightPlaneN

    //                        let mutable sampleCount = 0
    //                        let mutable sampleIdx = 0
    //                        let samples = Arr<N<MAX_SAMPLE_NUM_WO_RANDOM>, V3d>() // all samples except random samples
                                
    //                        let names = Arr<N<MAX_SAMPLE_NUM_WO_RANDOM>, string>()

    //                        // corners
    //                        if true then   
    //                            for l in 0 .. Config.Light.MAX_PATCH_SIZE_PLUS_ONE - 1 do
    //                                if l < clippedVc then                                                
    //                                    // if not (sampleAlreadyExisting samples sampleIdx clippedVa.[l]) then
    //                                        samples.[sampleIdx] <- V3d(clippedVa.[l])
                                                
    //                                        names.[sampleIdx] <- "Corner" + string(l)
    //                                        sampleIdx <- sampleIdx + 1
    //                                        sampleCount <- sampleIdx
                                                
                                            
    //                        // barycenter
    //                        if false then                                                   
    //                            // if not (sampleAlreadyExisting samples sampleIdx barycenter) then
    //                                samples.[sampleIdx] <- V3d(barycenter)
                                        
    //                                names.[sampleIdx] <- "Barycenter"
    //                                sampleIdx <- sampleIdx + 1
    //                                sampleCount <- sampleIdx


    //                        // norm
    //                        if true then                                                  
    //                            // if not (sampleAlreadyExisting samples sampleIdx normPlanePoint) then
    //                                samples.[sampleIdx] <- V3d(normPlanePoint)
                                        
    //                                names.[sampleIdx] <- "Norm"
    //                                sampleIdx <- sampleIdx + 1
    //                                sampleCount <- sampleIdx

                                    
    //                        // mrp
    //                        if true then                                                  
    //                            // if not (sampleAlreadyExisting samples sampleIdx mrp) then
    //                                samples.[sampleIdx] <- V3d(mrp)
                                        
    //                                names.[sampleIdx] <- "MRP"
    //                                sampleIdx <- sampleIdx + 1
    //                                sampleCount <- sampleIdx

                                    
    //                        // closest
    //                        if true then  
    //                            // if not (sampleAlreadyExisting samples sampleIdx closestPoint) then
    //                                samples.[sampleIdx] <- closestPoint
                                        
    //                                names.[sampleIdx] <- "Closest"
    //                                sampleIdx <- sampleIdx + 1
    //                                sampleCount <- sampleIdx



    //                        let samplesWeightScale = Arr<N<MAX_SAMPLE_NUM_WO_RANDOM>, float>()
    //                        let neighborhoodSize = Arr<N<MAX_SAMPLE_NUM_WO_RANDOM>, int>() 
                                                     
    //                        if sampleIdx > 0 then
    //                            for r in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
    //                                if r < sampleIdx then 
    //                                    samplesWeightScale.[r] <- 1.0
                                            
    //                            for r in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
    //                                if r < sampleIdx then 

    //                                    for o in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
    //                                        if r < o && o < sampleIdx then

    //                                            let dist  = Vec.length (samples.[r] - samples.[o])

    //                                            if dist < blendDistance then
    //                                                neighborhoodSize.[r] <- neighborhoodSize.[r] + 1
    //                                                neighborhoodSize.[o] <- neighborhoodSize.[o] + 1
                                                        
    //                                    let scale = 
    //                                        if neighborhoodSize.[r] = 0 then
    //                                            1.0
    //                                        elif neighborhoodSize.[r] = 1 then
    //                                            MIN_WEIGHT_SCALE_FACTOR_1
    //                                        elif neighborhoodSize.[r] = 2 then 
    //                                            MIN_WEIGHT_SCALE_FACTOR_2
    //                                        else
    //                                            MIN_WEIGHT_SCALE_FACTOR_3

    //                                    for o in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
    //                                        if o < sampleIdx && o <> r then
    //                                            let dist  = Vec.length (samples.[r] - samples.[o])
    //                                            samplesWeightScale.[r] <- (computeSampleScale (blendDistance) dist scale) * samplesWeightScale.[r]
                                                    


    //                        (samples |> Arr.map (fun s -> t2w * s + P), sampleCount)

    //                    else
    //                        (Arr<N<MAX_SAMPLE_NUM_WO_RANDOM>, V3d>([t2w * closestPoint + P]), 1)

    //                else
    //                    (Arr<N<MAX_SAMPLE_NUM_WO_RANDOM>, V3d>(), 0)

    //            return computeLightData 0
                
    //        }
        
    //    let getTrafo idx = samplePointPositions |> Mod.map( fun (positions, c) ->  if idx < c then Trafo3d.Translation positions.[idx] else Trafo3d.Identity )
           
                    
    //    let mutable sg = Sg.empty
            
    //    for i in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do           
    //        sg <- Sg.group' [sg; pointSg C4b.Red (getTrafo i)]

    //    sg
            
                        

    let private setupSS_RenderTask (data : RenderData) (ssData : SSData) (signature : IFramebufferSignature) (sceneSg : ISg) (ssEffect : FShade.Effect)= 

        (*
        let pointSg color trafo = 
                IndexedGeometryPrimitives.solidSubdivisionSphere (Sphere3d(V3d.Zero, 0.01)) 6 color
            |> Sg.ofIndexedGeometry
            |> Sg.trafo trafo
            |> Sg.effect [
                    DefaultSurfaces.trafo |> toEffect
                    DefaultSurfaces.vertexColor |> toEffect
                ]
                    
        let getTrafo sidx = 
                data.lights.SamplePoints |>
                Mod.map(fun sp -> 
                    Trafo3d.Translation sp.[sidx]
                )
        let pointSg = 
            ssData.numSRSamples |> Mod.map (fun numSRSamples ->
                let mutable sg = Sg.empty
                if numSRSamples > 0 then
                    for i in 0 .. numSRSamples - 1 do               
                        let sampleSg = pointSg C4b.Red (getTrafo i)
                        sg <- Sg.group' [sg; sampleSg]
                sg
            |> Sg.dynamic
            *)
        let measurePointPos     = V3d(-14.0, 0.0, 0.0)
        let measurePointTrafo   = measurePointPos |> Trafo3d.Translation |> Mod.constant
        let measurePoint        = pointSg C4b.VRVisGreen measurePointTrafo
        (*
        let sceneSg = 
            [
                sceneSg
                measurePoint
                getSamplePointSg data.lights ssData (measurePointPos, V3d.OOI)
            ]
            |> Sg.group'
        *)

        sceneSg 
            |> setupFbEffects [ 
                    ssEffect
                    EffectUtils.effectClearNaN |> toEffect
                ]
            |> Light.Sg.addLightCollectionSg (data.lights) (data.lightData)
            |> Light.Sg.setLightCollectionUniforms data.lights
            |> setupPhotometricData data.photometricData
            |> setupCamera data.view data.projTrafo data.viewportSize 
            |> setUniformDT data.dt
            |> setUniformUsePhotometry data.lightData.usePhotometry
            |> setUniformDiffuseExitance data.lightData.diffuseExitance
            |> Sg.uniform "sampleCorners"           ssData.sampleCorners
            |> Sg.uniform "sampleBarycenter"        ssData.sampleBarycenter
            |> Sg.uniform "sampleClosest"           ssData.sampleClosest
            |> Sg.uniform "sampleNorm"              ssData.sampleNorm
            |> Sg.uniform "sampleMRP"               ssData.sampleMRP
            |> Sg.uniform "sampleRandom"            ssData.sampleRandom
            |> Sg.uniform "sampleLight"             ssData.sampleLight
            |> Sg.uniform "blendSamples"            ssData.blendSamples
            |> Sg.uniform "blendEasing"             ssData.blendEasing
            |> Sg.uniform "numSRSamples"            ssData.numSRSamples
            |> Sg.uniform "blendDistance"           ssData.blendDistance
            |> Sg.uniform "weightScaleSRSamples"    ssData.SRSWeightScale
            |> Sg.uniform "tangentApproxDist"       ssData.TangentApproxDist
            |> Sg.uniform "weightScaleSRSamplesIrr" ssData.SRSWeightScaleIrr
            |> Sg.uniform "tangentApproxDistIrr"    ssData.TangentApproxDistIrr
            |> Sg.uniform "combinedLerpValue"       ssData.CombinedLerpValue
            |> Sg.compile data.runtime signature
                

    let private setupSS_Fb (data : RenderData) (ssData : SSData) (signature : IFramebufferSignature)  (sceneSg : ISg) (ssEffect : FShade.Effect) = 
        setupSS_RenderTask data ssData signature sceneSg ssEffect
        |> RenderTask.renderToColor data.viewportSize 
            
            
    let ssApproxFb (data : RenderData) (ssData : SSData) (signature : IFramebufferSignature)  (sceneSg : ISg) =
        structuredSamplingRandom |> toEffect |> setupSS_Fb data ssData signature sceneSg 

    //let ssIrrApproxFb (data : RenderData) (ssData : SSData) (signature : IFramebufferSignature)  (sceneSg : ISg) =
    //    structuredIrradianceSampling |> toEffect |> setupSS_Fb data ssData signature sceneSg

        
    // sampleCorners sampleBarycenter sampleClosest sampleNorm sampleMRP sampleRandom numSRSamples blendSamples blendEasing blendDistance
    let encodeSettingsForName (ssData : SSData) = 
            
        let mTb m = m |> Mod.force

        let mutable settings = ""

        if mTb ssData.sampleCorners then settings <- String.concat "_" [ settings; "sCorners" ]
        if mTb ssData.sampleBarycenter then settings <- String.concat "_" [ settings; "sBarycenter"] 
        if mTb ssData.sampleClosest then settings <- String.concat "_" [ settings; "sClosest" ]
        if mTb ssData.sampleNorm then settings <- String.concat "_" [ settings; "sNorm" ]
        if mTb ssData.sampleMRP then settings <- String.concat "_" [ settings; "sMRP" ]
        if mTb ssData.sampleRandom then settings <- String.concat "_" [ settings; (sprintf "sRandom-%i" (ssData.numSRSamples |> Mod.force)) ]
        if mTb ssData.sampleLight then settings <- String.concat "_" [ settings; "sampleLight" ]

        if mTb ssData.blendSamples then
            let mutable s = sprintf "blend-%f" (ssData.blendDistance |> Mod.force)

            if mTb ssData.blendEasing then
                s <- String.concat "-" [s; "ease"]
                    
            settings <- String.concat "_" [ settings; s]

        settings