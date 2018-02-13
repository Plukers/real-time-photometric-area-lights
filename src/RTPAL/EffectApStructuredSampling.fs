namespace Render

module EffectApStructuredSampling =
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open FShade

    open Light.Effect
    open EffectUtils
    open PhotometricLight

    type UniformScope with
        member uniform.sampleCorners            : bool  = uniform?sampleCorners     
        member uniform.sampleBarycenter         : bool  = uniform?sampleBarycenter  
        member uniform.sampleClosest            : bool  = uniform?sampleClosest     
        member uniform.sampleNorm               : bool  = uniform?sampleNorm 
        member uniform.sampleMRP                : bool  = uniform?sampleMRP 
        member uniform.sampleRandom             : bool  = uniform?sampleRandom
        member uniform.blendSamples             : bool  = uniform?blendSamples
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
    let NEIGHBORHOOD_SIZE = 0.4

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
    let computeSampleScale dist scale =
        if NEIGHBORHOOD_SIZE < dist then
            1.0
        else
            let dist = dist / NEIGHBORHOOD_SIZE
            (1.0 - dist) * scale + dist * 1.0
            
    // solid angle https://en.wikipedia.org/wiki/Solid_angle#Cone,_spherical_cap,_hemisphere
    [<ReflectedDefinition>]
    let private computeApproximateSolidAnglePerSample (t2w : M33d) (sampleCount : int) (thetaScale : float) (addr : int) (p : V3d) = 
        let i = p |> Vec.normalize 
        let radiusScale = abs (Vec.dot -(t2w * i) uniform.LForwards.[addr])
        let sampleDensity = thetaScale * radiusScale * uniform.LAreas.[addr] / float(sampleCount) * 0.5
        let theta = asin (sampleDensity / ((p |> Vec.length)))
        2.0 * PI * (1.0 - cos theta) 
                                    

    [<ReflectedDefinition>]
    let private quadraticDistanceDerivative (c : float) (a :float) (x0 : float) (x : float) =

        //let x0 = 0.4
        let y0 = c / (x0 * x0)

        (-2.0 * c * x0) / (pow (a + x0 * x0) 2.0) * x + y0 + (2.0 * c * x0 * x0) / (pow (a + x0 * x0) 2.0)
        
    
    
    [<ReflectedDefinition>]
    let private sampleIrr (t2w : M33d) (scale : float) (addr : int) (p : V3d) = 
        let i = p |> Vec.normalize  
 
        let dotOut = max 1e-9 (abs (Vec.dot -(t2w * i) uniform.LForwards.[addr]))
        
        let irr = getPhotometricIntensity -(t2w * i) uniform.LForwards.[addr]  uniform.LUps.[addr] // / (uniform.LAreas.[addr] * dotOut)

        //let dist = Vec.length p
        //let qdd = quadraticDistanceDerivative (uniform.LAreas.[addr] * dotOut * i.Z) (max 1e-9 scale) uniform.tangentApproxDistIrr dist

        let weight = scale *  i.Z / (Vec.lengthSquared p + 1e-9) // add i.Z for a better weight
        
        let sampledIrr = weight * irr
        (*
        let dist = Vec.length p
        let weight = 
            if dist < uniform.tangentApproxDistIrr then
                quadraticDistanceDerivative (uniform.LAreas.[addr] * dotOut * i.Z) (max 1e-9 scale) uniform.tangentApproxDistIrr dist
            else
                (uniform.LAreas.[addr] * dotOut) * weight
        *)
        let weight = (uniform.LAreas.[addr] * dotOut) * weight

        (sampledIrr, weight)
        

    let structuredIrradianceSampling (v : Vertex) = 
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


                                let mutable barycenter = V3d.Zero
                                for l in 0 .. Config.MAX_PATCH_SIZE_PLUS_ONE - 1 do
                                    if l < clippedVc then
                                        barycenter <- barycenter + clippedVa.[l]
                                    
                                let barycenter = barycenter / (float clippedVc)
               
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
                                        
                                        let closestPoint = clampPointToPolygon clippedVa clippedVc closestPoint t2l
                                        let normPlanePoint =   clampPointToPolygon clippedVa clippedVc normPlanePoint t2l 
     
                                        (closestPoint, normPlanePoint)

                                    let mrpDir = ((closestPoint |> Vec.normalize) + (normPlanePoint |> Vec.normalize)) |> Vec.normalize
                                    let mrp = linePlaneIntersection V3d.Zero mrpDir (clippedVa.[0]) lightPlaneN


                                    let mutable sampleCount = 0
                                    let mutable sampleIdx = 0
                                    let samples = Arr<N<MAX_SAMPLE_NUM_WO_RANDOM>, V3d>() // all samples except random samples
                                    
                                    if uniform.sampleCorners then   
                                        for l in 0 .. Config.MAX_PATCH_SIZE_PLUS_ONE - 1 do
                                            if l < clippedVc then
                                                // if not (sampleAlreadyExisting samples sampleIdx clippedVa.[l]) then
                                                samples.[sampleIdx] <- V3d(clippedVa.[l])
                                                sampleIdx <- sampleIdx + 1
                                                sampleCount <- sampleIdx
                                            
                                    if uniform.sampleBarycenter (* && not (sampleAlreadyExisting samples sampleIdx barycenter)      *)  then
                                            samples.[sampleIdx] <- V3d(barycenter)
                                            sampleIdx <- sampleIdx + 1
                                            sampleCount <- sampleIdx

                                    if uniform.sampleNorm       (* && not (sampleAlreadyExisting samples sampleIdx normPlanePoint)  *)  then
                                            samples.[sampleIdx] <- V3d(normPlanePoint)
                                            sampleIdx <- sampleIdx + 1
                                            sampleCount <- sampleIdx

                                    if uniform.sampleMRP        (* && not (sampleAlreadyExisting samples sampleIdx mrp)             *)  then
                                            samples.[sampleIdx] <- V3d(mrp)
                                            sampleIdx <- sampleIdx + 1
                                            sampleCount <- sampleIdx

                                    if uniform.sampleClosest    (* && not (sampleAlreadyExisting samples sampleIdx closestPoint)    *)  then
                                            samples.[sampleIdx] <- closestPoint
                                            sampleIdx <- sampleIdx + 1
                                            sampleCount <- sampleIdx

                                    if uniform.sampleRandom then        
                                        sampleCount <- sampleCount + uniform.numSRSamples


                                    
                                    let samplesWeightScale = Arr<N<MAX_SAMPLE_NUM_WO_RANDOM>, float>()
                                    let neighborhoodSize = Arr<N<MAX_SAMPLE_NUM_WO_RANDOM>, int>() 
                                                     
                                    let mutable patchIllumination = 0.0
                                    let mutable weightSum = 0.0

                                    if sampleIdx > 0 then
                                        for r in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
                                            if r < sampleIdx then 
                                                samplesWeightScale.[r] <- 1.0

                                        if uniform.blendSamples then 

                                            for r in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
                                                if r < sampleIdx then 

                                                    for o in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
                                                        if r < o && o < sampleIdx then

                                                            let dist  = Vec.length (samples.[r] - samples.[o])

                                                            if dist < NEIGHBORHOOD_SIZE then
                                                                neighborhoodSize.[r] <- neighborhoodSize.[r] + 1
                                                                neighborhoodSize.[o] <- neighborhoodSize.[o] + 1


                                                    let scale = 
                                                        if neighborhoodSize.[r] = 0 then
                                                            1.0
                                                        elif neighborhoodSize.[r] = 1 then
                                                            MIN_WEIGHT_SCALE_FACTOR_1
                                                        elif neighborhoodSize.[r] = 2 then 
                                                            MIN_WEIGHT_SCALE_FACTOR_2
                                                        else
                                                            MIN_WEIGHT_SCALE_FACTOR_3

                                                    for o in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
                                                        if o < sampleIdx && o <> r then
                                                            let dist  = Vec.length (samples.[r] - samples.[o])
                                                            samplesWeightScale.[r] <- (computeSampleScale dist scale) * samplesWeightScale.[r]

                                        for l in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
                                            if l < sampleIdx then 
                                                //let scale = uniform.weightScaleSRSamplesIrr * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDistIrr addr samples.[l]

                                                let (irr, weight) = sampleIrr t2w samplesWeightScale.[l] addr samples.[l]
                                                patchIllumination <- patchIllumination + irr
                                                weightSum <- weightSum + weight
                                                       
                                    if uniform.sampleRandom && uniform.numSRSamples > 0 then
                                        for l in 0 .. uniform.numSRSamples do
                                            let samplePoint = w2t * (uniform.LSamplePoints.[l] - P)

                                            if samplePoint.Z >= eps then
                                                // let scale = uniform.weightScaleSRSamplesIrr * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDistIrr addr samplePoint

                                                let (irr, weight) = sampleIrr t2w 1.0 addr samplePoint
                                                patchIllumination <- patchIllumination + irr
                                                weightSum <- weightSum + weight
                                                
                                    let L =
                                        if sampleCount > 0 then
                                            patchIllumination / weightSum
                                        else 
                                            0.0

                                    
                                    for l in 0 .. Config.MAX_PATCH_SIZE_PLUS_ONE - 1 do
                                            if l < clippedVc then
                                                // Project polygon light onto sphere
                                                clippedVa.[l] <- Vec.normalize clippedVa.[l]

                                    for l in 0 .. clippedVc - 1 do
                                        // Project polygon light onto sphere
                                        clippedVa.[l] <- Vec.normalize clippedVa.[l]

                                    let I = abs (baumFormFactor(clippedVa, clippedVc)) / (2.0) // should be divided by 2 PI, but PI is already in the brdf
                                        
                                    //let clampedDist = (clamp 0.0 uniform.scaleSRSampleDist (Vec.length closestPoint)) / uniform.scaleSRSampleDist
                                    //let scale = clampedDist * 1.0 + (1.0 - clampedDist) * uniform.weightScaleSRSamplesIrr

                                    illumination <- illumination + L * brdf * I //* scale // * i.Z  
                                    
                                    (*
                                    if sampleCount < 5 then
                                        illumination <- V4d(0.0, 0.0, 0.0, 1.0) 
                                    if sampleCount = 5 then
                                        illumination <- V4d(1.0, 0.0, 0.0, 1.0) 
                                    elif sampleCount = 6 then
                                        illumination <- V4d(0.5, 0.5, 0.0, 1.0) 
                                    elif sampleCount = 7 then
                                        illumination <- V4d(0.0, 1.0, 0.0, 1.0) 
                                    elif sampleCount = 8 then
                                        illumination <- V4d(0.0, 0.5, 0.5, 1.0) 
                                    elif sampleCount = 9 then
                                        illumination <- V4d(0.0, 0.0, 1.0, 1.0) 
                                    else 
                                        illumination <- V4d(1.0, 1.0, 1.0, 1.0) 
                                    *)
                                    
                                ()
                                                                
                            ////////////////////////////////////////////////////////
                        ()

            return V4d(illumination.XYZ, v.c.W)
        }
    

    
    [<ReflectedDefinition>]
    let private sample (t2w : M33d) (scale : float) (addr : int) (p : V3d) = 

        let i = p |> Vec.normalize  
 
        //let dotOut = max 1e-5 (abs (Vec.dot -(t2w * i) uniform.LForwards.[addr]))
        let irr = getPhotometricIntensity -(t2w * i) uniform.LForwards.[addr]  uniform.LUps.[addr] // / (uniform.LAreas.[addr] * dotOut)
        
        let weight = (* uniform.LAreas.[addr] * dotOut *) 1.0 / (Vec.lengthSquared p + (max 1e-9 scale))
        (*
        let dist = Vec.length p
        let weight = 
            if dist < uniform.tangentApproxDist then
                quadraticDistanceDerivative 1.0 (max 1e-9 scale) uniform.tangentApproxDist dist
            else
                weight
        *)
        weight * irr * i.Z

    let structuredSampling (v : Vertex) = 
        fragment {

            ////////////////////////////////////////////////////////

            let P = v.wp.XYZ

            let t2w = v.n |> Vec.normalize |> basisFrisvad 
            let w2t = t2w |> Mat.transpose
            
            let mutable illumination = V4d.Zero
            
            ////////////////////////////////////////////////////////

            for addr in 0 .. (Config.NUM_LIGHTS - 1) do 
                match uniform.Lights.[addr] with
                    | -1 -> ()
                    |  _ ->    
                        
                        let vAddr = addr * Config.VERT_PER_LIGHT
                        let iAddr = addr * Config.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT
                        let sAddr = addr * Config.SS_LIGHT_SAMPLES_PER_LIGHT

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
                                    
                                    
                                    let normPlanePoint = linePlaneIntersection V3d.Zero up (clippedVa.[0]) lightPlaneN // tangent space
                                    
                                    let (closestPoint, normPlanePoint) = 
                                        
                                        let closestPoint = clampPointToPolygon clippedVa clippedVc closestPoint t2l
                                        let normPlanePoint =   clampPointToPolygon clippedVa clippedVc normPlanePoint t2l 
     
                                        (closestPoint, normPlanePoint)
                                    
                                    let mutable barycenter = V3d.Zero
                                    for l in 0 .. clippedVc - 1 do
                                        barycenter <- barycenter + clippedVa.[l]
                                    
                                    let barycenter = barycenter / (float clippedVc)

                                    let mrpDir = ((closestPoint |> Vec.normalize) + (normPlanePoint |> Vec.normalize)) |> Vec.normalize
                                    let mrp = linePlaneIntersection V3d.Zero mrpDir (clippedVa.[0]) lightPlaneN

                                    

                                    let mutable sampleCount = 0
                                    let mutable sampleIdx = 0
                                    let samples = Arr<N<MAX_SAMPLE_NUM_WO_RANDOM>, V3d>() // all samples except random samples
                                    
                                    if uniform.sampleCorners then   
                                        for l in 0 .. Config.MAX_PATCH_SIZE_PLUS_ONE - 1 do
                                            if l < clippedVc then
                                                if not (sampleAlreadyExisting samples sampleIdx clippedVa.[l]) then
                                                    samples.[sampleIdx] <- V3d(clippedVa.[l])
                                                    sampleIdx <- sampleIdx + 1
                                                    sampleCount <- sampleIdx
                                            
                                    if uniform.sampleBarycenter && not (sampleAlreadyExisting samples sampleIdx barycenter) then
                                            samples.[sampleIdx] <- V3d(barycenter)
                                            sampleIdx <- sampleIdx + 1
                                            sampleCount <- sampleIdx

                                    if uniform.sampleNorm && not (sampleAlreadyExisting samples sampleIdx normPlanePoint) then
                                            samples.[sampleIdx] <- V3d(normPlanePoint)
                                            sampleIdx <- sampleIdx + 1
                                            sampleCount <- sampleIdx

                                    if uniform.sampleMRP && not (sampleAlreadyExisting samples sampleIdx mrp) then
                                            samples.[sampleIdx] <- V3d(mrp)
                                            sampleIdx <- sampleIdx + 1
                                            sampleCount <- sampleIdx

                                    if uniform.sampleClosest && not (sampleAlreadyExisting samples sampleIdx closestPoint) then
                                            samples.[sampleIdx] <- closestPoint
                                            sampleIdx <- sampleIdx + 1
                                            sampleCount <- sampleIdx

                                    if uniform.sampleRandom then        
                                        sampleCount <- sampleCount + uniform.numSRSamples
                                                                                                                          

                                    let mutable patchIllumination = 0.0

                                    if sampleIdx > 0 then
                                        for l in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
                                            if l < sampleIdx then 
                                                let scale = uniform.weightScaleSRSamples * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr samples.[l]
                                                let irr = sample t2w scale addr samples.[l]
                                                patchIllumination <- patchIllumination + irr

                                    if uniform.sampleRandom && uniform.numSRSamples > 0 then
                                        for l in 0 .. uniform.numSRSamples - 1 do
                                            let samplePoint = w2t * (uniform.LSamplePoints.[l] - P)

                                            if samplePoint.Z >= eps then
                                                let scale = uniform.weightScaleSRSamples * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr samplePoint
                                                let irr = sample t2w scale addr samplePoint
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



    let combinedStructuredSampling (v : Vertex) = 
        fragment {

            ////////////////////////////////////////////////////////

            let P = v.wp.XYZ

            let t2w = v.n |> Vec.normalize |> basisFrisvad 
            let w2t = t2w |> Mat.transpose
            
            let mutable illumination = V4d.Zero

            let brdf = v.c / PI 
            
            ////////////////////////////////////////////////////////

            for addr in 0 .. (Config.NUM_LIGHTS - 1) do 
                match uniform.Lights.[addr] with
                    | -1 -> ()
                    |  _ ->    
                        
                        let vAddr = addr * Config.VERT_PER_LIGHT
                        let iAddr = addr * Config.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT
                        let sAddr = addr * Config.SS_LIGHT_SAMPLES_PER_LIGHT

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
                                    
                                    
                                    let normPlanePoint = linePlaneIntersection V3d.Zero up (clippedVa.[0]) lightPlaneN // tangent space
                                    
                                    let (closestPoint, normPlanePoint) = 
                                        
                                        let closestPoint = clampPointToPolygon clippedVa clippedVc closestPoint t2l
                                        let normPlanePoint =   clampPointToPolygon clippedVa clippedVc normPlanePoint t2l 
     
                                        (closestPoint, normPlanePoint)
                                    
                                    let mutable barycenter = V3d.Zero
                                    for l in 0 .. clippedVc - 1 do
                                        barycenter <- barycenter + clippedVa.[l]
                                    
                                    let barycenter = barycenter / (float clippedVc)

                                    let mrpDir = ((closestPoint |> Vec.normalize) + (normPlanePoint |> Vec.normalize)) |> Vec.normalize
                                    let mrp = linePlaneIntersection V3d.Zero mrpDir (clippedVa.[0]) lightPlaneN



                                    
                                    let mutable sampleCount = 0
                                    if uniform.sampleCorners then       sampleCount <- sampleCount + clippedVc
                                    if uniform.sampleBarycenter then    sampleCount <- sampleCount + 1
                                    if uniform.sampleNorm then          sampleCount <- sampleCount + 1
                                    if uniform.sampleMRP then           sampleCount <- sampleCount + 1
                                    if uniform.sampleClosest then       sampleCount <- sampleCount + 1
                                    if uniform.sampleRandom then        sampleCount <- sampleCount + uniform.numSRSamples

                                                                        
                                    let mutable patchIllumination = 0.0
                                    let mutable patchIlluminationIrr = 0.0
                                    let mutable weightSum = 0.0
                                                                        
                                    if uniform.sampleCorners then
                                        for l in 0 .. clippedVc - 1 do
                                            let omega = computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr clippedVa.[l]

                                            let scale = uniform.weightScaleSRSamples * omega
                                            let irr = sample t2w scale addr clippedVa.[l]
                                            patchIllumination <- patchIllumination + irr

                                            let scale = uniform.weightScaleSRSamplesIrr * omega
                                            let (irr, weight) = sampleIrr t2w scale addr clippedVa.[l]
                                            patchIlluminationIrr <- patchIlluminationIrr + irr
                                            weightSum <- weightSum + weight

                                    if uniform.sampleBarycenter then
                                        let omega = computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr barycenter

                                        let scale = uniform.weightScaleSRSamples * omega
                                        let irr = sample t2w scale addr barycenter
                                        patchIllumination <- patchIllumination + irr

                                        let scale = uniform.weightScaleSRSamplesIrr * omega
                                        let (irr, weight) = sampleIrr t2w scale addr barycenter
                                        patchIlluminationIrr <- patchIlluminationIrr + irr
                                        weightSum <- weightSum + weight

                                    if uniform.sampleNorm then 
                                        let omega = computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr normPlanePoint

                                        let scale = uniform.weightScaleSRSamples * omega
                                        let irr = sample t2w scale addr normPlanePoint
                                        patchIllumination <- patchIllumination + irr

                                        let scale = uniform.weightScaleSRSamplesIrr * omega
                                        let (irr, weight) = sampleIrr t2w scale addr normPlanePoint
                                        patchIlluminationIrr <- patchIlluminationIrr + irr
                                        weightSum <- weightSum + weight

                                    if uniform.sampleMRP then
                                        let omega = computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr mrp

                                        let scale = uniform.weightScaleSRSamples * omega
                                        let irr = sample t2w scale addr mrp
                                        patchIllumination <- patchIllumination + irr

                                        let scale = uniform.weightScaleSRSamplesIrr * omega
                                        let (irr, weight) = sampleIrr t2w scale addr mrp
                                        patchIlluminationIrr <- patchIlluminationIrr + irr
                                        weightSum <- weightSum + weight

                                    if uniform.sampleClosest then 
                                        let omega = computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr closestPoint

                                        let scale = uniform.weightScaleSRSamples * omega
                                        let irr = sample t2w scale addr closestPoint
                                        patchIllumination <- patchIllumination + irr

                                        let scale = uniform.weightScaleSRSamplesIrr * omega
                                        let (irr, weight) = sampleIrr t2w scale addr closestPoint
                                        patchIlluminationIrr <- patchIlluminationIrr + irr
                                        weightSum <- weightSum + weight

                                    if uniform.sampleRandom && uniform.numSRSamples > 0 then
                                        for l in 0 .. uniform.numSRSamples - 1 do
                                            let samplePoint = w2t * (uniform.LSamplePoints.[l] - P)

                                            if samplePoint.Z >= eps then
                                                let omega = computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr samplePoint


                                                let scale = uniform.weightScaleSRSamples * omega
                                                let irr = sample t2w scale addr samplePoint
                                                patchIllumination <- patchIllumination + irr

                                                let scale = uniform.weightScaleSRSamplesIrr * omega
                                                let (irr, weight) = sampleIrr t2w scale addr samplePoint
                                                patchIlluminationIrr <- patchIlluminationIrr + irr
                                                weightSum <- weightSum + weight

                                    if sampleCount > 0 then

                                        let L = patchIlluminationIrr / weightSum
                                        
                                        for l in 0 .. clippedVc - 1 do
                                            // Project polygon light onto sphere
                                            clippedVa.[l] <- Vec.normalize clippedVa.[l]

                                        let I = abs (baumFormFactor(clippedVa, clippedVc)) / (2.0) // should be divided by 2 PI, but PI is already in the brdf
                                        
                                        let patchIllumination = (1.0 / float(sampleCount)) * (v.c / PI) * patchIllumination 
                                        let patchIlluminationIrr =  L * brdf * I

                                        let patchIllumination = uniform.combinedLerpValue * patchIllumination + (1.0 - uniform.combinedLerpValue) * patchIlluminationIrr
                                        
                                           
                                        illumination <- illumination + patchIllumination
                                                                
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
            blendSamples         : IMod<bool>
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
                blendSamples         = m.blendSamples
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

        // closestPointTrafo, normPlanePointTrafo, mrpTrafo
        let private getSamplePointSg (lc : Light.LightCollection) (point : V3d * V3d) = 

            let M33dFromCols (c1 : V3d) (c2 : V3d) (c3 : V3d) =
                M33d(c1.X, c2.X, c3.X, c1.Y, c2.Y, c3.Y, c1.Z, c2.Z, c3.Z)

            let basisFrisvad (n : V3d) = 
                let c1 = V3d(
                            1.0 - (n.X  * n.X) / (1.0 + n.Z),
                            (-n.X * n.Y) / (1.0 + n.Z),
                            -n.X
                            )

                let c2 = V3d(
                            (-n.X * n.Y) / (1.0 + n.Z),
                            1.0 - (n.Y  * n.Y) / (1.0 + n.Z),
                            -n.Y
                            )

                let c3 = n
            
                M33dFromCols c1 c2 c3
            
            let (P, n) = point

            let t2w = n |> Vec.normalize |> basisFrisvad 
            let w2t = t2w |> Mat.transpose

            let samplePointPositions =
                adaptive {

                    let! lights = lc.Lights
                
                    ////////////////////////////////////////////////////////

                    let addr = 0

                    let vAddr = addr * Config.VERT_PER_LIGHT
                    let iAddr = addr * Config.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT

                    ////////////////////////////////////////////////////////

                    let! lUps = lc.Ups
                    let! lForwards = lc.Forwards

                    let l2w = M33dFromCols (V3d.Cross((lUps.[addr]), (lForwards.[addr]))) lUps.[addr] lForwards.[addr]
                            
                    let w2l = l2w |> Mat.transpose

                    let t2l = w2l * t2w
                        
                    ////////////////////////////////////////////////////////
                
                    let! lPatchIndices = lc.PatchIndices
                    let! lVertices = lc.Vertices
                
                    let! lBaseComponents = lc.BaseComponents
                    let! lForwards = lc.Forwards
               
                
                    let computeLightData iIdx = 
                            
                        let mutable vt = Arr<N<Config.MAX_PATCH_SIZE>, V3d>() 
                            
                        for vtc in 0 .. lBaseComponents.[addr] - 1 do
                            let vtcAddr = lPatchIndices.[iIdx + vtc] + vAddr
                            vt.[vtc] <- w2t * (lVertices.[vtcAddr] - P)

                        ////////////////////////////////////////////////////////

                        let (clippedVa, clippedVc) = clipPatch(V3d.Zero, V3d.OOI, vt, lBaseComponents.[addr])

                        if clippedVc <> 0 then

                            let eps = 1e-9
                            let epb = 1e-3

                            let mutable barycenter = V3d.Zero
                            for l in 0 .. clippedVc - 1 do
                                barycenter <- barycenter + clippedVa.[l]
                                    
                            let barycenter = barycenter / (float clippedVc)
               
                            let lightPlaneN = w2t * lForwards.[addr] |> Vec.normalize                                

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
                                        
                                    let closestPoint = clampPointToPolygon clippedVa clippedVc closestPoint t2l
                                    let normPlanePoint =   clampPointToPolygon clippedVa clippedVc normPlanePoint t2l 
     
                                    (closestPoint, normPlanePoint)

                                printfn "Distance : %f" (Vec.length (closestPoint - normPlanePoint))

                                let mrpDir = ((closestPoint |> Vec.normalize) + (normPlanePoint |> Vec.normalize)) |> Vec.normalize
                                let mrp = linePlaneIntersection V3d.Zero mrpDir (clippedVa.[0]) lightPlaneN

                                let mutable sampleCount = 0
                                let mutable sampleIdx = 0
                                let samples = Arr<N<MAX_SAMPLE_NUM_WO_RANDOM>, V3d>() // all samples except random samples

                                let minRequiredDistance = 1e-5

                                // corners
                                if true then   
                                    for l in 0 .. Config.MAX_PATCH_SIZE_PLUS_ONE - 1 do
                                        if l < clippedVc then

                                            let mutable minDist = 1e16
                                            for i in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
                                                if i < sampleIdx then
                                                    let dist = Vec.length (clippedVa.[l] - samples.[i])
                                                    if dist < minDist then minDist <- dist

                                            let alreadyExisting =
                                                if minDist > minRequiredDistance || sampleIdx = 0 then
                                                    false
                                                else
                                                    true
                                                
                                            if not alreadyExisting then // (sampleAlreadyExisting samples sampleIdx clippedVa.[l]) then
                                                samples.[sampleIdx] <- V3d(clippedVa.[l])
                                                sampleIdx <- sampleIdx + 1
                                                sampleCount <- sampleIdx
                                            
                                // barycenter
                                if false then   
                                    let mutable minDist = 1e16
                                    for i in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
                                        if i < sampleIdx then
                                            let dist = Vec.length (barycenter - samples.[i])
                                            if dist < minDist then minDist <- dist

                                    let alreadyExisting =
                                        if minDist > minRequiredDistance || sampleIdx = 0 then
                                            false
                                        else
                                            true
                                                
                                    if not alreadyExisting then // not (sampleAlreadyExisting samples sampleIdx barycenter) then
                                        samples.[sampleIdx] <- V3d(barycenter)
                                        sampleIdx <- sampleIdx + 1
                                        sampleCount <- sampleIdx

                                // norm
                                if true then  
                                    let mutable minDist = 1e16
                                    for i in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
                                        if i < sampleIdx then
                                            let dist = Vec.length (normPlanePoint - samples.[i])
                                            if dist < minDist then minDist <- dist

                                    let alreadyExisting =
                                        if minDist > minRequiredDistance || sampleIdx = 0 then
                                            false
                                        else
                                            true
                                                
                                    if not alreadyExisting then // not (sampleAlreadyExisting samples sampleIdx normPlanePoint) then
                                        samples.[sampleIdx] <- V3d(normPlanePoint)
                                        sampleIdx <- sampleIdx + 1
                                        sampleCount <- sampleIdx
                                    
                                // mrp
                                if true then  
                                    let mutable minDist = 1e16
                                    for i in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
                                        if i < sampleIdx then
                                            let dist = Vec.length (mrp - samples.[i])
                                            if dist < minDist then minDist <- dist

                                    let alreadyExisting =
                                        if minDist > minRequiredDistance || sampleIdx = 0 then
                                            false
                                        else
                                            true
                                                
                                    if not alreadyExisting then // not (sampleAlreadyExisting samples sampleIdx mrp) then
                                        samples.[sampleIdx] <- V3d(mrp)
                                        sampleIdx <- sampleIdx + 1
                                        sampleCount <- sampleIdx
                                    
                                // closest
                                if true then  
                                    let mutable minDist = 1e16
                                    for i in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do
                                        if i < sampleIdx then
                                            let dist = Vec.length (closestPoint - samples.[i])
                                            if dist < minDist then minDist <- dist

                                    let alreadyExisting =
                                        if minDist > minRequiredDistance || sampleIdx = 0 then
                                            false
                                        else
                                            true

                                    if not alreadyExisting then // not (sampleAlreadyExisting samples sampleIdx closestPoint) then
                                        samples.[sampleIdx] <- closestPoint
                                        sampleIdx <- sampleIdx + 1
                                        sampleCount <- sampleIdx

                                let samples = samples |> Arr.map (fun s -> t2w * s + P)

                                (samples, sampleCount)

                            else
                                (Arr<N<MAX_SAMPLE_NUM_WO_RANDOM>, V3d>([t2w * closestPoint + P]), 1)

                        else
                            (Arr<N<MAX_SAMPLE_NUM_WO_RANDOM>, V3d>(), 0)

                    return computeLightData 0
                
                }
        
            let getTrafo idx = 
                samplePointPositions  |>
                Mod.map(fun (positions, c) -> 
                    printfn "Sample %i; Count: %i" idx c
                    if idx < c then Trafo3d.Translation positions.[idx] else Trafo3d.Identity
                    )
           
                    
            let mutable sg = Sg.empty
            
            for i in 0 .. MAX_SAMPLE_NUM_WO_RANDOM - 1 do           
                sg <- Sg.group' [sg; pointSg C4b.Red (getTrafo i)]

            sg
            
                        

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
                    //pointSg
                    //measurePoint
                    //getSamplePointSg data.lights (measurePointPos, V3d.OOI)
                ]
                |> Sg.group'
            *)

            sceneSg 
                |> setupFbEffects [ 
                        ssEffect
                        EffectUtils.effectClearNaN |> toEffect
                    ]
                |> Light.Sg.setLightCollectionUniforms data.lights
                |> setupPhotometricData data.photometricData
                |> setupCamera data.view data.projTrafo data.viewportSize 
                |> Sg.uniform "sampleCorners"           ssData.sampleCorners
                |> Sg.uniform "sampleBarycenter"        ssData.sampleBarycenter
                |> Sg.uniform "sampleClosest"           ssData.sampleClosest
                |> Sg.uniform "sampleNorm"              ssData.sampleNorm
                |> Sg.uniform "sampleMRP"               ssData.sampleMRP
                |> Sg.uniform "sampleRandom"            ssData.sampleRandom
                |> Sg.uniform "blendSamples"            ssData.blendSamples
                |> Sg.uniform "numSRSamples"            ssData.numSRSamples
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
            structuredSampling |> toEffect |> setupSS_Fb data ssData signature sceneSg 

        let ssIrrApproxFb (data : RenderData) (ssData : SSData) (signature : IFramebufferSignature)  (sceneSg : ISg) =
            structuredIrradianceSampling |> toEffect |> setupSS_Fb data ssData signature sceneSg

        let ssCombinedApproxFb (data : RenderData) (ssData : SSData) (signature : IFramebufferSignature)  (sceneSg : ISg) =
            combinedStructuredSampling |> toEffect |> setupSS_Fb data ssData signature sceneSg


        
        
        