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

        let weight = i.Z / (Vec.lengthSquared p + (max 1e-9 scale)) // add i.Z for a better weight
        
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
                                    if uniform.sampleCorners then       sampleCount <- sampleCount + 1
                                    if uniform.sampleBarycenter then    sampleCount <- sampleCount + 1
                                    if uniform.sampleNorm then          sampleCount <- sampleCount + 1
                                    if uniform.sampleMRP then           sampleCount <- sampleCount + 1
                                    if uniform.sampleClosest then       sampleCount <- sampleCount + 1
                                    if uniform.sampleRandom then        sampleCount <- sampleCount + uniform.numSRSamples

                                                                                   

                                    let mutable patchIllumination = 0.0
                                    let mutable weightSum = 0.0
                                                                        
                                    if uniform.sampleCorners then
                                        for l in 0 .. clippedVc - 1 do
                                            let scale = uniform.weightScaleSRSamplesIrr * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDistIrr addr clippedVa.[l]

                                            let (irr, weight) = sampleIrr t2w scale addr clippedVa.[l]
                                            patchIllumination <- patchIllumination + irr
                                            weightSum <- weightSum + weight

                                    if uniform.sampleBarycenter then 
                                        let scale = uniform.weightScaleSRSamplesIrr * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDistIrr addr barycenter

                                        let (irr, weight) = sampleIrr t2w scale addr barycenter
                                        patchIllumination <- patchIllumination + irr
                                        weightSum <- weightSum + weight

                                    if uniform.sampleNorm then 
                                        let scale = uniform.weightScaleSRSamplesIrr * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDistIrr  addr normPlanePoint

                                        let (irr, weight) = sampleIrr t2w scale addr normPlanePoint
                                        patchIllumination <- patchIllumination + irr
                                        weightSum <- weightSum + weight

                                    if uniform.sampleClosest then 
                                        let scale = uniform.weightScaleSRSamplesIrr * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDistIrr addr closestPoint

                                        let (irr, weight) = sampleIrr t2w scale addr closestPoint
                                        patchIllumination <- patchIllumination + irr
                                        weightSum <- weightSum + weight

                                    if uniform.sampleMRP then   
                                        let scale = uniform.weightScaleSRSamplesIrr * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDistIrr addr mrp

                                        let (irr, weight) = sampleIrr t2w scale addr mrp
                                        patchIllumination <- patchIllumination + irr
                                        weightSum <- weightSum + weight

                                    if uniform.sampleRandom && uniform.numSRSamples > 0 then
                                        for l in 0 .. uniform.numSRSamples do
                                            let samplePoint = w2t * (uniform.LSamplePoints.[l] - P)

                                            if samplePoint.Z >= eps then
                                                let scale = uniform.weightScaleSRSamplesIrr * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDistIrr addr samplePoint

                                                let (irr, weight) = sampleIrr t2w scale addr samplePoint
                                                patchIllumination <- patchIllumination + irr
                                                weightSum <- weightSum + weight
                                                
                                    let L =
                                        if sampleCount > 0 then
                                            patchIllumination / weightSum
                                        else 
                                            0.0

                                    

                                    for l in 0 .. clippedVc - 1 do
                                        // Project polygon light onto sphere
                                        clippedVa.[l] <- Vec.normalize clippedVa.[l]

                                    let I = abs (baumFormFactor(clippedVa, clippedVc)) / (2.0) // should be divided by 2 PI, but PI is already in the brdf
                                        
                                    //let clampedDist = (clamp 0.0 uniform.scaleSRSampleDist (Vec.length closestPoint)) / uniform.scaleSRSampleDist
                                    //let scale = clampedDist * 1.0 + (1.0 - clampedDist) * uniform.weightScaleSRSamplesIrr

                                    illumination <- illumination + L * brdf * I //* scale // * i.Z  
                                    
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
                                    if uniform.sampleCorners then       sampleCount <- sampleCount + 1
                                    if uniform.sampleBarycenter then    sampleCount <- sampleCount + 1
                                    if uniform.sampleNorm then          sampleCount <- sampleCount + 1
                                    if uniform.sampleMRP then           sampleCount <- sampleCount + 1
                                    if uniform.sampleClosest then       sampleCount <- sampleCount + 1
                                    if uniform.sampleRandom then        sampleCount <- sampleCount + uniform.numSRSamples
                                    
                                    
                                    let mutable patchIllumination = 0.0
                                                                        
                                    if uniform.sampleCorners then
                                        for l in 0 .. clippedVc - 1 do
                                            let scale = uniform.weightScaleSRSamples * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr clippedVa.[l]
                                            let irr = sample t2w scale addr clippedVa.[l]
                                            patchIllumination <- patchIllumination + irr

                                    if uniform.sampleBarycenter then
                                        let scale = uniform.weightScaleSRSamples * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr barycenter
                                        let irr = sample t2w scale addr barycenter
                                        patchIllumination <- patchIllumination + irr

                                    if uniform.sampleNorm then 
                                        let scale = uniform.weightScaleSRSamples * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr normPlanePoint
                                        let irr = sample t2w scale addr normPlanePoint
                                        patchIllumination <- patchIllumination + irr

                                    if uniform.sampleMRP then
                                        let scale = uniform.weightScaleSRSamples * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr mrp
                                        let irr = sample t2w scale addr mrp
                                        patchIllumination <- patchIllumination + irr

                                    if uniform.sampleClosest then 
                                        let scale = uniform.weightScaleSRSamples * computeApproximateSolidAnglePerSample t2w sampleCount uniform.tangentApproxDist addr closestPoint
                                        let irr = sample t2w scale addr closestPoint
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
                                    if uniform.sampleCorners then       sampleCount <- sampleCount + 1
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
                numSRSamples         = m.numOfSRSamples.value |> Mod.map (fun numSRS -> (int)(ceil numSRS))
                SRSWeightScale       = m.SRSWeightScale.value
                TangentApproxDist    = m.TangentApproxDist.value
                SRSWeightScaleIrr    = m.SRSWeightScaleIrr.value
                TangentApproxDistIrr = m.TangentApproxDistIrr.value
                CombinedLerpValue    = m.CombinedSSWeight.value
            }

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

                ) |> Sg.dynamic

            let sceneSg = Sg.group' [sceneSg; pointSg]
            *)
            
            sceneSg
                |> setupFbEffects [ 
                        ssEffect
                        EffectUtils.effectClearNaN |> toEffect
                    ]
                |> Sg.uniform "sampleCorners"           ssData.sampleCorners
                |> Sg.uniform "sampleBarycenter"        ssData.sampleBarycenter
                |> Sg.uniform "sampleClosest"           ssData.sampleClosest
                |> Sg.uniform "sampleNorm"              ssData.sampleNorm
                |> Sg.uniform "sampleMRP"               ssData.sampleMRP
                |> Sg.uniform "sampleRandom"            ssData.sampleRandom
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


        
        
        