namespace Render

module EffectApStructuredSampling =
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open FShade

    open Light.Effect
    open EffectUtils
    open PhotometricLight

    type UniformScope with
        member uniform.sampleCorners   : bool = uniform?sampleCorners     
        member uniform.sampleBarycenter: bool = uniform?sampleBarycenter  
        member uniform.sampleClosest   : bool = uniform?sampleClosest     
        member uniform.sampleNorm      : bool = uniform?sampleNorm 
        member uniform.sampleMRP       : bool = uniform?sampleMRP 
        member uniform.sampleRandom    : bool = uniform?sampleRandom
        member uniform.numSRSamples    : int  = uniform?numSRSamples
    
    type Vertex = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
    }  
    
    
    [<ReflectedDefinition>]
    let private sampleForBaum (t2w : M33d) (addr : int) (p : V3d) = 
        let i = p |> Vec.normalize  
 
        let dotOut = max 1e-9 (abs (Vec.dot -(t2w * i) uniform.LForwards.[addr]))


        let irr = getPhotometricIntensity -(t2w * i) uniform.LForwards.[addr]  uniform.LUps.[addr] // / (uniform.LAreas.[addr] * dotOut)

        let weight = i.Z / (max 1e-9 (Vec.lengthSquared p)) // add i.Z for a better weight

        let sampledIrr = weight * irr

        let weight = (uniform.LAreas.[addr] * dotOut) * weight

        (sampledIrr, weight)
        

    let structuredSamplingBaum (v : Vertex) = 
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



                                    

                                    let mutable patchIllumination = 0.0
                                    let mutable weightSum = 0.0
                                    let mutable sampleCount = 0
                                                                        
                                    if uniform.sampleCorners then
                                        for l in 0 .. clippedVc - 1 do
                                            let (irr, weight) = sampleForBaum t2w addr clippedVa.[l]
                                            patchIllumination <- patchIllumination + irr
                                            weightSum <- weightSum + weight
                                            sampleCount <- sampleCount + 1

                                    if uniform.sampleBarycenter then 
                                        let (irr, weight) = sampleForBaum t2w addr barycenter
                                        patchIllumination <- patchIllumination + irr
                                        weightSum <- weightSum + weight
                                        sampleCount <- sampleCount + 1

                                    if uniform.sampleNorm then 
                                        let (irr, weight) = sampleForBaum t2w addr normPlanePoint
                                        patchIllumination <- patchIllumination + irr
                                        weightSum <- weightSum + weight
                                        sampleCount <- sampleCount + 1

                                    if uniform.sampleClosest then 
                                        let (irr, weight) = sampleForBaum t2w addr closestPoint
                                        patchIllumination <- patchIllumination + irr
                                        weightSum <- weightSum + weight
                                        sampleCount <- sampleCount + 1

                                    if uniform.sampleMRP then
                                        let mrpDir = ((closestPoint |> Vec.normalize) + (normPlanePoint |> Vec.normalize)) |> Vec.normalize
                                        let mrp = linePlaneIntersection V3d.Zero mrpDir (clippedVa.[0]) lightPlaneN
                                        
                                        let (irr, weight) = sampleForBaum t2w addr mrp
                                        patchIllumination <- patchIllumination + irr
                                        weightSum <- weightSum + weight
                                        sampleCount <- sampleCount + 1

                                    if uniform.sampleRandom then
                                        for l in 0 .. uniform.numSRSamples do
                                            let samplePoint = w2t * (uniform.LSamplePoints.[l] - P)

                                            if samplePoint.Z >= eps then
                                                let (irr, weight) = sampleForBaum t2w addr samplePoint
                                                patchIllumination <- patchIllumination + irr
                                                weightSum <- weightSum + weight
                                                sampleCount <- sampleCount + 1
                                                
                                    let L =
                                        if sampleCount > 0 then
                                            patchIllumination / weightSum
                                        else 
                                            0.0

                                    if L > eps then 

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
    

    
    [<ReflectedDefinition>]
    let private sample (t2w : M33d) (addr : int) (p : V3d) = 

        let i = p |> Vec.normalize  
 
        //let dotOut = max 1e-5 (abs (Vec.dot -(t2w * i) uniform.LForwards.[addr]))
        let irr = getPhotometricIntensity -(t2w * i) uniform.LForwards.[addr]  uniform.LUps.[addr] // / (uniform.LAreas.[addr] * dotOut)

        let weight = (* uniform.LAreas.[addr] * dotOut *) 1.0 / (max (Vec.lengthSquared p) 1e-9)

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

                                    
                                    let mutable patchIllumination = 0.0
                                    let mutable sampleCount = 0
                                                                        
                                    if uniform.sampleCorners then
                                        for l in 0 .. clippedVc - 1 do
                                             let irr = sample t2w addr clippedVa.[l]
                                             patchIllumination <- patchIllumination + irr
                                             sampleCount <- sampleCount + 1

                                    if uniform.sampleBarycenter then
                                        let irr = sample t2w addr barycenter
                                        patchIllumination <- patchIllumination + irr
                                        sampleCount <- sampleCount + 1

                                    if uniform.sampleNorm then 
                                        let irr= sample t2w addr normPlanePoint
                                        patchIllumination <- patchIllumination + irr
                                        sampleCount <- sampleCount + 1

                                    if uniform.sampleMRP then
                                        let mrpDir = ((closestPoint |> Vec.normalize) + (normPlanePoint |> Vec.normalize)) |> Vec.normalize
                                        let mrp = linePlaneIntersection V3d.Zero mrpDir (clippedVa.[0]) lightPlaneN

                                        let irr = sample t2w addr mrp

                                        patchIllumination <- patchIllumination + irr
                                        sampleCount <- sampleCount + 1

                                    if uniform.sampleClosest then 
                                        let irr = sample t2w addr closestPoint
                                        patchIllumination <- patchIllumination + irr
                                        sampleCount <- sampleCount + 1

                                    if uniform.sampleRandom then
                                        for l in 0 .. uniform.numSRSamples do
                                            let samplePoint = w2t * (uniform.LSamplePoints.[l] - P)

                                            if samplePoint.Z >= eps then
                                                let irr = sample t2w addr samplePoint
                                                patchIllumination <- patchIllumination + irr
                                                sampleCount <- sampleCount + 1

                                    if sampleCount > 0 then
                                        illumination <- illumination + (1.0 / float(sampleCount)) * (v.c / PI) * patchIllumination
                                ()
                                                                
                            ////////////////////////////////////////////////////////
                        ()

            return V4d(illumination.XYZ, v.c.W)
        }
        
        
        