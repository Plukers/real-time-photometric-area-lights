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
    
    type Vertex = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
    }  
    (*
    [<ReflectedDefinition>]
    let private sample L weightSum t2w addr (p : V3d) = 

        let i = p |> Vec.normalize  

        let dotOut = max 1e-5 (abs (Vec.dot -(t2w * i) uniform.LForwards.[addr]))

        let weight = dotOut / (Vec.lengthSquared p)

        let weightSum = weightSum + weight
        let L = L +  weight * getPhotometricIntensity -(t2w * i) uniform.LForwards.[addr]  uniform.LUps.[addr] / (uniform.LAreas.[addr] * dotOut)

        (L, weightSum)
    
    let structuredSampling (v : Vertex) = 
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


                                    let mutable sampleSums = (0.0, 0.0) // L, weightSum
                                    
                                    if uniform.sampleCorners then
                                        for l in 0 .. clippedVc - 1 do
                                            let (L, weightSum) = sampleSums
                                            let (L, weightSum) = sample L weightSum t2w addr clippedVa.[l]
                                            sampleSums <- (L, weightSum)

                                    if uniform.sampleBarycenter then 
                                        let (L, weightSum) = sampleSums
                                        let (L, weightSum) = sample L weightSum t2w addr barycenter
                                        sampleSums <- (L, weightSum)

                                    if uniform.sampleNorm then 
                                        let (L, weightSum) = sampleSums
                                        let (L, weightSum) = sample L weightSum t2w addr normPlanePoint
                                        sampleSums <- (L, weightSum)

                                    if uniform.sampleClosest then 
                                        let (L, weightSum) = sampleSums
                                        let (L, weightSum) = sample L weightSum t2w addr closestPoint 
                                        sampleSums <- (L, weightSum)


                                    let (L, weightSum) = sampleSums

                                    let L =
                                        if weightSum <> 0.0 then
                                            L / weightSum
                                        else 
                                            0.0

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
    *)

    
    [<ReflectedDefinition>]
    let private sample (t2w : M33d) (addr : int) (p : V3d) = 

        let i = p |> Vec.normalize  
 
        let dotOut = max 1e-5 (abs (Vec.dot -(t2w * i) uniform.LForwards.[addr]))
        let irr = getPhotometricIntensity -(t2w * i) uniform.LForwards.[addr]  uniform.LUps.[addr] / (uniform.LAreas.[addr] * dotOut)

        irr * i.Z

    let structuredSampling (v : Vertex) = 
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

                                    let solidAngle = 
                                        if clippedVc = 3 then
                                            computeSolidAngle clippedVa.[0] clippedVa.[1] clippedVa.[2]
                                        elif clippedVc = 4 then
                                            let sa1 = computeSolidAngle clippedVa.[0] clippedVa.[1] clippedVa.[2]
                                            let sa2 = computeSolidAngle clippedVa.[0] clippedVa.[2] clippedVa.[3]
                                            sa1 + sa2
                                        else
                                            let sa1 = computeSolidAngle clippedVa.[0] clippedVa.[1] clippedVa.[2]
                                            let sa2 = computeSolidAngle clippedVa.[0] clippedVa.[2] clippedVa.[3]
                                            let sa3 = computeSolidAngle clippedVa.[0] clippedVa.[3] clippedVa.[4]
                                            sa1 + sa2 + sa3


                                    let mutable patchIllumination = 0.0
                                    let mutable sampleCount = 0
                                                                        
                                    if uniform.sampleCorners then
                                        for l in 0 .. clippedVc - 1 do
                                             patchIllumination <- patchIllumination + sample t2w addr clippedVa.[l] 
                                             sampleCount <- sampleCount + 1 

                                    if uniform.sampleBarycenter then
                                         patchIllumination <- patchIllumination + sample t2w addr barycenter 
                                         sampleCount <- sampleCount + 1

                                    if uniform.sampleNorm then 
                                         patchIllumination <- patchIllumination + sample t2w addr normPlanePoint
                                         sampleCount <- sampleCount + 1

                                    if uniform.sampleMRP then
                                         let mrpDir = ((closestPoint |> Vec.normalize) + (barycenter |> Vec.normalize)) |> Vec.normalize
                                         let mrp = linePlaneIntersection V3d.Zero mrpDir (clippedVa.[0]) lightPlaneN

                                         patchIllumination <- patchIllumination + sample t2w addr mrp 

                                    if uniform.sampleClosest then 
                                         patchIllumination <- patchIllumination + sample t2w addr closestPoint 
                                         sampleCount <- sampleCount + 1

                                    illumination <- illumination + (v.c / PI) * solidAngle * patchIllumination / float(sampleCount)
                                ()
                                                                
                            ////////////////////////////////////////////////////////
                        ()

            return V4d(illumination.XYZ, v.c.W)
        }

        
