﻿namespace Render

module EffectApPoint =
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
    
    let centerPointApprox (v : Vertex) = 
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
                        let iAddr = addr * Config.MAX_EVAL_IDX_BUFFER_SIZE_PER_LIGHT

                        for iIdx in iAddr .. 3 .. (iAddr + uniform.LNumEvalIndices.[addr] - 1) do
                            
                            let v0Addr = uniform.LEvalIndices.[iIdx + 0] + vAddr
                            let v0 = w2t * (uniform.LVertices.[v0Addr] - P)
                           
                            let v1Addr = uniform.LEvalIndices.[iIdx + 1] + vAddr
                            let v1 = w2t * (uniform.LVertices.[v1Addr] - P)
                           
                            let v2Addr = uniform.LEvalIndices.[iIdx + 2] + vAddr
                            let v2 = w2t * (uniform.LVertices.[v2Addr] - P) 

                            ////////////////////////////////////////////////////////

                            let (clippedVa, clippedVc) = clipTriangle(V3d.Zero, V3d.OOI, Arr<N<3>, V3d>([| v0; v1; v2|]))

                            if clippedVc <> 0 then

                                let mutable barycenter = V3d.Zero
                                for l in 0 .. clippedVc - 1 do
                                    barycenter <- barycenter + clippedVa.[l]
                                    
                                let i = barycenter / (float clippedVc)
                                let d = Vec.length i
                                let i = i |> Vec.normalize
                                
                                let irr = getPhotometricIntensity -(t2w * i) uniform.LForwards.[addr]  uniform.LUps.[addr]

                                if irr > 0.0 then 
                                    let irr = irr / (d * d) // todo weight with solid angle
                                    illumination <- illumination + irr * brdf * i.Z                            
                                    
                                    
                                ()
                                                                
                            ////////////////////////////////////////////////////////
                        ()

            return V4d(illumination.XYZ, v.c.W)
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
                        let iAddr = addr * Config.MAX_EVAL_IDX_BUFFER_SIZE_PER_LIGHT

                        ////////////////////////////////////////////////////////

                        let l2w = M33dFromCols  (V3d.Cross((uniform.LUps.[addr]), (uniform.LForwards.[addr]))) uniform.LUps.[addr] uniform.LForwards.[addr]
                            
                        let w2l = l2w |> Mat.transpose

                        let t2l = w2l * t2w

                        ////////////////////////////////////////////////////////

                        for iIdx in iAddr .. 3 .. (iAddr + uniform.LNumEvalIndices.[addr] - 1) do
                            
                            let v0Addr = uniform.LEvalIndices.[iIdx + 0] + vAddr
                            let v0 = w2t * (uniform.LVertices.[v0Addr] - P)
                           
                            let v1Addr = uniform.LEvalIndices.[iIdx + 1] + vAddr
                            let v1 = w2t * (uniform.LVertices.[v1Addr] - P)
                           
                            let v2Addr = uniform.LEvalIndices.[iIdx + 2] + vAddr
                            let v2 = w2t * (uniform.LVertices.[v2Addr] - P) 

                            ////////////////////////////////////////////////////////

                            let (clippedVa, clippedVc) = clipTriangle(V3d.Zero, V3d.OOI, Arr<N<3>, V3d>([| v0; v1; v2|]))

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
                                    
                                    (*
                                    let mutable up = clippedVa.[0]
                                    let mutable avgc = 1.0
                                    for l in 1 .. clippedVc - 1 do
                                        let zdiff = up.Z - clippedVa.[l].Z

                                        if abs(zdiff) < eps then
                                            up <- (up * avgc) + clippedVa.[l]
                                            avgc <- avgc + 1.0
                                            up <- up / avgc
                                            
                                        else
                                            if zdiff < 0.0 then
                                                up <- clippedVa.[l]
                                    *)
                                    
                                    let normPlaneP = linePlaneIntersection V3d.Zero up (clippedVa.[0]) lightPlaneN // tangent space

                                    // find most representative point

                                    // let mrpDir = closestPointDir + normPlanePointDir |> Vec.normalize
                                    // let mrpDir = closestPointDir //TODO remove

                                    // let mrp = linePlaneIntersection V3d.Zero mrpDir (clippedVa.[0]) lightPlaneN // tangent space
                              
                                    let (closestPointDir, normPlanePDir, sa) = 

                                        if clippedVc = 3 then
                                            let sa = computeSolidAngle clippedVa.[0] clippedVa.[1] clippedVa.[2]

                                            let closestPoint = clampPointToTriangle clippedVa.[0] clippedVa.[1] clippedVa.[2] closestPoint t2l   
                                            let normPlaneP =   clampPointToTriangle clippedVa.[0] clippedVa.[1] clippedVa.[2] normPlaneP t2l 

                                            (closestPoint |> Vec.normalize, normPlaneP |> Vec.normalize, sa)

                                        else

                                            let sa1 = computeSolidAngle clippedVa.[0] clippedVa.[1] clippedVa.[2]
                                            let sa2 = computeSolidAngle clippedVa.[0] clippedVa.[2] clippedVa.[3]
                                            let sa = sa1 + sa2

                                            let closestPoint = clampPointToPolygon clippedVa.[0] clippedVa.[1] clippedVa.[2] clippedVa.[3] closestPoint t2l   
                                            let normPlaneP =   clampPointToPolygon clippedVa.[0] clippedVa.[1] clippedVa.[2] clippedVa.[3] normPlaneP t2l  
     
                                            (closestPoint |> Vec.normalize, normPlaneP |> Vec.normalize, sa)

                                    let mrpDir  = closestPointDir + normPlanePDir |> Vec.normalize
                                            

                                            
                                    let i =  mrpDir                                
                                    let dotOut = max 1e-5 (abs (Vec.dot -(t2w * i) uniform.LForwards.[addr]))
                                    let L = getPhotometricIntensity -(t2w * i) uniform.LForwards.[addr]  uniform.LUps.[addr] / (uniform.LAreas.[addr] * dotOut)

                                    if L > 0.0 then 

                                        for l in 0 .. clippedVc - 1 do
                                            // Project polygon light onto sphere
                                            clippedVa.[l] <- Vec.normalize clippedVa.[l]

                                        let I = abs (baumFormFactor(clippedVa, clippedVc)) / (2.0) // should be divided by 2 PI, but PI is already in the brdf
                                        // let I = sa
                                        illumination <- illumination + L * brdf * I // * i.Z  
                                (*
                                else

                                    let closestPoint = clampPointToTriangle clippedVa.[0] clippedVa.[1] clippedVa.[2] closestPoint t2l 

                                    if Vec.length closestPoint < eps then // inside light

                                    else
                                        let i = closestPoint |> Vec.normalize
                                
                                        let dotOut = max 1e-5 (abs (Vec.dot -(t2w * i) uniform.LForwards.[addr]))
                                        let L = getPhotometricIntensity -(t2w * i) uniform.LForwards.[addr]  uniform.LUps.[addr] / (uniform.LAreas.[addr] * dotOut)

                                        if L > 0.0 then 

                                            for l in 0 .. clippedVc - 1 dos
                                                clippedVa.[l] <- Vec.normalize clippedVa.[l]

                                            let I = abs (baumFormFactor(clippedVa, clippedVc)) / (2.0) // should be divided by 2 PI, but PI is already in the brdf
                                            // let I = sa
                                            illumination <- illumination + L * brdf * I // * i.Z  
                                *)
                                    
                                ()
                                                                
                            ////////////////////////////////////////////////////////
                        ()

            return V4d(illumination.XYZ, v.c.W)
        }

