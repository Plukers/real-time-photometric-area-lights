namespace Render

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
                        let iAddr = addr * Config.MAX_IDX_BUFFER_SIZE_PER_LIGHT

                        for iIdx in iAddr .. 3 .. (iAddr + uniform.LNumIndices.[addr] - 1) do
                            
                            let v0Addr = uniform.LIndices.[iIdx + 0] + vAddr
                            let v0 = w2t * (uniform.LVertices.[v0Addr] - P)
                           
                            let v1Addr = uniform.LIndices.[iIdx + 1] + vAddr
                            let v1 = w2t * (uniform.LVertices.[v1Addr] - P)
                           
                            let v2Addr = uniform.LIndices.[iIdx + 2] + vAddr
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

    [<ReflectedDefinition>]       
    let clampPointToTriangle (a : V3d) (b : V3d) (c : V3d) (p : V3d) (tts : M33d) = // tls = transformation to triangle space matrix
        let a2d = 
            let v = tts * (a - a)
            V2d(v.X, v.Y)

        let b2d = 
            let v = tts * (b - a)
            V2d(v.X, v.Y)

        let c2d = 
            let v = tts * (c - a)
            V2d(v.X, v.Y)

        let p2d = 
            let v = tts * (p - a)
            V2d(v.X, v.Y)
          
        let clampedP = 
            match barycentricCoordinates p2d a2d b2d c2d with
            | (u, v, w) when u <= 0.0 && v <= 0.0 && w >  0.0 -> c                        //   I
            | (u, v, w) when u >  0.0 && v <  0.0 && w >  0.0 -> projectPointOnLine c a p //  II
            | (u, v, w) when u >  0.0 && v <= 0.0 && w <= 0.0 -> a                        // III
            | (u, v, w) when u >  0.0 && v >  0.0 && w <  0.0 -> projectPointOnLine a b p //  IV
            | (u, v, w) when u <= 0.0 && v >  0.0 && w <= 0.0 -> b                        //   V
            | (u, v, w) when u <  0.0 && v >  0.0 && w >  0.0 -> projectPointOnLine b c p //  VI
            | _ -> p // all coordinates are positive

        clampedP

    [<ReflectedDefinition>] 
    let linePointDistance (l1 : V2d) (l2 : V2d) (pnt : V2d) =
            let dir = l2 - l1 |> Vec.normalize
            let n = V2d(dir.Y, -dir.X)
            pnt - l1 |> Vec.dot n

    [<ReflectedDefinition>] 
    let clampPointToPolygon (a : V3d) (b : V3d) (c : V3d) (d : V3d) (p : V3d) (tps : M33d) = // tls = transformation to polygon space matrix
        let a2d = 
            let v = tps * (a - a)
            V2d(v.X, v.Y)

        let b2d = 
            let v = tps * (b - a)
            V2d(v.X, v.Y)

        let c2d = 
            let v = tps * (c - a)
            V2d(v.X, v.Y)

        let d2d = 
            let v = tps * (d - a)
            V2d(v.X, v.Y)

        let p2d = 
            let v = tps * (p - a)
            V2d(v.X, v.Y)

        let t = linePointDistance a2d b2d p2d
        let u = linePointDistance b2d c2d p2d
        let v = linePointDistance c2d d2d p2d
        let w = linePointDistance d2d a2d p2d
        
        match (t, u, v, w) with
        | (t, u, v, w) when t >  0.0 && u <= 0.0 && v <= 0.0 && w <= 0.0 -> projectPointOnLine a b p //    I
        | (t, u, v, w) when t >  0.0 && u >  0.0 && v <= 0.0 && w <= 0.0 -> b                        //   II
        | (t, u, v, w) when t <= 0.0 && u >  0.0 && v <= 0.0 && w <= 0.0 -> projectPointOnLine b c p //  III
        | (t, u, v, w) when t <= 0.0 && u >  0.0 && v >  0.0 && w <= 0.0 -> c                        //   IV
        | (t, u, v, w) when t <= 0.0 && u <= 0.0 && v >  0.0 && w <= 0.0 -> projectPointOnLine c d p //    V
        | (t, u, v, w) when t <= 0.0 && u <= 0.0 && v >  0.0 && w >  0.0 -> d                        //   VI
        | (t, u, v, w) when t <= 0.0 && u <= 0.0 && v <= 0.0 && w >  0.0 -> projectPointOnLine d a p //  VII
        | (t, u, v, w) when t >  0.0 && u <= 0.0 && v <= 0.0 && w >  0.0 -> a                        // VIII
        | _ -> p // all coordinates are positive
        
        
    
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
                        let iAddr = addr * Config.MAX_IDX_BUFFER_SIZE_PER_LIGHT

                        ////////////////////////////////////////////////////////

                        let l2w = M33dFromCols  (V3d.Cross((uniform.LUps.[addr]), (uniform.LForwards.[addr]))) uniform.LUps.[addr] uniform.LForwards.[addr]
                            
                        let w2l = l2w |> Mat.transpose

                        let t2l = w2l * t2w

                        ////////////////////////////////////////////////////////

                        for iIdx in iAddr .. 3 .. (iAddr + uniform.LNumIndices.[addr] - 1) do
                            
                            let v0Addr = uniform.LIndices.[iIdx + 0] + vAddr
                            let v0 = w2t * (uniform.LVertices.[v0Addr] - P)
                           
                            let v1Addr = uniform.LIndices.[iIdx + 1] + vAddr
                            let v1 = w2t * (uniform.LVertices.[v1Addr] - P)
                           
                            let v2Addr = uniform.LIndices.[iIdx + 2] + vAddr
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
                                            let lpn = 
                                                if (Vec.dot up lightPlaneN) > 0.0 then
                                                    lightPlaneN
                                                else
                                                    -lightPlaneN

                                            up <- up + (abs(Vec.dot up lpn) + epb) * (-lpn) |> Vec.normalize
                                    
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
                                    let d = Vec.length i
                                    let i = i |> Vec.normalize
                                
                                    let dotOut = max 1e-5 (abs (Vec.dot -(t2w * i) uniform.LForwards.[addr]))
                                    let irr = getPhotometricIntensity -(t2w * i) uniform.LForwards.[addr]  uniform.LUps.[addr] / (uniform.LAreas.[addr] * dotOut)

                                    if irr > 0.0 then 
                                        let irr = irr * sa
                                        illumination <- illumination + irr * brdf * i.Z  
                                        
                                  (*  
                                else
                                    let V = (uniform.CameraLocation - P) |> Vec.normalize
                                    
                                    let dotOut = max 1e-5 (abs (Vec.dot V uniform.LForwards.[addr]))

                                    let irr = getPhotometricIntensity V uniform.LForwards.[addr]  uniform.LUps.[addr] / (uniform.LAreas.[addr] * dotOut)

                                    if irr > 0.0 then 
                                        let irr = irr
                                        illumination <- illumination + irr * brdf
                                    *)
                                ()
                                                                
                            ////////////////////////////////////////////////////////
                        ()

            return V4d(illumination.XYZ, v.c.W)
        }

