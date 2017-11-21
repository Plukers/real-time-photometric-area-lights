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
    let clampMRPToTriangle (a : V3d) (b : V3d) (c : V3d) (mrp : V3d) (t2l : M33d) = 
        let a2d = 
            let v = t2l * (a - a)
            V2d(v.X, v.Y)

        let b2d = 
            let v = t2l * (b - a)
            V2d(v.X, v.Y)

        let c2d = 
            let v = t2l * (c - a)
            V2d(v.X, v.Y)

        let m2d = 
            let v = t2l * (mrp - a)
            V2d(v.X, v.Y)
          
        let clampedMrp = 
            match barycentricCoordinates m2d a2d b2d c2d with
            | (u, v, w) when u <= 0.0 && v <= 0.0 && w >  0.0 -> c                          //   I
            | (u, v, w) when u >  0.0 && v <  0.0 && w >  0.0 -> projectPointOnLine c a mrp //  II
            | (u, v, w) when u >  0.0 && v <= 0.0 && w <= 0.0 -> a                          // III
            | (u, v, w) when u >  0.0 && v >  0.0 && w <  0.0 -> projectPointOnLine a b mrp //  IV
            | (u, v, w) when u <= 0.0 && v >  0.0 && w <= 0.0 -> b                          //   V
            | (u, v, w) when u <  0.0 && v >  0.0 && w >  0.0 -> projectPointOnLine b c mrp //  VI
            | _ -> mrp // all coordinates are positive

        clampedMrp
        

    
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

                        let l2w = M33dFromCols uniform.LForwards.[addr] (V3d.Cross(uniform.LForwards.[addr], uniform.LUps.[addr])) uniform.LUps.[addr] 
                            
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
                                let epb = 0.5
                                                    
                                let lightPlaneN = w2t * uniform.LForwards.[addr] |> Vec.normalize                                

                                // find closest point limited to upper hemisphere
                                let t = - (clippedVa.[0]) |> Vec.dot lightPlaneN
                                let mutable closestPoint = t * (-lightPlaneN)
                                                        
                                let abovePlane = 
                                    if (Vec.dot closestPoint V3d.OOI) < 0.0 then
                                        let newDir = V3d(closestPoint.X, closestPoint.Y, 0.0) |> Vec.normalize
                                        closestPoint <- linePlaneIntersection V3d.Zero newDir (clippedVa.[0]) lightPlaneN
                                        true
                                    else 
                                        false

                                let insideLightPlane = (Vec.length closestPoint) < eps

                                if not insideLightPlane then
                                    
                                    let closestPointDir = closestPoint |> Vec.normalize

                                    // intersect normal with plane
                                    let mutable up = V3d.OOI
                                
                                    if abs(Vec.dot up lightPlaneN) < eps then
                                        up <- up + (epb * closestPointDir) |> Vec.normalize     
                                    else
                                        if abovePlane then
                                            let lpn = 
                                                if (Vec.dot up lightPlaneN) > 0.0 then
                                                    lightPlaneN
                                                else
                                                    -lightPlaneN

                                            up <- up + (abs(Vec.dot up lpn) + epb) * (-lpn) |> Vec.normalize
                                            

                                    let normPlanePointDir = up // linePlaneIntersection V3d.Zero up (clippedVa.[0]) lightPlaneN |> Vec.normalize

                                    // find most representative point
                                    let mrpDir = closestPointDir + normPlanePointDir |> Vec.normalize

                                    let mrp = linePlaneIntersection V3d.Zero mrpDir (clippedVa.[0]) lightPlaneN // tangent space
                              
                                    let (mrp, sa) = 

                                        if clippedVc = 3 then
                                            let sa = computeSolidAngle clippedVa.[0] clippedVa.[1] clippedVa.[2]

                                            let mrp = clampMRPToTriangle clippedVa.[0] clippedVa.[1] clippedVa.[2] mrp t2l   
                                        
                                            (mrp, sa)

                                        else

                                            let sa1 = computeSolidAngle clippedVa.[0] clippedVa.[1] clippedVa.[2]
                                            let sa2 = computeSolidAngle clippedVa.[0] clippedVa.[2] clippedVa.[3]
                                            let sa = sa1 + sa2

                                            let mutable barycenterA = V3d.Zero
                                            barycenterA <- barycenterA + clippedVa.[0] / 3.0
                                            barycenterA <- barycenterA + clippedVa.[1] / 3.0
                                            barycenterA <- barycenterA + clippedVa.[2] / 3.0

                                            let mutable barycenterB = V3d.Zero
                                            barycenterB <- barycenterB + clippedVa.[0] / 3.0
                                            barycenterB <- barycenterB + clippedVa.[2] / 3.0
                                            barycenterB <- barycenterB + clippedVa.[3] / 3.0       

                                            let mrp = 
                                                if ((mrp - barycenterA) |> Vec.length) > ((mrp - barycenterB) |> Vec.length) then
                                                    clampMRPToTriangle clippedVa.[0] clippedVa.[1] clippedVa.[2] mrp t2l
                                                else
                                                    clampMRPToTriangle clippedVa.[0] clippedVa.[2] clippedVa.[3] mrp t2l

                                            (mrp, sa)


                                    let i =  mrp
                                    let d = Vec.length i
                                    let i = i |> Vec.normalize
                                
                                    let irr = getPhotometricIntensity -(t2w * i) uniform.LForwards.[addr]  uniform.LUps.[addr]

                                    if irr > 0.0 then 
                                        let irr = irr / (d * d)
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

