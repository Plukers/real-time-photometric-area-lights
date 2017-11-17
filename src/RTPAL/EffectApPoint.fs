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

        // let (u, v, w) = barycentricCoordinates m2d a2d b2d c2d

        (*  
        if   u <= 0.0 && v <= 0.0 && w >  0.0 then c
        elif u >  0.0 && v <  0.0 && w >  0.0 then projectPointOnLine c a mrp
        elif u >  0.0 && v <= 0.0 && w <= 0.0 then a
        elif u >  0.0 && v >  0.0 && w <  0.0 then projectPointOnLine a b mrp
        elif u <= 0.0 && v >  0.0 && w <= 0.0 then b
        elif u <  0.0 && v >  0.0 && w >  0.0 then projectPointOnLine b c mrp
        else mrp
        *)
          
        match barycentricCoordinates m2d a2d b2d c2d with
        | (u, v, w) when u <= 0.0 && v <= 0.0 && w >  0.0 -> c // I
        | (u, v, w) when u >  0.0 && v <  0.0 && w >  0.0 ->   // II
            projectPointOnLine c a mrp
        | (u, v, w) when u >  0.0 && v <= 0.0 && w <= 0.0 -> a // III
        | (u, v, w) when u >  0.0 && v >  0.0 && w <  0.0 ->   // IV
            projectPointOnLine a b mrp
        | (u, v, w) when u <= 0.0 && v >  0.0 && w <= 0.0 -> b // V
        | (u, v, w) when u <  0.0 && v >  0.0 && w >  0.0 ->   // VI
            projectPointOnLine b c mrp
        | _ -> mrp // all coordinates are positive
        

    
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

                        let l2w = uniform.LForwards.[addr] |> Vec.normalize |> basisFrisvad 
                        let w2l = t2w |> Mat.transpose

                        let t2l = w2l * t2w
                        let l2t = w2t * l2w

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
                                                                                                        
                                // find closest point limited to upper hemisphere
                                let lightPlaneN = w2t * uniform.LForwards.[addr] |> Vec.normalize
                                
                                let t = - (clippedVa.[0]) |> Vec.dot lightPlaneN
                                let mutable closestPoint = -t * lightPlaneN

                                if (Vec.dot closestPoint V3d.OOI) < 0.0 then
                                    let newDir = V3d(closestPoint.X, closestPoint.Y, 0.0) |> Vec.normalize
                                    closestPoint <- linePlaneIntersection V3d.Zero newDir (clippedVa.[0]) lightPlaneN

                                let closestPoint = closestPoint |> Vec.normalize

                                // intersect normal with plane
                                let normPlanePoint = linePlaneIntersection V3d.Zero V3d.OOI (clippedVa.[0]) lightPlaneN |> Vec.normalize

                                // find most representative point
                                let mrp = closestPoint + normPlanePoint |> Vec.normalize

                                let projectedMRP = linePlaneIntersection V3d.Zero mrp (clippedVa.[0]) lightPlaneN |> Vec.normalize
                              
                                let mrp = 

                                    if clippedVc = 3 then
                                        clampMRPToTriangle clippedVa.[0] clippedVa.[1] clippedVa.[2] projectedMRP t2l                                  
                                    else

                                        let mutable barycenterA = V3d.Zero
                                        barycenterA <- barycenterA + clippedVa.[0] / 3.0
                                        barycenterA <- barycenterA + clippedVa.[1] / 3.0
                                        barycenterA <- barycenterA + clippedVa.[2] / 3.0

                                        let mutable barycenterB = V3d.Zero
                                        barycenterB <- barycenterB + clippedVa.[0] / 3.0
                                        barycenterB <- barycenterB + clippedVa.[2] / 3.0
                                        barycenterB <- barycenterB + clippedVa.[3] / 3.0       

                                        if ((projectedMRP - barycenterA) |> Vec.length) > ((projectedMRP - barycenterB) |> Vec.length) then
                                            clampMRPToTriangle clippedVa.[0] clippedVa.[1] clippedVa.[2] projectedMRP t2l
                                        else
                                            clampMRPToTriangle clippedVa.[0] clippedVa.[2] clippedVa.[3] projectedMRP t2l

                                let i =  mrp
                                let d = Vec.length i
                                let i = i |> Vec.normalize
                                
                                let irr = getPhotometricIntensity -(t2w * i) uniform.LForwards.[addr]  uniform.LUps.[addr]

                                if irr > 0.0 then 
                                    let irr = irr / (d * d)
                                    illumination <- illumination + irr * brdf * i.Z                            
                                    
                                    
                                ()
                                                                
                            ////////////////////////////////////////////////////////
                        ()

            return V4d(illumination.XYZ, v.c.W)
        }

