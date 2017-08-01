namespace Render

(*
    Ground Truth Rendering Effect
    Single Bounce Path Tracing
*)
module GTEffect = 
    open System

    open FShade
    open Aardvark.Base
    open Aardvark.Base.Rendering

    // open extended UniformScopes
    open Utils.HaltonSequence
    open Light.Effect

    [<ReflectedDefinition>]
    let private PI = Math.PI
    
    [<ReflectedDefinition>]
    let private floorV4d (v : V4d) = 
        V4d(Math.Floor(v.X), Math.Floor(v.Y), Math.Floor(v.Z), Math.Floor(v.W))
   
    [<ReflectedDefinition>]
    let private fractV4d (v : V4d) = 
        v - floorV4d(v)

    (*
        Creates a hash usable as jitter computed from a 2D coordinate
        Taken from https://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/
    *)
    [<ReflectedDefinition>]
    let private fast32Hash (coordinate : V2d) = 
        let offset = V2d(26.0, 161.0)
        let domain = 71.0
        let someLargeFloat = 951.135664

        let mutable P = V4d(coordinate.X, coordinate.Y, coordinate.X, coordinate.Y)
        P <- P + V4d(0.0, 0.0, 1.0, 1.0)
        
        P <- P - floorV4d(P / domain) * domain
        
        P <- P + V4d(offset.X, offset.Y, offset.X, offset.Y)
        P <- P * P

        let xzxz = V4d(P.X, P.Z, P.X, P.Z)
        let yyww = V4d(P.Y, P.Y, P.W, P.W)

        fractV4d(xzxz * yyww * V4d(1.0 / someLargeFloat))
        
    (*
        Uniformly samples a direction from the hemisphere for two given random numbers
        Works in the tangent space
        http://www.rorydriscoll.com/2009/01/07/better-sampling/
    *)
    [<ReflectedDefinition>]
    let sampleHemisphere u1 u2 = 

        let r = Math.Sqrt(1.0 - u1 * u1)
        let phi = 2.0 * PI * u2

        V3d(Math.Cos(phi) * r, Math.Sin(phi) * r, u1) |> Vec.normalize


    (*
        Computes an orthonormal basis for a given n.
        The resulting basis has n as it's z-axis.
        Assumes that n has unit length.

        Frisvad, J. R. (2012). 
        Building an orthonormal basis from a 3D unit vector without normalization. 
        Journal of Graphics Tools, 16(3), 151-159.
    *)
    [<ReflectedDefinition>]
    let basisFrisvad (n : V3d) = 

        let c1 = V3d(
                    1.0 - n.X  * n.X / (1.0 + n.Z),
                    -n.X * n.Y / (1.0 + n.Z),
                    -n.X
                    )

        let c2 = V3d(
                    -n.X * n.Y / (1.0 + n.Z),
                    1.0 - n.Y  * n.Y / (1.0 + n.Y),
                    -n.Y
                    )

        M33d.FromRows(c1, c2, n) 
        // Todo try matrix constructor
        // If it does not work, try  [<GLSLIntrinsic("mix({1},{2},{0})")>]
        // M33d(


    (*
        Computes the intersaction point of a ray and triangle. 

        Möller, T., & Trumbore, B. (2005, July). 
        Fast, minimum storage ray/triangle intersection.
        In ACM SIGGRAPH 2005 Courses (p. 7). ACM.

        https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
    *)
    [<ReflectedDefinition>]
    let rayTriangleIntersaction (orig : V3d) (dir : V3d) (v0 : V3d) (v1 : V3d) (v2 : V3d) = 
        let e1 = v1 - v0
        let e2 = v2 - v0
        
        let pVec = Vec.cross dir e2
        let det  = Vec.dot e1 pVec
        
        if (det < 1e-8 && det > -1e-8) then
            0.0
        else
            let invDet = 1.0 / det
            let tVec   = orig - v0
            let u      = (Vec.dot tVec pVec) * invDet

            if (u < 0.0 || u > 1.0) then
                0.0
            else
                let qVec = Vec.cross tVec e1
                let v    = (Vec.dot dir qVec) * invDet

                if (v < 0.0 || u + v > 1.0) then
                    0.0
                else 
                    ((Vec.dot e2 qVec) * invDet)
        
        
    let groundTruthLighting (v : Effects.Vertex) = 
        fragment {

            let worldV = (uniform.CameraLocation - v.wp.XYZ) |> Vec.normalize

            let w2tP = basisFrisvad v.n
            //let t2wP = basisFrisvad v.n |> Mat.inverse

            //let t2wV = w2tP
            //let w2tV = t2wV |> Mat.transpose

            // Transform view vector into tangent space
            //let o = w2tV * worldV
            M33d(
            return V4d(w2tP * V3d(0.0, -0.5, -0.5), 1.0)
            (*
            // Compute a jitter
            let jitter = (fast32Hash v.tc).XY                

            let haltonSamples : Arr<N<Config.NUM_SAMPLES>, V2d> = uniform.HaltonSamples

            let mutable illumination = V4d.Zero
            
            // Iterate over Samples
            for sIdx in 0 .. Config.NUM_SAMPLES - 1 do
                   
                let u1 = 
                    let x = jitter.X + haltonSamples.[sIdx].X
                    x - Math.Floor(x)

                let u2 = 
                    let x = jitter.Y + haltonSamples.[sIdx].Y
                    x - Math.Floor(x)

                let i = sampleHemisphere u1 u2 
                
                // Check if i hits a light
                // If it does, compute the illumination
                for addr in 0 .. (Config.NUM_LIGHTS - 1) do 
                    match uniform.Lights.[addr] with
                    | -1 -> ()
                    |  _ ->

                        
                        let vAddr = addr * Config.VERT_PER_LIGHT
                        let iAddr = addr * Config.MAX_IDX_BUFFER_SIZE_PER_LIGHT

                        for iIdx in iAddr .. 3 .. (iAddr + uniform.LNumIndices.[addr] - 1) do
                            
                            let v0Addr = uniform.LIndices.[iIdx + 0] + vAddr
                            let v0 = w2tP * uniform.LVertices.[v0Addr]

                            let v1Addr = uniform.LIndices.[iIdx + 1] + vAddr
                            let v1 = w2tP * uniform.LVertices.[v1Addr]

                            let v2Addr = uniform.LIndices.[iIdx + 2] + vAddr
                            let v2 =  w2tP * uniform.LVertices.[v2Addr]

                            illumination <- illumination + V4d(v2, 1.0)
                            (*

                            let t = rayTriangleIntersaction V3d.Zero i v0 v1 v2
                            
                            if t > 1e-8 then
                                illumination <- illumination + V4d(t)
                                // compute irradiance from light
                                let irr = (Vec.dot -i uniform.LForwards.[addr] ) * uniform.LIntensities.[addr]

                                illumination <- 
                                    // TODO use real material values
                                    let ill = BRDF_GGX.evaluate i o V3d.OOI 0.3 2.0 irr (V3d(0.3, 0.0, 0.05)) t
                                    illumination + ill
                                
                                ()
                            *)
                            ()
                             
                        ()
                         
                ()
                
            return V4d((v.c + illumination).XYZ, v.c.W)
            *)
            }

    
