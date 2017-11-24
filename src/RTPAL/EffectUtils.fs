namespace Render

module EffectUtils =

    open System

    open FShade
    open Aardvark.Base
    open FShade.Imperative
    open Aardvark.Base.Rendering.Effects

    open Light.Effect
    open PhotometricLight
    

    type UniformScope with
        member uniform.FrameCount : int = uniform?FrameCount

    [<ReflectedDefinition>]
    let PI = Math.PI
    
    [<ReflectedDefinition>]
    let floorV4d (v : V4d) = 
        V4d(floor v.X, floor v.Y, floor v.Z, floor v.W)

    [<ReflectedDefinition>]
    let fractV4d (v : V4d) = 
        v - floorV4d(v)
        
    (*
        Creates a hash usable as jitter computed from a 2D coordinate
        Taken from https://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/
    *)
    [<ReflectedDefinition>]
    let fast32Hash (coordinate : V3d) = 
        let offset = V2d(26.0, 161.0)
        let domain = 71.0
        let someLargeFloat = 951.135664 //+ coordinate.Z

        let mutable P = V4d(coordinate.X, coordinate.Y, coordinate.X, coordinate.Y)
        P <- P + V4d(0.0, 0.0, 1.0, 1.0)
        
        P <- P - floorV4d(P / domain) * domain
        
        P <- P + V4d(offset.X, offset.Y, offset.X, offset.Y)
        P <- P * P

        let xzxz = V4d(P.X, P.Z, P.X, P.Z)
        let yyww = V4d(P.Y, P.Y, P.W, P.W)

        fractV4d(xzxz * yyww * V4d(1.0 / someLargeFloat))

    [<GLSLIntrinsic("isinf({0})")>]
    let isinf (s) =
        onlyInShaderCode<bool> "isinf"

    [<GLSLIntrinsic("isnan({0})")>]
    let isnan (s) =
        onlyInShaderCode<bool> "isnan"
    
    [<ReflectedDefinition>]
    let isnanVec (vec : V4d) = 
        let mutable b = false
        b <- b || isnan vec.X
        b <- b || isnan vec.Y
        b <- b || isnan vec.Z
        b
        
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
        Samples a direction from the hemisphere for two given random numbers where the samples are cosine weighted
        Works in the tangent space
        http://www.rorydriscoll.com/2009/01/07/better-sampling/
    *)
    [<ReflectedDefinition>]
    let cosineSampleHemisphere u1 u2 = 
        let r = Math.Sqrt(u1)
        let theta = 2.0 * PI * u2

        V3d(r * Math.Cos(theta), r * Math.Sin(theta), Math.Sqrt(max 0.0 (1.0 - u1))) |> Vec.normalize

    // glsl mat3 is column major
    // transpose because of inverted multiplication logic
    [<GLSLIntrinsic("transpose(mat3({0},{1},{2},{3},{4},{5},{6},{7},{8}))")>] 
    let inline private glslM33d (a00 : 'a) (a10 : 'a) (a20 : 'a) (a01 : 'a) (a11 : 'a) (a21 : 'a) (a02 : 'a) (a12 : 'a) (a22 : 'a) = 
        onlyInShaderCode<M33d> "mat3"

    [<ReflectedDefinition>]
    let M33dFromCols (c1 : V3d) (c2 : V3d) (c3 : V3d) =
        glslM33d c1.X c1.Y c1.Z c2.X c2.Y c2.Z c3.X c3.Y c3.Z

    [<ReflectedDefinition>]
    let M33dFromRows (r1 : V3d) (r2 : V3d) (r3 : V3d) =
        glslM33d r1.X r2.X r3.X r1.Y r2.Y r3.Y r1.Z r2.Z r3.Z

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
                    1.0 - (n.X  * n.X) / (1.0 + n.Z),
                    (-n.X * n.Y) / (1.0 + n.Z),
                    -n.X
                    )

        let c2 = V3d(
                    (-n.X * n.Y) / (1.0 + n.Z),
                    1.0 - (n.Y  * n.Y) / (1.0 + n.Z),
                    -n.Y
                    )

        M33dFromCols c1 c2 n


    (*
        Compute barycentric coordinates (u, v, w) for point p with respect to triangle (a, b, c)

        https://gamedev.stackexchange.com/questions/23743/whats-the-most-efficient-way-to-find-barycentric-coordinates

        Ericson, C. (2004). 
        Real-time collision detection. 
        CRC Press.
    *)
    [<ReflectedDefinition>]
    let barycentricCoordinates (p : V2d) (a : V2d) (b : V2d) (c : V2d) = 

        let v0 = b - a
        let v1 = c - a
        let v2 = p - a

        let d00 = Vec.dot v0 v0
        let d01 = Vec.dot v0 v1
        let d11 = Vec.dot v1 v1
        let d20 = Vec.dot v2 v0
        let d21 = Vec.dot v2 v1

        let denom = d00 * d11 - d01 * d01

        let v = (d11 * d20 - d01 * d21) / denom
        let w = (d00 * d21 - d01 * d20) / denom
        let u = 1.0 - v - w

        (u, v, w)

    (*
        https://en.wikipedia.org/wiki/Vector_projection#Vector_projection_2
    *)
    [<ReflectedDefinition>]
    let projectPointOnLine (a : V3d) (b : V3d) (p : V3d) = 

        let ab = b - a
        let ap = p - a
                                    
        a + ((Vec.dot ab ap) / (Vec.dot ab ab)) * ab
        

    [<ReflectedDefinition>]
    let linePlaneIntersection (lineO : V3d) (lineDir : V3d) (planeP : V3d) (planeN : V3d) = 
        let t = (Vec.dot (planeP - lineO) planeN) / (Vec.dot lineDir planeN)
        lineO + t * lineDir

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

            if (u < 0.0 || 1.0 < u) then
                0.0
            else
                let qVec = Vec.cross tVec e1
                let v    = (Vec.dot dir qVec) * invDet

                if (v < 0.0 || 1.0 < u + v) then
                    0.0
                else 
                    ((Vec.dot e2 qVec) * invDet)

    [<ReflectedDefinition>]   
    let private projetToLineSegment a b p = 
        let projection = projectPointOnLine a b p
        
        let pa = a - projection     
        let pb = b - projection           

        if (Vec.dot pa pb) < 0.0 then
            projection
        elif (Vec.length pa) < Vec.length(pb) then
            a
        else
            b

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
          
        let eps = 1e-9

        let clampedP = 
            match barycentricCoordinates p2d a2d b2d c2d with
            | (u, v, w) when u <= eps && v <= eps && w >  eps -> c                         //   I
            | (u, v, w) when u >  eps && v <  eps && w >  eps -> projetToLineSegment c a p //  II
            | (u, v, w) when u >  eps && v <= eps && w <= eps -> a                         // III
            | (u, v, w) when u >  eps && v >  eps && w <  eps -> projetToLineSegment a b p //  IV
            | (u, v, w) when u <= eps && v >  eps && w <= eps -> b                         //   V
            | (u, v, w) when u <  eps && v >  eps && w >  eps -> projetToLineSegment b c p //  VI
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
        
        let eps = 1e-9

        match (t, u, v, w) with
        | (t, u, v, w) when t >  eps && u <= eps && v <= eps && w <= eps -> projetToLineSegment a b p //    I
        | (t, u, v, w) when t >  eps && u >  eps && v <= eps && w <= eps -> b                         //   II
        | (t, u, v, w) when t <= eps && u >  eps && v <= eps && w <= eps -> projetToLineSegment b c p //  III
        | (t, u, v, w) when t <= eps && u >  eps && v >  eps && w <= eps -> c                         //   IV
        | (t, u, v, w) when t <= eps && u <= eps && v >  eps && w <= eps -> projetToLineSegment c d p //    V
        | (t, u, v, w) when t <= eps && u <= eps && v >  eps && w >  eps -> d                         //   VI
        | (t, u, v, w) when t <= eps && u <= eps && v <= eps && w >  eps -> projetToLineSegment d a p //  VII
        | (t, u, v, w) when t >  eps && u <= eps && v <= eps && w >  eps -> a                         // VIII
        | _ -> p // all coordinates are positive

    [<ReflectedDefinition>] 
    let Lerp (a : V3d) (b : V3d) (s : float) : V3d = (1.0 - s) * a + s * b

    [<ReflectedDefinition>] 
    let clipTriangle(p : V3d, n : V3d, vertices : Arr<N<3>, V3d>) =
        let plane = V4d(n, -V3d.Dot(p, n))
        let eps = 1e-9

        let mutable vc = 0
        let va = Arr<N<4>, V3d>()

        let vb = V4d(vertices.[0], 1.0)
        let hb = V4d.Dot(plane, vb) 
        let hbv = hb > eps
        let hbn = hb < -eps
        
        if (hb >= -eps) then 
            va.[vc] <- vb.XYZ
            vc <- vc + 1

        let mutable v0 = vb
        let mutable h0 = hb
        let mutable h0v = hbv
        let mutable h0n = hbn
        
        for vi in 1..2 do
            let v1 = V4d(vertices.[vi], 1.0)
            let h1 = V4d.Dot(plane, v1)
            let h1v = h1 > eps
            let h1n = h1 < -eps
            if (h0v && h1n || h0n && h1v) then
                va.[vc] <- Lerp v0.XYZ v1.XYZ (h0 / (h0 - h1))
                vc <- vc + 1

            if (h1 >= -eps) then 
                va.[vc] <- v1.XYZ
                vc <- vc + 1

            v0 <- v1
            h0 <- h1
            h0v <- h1v
            h0n <- h1n
        
        // last edge to vertices[0]
        if (h0v && hbn || h0n && hbv) then
            va.[vc] <- Lerp v0.XYZ vb.XYZ (h0 / (h0 - hb))
            vc <- vc + 1

        (va,vc)

    [<ReflectedDefinition>] 
    let private integrateSegment(a: V3d, b: V3d) =              
        let theta = acos ( clamp -1.0 1.0 (V3d.Dot(a, b)))
        V3d.Cross(a, b).Z * if theta < 1e-5 then 1.0 else theta/sin(theta)

    [<ReflectedDefinition>] 
    let baumFormFactor(va : Arr<N<4>, V3d>, vc : int) =

        let mutable sum = 0.0

        for vi in 0 .. vc - 2 do
            sum <- sum + integrateSegment(va.[vi], va.[vi + 1])

        sum <- sum + integrateSegment(va.[vc - 1], va.[0])

        sum

    (*
        Computes the solid angle for a planar triangle as seen from the origin.

        Van Oosterom, A., & Strackee, J. (1983). 
        The solid angle of a plane triangle. 
        IEEE transactions on Biomedical Engineering, (2), 125-126.
      
        https://en.wikipedia.org/wiki/Solid_angle#Tetrahedron
    *)
    [<ReflectedDefinition>]
    let computeSolidAngle (va : V3d) (vb : V3d) (vc : V3d) =

        let ma = Vec.length va
        let mb = Vec.length vb
        let mc = Vec.length vc
        
        let numerator = abs (Vec.dot va (Vec.cross vb vc))

        let denom = ma * mb * mc + (Vec.dot va vb) * mc + (Vec.dot va vc) * mb + (Vec.dot vb vc) * ma
        
        let halfSA = atan(numerator / denom)

        2.0 * if halfSA >= 0.0 then halfSA else halfSA + PI




    (*
        i in world space
        light space { up X -forward, up, -forward }
    *)
    [<ReflectedDefinition>] 
    let public getPhotometricIntensity (i : V3d) (forward : V3d) (up : V3d) =    
        
        let basis = // TODO compute once and pass as uniform
            M33dFromCols (V3d.Cross(up, -forward)) up -forward
            |> Mat.transpose
            
        let i = basis * i |> Vec.normalize
        
        let i = new V3d(-i.X, -i.Y, i.Z)

        // Vertical Texture coords
        let phi = 1.0 - acos(clamp -1.0 1.0 i.Z) * Constant.PiInv // map to 0..1
        let phi = clamp 0.0 1.0 ((phi + uniform.ProfileAddressing.X) * uniform.ProfileAddressing.Y)

        // Horizontal Texture coords
        let theta = (atan2 i.Y i.X) * 0.5 * Constant.PiInv + 0.5 // map to 0..1
        let theta = 1.0 - abs (1.0 - abs (((theta + uniform.ProfileAddressing.Z) * uniform.ProfileAddressing.W) % 2.0))

        let offset = uniform.TextureOffsetScale.XZ  //var Offset = new Float2(0.5, 0.5) / (intensityTexture.Size);
        let scale = uniform.TextureOffsetScale.YW   //var Scale = (intensityTexture.Size - Float2.II) / intensityTexture.Size;
        let crd = V2d(phi, theta) * scale + offset
        intensityProfileSampler.SampleLevel(V2d(crd.X, 1.0 - crd.Y), 0.0).X

    let effectClearNaN (v : Vertex)= 
        fragment {

            if isnanVec v.c then
                return V4d(0.0, 0.0, 0.0, 1.0)
            else 
                return v.c

            }

    let debugShaderOutput shader = 
   
        let config =
            EffectConfig.ofList [ 
                Intrinsics.Color, typeof<V4d>, 0
            ]

        let cModule = 
            Effect.ofFunction shader
               |> Effect.toModule config
               |> ModuleCompiler.compile glsl410

        let glsl = 
            cModule
                |> GLSL.Assembler.assemble glsl410
                
        printfn "+--------- Start Shader Output ---------+"
        printfn "%A" glsl.builtIns
        printfn "%s" glsl.code
        printfn "+---------- End Shader Output ----------+"