namespace Render

(*
    Ground Truth Rendering Effect
    Single Bounce Path Tracing
*)
module GTEffect = 
    open System

    open FShade
    open FShade.Imperative
    open Aardvark.Base
    open Aardvark.Base.Rendering

    // open extended UniformScopes
    open Utils.HaltonSequence
    open Light.Effect

    type UniformScope with
        member uniform.FrameCount : int = uniform?FrameCount

    [<ReflectedDefinition>]
    let private PI = Math.PI
    
    [<ReflectedDefinition>]
    let private floorV4d (v : V4d) = 
        V4d(floor v.X, floor v.Y, floor v.Z, floor v.W)

    [<ReflectedDefinition>]
    let private fractV4d (v : V4d) = 
        v - floorV4d(v)
        

    (*
        Creates a hash usable as jitter computed from a 2D coordinate
        Taken from https://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/
    *)
    [<ReflectedDefinition>]
    let private fast32Hash (coordinate : V3d) = 
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
        
    (*
        Uniformly samples a direction from the hemisphere for two given random numbers
        Works in the tangent space
        http://www.rorydriscoll.com/2009/01/07/better-sampling/
    *)
    [<ReflectedDefinition>]
    let private sampleHemisphere u1 u2 = 

        let r = Math.Sqrt(1.0 - u1 * u1)
        let phi = 2.0 * PI * u2

        V3d(Math.Cos(phi) * r, Math.Sin(phi) * r, u1) |> Vec.normalize

    (*
        Samples a direction from the hemisphere for two given random numbers where the samples are cosine weighted
        Works in the tangent space
        http://www.rorydriscoll.com/2009/01/07/better-sampling/
    *)
    [<ReflectedDefinition>]
    let private cosineSampleHemisphere u1 u2 = 
        let r = Math.Sqrt(u1)
        let theta = 2.0 * PI * u2

        V3d(r * Math.Cos(theta), r * Math.Sin(theta), Math.Sqrt(max 0.0 (1.0 - u1))) |> Vec.normalize

    // glsl mat3 is column major
    // transpose because of inverted multiplication logic
    [<GLSLIntrinsic("transpose(mat3({0},{1},{2},{3},{4},{5},{6},{7},{8}))")>] 
    let inline private glslM33d (a00 : 'a) (a10 : 'a) (a20 : 'a) (a01 : 'a) (a11 : 'a) (a21 : 'a) (a02 : 'a) (a12 : 'a) (a22 : 'a) = 
        onlyInShaderCode<M33d> "mat3"

    [<ReflectedDefinition>]
    let private M33dFromCols (c1 : V3d) (c2 : V3d) (c3 : V3d) =
        glslM33d c1.X c1.Y c1.Z c2.X c2.Y c2.Z c3.X c3.Y c3.Z

    [<ReflectedDefinition>]
    let private M33dFromRows (r1 : V3d) (r2 : V3d) (r3 : V3d) =
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
    let private basisFrisvad (n : V3d) = 

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
        Computes the intersaction point of a ray and triangle. 

        Möller, T., & Trumbore, B. (2005, July). 
        Fast, minimum storage ray/triangle intersection.
        In ACM SIGGRAPH 2005 Courses (p. 7). ACM.

        https://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm
    *)
    [<ReflectedDefinition>]
    let private rayTriangleIntersaction (orig : V3d) (dir : V3d) (v0 : V3d) (v1 : V3d) (v2 : V3d) = 
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

    type GTVertex = {
        [<Position>]        pos     : V4d
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
        [<FragCoord>]       fc      : V4d
    }        
        
    let groundTruthLighting (v : GTVertex) = 
        fragment {
            // TODO use real material values
            let alpha = 0.3
            let f0 = 2.0
            let atten = V3d(0.3, 0.0, 0.05)

            let P = v.wp.XYZ
            let worldV = ( uniform.CameraLocation - P) |> Vec.normalize

            let w2t = v.n |> Vec.normalize |> basisFrisvad |> Mat.transpose
            // let t2w = w2t |> Mat.inverse
            
            // Transform view vector into tangent space
            let o = w2t * worldV
            
            // Compute a jitter
            let jitter = (fast32Hash v.fc.XYZ).XY              

            let mutable illumination = V4d.Zero
            
            // Iterate over Samples
            for sIdx in 0 .. Config.NUM_SAMPLES - 1 do
                   
                let u1 = 
                    let x = jitter.X + uniform.HaltonSamples.[sIdx].X
                    x - Math.Floor(x)

                let u2 = 
                    let x = jitter.Y + uniform.HaltonSamples.[sIdx].Y
                    x - Math.Floor(x)

                // let m = BRDF_GGX.sampleGGX u1 u2 alpha                
                // let i = sampleHemisphere u1 u2
                let i = cosineSampleHemisphere u1 u2   

                let pdf = i.Z / PI

                // let i = 2.0 * (o.Dot(m)) * m - o       
                
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
                            let v0 = w2t * (uniform.LVertices.[v0Addr] - P)
                           
                            let v1Addr = uniform.LIndices.[iIdx + 1] + vAddr
                            let v1 = w2t * (uniform.LVertices.[v1Addr] - P)
                           
                            let v2Addr = uniform.LIndices.[iIdx + 2] + vAddr
                            let v2 = w2t * (uniform.LVertices.[v2Addr] - P)                       

                            let t = rayTriangleIntersaction V3d.Zero i v0 v1 v2

                            if t > 1e-8 then
                                let irr = 10.0// (Vec.dot -i (w2t * uniform.LForwards.[addr])) * uniform.LIntensities.[addr]

                                illumination <-
                                    let weight = 1.0 / PI
                                        // BRDF_GGX.weightGGX i o V3d.OOI alpha
                                    let albedo = 0.5
                                    let brdf = v.c / PI
                                    illumination + irr * brdf / pdf * i.Z // + BRDF_GGX.evaluate i o V3d.OOI alpha f0 irr atten t                                    
                                ()                            
                            ()  
                        ()  
                ()

            illumination <- 2.0 * illumination / V4d(Config.NUM_SAMPLES);

            let alpha = 1.0 / (float)(uniform.FrameCount + 1)

            return V4d(illumination.XYZ, alpha)
            }


    (*
    type UniformScope with
        member uniform.IterationTex    : ShaderTextureHandle = uniform?IterationTex
        member uniform.AccumulationTex : ShaderTextureHandle = uniform?AccumulationTex
    *)
    // let iterationTexSym = Sym.ofString "IterationTex"

   (*
    let accumulationTexSym = Sym.ofString "AccumulationTex"
    let private accumulationTex = 
        sampler2d {
            texture uniform.AccumulationTex
            filter Filter.MinMagMipPoint
        }
    *)
    let private iterationTex =
        sampler2d {
            texture uniform?DiffuseColorTexture
            filter Filter.MinMagMipPoint
        }

    type PostVertex = { [<TexCoord>] tc : V2d }     

    let passThrough (v : PostVertex) =
        fragment {
            return iterationTex.Sample(v.tc)
        }
       
    let debugOutput = 
   
        let config =
            EffectConfig.ofList [ 
                Intrinsics.Color, typeof<V4d>, 0
            ]

        let cModule = 
            Effect.ofFunction groundTruthLighting
               |> Effect.toModule config
               |> ModuleCompiler.compile glsl410

        let glsl = 
            cModule
                |> GLSL.Assembler.assemble glsl410

        printfn "+-------- Ground Truth Lighting --------+"
        printfn "+--------- Start Shader Output ---------+"
        printfn "%A" glsl.builtIns
        printfn "%s" glsl.code
        printfn "+---------- End Shader Output ----------+"
        
    
