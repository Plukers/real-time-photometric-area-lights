namespace Render

(*
    Ground Truth Rendering Effect
    Single Bounce Path Tracing
*)
module GDEffect = 
    open System

    open FShade
    open Aardvark.Base
    open Aardvark.Base.Rendering

    [<ReflectedDefinition>]
    let private PI = Math.PI
    
    [<ReflectedDefinition>]
    let private floorV4d (v : V4d) = 
        V4d(Math.Floor(v.X), Math.Floor(v.Y), Math.Floor(v.Z), Math.Floor(v.W))
   
    [<ReflectedDefinition>]
    let private fractV4d (v : V4d) = 
        v - floorV4d(v)

    (*
        creates a hash usable as jitter computed from a 2D coordinate
        taken from https://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/
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

        fractV4d(P.XZXZ * P.YYWW * V4d(1.0 / someLargeFloat))
        
    (*
        Uniformly samples a direction from the hemisphere for two given random numbers
        Works in the tangent space
        http://www.rorydriscoll.com/2009/01/07/better-sampling/
    *)
    [<ReflectedDefinition>]
    let sampleHemisphere u1 u2 = 

        let r = Math.Sqrt(1.0 - u1 * u1)
        let phi = 2.0 * PI * u2

        V3d(Math.Cos(phi) * r, Math.Sin(phi) * r, u1)


    (*
        Computes an orthonormal basis for a given n
        The resulting basis has n as it's z-axis.
        Assumes that n has unit length

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

        M33d.FromCols(c1, c2, n) 
        
    let private Shader (v : Effects.Vertex) = 
            fragment {

                let worldV = (uniform.CameraLocation - v.wp.XYZ) |> Vec.normalize

                let t2w = basisFrisvad v.n
                let w2t = t2w |> Mat.transpose

                // Transform view vector into tangent space
                let o = w2t * worldV

                // Compute a jitter
                let jitter = (fast32Hash v.tc).XY
                

                let haltonSamples : Arr<N<Config.NUM_SAMPLES>, V2d> = uniform?HaltonSamples

                // Iterate over Samples
                for i in 0 .. Config.NUM_SAMPLES do

                    let a = haltonSamples.[i].X
                    let b = jitter.X
                   
                    let u1 = 
                        let x = jitter.X + haltonSamples.[i].X
                        x - Math.Floor(x)

                    let u2 = 
                        let x = jitter.Y + haltonSamples.[i].Y
                        x - Math.Floor(x)

                    let i = sampleHemisphere u1 u2 

                    // TODO

                    // Check if i hits a light
                    // If it does, compute the illumination

                    ()
                    
                return V4d(1.0, 1.0, 1.0, 1.0)
                }


    let PixelShader =
        toEffect Shader 

    
