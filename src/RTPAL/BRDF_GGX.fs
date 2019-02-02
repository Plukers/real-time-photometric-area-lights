
(*
    Module providing a GGX BRDF evaluate functions
*)
module BRDF_GGX

open System
open Aardvark.Base
open Aardvark.Base.Rendering
open FShade

    [<ReflectedDefinition>]
let private PI = Math.PI

[<ReflectedDefinition>] 
let private D alpha cosThetaM = 

    let alphaSquare = alpha * alpha
    let cosSquare = cosThetaM * cosThetaM

    let d = cosSquare * (alphaSquare - 1.0) + 1.0
    let dSquare = d * d

    alphaSquare / (PI * dSquare)

[<ReflectedDefinition>] 
let private G1 alpha cosThetaM =
    
    let alphaSquare = alpha * alpha
    let cosSquare = cosThetaM * cosThetaM

    2.0 * cosThetaM / (cosThetaM + sqrt(alphaSquare + (1.0 - alphaSquare) * cosSquare))   

[<ReflectedDefinition>]
let private schlickFresnel F0 u = 
    F0 + (1.0 - F0) * Math.Pow((1.0 - u), 5.0)

[<ReflectedDefinition>]
let private attenuation distance attenuation = 
    1.0 / Vec.dot attenuation (V3d(1.0, distance, distance * distance))

[<ReflectedDefinition>]
let evaluate (i : V3d) (o : V3d) (n : V3d) (roughness : float) (f0 : float) (lightIntensity : float) (lightAtt : V3d) (lightDist : float) =

    let nOo = clamp 0.0 1.0 (Vec.dot n o) 
    let nOi = clamp 0.0 1.0 (Vec.dot n i)

    if nOo > 0.0 && nOi > 0.0 then                        

        let m = i + o |> Vec.normalize

        let nOm = Vec.dot n m                            
        let oOm = Vec.dot o m                    
        let iOm = Vec.dot i m

        let alpha = roughness

        let d = D alpha nOm
                    
        let g = (G1 alpha nOi) * (G1 alpha nOo)

        let f = schlickFresnel f0 iOm 

        let I = (d * g * f) / (4.0 * nOi * nOo)

        let att = attenuation lightDist lightAtt

        let intensity = lightIntensity * att * (clamp 0.0 1.0 nOi)
        let brdf = (V4d.One / PI) + V4d.One * I

        intensity * brdf
        
    else
        V4d.Zero

[<ReflectedDefinition>]
let weightGGX (i : V3d) (o : V3d) (n : V3d) (roughness : float) =

    let m = i + o |> Vec.normalize

    (i.Dot(m) * (G1 roughness (i.Dot(m))) * (G1 roughness (o.Dot(m)))) / (i.Dot(n) * m.Dot(n))

[<ReflectedDefinition>]
let sampleGGX u1 u2 alpha = 

    let r = Math.Atan(alpha * Math.Sqrt(u1) / (1.0 - u1))
    let phi = 2.0 * PI * u2

    V3d(Math.Cos(phi) * Math.Sin(r), Math.Sin(phi) * Math.Sin(r), Math.Cos(r)) |> Vec.normalize
