namespace Render

(*
    Compares renderings of different effects with a transfer function
*)
module EffectCompare =

    open Aardvark.Base
    open Aardvark.Base.Rendering
    open Aardvark.Base.Rendering.Effects
    open Aardvark.Base.Vec
    open FShade

    open EffectUtils

    type CompareVertex = {
            [<TexCoord>] tc : V2d
        } 

    module Compute = 

        type UniformScope with
            member uniform.GTTex : ShaderTextureHandle = uniform?GTTex
            member uniform.CompTex : ShaderTextureHandle = uniform?CompTex

        let private gtTex =
            sampler2d {
                texture uniform.GTTex
                filter Filter.MinMagMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let private compTex =
            sampler2d {
                texture uniform.CompTex
                filter Filter.MinMagMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }
            
        let computeError (v : CompareVertex) = 
            fragment {
                let luminance = V3d(0.2126, 0.7152, 0.0722)

                let gt = dot luminance (V3d(gtTex.Sample(v.tc))) 
                let comp = dot luminance (V3d(compTex.Sample(v.tc))) 
            
                let diff = comp - gt   
                let err = diff * diff
                
                return V4d(err, float (sign diff), 0.0, 1.0)
            }


     module Visualize = 
    
        let private texError =
            sampler2d {
                texture uniform?TexError
                filter Filter.MinMagMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }
            
        let visualize (v : CompareVertex) = 
            fragment {  
                let upp1 = 0.75
                let b1 = V3d(1.0, 0.5089, 0.34902)      // [255, 130,  89] 
                let d1 = V3d(0.21177, 0.29412, 0.69804) // [ 54,  75, 178] 

                let upp2 = 1.5
                let b2 = V3d(1.0, 0.35686, 0.14902)     // [255,  91,  38] 
                let d2 = V3d(0.07059, 0.18039, 0.69804) // [ 18,  46, 178]

                let s = texError.Sample(v.tc)

                let error = sqrt (s.X)
                let sign =  s.Y
                                                     

                let (cTrue, cFalse, low, upp) =
                    if sign > 0.0 then
                        if error <= upp1 then // Too bright
                            (V3d(1.0), b1, 0.0, upp1) 
                        else 
                            (b1, b2, upp1, upp2) 
                    else 
                        if error <= upp1 then // Too dark
                            (V3d(1.0), d1, 0.0, upp1) 
                        else 
                           (d1, d2, upp1, upp2) 

               
                
                let error = (clamp low upp error) - low

                
                return V4d((Lerp cTrue cFalse (error / (upp - low))), 1.0)

            }

        ()
