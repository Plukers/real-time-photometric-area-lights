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
                let upp = 0.7

                let s = texError.Sample(v.tc)

                let error = sqrt (s.X)
                let sign =  s.Y
            
                let error = clamp 0.0 upp error
                    
                let cTrue = V3d(1.0)

                let cFalse =
                    if sign > 0.0 then
                        V3d(1.0, 0.35686, 0.14902) // Too bright
                    else 
                        V3d(0.24706, 0.38039, 1.0) // Too dark

                
                return V4d((Lerp cTrue cFalse (error / upp)), 1.0)

            }

        ()
