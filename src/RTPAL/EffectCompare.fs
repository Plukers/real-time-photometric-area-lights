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
            member uniform.TexA : ShaderTextureHandle = uniform?TexA
            member uniform.TexB : ShaderTextureHandle = uniform?TexB

        let private texA =
            sampler2d {
                texture uniform.TexA
                filter Filter.MinMagMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }

        let private texB =
            sampler2d {
                texture uniform.TexB
                filter Filter.MinMagMipLinear
                addressU WrapMode.Wrap
                addressV WrapMode.Wrap
            }
            
        let computeError (v : CompareVertex) = 
            fragment {
                let luminance = V3d(0.2126, 0.7152, 0.0722)

                let a = dot luminance (V3d(texA.Sample(v.tc))) 
                let b = dot luminance (V3d(texB.Sample(v.tc))) 
            
                let diff = a - b   
                let err = diff * diff

                return V4d(err, 0.0, 0.0, 1.0)
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

                let error = sqrt (texError.Sample(v.tc).X)
            
                let error = clamp 0.0 upp error
                    
                let cGood = V3d(1.0)
                let cBad = V3d(1.0, 0.35686, 0.14902)
            
                return V4d((Lerp cGood cBad (error / upp)), 1.0)
            }

        ()
