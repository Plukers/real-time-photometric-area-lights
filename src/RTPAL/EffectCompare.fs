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

    type CompareVertex = {
        [<TexCoord>] tc : V2d
    } 

    let compare (v : CompareVertex) = 
        fragment {       
            let upp = 0.02

            let ta = texA.Sample(v.tc) 
            let tb = texB.Sample(v.tc) 
            
            let err = 
                let diff = V3d(ta - tb)  
                clamp 0.0 upp (sqrt (dot diff diff))

            let cGood = V3d(1.0)
            let cBad = V3d(1.0, 0.35686, 0.14902)
            
            return V4d((Lerp cGood cBad (err / upp)), 1.0)
        }

    ()
