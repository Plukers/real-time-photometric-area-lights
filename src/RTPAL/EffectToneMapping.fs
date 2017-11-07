namespace Render


module EffectToneMapping =

    open Aardvark.Base
    open Aardvark.Base.Rendering
    open FShade

    type UniformScope with
        member uniform.InputTex : ShaderTextureHandle = uniform?InputTex
        member unfirom.ToneMapScale : float = uniform?ToneMapScale

    let private InputTex =
        sampler2d {
            texture uniform.InputTex
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    type ToneMapVertex = {
        [<TexCoord>] tc : V2d
    } 

    
    let toneMap (v : ToneMapVertex) = 
        fragment {   
            let tc = V2i(V2d(InputTex.Size) * v.tc)

            let L  = InputTex.Item (V2i(V2d(InputTex.Size) * v.tc))
            let Lavg = InputTex.Item ( V2i(0, 0), InputTex.MipMapLevels) 
            
            
            let Lscaled = uniform.ToneMapScale * L / Lavg
            
            return Lscaled / (1.0 + Lscaled)
        }

    ()