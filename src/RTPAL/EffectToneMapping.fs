namespace Render


module EffectToneMapping =

    open Aardvark.Base
    open Aardvark.Base.Rendering
    open FShade

    type UniformScope with
        member uniform.InputTex      : ShaderTextureHandle = uniform?InputTex
        member uniform.ActivateTM    : bool                = uniform?ActivateTM
        member uniform.ToneMapScale  : float               = uniform?ToneMapScale

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
    (*
    [<GLSLIntrinsic("textureQueryLevels({0})", "GL_ARB_texture_query_levels")>]
    let mipLevels (s : Sampler2d) =
        0
        *)
    let toneMap (v : ToneMapVertex) = 
        fragment {   
            
            let tc = V2i(V2d(InputTex.Size) * v.tc)
            let L  = InputTex.Item (V2i(V2d(InputTex.Size) * v.tc))

            if uniform.ActivateTM then
                
                //let Lavg = InputTex.Read(V2i(0, 0), mipLevels InputTex - 1)
                        
                let Lscaled = uniform.ToneMapScale * L            
                return Lscaled / (1.0 + Lscaled)

            else
                return L
            
        }

    ()