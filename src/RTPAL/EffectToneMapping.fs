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

    [<GLSLIntrinsic("textureQueryLevels({0})", "GL_ARB_texture_query_levels")>]
    let mipLevels (s : Sampler2d) =
        0

    [<GLSLIntrinsic("isinf({0})")>]
    let isinf (s) =
        onlyInShaderCode<bool> "isinf"

    [<GLSLIntrinsic("isnan({0})")>]
    let isnan (s) =
        onlyInShaderCode<bool> "isnan"

    let toneMap (v : ToneMapVertex) = 
        fragment {   
            let tc = V2i(V2d(InputTex.Size) * v.tc)

            let L  = InputTex.Item (V2i(V2d(InputTex.Size) * v.tc))
            
            let Lavg = InputTex.Read(V2i(0, 0), mipLevels InputTex - 1)

            (*
            let size = InputTex.GetSize (mipLevels InputTex - 1)
            
            if size.X = 1 && size.Y = 1 then
                return V4d(0.0, 1.0, 0.0, 1.0)
            else
                return V4d(1.0, 0.0, 0.0, 1.0)
            *)
            let isinfVec = 
                let mutable b = false
                b <- b || isinf Lavg.X
                b <- b || isinf Lavg.Y
                b <- b || isinf Lavg.Z
                b <- b || isinf Lavg.W
                b
            
            let isnanVec = 
                let mutable b = false
                b <- b || isnan Lavg.X
                b <- b || isnan Lavg.Y
                b <- b || isnan Lavg.Z
                b <- b || isnan Lavg.W
                b
            
            if isinfVec || isnanVec then
                return V4d(1.0, 0.0, 0.0, 1.0)
            else            
                let Lscaled = uniform.ToneMapScale * L / Lavg
            
                return Lscaled / (1.0 + Lscaled)
            
        }

    ()