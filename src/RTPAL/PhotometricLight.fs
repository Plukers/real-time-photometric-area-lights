namespace Render

open Aardvark.Base
open Aardvark.Base.Incremental
open FShade

module PhotometricLight =
    
    let ProfileAddressing           = Symbol.Create "ProfileAddressing"
    let TextureOffsetScale          = Symbol.Create "TextureOffsetScale"
    let IntensityTexture            = Symbol.Create "IntensityTexture"

    type UniformScope with
        member x.ProfileAddressing        : V4d  = x?ProfileAddressing
        member x.TextureOffsetScale       : V4d  = x?TextureOffsetScale      

    type Uniforms () =
        
        let profileAddressing     = Mod.init V4f.OOOO
        let textureOffsetScale    = Mod.init V4f.OOOO
        let intensityTexture      = Mod.init (NullTexture() :> ITexture)

        let map = SymDict.ofSeq [
                    ProfileAddressing,     profileAddressing     :> IMod
                    TextureOffsetScale,    textureOffsetScale    :> IMod
                    IntensityTexture,      intensityTexture      :> IMod
                ]

        member x.Map = map

        member x.ProfileAddressing
            with get () = profileAddressing.GetValue()
            and set (value) = profileAddressing.Value <- value

        member x.TextureOffsetScale
            with get () = textureOffsetScale.GetValue()
            and set (value) = textureOffsetScale.Value <- value

        member x.IntensityTexture
            with get () = intensityTexture.GetValue()
            and set (value) = intensityTexture.Value <- value
            

    let intensityProfileSampler = 
        sampler2d {
            texture uniform?IntensityTexture
            filter Filter.MinMagLinear
            addressU WrapMode.Border
            addressV WrapMode.Wrap
            borderColor C4f.Black
        }

    

