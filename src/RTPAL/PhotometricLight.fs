namespace Render


module PhotometricLight =

    open Aardvark.Base
    open Aardvark.Base.Incremental

    open Aardvark.Data.Photometry

    open Aardvark.SceneGraph.SgFSharp
    open Aardvark.SceneGraph

    open FShade


    
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

    let setupPhotometricData (photometricData : IMod<Option<IntensityProfileSampler>>) (sg : ISg) = 
        photometricData |> Mod.map( fun (pd : Option<IntensityProfileSampler>) ->
            match pd with 
            | Some data ->
                sg
                    |> Sg.uniform "ProfileAddressing" (data.AddressingParameters |> Mod.init)
                    |> Sg.uniform "TextureOffsetScale" (data.ImageOffsetScale |> Mod.init)
                    |> Sg.texture IntensityTexture (((PixTexture2d(PixImageMipMap(data.Image), false)) :> ITexture) |> Mod.constant)
            | None -> sg
        )
        |> Sg.dynamic

    

