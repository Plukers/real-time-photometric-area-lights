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

    [<ReflectedDefinition>] 
    let public getPhotometricIntensity (v : V3d) =    

        let v = v.Normalized
        let v = new V3d(-v.X, -v.Y, v.Z)

        // Vertical Texture coords
        let phi = 1.0 - acos(clamp -1.0 1.0 v.Z) * Constant.PiInv // map to 0..1
        let phi = clamp 0.0 1.0 ((phi + uniform.ProfileAddressing.X) * uniform.ProfileAddressing.Y)

        // Horizontal Texture coords
        let theta = (atan2 v.Y v.X) * 0.5 * Constant.PiInv + 0.5 // map to 0..1
        let theta = 1.0 - abs (1.0 - abs (((theta + uniform.ProfileAddressing.Z) * uniform.ProfileAddressing.W) % 2.0))

        let offset = uniform.TextureOffsetScale.XZ  //var Offset = new Float2(0.5, 0.5) / (intensityTexture.Size);
        let scale = uniform.TextureOffsetScale.YW   //var Scale = (intensityTexture.Size - Float2.II) / intensityTexture.Size;
        let crd = V2d(phi, theta) * scale + offset
        intensityProfileSampler.SampleLevel(V2d(crd.X, 1.0 - crd.Y), 0.0).X

    

