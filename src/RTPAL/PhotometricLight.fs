namespace Render

open Aardvark.Base
open Aardvark.Base.Incremental
open FShade

module PhotometricLight =
    open EffectUtils
    
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

    // i in world space
    // light space { up X -forward, up, -forward }
    [<ReflectedDefinition>] 
    let public getPhotometricIntensity (i : V3d) (forward : V3d) (up : V3d) =    
        
        let basis = // TODO compute once and pass as uniform
            M33dFromCols (V3d.Cross(up, -forward)) up -forward
            |> Mat.transpose
            
        let i = basis * i |> Vec.normalize
        
        let i = new V3d(-i.X, -i.Y, i.Z)

        // Vertical Texture coords
        let phi = 1.0 - acos(clamp -1.0 1.0 i.Z) * Constant.PiInv // map to 0..1
        let phi = clamp 0.0 1.0 ((phi + uniform.ProfileAddressing.X) * uniform.ProfileAddressing.Y)

        // Horizontal Texture coords
        let theta = (atan2 i.Y i.X) * 0.5 * Constant.PiInv + 0.5 // map to 0..1
        let theta = 1.0 - abs (1.0 - abs (((theta + uniform.ProfileAddressing.Z) * uniform.ProfileAddressing.W) % 2.0))

        let offset = uniform.TextureOffsetScale.XZ  //var Offset = new Float2(0.5, 0.5) / (intensityTexture.Size);
        let scale = uniform.TextureOffsetScale.YW   //var Scale = (intensityTexture.Size - Float2.II) / intensityTexture.Size;
        let crd = V2d(phi, theta) * scale + offset
        intensityProfileSampler.SampleLevel(V2d(crd.X, 1.0 - crd.Y), 0.0).X

    

