namespace Render

module RenderInterop =
    
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Data.Photometry    
    open Aardvark.Base.Rendering

    
    open Aardvark.SceneGraph
    
    open Aardvark.Application.WinForms

    open Light
    open Light.Sg

    type RenderData = {
            runtime : Aardvark.Rendering.GL.Runtime

            dt : IMod<float>
            
            sceneSg : ISg

            view         : IMod<CameraView>
            projTrafo    : IMod<Trafo3d>
            viewportSize : IMod<V2i>

            mode            : IMod<RenderMode>
            compare         : IMod<RenderMode>
            lights          : LightCollection
            lightData       : LightSgData
            photometricData : IMod<Option<IntensityProfileSampler>>

            toneMap : IMod<bool>
            toneMapScale : IMod<float>

            skewClipPlane : IMod<bool>
        }

    type RenderFeedback = {
            // global
            fps : ModRef<float>

            // ground truth
            frameCount : ModRef<int>

            // compare
            compareTexture : IOutputMod<ITexture>
        }

    let initialRenderData (app : OpenGlApplication) (view : IMod<CameraView>) (projTrafo : IMod<Trafo3d>) (viewportSize : V2i) (m : MRenderState) (dt : IMod<float>) (sceneSg : ISg) (lightData : LightSgData) =
        {
            runtime = app.Runtime
            dt = dt
            sceneSg = sceneSg
            view = view
            projTrafo = projTrafo 
            viewportSize = viewportSize |> Mod.init
            lights = m.lights |> Mod.force // mod force necessary ? 
            lightData = lightData
            photometricData = m.photometryData
            mode = m.renderMode
            compare = m.compare
            toneMap = m.toneMap
            toneMapScale = m.toneMapScale.value
            skewClipPlane = m.skewClipPlane
        }

