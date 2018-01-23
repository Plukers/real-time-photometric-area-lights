namespace Render

module RenderInterop =
    
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Data.Photometry    
    open Aardvark.Base.Rendering
    
    open Aardvark.Application.WinForms

    open Light

    type RenderData = {
            runtime : Aardvark.Rendering.GL.Runtime

            scenePath : IMod<string>

            view         : IMod<CameraView>
            frustum      : IMod<Frustum>
            viewportSize : IMod<V2i>

            mode            : IMod<RenderMode>
            compare         : IMod<RenderMode>
            lights          : LightCollection
            photometricData : IMod<Option<IntensityProfileSampler>>

            toneMap : IMod<bool>
            toneMapScale : IMod<float>
        }

    type RenderFeedback = {
            // global
            fps : ModRef<float>

            // ground truth
            frameCount : ModRef<int>

            // compare
            compareTexture : IOutputMod<ITexture>
        }

    let initialRenderData (app : OpenGlApplication) (view : IMod<CameraView>) (frustum : IMod<Frustum>) (viewportSize : V2i) (m : MRenderState) =
        {
            runtime = app.Runtime
            scenePath = m.scenePath
            view = view
            frustum = frustum // Frustum.perspective 60.0 0.1 100.0 ((float)viewportSize.X / (float)viewportSize.Y) |> Mod.init 
            viewportSize = viewportSize |> Mod.init
            lights = m.lights |> Mod.force // mod force necessary ? 
            photometricData = m.photometryData
            mode = m.renderMode
            compare = m.compare
            toneMap = m.toneMap
            toneMapScale = m.toneMapScale.value
        }

