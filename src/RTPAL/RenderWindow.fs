namespace Render

module RenderWindow =
    open Aardvark.Base
    open Aardvark.Base.Incremental

    open Aardvark.Application.WinForms
    open Aardvark.Data.Photometry

    open Light
            
    type SharedRenderData = {
        runtime : Aardvark.Rendering.GL.Runtime

        viewTrafo : IMod<Trafo3d>
        projTrafo : IMod<Trafo3d>
        viewportSize : IMod<V2i>

        lights : LightCollection
        photometricData : IMod<Option<IntensityProfileSampler>>
        }

    let openWindow (app : OpenGlApplication) (title : string) (task : IRenderTask) = 

        let win = app.CreateGameWindow()
        win.Title <- title

        ()

