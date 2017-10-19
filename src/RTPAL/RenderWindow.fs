namespace Render

module RenderWindow =
    open Aardvark.Base
    open Aardvark.Base.Incremental

    open Aardvark.Application.WinForms
        
    type RenderWindowBridge = {
        view : IMod<CameraView>
        proj : IMod<Frustum>

        runtime : Aardvark.Rendering.GL.Runtime
        }

    let openWindow (app : OpenGlApplication) (title : string) (task : IRenderTask) = 

        let win = app.CreateGameWindow()
        win.Title <- title

        ()

