open System

open Suave
open System.Windows.Forms

open Aardvark.Base
open Aardvark.Application.WinForms
open Aardvark.UI
open Aardvark.SceneGraph.IO
open Aardvark.Rendering.Vulkan

[<EntryPoint; STAThread>]
let main argv = 
    
    Xilium.CefGlue.ChromiumUtilities.unpackCef()
    Aardvark.UI.Chromium.init argv

    Loader.Assimp.initialize()

    Ag.initialize()
    Aardvark.Init()
    use app = new OpenGlApplication(true)

    // FShade.EffectDebugger.attach()
    
    let runtime = app.Runtime
    
    use form = new Form(Width = 408, Height = 630)
    let a = RenderApp.app app form |> App.start

    WebPart.startServer 4321 [ 
        MutableApp.toWebPart runtime a
    ]  

    use ctrl = new AardvarkCefBrowser()
    ctrl.Dock <- DockStyle.Fill
    form.Controls.Add ctrl
    ctrl.StartUrl <- "http://localhost:4321/"

    // ctrl.ShowDevTools()

    Application.Run form
    System.Environment.Exit 0
    
    0
