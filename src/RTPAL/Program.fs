open System

open Suave
open System.Windows.Forms

open Aardvark.Base
open Aardvark.Application.WinForms
open Aardvark.UI

[<EntryPoint>]
let main argv = 
    Xilium.CefGlue.ChromiumUtilities.unpackCef()
    Aardvark.UI.Chromium.init argv

    Ag.initialize()
    Aardvark.Init()
    use app = new OpenGlApplication()
    let runtime = app.Runtime

    let a = SimpleTestApp.app |> App.start

    WebPart.startServer 4321 [ 
        MutableApp.toWebPart runtime a
    ]  

    use form = new Form(Width = 1024, Height = 768)
    use ctrl = new AardvarkCefBrowser()
    ctrl.Dock <- DockStyle.Fill
    form.Controls.Add ctrl
    ctrl.StartUrl <- "http://localhost:4321/"

    ctrl.ShowDevTools()

    Application.Run form
    System.Environment.Exit 0
    0
