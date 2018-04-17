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
    
    /////////////////////////////////////////////////////////////
    Xilium.CefGlue.ChromiumUtilities.unpackCef()
    Chromium.init argv

    Loader.Assimp.initialize()

    Ag.initialize()
    Aardvark.Init()
    use app = new OpenGlApplication(true)

    // FShade.EffectDebugger.attach()
    
    let runtime = app.Runtime
    
    use form = new Form(Width = 800, Height = 600)
    let a = RenderApp.app app form |> App.start

    WebPart.startServer 4321 [ 
        MutableApp.toWebPart runtime a
    ]  

    use ctrl = new AardvarkCefBrowser()
    ctrl.Dock <- DockStyle.Fill
    form.Controls.Add ctrl
    ctrl.StartUrl <- "http://localhost:4321/"

    //ctrl.ShowDevTools()
    

    Application.Run form
    System.Environment.Exit 0
    /////////////////////////////////////////////////////////////


    /////////////////////////////////////////////////////////////
    // Generate a random texture with uniform values
    //let rnd = System.Random(081815)
    //let pixelFunc (x : int64) (y : int64) = C3f(rnd.NextDouble(), rnd.NextDouble (), rnd.NextDouble ())
    //let pixelFunc = Func<int64, int64, C3f>(pixelFunc)


    //let texture = PixImage<float32>(PixImageInfo(PixFormat.FloatRGB, V2i(1024, 1024)))
    //let colorMatrix = texture.GetMatrix<C3f>();

    //colorMatrix.SetByCoord(pixelFunc) |> ignore

    //texture.SaveAsImage("texture",PixFileFormat.Exr)
    /////////////////////////////////////////////////////////////

    0
