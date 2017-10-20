namespace Render

module WindowCreator =
    open Aardvark.Base
    open Aardvark.Base.Incremental

    open Aardvark.Application.WinForms
    open Aardvark.Data.Photometry

    open Light
    open Aardvark.SceneGraph

    open Rendering
    open Rendering.GroundTruth
    open Rendering.BaumFormFactor
    open Rendering.Compare

    open Utils
    open Aardvark.Application

    let private openWindow (app : OpenGlApplication) (title : string) (size : IMod<V2i>) (task : IRenderTask) (update: OpenTK.FrameEventArgs -> unit) =
        
        let win = app.CreateGameWindow()
        win.Title <- title
        
        let size = size |> Mod.force
        win.Height <- size.Y
        win.Width <- size.X

        win.RenderTask <- task
        win.UpdateFrame.Add(update)
        win.Run()
        
        DefaultCameraController.control win.Mouse win.Keyboard win.Time 

    let create (app : OpenGlApplication) (srd : RenderData) (mode : IMod<RenderMode>) = 


        (*

        match mode with 
        | RenderMode.GroundTruth -> 

            let gtData = 
                {
                    haltonSequence = ResetMod.Create(HaltonSequence.init :> seq<V2d>)
                    clear = ResetMod.Create(false) 
                    frameCount = ResetMod.Create(1) 
                }

            let renderTask = groundTruthRenderTask srd gtData

            let view = openWindow app "Ground Truth" srd.viewportSize renderTask (groundTruthRenderUpdate srd gtData) 
            
            ()

        | RenderMode.BaumFormFactor -> 

            let renderTask = baumFormFactorRenderTask srd

            openWindow app "Baum Form Factor" srd.viewportSize renderTask (fun args -> ()) 

            ()

        | RenderMode.Compare -> 
            
            let gtData = 
                {
                    haltonSequence = ResetMod.Create(HaltonSequence.init :> seq<V2d>) 
                    clear = ResetMod.Create(false)
                    frameCount = ResetMod.Create(1) 
                }

            let compData = 
                {
                    compare = ResetMod.Create(RenderMode.BaumFormFactor)
                }

            let renderTask = compareRenderTask srd gtData compData

            let gtUpdate = groundTruthRenderUpdate srd gtData

            let update (args : OpenTK.FrameEventArgs) =
                gtUpdate args

            openWindow app "Ground Truth" srd.viewportSize renderTask update

            ()
        | _ -> ()

        *)
        

