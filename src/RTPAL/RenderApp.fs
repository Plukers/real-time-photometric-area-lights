module RenderApp
    
    open Render
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Base.Incremental.Operators
    open Aardvark.Base.RenderTask

    open Aardvark.Application
    open Aardvark.Application.WinForms

    open Aardvark.Service

    open Aardvark.UI.Html.SemUi
    open System.Windows.Forms
    open Aardvark.UI

    open Aardvark.SceneGraph.IO
    open Aardvark.SceneGraph.RuntimeSgExtensions
    open Aardvark.Base.Rendering

    open Aardvark.Data.Photometry
    

    open Utils
    open Light
    open Rendering
    open Rendering.GroundTruth
    open Rendering.BaumFormFactor
    open Rendering.Compare
    open Rendering.Effects
    
    let createGameWindow (app : OpenGlApplication) (viewportSize : V2i) (m : MRenderState) =
        
        let win = app.CreateGameWindow()
        win.Title <- "Render"
        
        win.Height <- viewportSize.Y
        win.Width <- viewportSize.X

        let view = CameraView.lookAt (V3d(-1.0, 0.0, 0.0)) (V3d(1.0, 0.0, -1.0)) V3d.OOI
        let renderData = initialRenderData app (DefaultCameraController.control win.Mouse win.Keyboard win.Time view) viewportSize m 
        
        let gtData = initGTData            
        let compData = initCompData

        let (renderTask, diffFb, frameCount) = Effects.CreateAndLinkRenderTask renderData gtData compData

        win.RenderTask <- renderTask
        
        win.UpdateFrame.Add(groundTruthRenderUpdate renderData gtData)

        (win, diffFb, frameCount)
        

    let update (s : RenderState) (a : Action) =

        match a with            
            | IMPORT_PHOTOMETRY p ->
                if System.String.IsNullOrEmpty p = false then 
                    let lmd = 
                        try
                            Some(LightMeasurementData.FromFile(p))
                        with
                        | Failure msg -> 
                            printfn "%s" msg
                            None
                
                    match lmd with
                    | Some v ->  
                        { s with photometryData = Some(IntensityProfileSampler(v)); photometryName = Some(System.IO.Path.GetFileName p) }
                    | None -> s
                else 
                    s
            | CHANGE_RENDER_MODE mode -> { s with renderMode = mode }                
            | CHANGE_COMPARE mode -> { s with compare = mode }
            | COMPUTED_ERROR error -> { s with error = error }
            | OPENED_WINDOW -> s

    let openFileDialog (form : System.Windows.Forms.Form) =
        let mutable final = ""

        let action : System.Action = 
            new System.Action( fun () -> 
                let d = new System.Windows.Forms.OpenFileDialog()
                if d.ShowDialog() = DialogResult.OK then
                    final <- d.FileName
            ) 

        form.Invoke action |> ignore
        IMPORT_PHOTOMETRY final
        
    let view (app : OpenGlApplication) (form : System.Windows.Forms.Form) =
        
        let viewFunc (m : MRenderState) =
            
            let (win, diffFb, frameCount) = createGameWindow app (V2i(1024, 768)) m


            // TODO open game window in new thread

            
            let openWindowAction : System.Action = 
                new System.Action( fun () -> 
                    win.Run()
                ) 

            // view
            let semui =
                [ 
                    { kind = Stylesheet; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
                    { kind = Script; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
                ]   

            div [attribute "style" "display: flex; flex-direction: row; width: 100%; height: 100%; border: 0; padding: 0; margin: 0"] [

                require semui (
                    div [ attribute "class" "ui visible sidebar inverted vertical menu"; attribute "style" "min-width: 166px" ] [  

                            div [ clazz "item" ] [
                                    button [ clazz "ui button" ; onClick (fun _ -> 
                                            form.Invoke openWindowAction |> ignore
                                            OPENED_WINDOW
                                        )] [text "Open Window"]
                                ]
                        
                            Incremental.div (AttributeMap.ofList [clazz "Item"]) (
                                alist {       
                                    yield button [ clazz "ui button" ; onClick (fun _ -> openFileDialog form)] [text "Load Photometric Data"]

                                    let! photometryName = m.photometryName

                                    match photometryName with
                                    | Some name -> yield p[] [ text ("Loaded " + name)]
                                    | None -> ()
                                }
                            )
                        
                            div [ clazz "item"] [ 
                                b [] [text "Render Mode"]
                                div [ clazz "menu" ] [
                                    div [ clazz "item" ] [
                                        dropDown m.renderMode (fun mode -> CHANGE_RENDER_MODE mode)
                                    ]
                                ]
                            ] 
                                                        
                            Incremental.div (AttributeMap.ofList [clazz "Item"]) (
                                alist {                                
                                    let! mode = m.renderMode
                                    
                                    match mode with
                                    | RenderMode.GroundTruth ->
                                        let! fc = frameCount                                        
                                        yield p [] [ text ("Num Samples: " + string (fc * Config.NUM_SAMPLES))]
                                    | RenderMode.Compare ->
                                        
                                        let! c = m.compare

                                        if c = RenderMode.Compare then
                                            yield p [ clazz "ui label red" ] [ text ("Render mode Compare cannot be compared.") ]
                                            yield div [ clazz "ui divider"] []
                         
                                        yield p [] [ text ("Compare Ground Truth with")]
                                        yield p [] [ dropDown m.compare (fun mode -> CHANGE_COMPARE mode) ]

                                        let computeError = (fun _ -> 
            
                                            let diff = diffFb.GetValue()
                            
                                            let diffPixData = app.Runtime.Download(diff |> unbox<_>)
                                            let downlaoded = diffPixData.ToPixImage<float32>()
                                            let data = downlaoded.GetMatrix<C4f>()
                                            let ec = data.Elements |> Seq.fold ( fun cs c-> (C4f.White - c) + cs ) C4f.Black
                                        
                                            COMPUTED_ERROR (double (sqrt (ec.R * ec.R + ec.G * ec.G + ec.B  * ec.B)))
                                                    
                                        )
                                        
                                        yield div [ clazz "ui divider"] []
                                        yield button [clazz "ui button" ; onClick (fun _ -> computeError())] [text "Compute Error"]

                                        let! error = m.error
                                        yield p [] [ text ("Error " + error.ToString())]
                                    | _ -> ()
                                }     
                            )
                        ]
                    )
                    
                // render m runtime
            ]

        viewFunc

    
    let initialState =     

        // Load geometry
        let geometryFile = Path.combine [__SOURCE_DIRECTORY__;"meshes";"crytek-sponza";"sponza.obj"]

        // Setup Lights
        let lc = emptyLightCollection
        let light1 = addSquareLight lc 1.0 true
        
        match light1 with
        | Some lightId ->             
            let t = Trafo3d.Translation(8.0, 0.0, -5.0)
            transformLight lc lightId t |> ignore
        | None -> ()

        let photometryPath = Path.combine [__SOURCE_DIRECTORY__;"photometry";"D31267AA_NE2.ldt"]
        let photometryData = Some(IntensityProfileSampler(LightMeasurementData.FromFile(photometryPath)))    
                
        // initial state
        {            
            lights = lc
            renderMode = RenderMode.BaumFormFactor
            compare = RenderMode.BaumFormFactor 
            error = 0.0
            geometryFiles = []
            scenePath = geometryFile
            photometryName = Some(System.IO.Path.GetFileName photometryPath)
            photometryData = photometryData
        }
       

    let app (app : OpenGlApplication) (form : System.Windows.Forms.Form) =
        {
            unpersist = Unpersist.instance
            threads = fun model -> ThreadPool.empty
            initial = initialState
            update = update
            view = view app form
        }
