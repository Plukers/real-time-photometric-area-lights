module RenderApp
    
    open Render
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Base.Incremental.Operators
    open Aardvark.Base.RenderTask

    open Aardvark.Application
    open Aardvark.Application.WinForms

    open Aardvark.Service

    
    open Aardvark.UI
    open Aardvark.UI.Html.SemUi
    open Aardvark.UI.Combinators

    open System.Windows.Forms

    
    open Aardvark.SceneGraph
    open Aardvark.SceneGraph.IO
    open Aardvark.SceneGraph.RuntimeSgExtensions
    open Aardvark.Base.Rendering

    open Aardvark.Data.Photometry
    
    open Aardvark.Rendering.GL
    open Aardvark.UI.Chromium
    
    

    open Utils
    open Light
    open RenderInterop
    
    open EffectGT.Rendering
    open EffectApMRP.Rendering
    open EffectApStructuredSampling.Rendering
    open EffectCompare.Rendering

    open Rendering.Render


    let setupOfflineRendering (app : OpenGlApplication) (m : MRenderState) (sceneSg : ISg) = 

        let viewportSize = V2i(2048, 2048)

        let view = CameraView.lookAt (V3d(0.0, 0.0, 5.0)) (V3d(0.0, 0.0, -1.0)) V3d.IOO |> Mod.init

        let projTrafo = 
            { 
                left = -20.0
                right = 20.0
                bottom = -20.0
                top = 20.0
                near = 0.1
                far = 20.1
            } 
            |> Frustum.orthoTrafo |> Mod.init

        let renderMode = RenderMode.GroundTruth |> Mod.init
        let updateRenderMode mode = 
            transact (fun _ ->
                mode |> Mod.change renderMode 
            )
        let compareMode = RenderMode.GroundTruth |> Mod.init
        let updateCompareMode mode = 
            transact (fun _ ->
                mode |> Mod.change compareMode 
            )

        let toneMap = false |> Mod.init
        let changeToneMap tm = 
            transact (fun _ ->
                tm |> Mod.change toneMap 
            )

        let toneMapScale = 0.2 |> Mod.init
        let updateToneMapScale tms = 
            transact (fun _ ->
                tms |> Mod.change toneMapScale
            )
        
        let mutable photometryName = ""
        let photometryData = None |> Mod.init
        let updatePhotometryData name = 
            let photometryPath = Path.combine [__SOURCE_DIRECTORY__;"..";"..";"photometry";name]
            let lightData = LightMeasurementData.FromFile(photometryPath)
            let data = Some(IntensityProfileSampler(lightData))
        
            transact (fun _ ->
                data |> Mod.change photometryData 
            )

            photometryName <- name
           
        updatePhotometryData "ARC3_60712332_(STD).ldt"
        
        let renderData = 
                            {
                                runtime = app.Runtime
                                sceneSg = sceneSg
                                view = view
                                projTrafo = projTrafo 
                                viewportSize = viewportSize |> Mod.init
                                lights = m.lights |> Mod.force // mod force necessary ? 
                                photometricData = photometryData
                                mode = renderMode
                                compare = compareMode
                                toneMap = toneMap
                                toneMapScale = toneMapScale
                            }
            
            

        let gtData = initGTData' (true |> Mod.init)
        let mrpData = initMRPData m
        let ssData = initSSData m

        let (scRenderTask, _) = Rendering.Render.CreateAndLinkRenderTask renderData gtData mrpData ssData

        let update = groundTruthRenderUpdate renderData gtData

        let scSignature =
            app.Runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba32f; samples = 1 }
            ]

        let scColor = app.Runtime.CreateTexture(viewportSize, TextureFormat.Rgba32f, 1, 1, 1)

        // Create a framebuffer matching signature and capturing the render to texture targets
        let fbo = 
            app.Runtime.CreateFramebuffer(
                scSignature, 
                Map.ofList [
                    DefaultSemantic.Colors, ({ texture = scColor; slice = 0; level = 0 } :> IFramebufferOutput)
                ]
            )

            

        let activeTrafoLightId = 0        
        let numOfRotationSteps = 4
        let angle = (System.Math.PI / 2.0) / float(numOfRotationSteps)
        let mutable numOfSamples = 30000



        let imageFormat = PixFileFormat.Exr

        let createFileName step = 
            sprintf "%s_%i"  ((renderData.mode |> Mod.force).ToString()) step



        let doRotationIteration action =

            let mutable rotation = Trafo3d.Identity
            let rotationStep = Trafo3d.Rotation(V3d(0.0, angle, 0.0));
        
            for r in 0 .. numOfRotationSteps do 
                action r
                
                transformLight renderData.lights activeTrafoLightId (rotationStep)
                rotation <- rotation * rotationStep
            
            transformLight renderData.lights activeTrafoLightId (rotation.Inverse)
        
        let renderEffects path = 

            fun step -> 
                // Ground Truth
                updateRenderMode RenderMode.GroundTruth
                update true

                
                for i in 1 .. (numOfSamples / Config.NUM_SAMPLES) do
                    scRenderTask.Run(RenderToken.Empty, fbo)
                    update false
                    
                let filename = (createFileName step)

                app.Runtime.Download(scColor).SaveAsImage(Path.combine [path;String.concat "_" [(createFileName step); numOfSamples.ToString()]], imageFormat);
                (*
                // Structured Irradiance Sampling
                updateRenderMode RenderMode.StructuredIrrSampling
                scRenderTask.Run(RenderToken.Empty, fbo)
                app.Runtime.Download(scColor).SaveAsImage(Path.combine [path;createFileName step], imageFormat);
                *)



            
            
        
        let createImageTask = 
            async {

                let resultPath =  Path.combine [__SOURCE_DIRECTORY__;"..";"..";"results"]
                
                let renderReferenceData = 
                    fun step ->
                        updateRenderMode RenderMode.FormFactor
                        scRenderTask.Run(RenderToken.Empty, fbo)
                        app.Runtime.Download(scColor).SaveAsImage(Path.combine [resultPath;createFileName step], imageFormat);
                        
                        updateRenderMode RenderMode.SolidAngle
                        scRenderTask.Run(RenderToken.Empty, fbo)
                        app.Runtime.Download(scColor).SaveAsImage(Path.combine [resultPath;createFileName step], imageFormat);
                 
                //doRotationIteration renderReferenceData


                let photometryFiles = 
                    System.IO.Directory.GetFiles (Path.combine [__SOURCE_DIRECTORY__;"..";"..";"photometry"])
                    |> Array.map System.IO.Path.GetFileName 

                let mutable oneIter = true
                for f in photometryFiles do

                    if oneIter then 
                    
                        let dataPath =  Path.combine [__SOURCE_DIRECTORY__;"..";"..";"results";(System.IO.Path.GetFileNameWithoutExtension f)]
                        System.IO.Directory.CreateDirectory dataPath |> ignore
                    
                        updatePhotometryData f
                        
                        doRotationIteration (renderEffects dataPath)
                        
                        

                        oneIter <- false
                    



                ()
                }
            
        createImageTask


    let setupRendering (app : OpenGlApplication) (viewportSize : V2i) (m : MRenderState) =
        
        let win = app.CreateGameWindow()
        win.Title <- "Render"
        
        win.Height <- viewportSize.Y
        win.Width <- viewportSize.X

        
        let sceneSg = 
                m.scenePath 
                |> Mod.map( fun path -> path |> Utils.Assimp.loadFromFile true |> Sg.normalize) 
                |> Sg.dynamic
                |> Sg.scale 20.0

        
        let offlineRenderTask = setupOfflineRendering app m sceneSg


        
        // let view = CameraView.lookAt (V3d(1.0, 0.0, 0.0)) (V3d(-1.0, 0.0, -1.0)) V3d.OOI
        
        let view = 
            CameraView.lookAt (V3d(2.0, 0.0, 3.0)) (V3d(-4.0, 0.0, -1.0)) V3d.OOI
            |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
        let projTrafo =
            Frustum.perspective 60.0 0.1 100.0 ((float)viewportSize.X / (float)viewportSize.Y)
            |> Frustum.projTrafo |> Mod.init
        
        
        let sceneSg = sceneSg |> Light.Sg.addLightCollectionSg (m.lights |> Mod.force)

        let renderData = initialRenderData app view projTrafo viewportSize m sceneSg

        
        let gtData = initGTData m 
        let mrpData = initMRPData m
        let ssData = initSSData m

        let (renderTask, renderFeedback) = Rendering.Render.CreateAndLinkRenderTask renderData gtData mrpData ssData
        

        win.RenderTask <- renderTask

        let rtGroundTruthRenderUpdate =
            let updateRT = groundTruthRenderUpdate renderData gtData

            let update (args : OpenTK.FrameEventArgs) =
                updateRT false

            update
        
        win.UpdateFrame.Add(rtGroundTruthRenderUpdate)
        win.UpdateFrame.Add(fpsUpdate renderFeedback)

        (win, renderFeedback, offlineRenderTask)
        

    let update (s : RenderState) (a : Action) =

        match a with     
            | IMPORT_SCENE p -> {s with scenePath = p}
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
            | COMPUTED_ERROR (error, brightError, darkError) -> { s with error = error; brightError = brightError; darkError = darkError }
            | OPENED_WINDOW -> s
            | UPDATE_GROUND_TRUTH update -> { s with updateGroundTruth = update }
            | CHANGE_LIGHT_TRANSFORM_MODE mode -> { s with lightTransformMode = mode }
            | TRANSLATE_LIGHT (lightID, dir) ->             
                transformLight s.lights lightID (Trafo3d.Translation(dir))
                s
            | ROTATE_LIGHT (lightID, euler) ->
                transformLight s.lights lightID (Trafo3d.Rotation(euler))
                s
            | SET_MRP_CLOSEST_WEIGHT w ->
                try
                    let w = float w 
                    let w = clamp 0.0 1.0 w

                    let leftSum = 1.0 - w
                    let otherSum = s.mrpWeights.Y + s.mrpWeights.Z
                
                    let normal     = leftSum * (s.mrpWeights.Y / otherSum)
                    let barycenter = leftSum * (s.mrpWeights.Z / otherSum)

                    let sum = w + normal + barycenter

                    { s with mrpWeights = V3d(w / sum, normal / sum, barycenter / sum) }
                with
                | :? System.FormatException -> s
            | SET_MRP_NORMAL_WEIGHT w ->
                try
                    let w = float w 
                    let w = clamp 0.0 1.0 w

                    let leftSum = 1.0 - w
                    let otherSum = s.mrpWeights.X + s.mrpWeights.Z
                
                    let closest    = leftSum * (s.mrpWeights.X / otherSum)
                    let barycenter = leftSum * (s.mrpWeights.Z / otherSum)

                    let sum = closest + w + barycenter

                    { s with mrpWeights = V3d(closest / sum, w / sum, barycenter / sum) }
                with
                | :? System.FormatException -> s
            | SET_MRP_BARYCENTER_WEIGHT w ->
                try
                    let w = float w 
                    let w = clamp 0.0 1.0 w

                    let leftSum = 1.0 - w
                    let otherSum = s.mrpWeights.X + s.mrpWeights.Y
                
                    let closest = leftSum * (s.mrpWeights.X / otherSum)
                    let normal  = leftSum * (s.mrpWeights.Y / otherSum)

                    let sum = closest + normal + w

                    { s with mrpWeights = V3d(closest / sum, normal / sum, w / sum) }
                with
                | :? System.FormatException -> s
            | TOGGLE_SAMPLE_CORNERS -> { s with sampleCorners = (not s.sampleCorners) }
            | TOGGLE_SAMPLE_BARYCENTER -> { s with sampleBarycenter = (not s.sampleBarycenter) }
            | TOGGLE_SAMPLE_CLOSEST -> { s with sampleClosest = (not s.sampleClosest) }
            | TOGGLE_SAMPLE_NORM -> { s with sampleNorm = (not s.sampleNorm) }
            | TOGGLE_SAMPLE_MRP -> { s with sampleMRP = (not s.sampleMRP) }
            | TOGGLE_SAMPLE_RND -> { s with sampleRandom = (not s.sampleRandom) }
            | CHANGE_SRS_SAMPLE_NUM nss -> { s with numOfSRSamples = Numeric.update s.numOfSRSamples nss}
            | CHANGE_SRS_WEIGHT_SCALE srss -> { s with SRSWeightScale = Numeric.update s.SRSWeightScale srss }
            | CHANGE_TANGENT_APPROX_DIST tad -> { s with TangentApproxDist = Numeric.update s.TangentApproxDist tad }
            | CHANGE_SRS_WEIGHT_SCALE_IRR srss -> { s with SRSWeightScaleIrr = Numeric.update s.SRSWeightScaleIrr srss}
            | CHANGE_TANGENT_APPROX_DIST_IRR tad -> { s with TangentApproxDistIrr = Numeric.update s.TangentApproxDistIrr tad }
            | CHANGE_COMBINED_WEIGHT t -> { s with CombinedSSWeight = Numeric.update s.CombinedSSWeight t }

            | TOGGLE_TONEMAPPING -> { s with toneMap = (not s.toneMap) }
            | CHANGE_TONEMAP_SCALE tms -> { s with toneMapScale = Numeric.update s.toneMapScale tms}

            | RENDER_IMAGES createImageTask ->    
                createImageTask |> Async.Start
                s

    let openFileDialog (form : System.Windows.Forms.Form) =
        let mutable final = ""

        let action : System.Action = 
            new System.Action( fun () -> 
                let d = new System.Windows.Forms.OpenFileDialog()
                if d.ShowDialog() = DialogResult.OK then
                    final <- d.FileName
            ) 

        form.Invoke action |> ignore
        final
        
    let view (app : OpenGlApplication) (form : System.Windows.Forms.Form) =
        
        let viewFunc (m : MRenderState) =
            
            let (win, renderFeedback, offlineRenderTask) = setupRendering app (V2i(1024, 768)) m
            
            let openGameWindowAction : System.Action = 
                new System.Action( fun () -> 
                    win.Run()
                ) 

            let computeError = (fun _ -> 
            
                let comp = renderFeedback.compareTexture.GetValue()
                            
                let compPixData = app.Runtime.Download(comp |> unbox<_>)
                let downlaoded = compPixData.ToPixImage<float32>()
                let data = downlaoded.GetMatrix<C4f>()
                //let ec = data.Elements |> Seq.fold ( fun (cs : double) c -> (double c.R) + cs) 0.0

                let mutable ec : double = 0.0

                let mutable brightEcCount = 0
                let mutable brightEc : double = 0.0

                let mutable darkEcCount = 0
                let mutable darkEc : double = 0.0
                
                data.Elements |> Seq.iter ( fun c ->
                    ec <- ec + (double c.R)
                    
                    if c.G > 0.0f then
                        brightEc <- brightEc + (double c.R)
                        brightEcCount <- brightEcCount + 1
                    else
                        darkEc <- darkEc + (double c.R)
                        darkEcCount <- darkEcCount + 1

                    )

                let ec = Fun.Sqrt(ec / (double data.Count))

                let brightEc = Fun.Sqrt(brightEc / (double brightEcCount))
                let darkEc = Fun.Sqrt(darkEc / (double darkEcCount))
                 

                COMPUTED_ERROR (ec, brightEc, darkEc)
                                                    
            )

            let toggleBox (state : IMod<bool>) (toggle : 'msg) =

                let attributes = 
                    amap {
                         yield "type" => "checkbox"
                         yield onChange (fun _ -> toggle)

                         let! check = state
                         if check then
                            yield "checked" => ""
                    }
                    
                Incremental.input (AttributeMap.ofAMap attributes)

            // view
            let semui =
                [ 
                    { kind = Stylesheet; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
                    { kind = Script; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
                ]   

            let activeTrafoLightId = 0                   // TODO make changeable
            let translationStepSize = 0.1                // TODO make changeable
            let rotationStepSize = System.Math.PI / 18.0 // TODO make changelable

            body [attribute "style" "display: flex; flex-direction: row; width: 100%; height: 100%; border: 0; margin: 0; padding: 1rem;"] [

                require semui (
                        div [] [

                            div [ clazz "ui stackable equal width grid" ] [

                                div [ clazz "column" ][ 
                                        button [ clazz "ui button" ; onClick (fun () -> 
                                                form.BeginInvoke openGameWindowAction |> ignore
                                                OPENED_WINDOW
                                            )] [text "Open Window"]

                                        button [ clazz "ui button" ; onClick (fun () -> 
                                                IMPORT_PHOTOMETRY (openFileDialog form)
                                            )] [text "Load Object"] 

                                        button [ clazz "ui button" ; onClick (fun () -> 
                                                IMPORT_PHOTOMETRY (openFileDialog form)
                                            )] [text "Load Photometric Data"]        

                                        button [ clazz "ui button" ; onClick (fun () -> 
                                                RENDER_IMAGES (offlineRenderTask)
                                            )] [text "Render Light Profiles"]    
                                                
                                    ]
                            ]

                            div [ clazz "ui stackable two column vertically divided grid container" ] [
                                div [ clazz "row" ] [
                                    div [ clazz "column" ] [ 
                                        div [ clazz "ui segment" ] [
                                            Incremental.div (AttributeMap.ofList []) (
                                                alist {     
                                                    let! sceneName = m.scenePath
                                                    yield p[] [ text ("Loaded Object: " + System.IO.Path.GetFileName(sceneName)) ]

                                                    let! photometryName = m.photometryName
                                                    match photometryName with
                                                    | Some name -> yield p[] [ text ("Loaded Photometry: " + name)]
                                                    | None -> ()

                                                    let! fps = renderFeedback.fps
                                                    yield p [] [ text ("FPS: " + (sprintf "%.2f" fps))]
                                                })

                                            div [ clazz "ui divider"] []

                                            div [ clazz "ui buttons"] [
                                                button [ clazz "ui button"; onClick (fun () -> CHANGE_LIGHT_TRANSFORM_MODE Translate) ] [ text "Translate" ]
                                                div [ clazz "or" ] []
                                                button [ clazz "ui button"; onClick (fun () -> CHANGE_LIGHT_TRANSFORM_MODE Rotate) ] [ text "Rotate" ]
                                            ]

                                            

                                            Incremental.div (AttributeMap.ofList [clazz "ui icon buttons"]) (
                                                alist {
                                                    let! mode = m.lightTransformMode
                                                    
                                                    yield   button [clazz "ui button"; onClick (fun () -> 
                                                                    match mode with 
                                                                    | Translate ->
                                                                        TRANSLATE_LIGHT (activeTrafoLightId, V3d(0.0, translationStepSize, 0.0)) 
                                                                    | Rotate ->
                                                                        ROTATE_LIGHT (activeTrafoLightId, V3d(0.0, 0.0, rotationStepSize)) 
                                                                )] [
                                                                i [ clazz "arrow left icon"][]
                                                            ]
                                                    yield   button [clazz "ui button"; onClick (fun () -> 
                                                                    match mode with 
                                                                    | Translate ->
                                                                        TRANSLATE_LIGHT (activeTrafoLightId, V3d(-translationStepSize, 0.0, 0.0))
                                                                    | Rotate ->
                                                                        ROTATE_LIGHT (activeTrafoLightId, V3d(0.0, rotationStepSize, 0.0)) 
                                                                )] [
                                                                i [ clazz "arrow down icon"][]
                                                            ]
                                                    yield   button [clazz "ui button"; onClick (fun () -> 
                                                                    match mode with 
                                                                    | Translate ->
                                                                        TRANSLATE_LIGHT (activeTrafoLightId, V3d(translationStepSize, 0.0, 0.0))
                                                                    | Rotate ->
                                                                        ROTATE_LIGHT (activeTrafoLightId, V3d(0.0, -rotationStepSize, 0.0))
                                                                )] [
                                                                i [ clazz "arrow up icon"][]
                                                            ]
                                                    yield   button [clazz "ui button"; onClick (fun () -> 
                                                                    match mode with 
                                                                    | Translate ->
                                                                        TRANSLATE_LIGHT (activeTrafoLightId, V3d(0.0, -translationStepSize, 0.0))
                                                                    | Rotate ->
                                                                        ROTATE_LIGHT (activeTrafoLightId, V3d(0.0, 0.0, -rotationStepSize)) 
                                                                )] [
                                                                i [ clazz "arrow right icon"][]
                                                            ]
                                                }
                                            )
                                            text " "
                                            Incremental.div (AttributeMap.ofList [clazz "ui icon buttons"]) (
                                                alist {      
                                                    let! mode = m.lightTransformMode

                                                    match mode with
                                                    | Translate ->
                                                        yield   button [clazz "ui button"; onClick (fun () -> 
                                                                    TRANSLATE_LIGHT (activeTrafoLightId, V3d(0.0, 0.0, -translationStepSize)) 
                                                                )] [
                                                                    i [ clazz "chevron down icon"][]
                                                                ]
                                                        yield   button [clazz "ui button"; onClick (fun () -> 
                                                                    TRANSLATE_LIGHT (activeTrafoLightId, V3d(0.0, 0.0, translationStepSize)) 
                                                                )] [
                                                                    i [ clazz "chevron up icon"][]
                                                                ]
                                                    | Rotate -> ()
                                                })  

                                            div [ clazz "ui divider"] []

                                            toggleBox m.toneMap TOGGLE_TONEMAPPING    
                                            text "Tonemapping"
                                            br[]                                                
                                            div [clazz "ui input"] [ Numeric.view' [InputBox] m.toneMapScale |> UI.map CHANGE_TONEMAP_SCALE ]
                                            br[]
                                              
                                        ]
                                    ]  
                                    div [ clazz "column" ] [ 

                                        div [ clazz "ui segment" ] [
                                            Incremental.div (AttributeMap.ofList []) (
                                                alist {
                                                    yield dropDown m.renderMode (fun mode -> CHANGE_RENDER_MODE mode)
                                                }
                                            )

                                            div [ clazz "ui divider"] []

                                            Incremental.div (AttributeMap.ofList []) (
                                                alist {
                                                    let! mode = m.renderMode

                                                    if mode = RenderMode.GroundTruth || mode = RenderMode.Compare then

                                                        let! updateGT = m.updateGroundTruth

                                                        yield p[][
                                                            if updateGT then
                                                                yield button [ clazz "ui button" ; onClick (fun () -> 
                                                                                UPDATE_GROUND_TRUTH false
                                                                            )] [text "Pause Update"]
                                                            else 
                                                                yield button [ clazz "ui button" ; onClick (fun () -> 
                                                                                UPDATE_GROUND_TRUTH true
                                                                            )] [text "Continue Update"]
                                                        ]
                                                        
                                                        let! fc = renderFeedback.frameCount   
                                                        yield p [] [ text ("Num Samples: " + string (fc * Config.NUM_SAMPLES))]

                                                        if updateGT then
                                                            let! fps = renderFeedback.fps
                                                            yield p [] [ text ("Samples/Second: " + (sprintf "%.2f" (fps * (float)Config.NUM_SAMPLES)))]

                                                                                                               
                                                }
                                            )

                                            Incremental.div (AttributeMap.ofList []) (
                                                alist {
                                                    let! mode = m.renderMode

                                                    if mode = RenderMode.Compare then
                                                        yield div [ clazz "ui divider"] []
                                        
                                                        let! c = m.compare

                                                        if c = RenderMode.Compare then
                                                            yield p [ clazz "ui label red" ] [ text ("Render mode Compare cannot be compared.") ]
                                                            yield div [ clazz "ui divider"] []
                         
                                                        yield p [] [ text ("Compare Ground Truth with")]
                                                        yield p [] [ dropDown m.compare (fun mode -> CHANGE_COMPARE mode) ]
                                                                                                
                                                }
                                            )

                                            Incremental.div (AttributeMap.ofList []) (
                                                alist {
                                                    let! mode = m.renderMode
                                                    let! c = m.compare

                                                    let ssActive = mode = RenderMode.StructuredSampling || mode = RenderMode.StructuredIrrSampling || mode = RenderMode.CombinedStructuredSampling
                                                    let ssCompActive = c = RenderMode.StructuredSampling || c = RenderMode.StructuredIrrSampling || c = RenderMode.CombinedStructuredSampling

                                                    if ssActive || (mode = RenderMode.Compare && ssCompActive) then    
                                                                                                                
                                                        yield p [] [     
                                                            yield toggleBox m.sampleCorners TOGGLE_SAMPLE_CORNERS 
                                                            yield text "Sample Corners"                                                                                               
                                                            yield br[]
                                                            
                                                            yield toggleBox m.sampleBarycenter TOGGLE_SAMPLE_BARYCENTER  
                                                            yield text "Sample Barycenter"                                                      
                                                            yield br[]

                                                            yield toggleBox m.sampleClosest TOGGLE_SAMPLE_CLOSEST  
                                                            yield text "Sample Closest"                     
                                                            yield br[]
                                                            
                                                            yield toggleBox m.sampleNorm TOGGLE_SAMPLE_NORM        
                                                            yield text "Sample Norm"                                                    
                                                            yield br[]        
                                                            
                                                            yield toggleBox m.sampleMRP TOGGLE_SAMPLE_MRP       
                                                            yield text "Sample MRP"                                                    
                                                            yield br[]   

                                                            yield toggleBox m.sampleRandom TOGGLE_SAMPLE_RND      
                                                            yield text "Sample Random"                                                    
                                                            yield br[]  
                                                            
                                                        ]

                                                        let! sampleRandom = m.sampleRandom

                                                        if sampleRandom then
                                                            yield p [] [
                                                                yield text "Samples"
                                                                yield div [clazz "ui input"] [ Numeric.view' [InputBox] m.numOfSRSamples |> UI.map CHANGE_SRS_SAMPLE_NUM ]
                                                                yield br[] 
                                                            ]
                                                        
                                                        if mode = RenderMode.StructuredSampling || mode = RenderMode.CombinedStructuredSampling || c = RenderMode.StructuredSampling || c = RenderMode.CombinedStructuredSampling then
                                                            yield p [] [   
                                                                yield p [style "font-weight: bold;"] [ 
                                                                    text ("Structured Sampling")
                                                                ]
                                                                
                                                                yield text "Scale factor"
                                                                yield div [clazz "ui input"] [ Numeric.view' [InputBox] m.SRSWeightScale |> UI.map CHANGE_SRS_WEIGHT_SCALE ]
                                                                yield br[] 

                                                                yield text "Tangent Approx Dist"
                                                                yield div [clazz "ui input"] [ Numeric.view' [InputBox] m.TangentApproxDist |> UI.map CHANGE_TANGENT_APPROX_DIST ]
                                                                yield br[] 
                                                            ]

                                                        if mode = RenderMode.StructuredIrrSampling || mode = RenderMode.CombinedStructuredSampling || c = RenderMode.StructuredIrrSampling || c = RenderMode.CombinedStructuredSampling then
                                                            yield p [] [   
                                                                yield p [style "font-weight: bold;"] [ 
                                                                    text ("Structured Irradiance Sampling")
                                                                ]
                                                                
                                                                yield text "Scale factor"
                                                                yield div [clazz "ui input"] [ Numeric.view' [InputBox] m.SRSWeightScaleIrr |> UI.map CHANGE_SRS_WEIGHT_SCALE_IRR ]
                                                                yield br[] 

                                                                yield text "Tangent Approx Dist"
                                                                yield div [clazz "ui input"] [ Numeric.view' [InputBox] m.TangentApproxDistIrr |> UI.map CHANGE_TANGENT_APPROX_DIST_IRR ]
                                                                yield br[] 
                                                            ]

                                                        if mode = RenderMode.CombinedStructuredSampling || c = RenderMode.CombinedStructuredSampling then
                                                            yield p [] [   
                                                                yield p [style "font-weight: bold;"] [ 
                                                                    text ("Combined Structured Sampling")
                                                                ]
                                                                
                                                                yield text "Lerp Value: 1.0 = Structured Sampling  - 0.0 = Irr Sampling"
                                                                yield div [clazz "ui input"] [ Numeric.view' [InputBox] m.CombinedSSWeight |> UI.map CHANGE_COMBINED_WEIGHT ]
                                                                yield br[]  
                                                            ]
                                                        
                                                }
                                            )

                                            Incremental.div (AttributeMap.ofList []) (
                                                alist {
                                                    let! mode = m.renderMode

                                                    if mode = RenderMode.Compare then
                                        
                                                        yield div [ clazz "ui divider"] []

                                                        let! error = m.error
                                                        let! brightError = m.brightError
                                                        let! darkError = m.darkError
                                                        
                                                        yield p [style "font-weight: bold;"] [ 
                                                            text ("Error: " + (sprintf "%.5f" error))
                                                            ]

                                                        yield p [] [ 
                                                            text ("Bright: " + (sprintf "%.5f" brightError)) 
                                                            br []
                                                            text ("Dark: " + (sprintf "%.5f" darkError)) 
                                                            ]
                                                        yield p [] [ button [clazz "ui button" ; onClick (fun () -> computeError())] [text "Compute Error"] ]
                                                }
                                            )

                                        ]
                                    ]                                  
                                ]
                            ]
                        ]
                        
                    )
            ]

        viewFunc

    
    let initialState =     

        // Load geometry
        // let geometryFile = Path.combine [__SOURCE_DIRECTORY__;"meshes";"crytek-sponza";"sponza.obj"]
        let geometryFile = Path.combine [__SOURCE_DIRECTORY__;"meshes";"plane.obj"]

        // Setup Lights
        let lc = emptyLightCollection
        //let light1 = addTriangleLight lc
        let light1 = addSquareLight lc
        
        match light1 with
        | Some lightId ->             
            // let t = Trafo3d.Translation(-8.0, 0.0, -5.0)        
            let t = Trafo3d.Translation(-15.0, 0.0, 0.6)
            transformLight lc lightId t |> ignore
        | None -> ()
        
        let photometryPath = Path.combine [__SOURCE_DIRECTORY__;"..";"..";"photometry";"ARC3_60712332_(STD).ldt"]
        let lightData = LightMeasurementData.FromFile(photometryPath)
        
        let photometryData = Some(IntensityProfileSampler(lightData))    
                
        // initial state
        {            
            lights = lc
            renderMode = RenderMode.GroundTruth
            updateGroundTruth = true
            compare = RenderMode.BaumFFApprox 
            error = 0.0
            brightError = 0.0
            darkError = 0.0
            scenePath = geometryFile
            photometryName = Some(System.IO.Path.GetFileName photometryPath)
            photometryData = photometryData
            lightTransformMode = Translate
            mrpWeights    = V3d(1.0/3.0, 1.0/3.0, 1.0/3.0)
            sampleCorners    = false
            sampleBarycenter = false
            sampleClosest    = false
            sampleNorm       = false
            sampleMRP        = false
            sampleRandom     = true
            numOfSRSamples   = {
                                value   = 8.0
                                min     = 0.0
                                max     = (float) Config.SS_LIGHT_SAMPLES_ALL_LIGHT
                                step    = 1.0
                                format  = "{0:0}"
                                }
            SRSWeightScale = {
                                value   = 0.0
                                min     = 0.0
                                max     = 1000.0
                                step    = 0.1
                                format  = "{0:F3}"
                             }
            TangentApproxDist = {
                                    value   = 1.0
                                    min     = 0.0
                                    max     = 1000.0
                                    step    = 0.5
                                    format  = "{0:F3}"
                                 }
            SRSWeightScaleIrr = {
                                value   = 0.0
                                min     = 0.0
                                max     = 1000.0
                                step    = 0.1
                                format  = "{0:F3}"
                             }
            TangentApproxDistIrr = {
                                    value   = 1.0
                                    min     = 0.0
                                    max     = 1000.0
                                    step    = 0.5
                                    format  = "{0:F3}"
                                 }    
            CombinedSSWeight = {
                                    value   = 0.5
                                    min     = 0.0
                                    max     = 1.0
                                    step    = 0.1
                                    format  = "{0:F3}"
                                 }              
            toneMap = true
            toneMapScale     = {
                                    value   = 0.2
                                    min     = 1e-3
                                    max     = 10.0
                                    step    = 0.001
                                    format  = "{0:F3}"
                                }
        }
       

    let app (app : OpenGlApplication) (form : System.Windows.Forms.Form) =
        {
            unpersist = Unpersist.instance
            threads = fun model -> ThreadPool.empty
            initial = initialState
            update = update
            view = view app form
        }
