module RenderApp
    
    open Render
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Base.Incremental.Operators
    open Aardvark.Service
    open Aardvark.Base.RenderTask
    open Aardvark.UI.Html.SemUi
    open System.Windows.Forms

    open Aardvark.SceneGraph.IO
    open Aardvark.SceneGraph.RuntimeSgExtensions
    open Aardvark.Base.Rendering
    open Aardvark.UI

    open Aardvark.Data.Photometry
    
    open Aardvark.Application.WinForms

    open Utils
    open Light
    open Rendering

    module ``VERY EVIL DESTROY ME``=
        let mutable computeError = Unchecked.defaultof<_>

    open ``VERY EVIL DESTROY ME``

    module GlobalApplicationData = 
        
        let mutable renderingApp : Option<OpenGlApplication> = None
        let mutable appToRenderInterop : Option<RenderData> = None 

    open GlobalApplicationData
    
    let update (s : RenderState) (a : Action) =

        let clear (s : RenderState) = 
            let newhs = HaltonSequence.init
            let newfc = 1
            { s with haltonSequence = newhs; frameCount = newfc; clear = true }

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
                        let clearedS = clear s
                        { clearedS with photometryData = Some(IntensityProfileSampler(v)); photometryName = Some(System.IO.Path.GetFileName p) }
                    | None -> s
                else 
                    s
            | CHANGE_RENDER_MODE mode -> { s with renderMode = mode }
            | GROUND_TRUTH_UPDATE ->
                let newhs = (s.haltonSequence |> Seq.toArray).[(s.haltonSequence |> Seq.length) - 1] |> HaltonSequence.next 
                let newfc = s.frameCount + 1
                { s with haltonSequence = newhs |> Seq.ofArray; frameCount = newfc; clear = false }
            | GROUND_TRUTH_CLEAR -> clear s
            | CHANGE_COMPARE mode -> { s with compare = mode }
            | COMPUTED_ERROR error -> { s with error = error }
            | CAMERA a -> { s with cameraState = Render.CameraController.update s.cameraState a }
            | OPEN_GROUND_TRUTH_WINDOW ->
                match renderingApp with
                | Some app -> 
                    match appToRenderInterop with
                    | Some sharedData ->
                        WindowCreator.create app sharedData RenderMode.GroundTruth
                        s
                    | None -> s
                | None -> s

    let render (m : MRenderState) (runtime : Aardvark.Rendering.GL.Runtime) =
        let normalizeTrafo (b : Box3d) =
            let size = b.Size
            let scale = 4.0 / size.NormMax

            let center = b.Center

            Trafo3d.Translation(-center) *
            Trafo3d.Scale(scale)

        let setupFbEffects effects sg =   
            sg
                |> Sg.effect ( List.append [
                                    DefaultSurfaces.trafo |> toEffect
                                    DefaultSurfaces.diffuseTexture |> toEffect
                                ] effects)

        let setupLights (sg : ISg<'a>) =
            sg
                |> Light.Sg.addLightCollectionSg (m.lights |> Mod.force)
                |> Light.Sg.setLightCollectionUniforms (m.lights |> Mod.force)  
                |> Sg.noEvents
                
        let setupCamera (clientValues : ClientValues) sg =
            sg
                |> Sg.viewTrafo clientValues.viewTrafo
                |> Sg.projTrafo clientValues.projTrafo
                |> Sg.uniform "ViewportSize" clientValues.size

        let setupPhotometricData (sg : ISg<'a>) = 
            Mod.map( fun (pd : Option<IntensityProfileSampler>) ->
                match pd with 
                | Some data -> 
                    sg
                        |> Sg.uniform "ProfileAddressing" (data.AddressingParameters |> Mod.init)
                        |> Sg.uniform "TextureOffsetScale" (data.ImageOffsetScale |> Mod.init)
                        |> Sg.texture Render.PhotometricLight.IntensityTexture (((PixTexture2d(PixImageMipMap(data.Image), false)) :> ITexture) |> Mod.constant)
                | None -> sg
            ) m.photometryData
            |> Sg.dynamic


        let sceneSg = 
            Mod.map( fun path -> path |> Utils.Assimp.loadFromFile true |> Sg.normalize) m.scenePath
            |> Sg.dynamic
            |> Sg.scale 18.0 // because sponza is so small
           

        let effectSg (clientValues : ClientValues) =   

            let fbToSg fb = 
                Sg.fullscreenQuad clientValues.size
                    |> Sg.texture DefaultSemantic.DiffuseColorTexture fb
                    |> Sg.effect [DefaultSurfaces.diffuseTexture |> toEffect]

            let sgs = Map.empty

            let signature =
                runtime.CreateFramebufferSignature [
                    DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba32f; samples = 1 }
                    DefaultSemantic.Depth, { format = RenderbufferFormat.Depth24Stencil8; samples = 1 }
                ]

            let depthSignature = 
                runtime.CreateFramebufferSignature [
                    DefaultSemantic.Depth, { format = RenderbufferFormat.DepthComponent32; samples = 1 }
                ]
            
            let groundTruthFb = 
                let renderToColorWithoutClear (size : IMod<V2i>) (task : IRenderTask) =
                    let sem = (Set.singleton DefaultSemantic.Colors)
                    let runtime = task.Runtime.Value
                    let signature = task.FramebufferSignature.Value
            
                    let fbo = runtime.CreateFramebuffer(signature, sem, size)

                    let res = 
                        new SequentialRenderTask([|task|]) |> renderTo fbo
        
                    sem |> Seq.map (fun k -> k, getResult k res) |> Map.ofSeq |> Map.find DefaultSemantic.Colors

                let mode = 
                    m.clear |> Mod.map ( fun c -> 
                        if c then
                            BlendMode(
                                true, 
                                SourceFactor = BlendFactor.One, 
                                DestinationFactor = BlendFactor.Zero,
                                Operation = BlendOperation.Add,
                                SourceAlphaFactor = BlendFactor.One,
                                DestinationAlphaFactor = BlendFactor.Zero,
                                AlphaOperation = BlendOperation.Add
                            )
                        else    
                            BlendMode(
                                true, 
                                SourceFactor = BlendFactor.SourceAlpha, 
                                DestinationFactor = BlendFactor.InvSourceAlpha,
                                Operation = BlendOperation.Add,
                                SourceAlphaFactor = BlendFactor.SourceAlpha,
                                DestinationAlphaFactor = BlendFactor.DestinationAlpha,
                                AlphaOperation = BlendOperation.Add
                            )
                        )
                
                let iterationRender =
                    sceneSg
                        |> setupFbEffects [ EffectGT.groundTruthLighting |> toEffect ]
                        |> setupLights
                        |> setupPhotometricData
                        |> setupCamera clientValues
                        |> Sg.uniform "HaltonSamples" (m.haltonSequence |> Mod.map Seq.toArray)
                        |> Sg.uniform "FrameCount" ( m.frameCount)
                        |> Sg.compile runtime signature
                        |> RenderTask.renderToColor clientValues.size
              
                Sg.fullscreenQuad clientValues.size 
                    |> Sg.blendMode mode
                    |> setupFbEffects []
                    |> Sg.texture DefaultSemantic.DiffuseColorTexture iterationRender
                    |> Sg.compile runtime signature
                    |> renderToColorWithoutClear clientValues.size

            let sgs = sgs |> Map.add RenderMode.GroundTruth (groundTruthFb |> fbToSg) 

            let baumFormFactorFb = 
                sceneSg
                    |> setupFbEffects [ EffectBaumFF.formFactorLighting |> toEffect ]
                    |> setupLights 
                    |> setupCamera clientValues
                    |> Sg.compile runtime signature
                    |> RenderTask.renderToColor clientValues.size

            let sgs = sgs |> Map.add RenderMode.BaumFormFactor (baumFormFactorFb |> fbToSg) 

            let compareSg = 
                
                let renderToFbo (fbo : IOutputMod<IFramebuffer>) (task : IRenderTask) =
                    let sem = (Set.singleton DefaultSemantic.Colors)
                    let res = 
                        new SequentialRenderTask([|task|]) |> renderTo fbo
        
                    sem |> Seq.map (fun k -> k, getResult k res) |> Map.ofSeq |> Map.find DefaultSemantic.Colors

                
                let diffSignature =
                    runtime.CreateFramebufferSignature [
                        DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba32f; samples = 1 }
                    ]
                                        
                let diffFb = Sg.fullscreenQuad clientValues.size
                                |> Sg.effect [ 
                                    EffectCompare.compare |> toEffect 
                                ]
                                |> Sg.texture (Sym.ofString "TexA") groundTruthFb
                                |> Sg.texture (Sym.ofString "TexB") 
                                    (
                                        Mod.bind (fun rm ->
                                            match rm with
                                                | RenderMode.GroundTruth -> groundTruthFb
                                                | RenderMode.BaumFormFactor -> baumFormFactorFb
                                                | _ -> groundTruthFb
                                            ) m.compare
                                    )
                                |> Sg.compile runtime diffSignature
                                |> RenderTask.renderToColor clientValues.size
                                

                computeError <- (fun _ -> 
                    let diff = diffFb.GetValue()
                            
                    let diffPixData = runtime.Download(diff |> unbox<_>)
                    let downlaoded = diffPixData.ToPixImage<float32>()
                    let data = downlaoded.GetMatrix<C4f>()
                    let ec = data.Elements |> Seq.fold ( fun cs c-> (C4f.White - c) + cs ) C4f.Black
                                        
                    COMPUTED_ERROR (float (sqrt (ec.R * ec.R + ec.G * ec.G + ec.B  * ec.B)))
                )

                let depthFb =
                    sceneSg
                        |> Sg.effect [ 
                            DefaultSurfaces.trafo |> toEffect
                            DefaultSurfaces.vertexColor |> toEffect 
                            ]
                        |> setupLights                       
                        |> setupCamera clientValues
                        |> Sg.compile runtime depthSignature
                        |> RenderTask.renderToDepth clientValues.size

                let sg = 
                    Sg.fullscreenQuad clientValues.size
                        |> Sg.effect [ 
                            EffectOutline.outlineV |> toEffect 
                            EffectOutline.outlineF |> toEffect 
                        ]
                        |> Sg.texture (Sym.ofString "Tex") diffFb
                        |> Sg.texture (Sym.ofString "TexDepth") depthFb

                sg

            let sgs = sgs |> Map.add RenderMode.Compare compareSg
            (*        
            let onOff (b : IMod<bool>) (s : ISg<'a>) =
                b |> Mod.map (function true -> s | false -> Sg.empty)
            *)
            let sg (mode : IMod<RenderMode>) (sceneGraphs : Map<RenderMode, ISg<'a>>) =
                mode |> Mod.map(fun m -> sceneGraphs.[m]) |> Sg.dynamic
            (*
            let sg = 
                [
                    groundTruthFb |> fbToSg |> Sg.onOff (m.renderMode |> Mod.map ( fun mode -> mode = RenderMode.GroundTruth))
                    baumFormFactorFb |> fbToSg |> Sg.onOff (m.renderMode |> Mod.map ( fun mode -> mode = RenderMode.BaumFormFactor))
                    compareSg |> Sg.onOff (m.renderMode |> Mod.map ( fun mode -> mode = RenderMode.Compare))
                ] |> Sg.ofList
            *)
            sg m.renderMode sgs
                

        let frustum = Frustum.perspective 60.0 0.1 100.0 1.0
        Render.CameraController.controlledControlWithClientValues m.cameraState CAMERA
            (Mod.constant frustum) 
            (AttributeMap.ofList [ attribute "style" "width:100%; height: 100%"]) (effectSg)

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
            

    let view (runtime : Aardvark.Rendering.GL.Runtime) (form : System.Windows.Forms.Form) =
        
        let viewFunc (m : MRenderState) =

            // interop
            match appToRenderInterop with 
            | Some interop ->                
                appToRenderInterop <- Some({ interop with photometricData = m.photometryData })
                ()
            | None -> ()

            // view
            let semui =
                [ 
                    { kind = Stylesheet; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
                    { kind = Script; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
                ]   

            div [attribute "style" "display: flex; flex-direction: row; width: 100%; height: 100%; border: 0; padding: 0; margin: 0"] [

                require semui (
                    div [ attribute "class" "ui visible sidebar inverted vertical menu"; attribute "style" "min-width: 166px" ] [  
                        
                            Incremental.div (AttributeMap.ofList [clazz "Item"]) (
                                alist {       
                                    yield button [clazz "ui button" ; onClick (fun _ -> openFileDialog form)] [text "Load Photometric Data"]

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
                                        yield button [clazz "ui button" ; onClick (fun _ -> OPEN_GROUND_TRUTH_WINDOW)] [text "Open"]

                                        let! fc = m.frameCount                                        
                                        yield p [] [ text ("Num Samples: " + string (fc * Config.NUM_SAMPLES))]
                                    | RenderMode.Compare ->
                                        
                                        let! c = m.compare

                                        if c = RenderMode.Compare then
                                            yield p [ clazz "ui label red" ] [ text ("Render mode Compare cannot be compared.") ]
                                            yield div [ clazz "ui divider"] []
                         
                                        yield p [] [ text ("Compare Ground Truth with")]
                                        yield p [] [ dropDown m.compare (fun mode -> CHANGE_COMPARE mode) ]
                                        
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
        let sceneSg = 
            geometryFile |> Utils.Assimp.loadFromFile true |> Sg.normalize
            |> Sg.scale 18.0 // because sponza is so small

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
            renderMode = RenderMode.Compare
            frameCount = 1
            clear = true
            haltonSequence = HaltonSequence.init
            compare = RenderMode.BaumFormFactor 
            error = 0.0
            geometryFiles = []
            scenePath = geometryFile
            sceneSg = sceneSg
            photometryName = Some(System.IO.Path.GetFileName photometryPath)
            photometryData = photometryData
            cameraState = Render.CameraController.initial
        }

    let initialInterop (runtime : Aardvark.Rendering.GL.Runtime) (initialRenderState : RenderState) =

        let view = CameraView.lookAt (V3d(-1.0, 0.0, 0.0)) (V3d(1.0, 0.0, -1.0)) V3d.OOI |> Mod.init
        let frustum = Frustum.perspective 60.0 0.1 100.0 1.0 |> Mod.init
        
        let init = {
                        runtime = runtime
                        sceneSg = initialRenderState.sceneSg
                        view = view 
                        frustum = frustum 
                        viewportSize = V2i(1024, 768) |> Mod.init
                        lights = initialRenderState.lights
                        photometricData = None |> Mod.init
                    }
         
        Some(init)

    let appThreads (state : RenderState) =
        
        let pool = ThreadPool.empty
        pool
        (*
        if state.cameraState.moving = false then
            let rec haltonUpdate() =
                proclist {
                    do! Proc.Sleep 200
                    yield GROUND_TRUTH_UPDATE
                    yield! haltonUpdate()
                }
            ThreadPool.add "haltonUpdate" (haltonUpdate()) pool
        else
            let rec clear() =
                proclist {
                    do! Proc.Sleep 200         
                    yield GROUND_TRUTH_CLEAR
                }
            ThreadPool.add "clear" (clear()) pool  
            *)
        

    let app (app : OpenGlApplication) (runtime : Aardvark.Rendering.GL.Runtime) (form : System.Windows.Forms.Form) =

        let initialState = initialState

        // interop
        renderingApp <- Some(app)

        appToRenderInterop <- initialInterop runtime initialState

        // app
        {
            unpersist = Unpersist.instance
            threads = fun model -> 
                Render.CameraController.threads model.cameraState 
                |> ThreadPool.map CAMERA
                |> ThreadPool.union (appThreads model)
            initial = initialState
            update = update
            view = view runtime form
        }
