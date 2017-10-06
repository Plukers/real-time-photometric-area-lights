module RenderApp
    
    open Render
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Base.Incremental.Operators
    open Aardvark.Service

    open Aardvark.SceneGraph.IO
    open Aardvark.SceneGraph.RuntimeSgExtensions
    open Aardvark.Base.Rendering
    open Aardvark.UI

    open Aardvark.Data.Photometry

    open Utils
    open Light
    open Aardvark.Base.RenderTask
    open Aardvark.UI.Html.SemUi
    open System.Windows.Forms
    

    let update (s : RenderState) (a : Action) =
        match a with            
            | IMPORT_GEOMETRY ->
                Log.startTimed "importing %A" s.geometryFiles
                let scenes = s.geometryFiles |> HSet.ofList |> HSet.map (Loader.Assimp.load)
                let bounds = scenes |> Seq.map (fun s -> s.bounds) |> Box3d
                let sgs = scenes |> HSet.map Sg.adapter
                Log.stop()
                { s with geometryFiles = []; scenes = sgs; bounds = bounds }
            | IMPORT_PHOTOMETRY p ->
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
            | CHANGE_RENDER_MODE mode ->
                { s with renderMode = mode }
            | GROUND_TRUTH_UPDATE ->
                let newhs = (s.haltonSequence |> Seq.toArray).[(s.haltonSequence |> Seq.length) - 1] |> HaltonSequence.next 
                let newfc = s.frameCount + 1
                { s with haltonSequence = newhs |> Seq.ofArray; frameCount = newfc; clear = false }
            | GROUND_TRUTH_CLEAR ->
                let newhs = HaltonSequence.init
                let newfc = 1
                { s with haltonSequence = newhs; frameCount = newfc; clear = true }
            | CHANGE_COMPARE_A mode ->
                { s with compareA = mode }
            | CHANGE_COMPARE_B mode ->
                { s with compareB = mode }
            | CAMERA a -> { s with cameraState = Render.CameraController.update s.cameraState a }

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
            m.scenes
            |> Sg.set
            |> Sg.trafo (m.bounds |> Mod.map normalizeTrafo)
            |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO, V3d.OOI, -V3d.OIO))
           
        let effectSg (clientValues : ClientValues) =   

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

            
            let baumFormFactorFb = 
                sceneSg
                    |> setupFbEffects [ EffectBaumFF.formFactorLighting |> toEffect ]
                    |> setupLights 
                    |> setupCamera clientValues
                    |> Sg.compile runtime signature
                    |> RenderTask.renderToColor clientValues.size
            

            let compareSg = 

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

                Sg.fullscreenQuad clientValues.size
                    |> Sg.effect [ 
                        EffectCompare.compareV |> toEffect
                        EffectCompare.compareF |> toEffect 
                    ]
                    |> Sg.texture (Sym.ofString "TexA") 
                        (
                            Mod.bind (fun rm ->
                                match rm with
                                    | RenderMode.GroundTruth -> groundTruthFb
                                    | RenderMode.BaumFormFactor -> baumFormFactorFb
                                    | _ -> groundTruthFb
                                ) m.compareA
                        )
                    |> Sg.texture (Sym.ofString "TexB") 
                        (
                            Mod.bind (fun rm ->
                                match rm with
                                    | RenderMode.GroundTruth -> groundTruthFb
                                    | RenderMode.BaumFormFactor -> baumFormFactorFb
                                    | _ -> groundTruthFb
                                ) m.compareB
                        )
                    |> Sg.texture (Sym.ofString "TexDepth") depthFb


            let fbToSg fb = 
                Sg.fullscreenQuad clientValues.size
                    |> Sg.texture DefaultSemantic.DiffuseColorTexture fb
                    |> Sg.effect [DefaultSurfaces.diffuseTexture |> toEffect]

            [
                groundTruthFb |> fbToSg |> Sg.onOff (m.renderMode |> Mod.map ( fun mode -> mode = RenderMode.GroundTruth))
                baumFormFactorFb |> fbToSg |> Sg.onOff (m.renderMode |> Mod.map ( fun mode -> mode = RenderMode.BaumFormFactor))
                compareSg |> Sg.onOff (m.renderMode |> Mod.map ( fun mode -> mode = RenderMode.Compare))
            ] |> Sg.ofList
            

        let frustum = Frustum.perspective 60.0 0.1 100.0 1.0
        Render.CameraController.controlledControlWithClientValues m.cameraState CAMERA
            (Mod.constant frustum) 
            (AttributeMap.ofList [ attribute "style" "width:100%; height: 100%"]) effectSg



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
                                        let! fc = m.frameCount                                        
                                        yield p[] [ text ("Num Samples: " + string (fc * Config.NUM_SAMPLES))]
                                    | RenderMode.Compare ->

                                        let! cA = m.compareA
                                        let! cB = m.compareB

                                        if cA = RenderMode.Compare || cB = RenderMode.Compare then
                                            yield p [ clazz "ui label red" ] [ text ("Render mode Compare cannot be compared.") ]
                                            yield div [ clazz "ui divider"] []
                         
                                        yield p [] [ dropDown m.compareA (fun mode -> CHANGE_COMPARE_A mode) ]
                                        yield p [] [ dropDown m.compareB (fun mode -> CHANGE_COMPARE_B mode) ]
                                    | _ -> ()
                                }     
                            )
                        ]
                    )
                    
                render m runtime
            ]

        viewFunc

    
    let initialState =     

        // Load geometry
        let geometryFiles = [Path.combine [__SOURCE_DIRECTORY__;"meshes";"crytek-sponza";"sponza.obj"]]
        // let files = [Path.combine [__SOURCE_DIRECTORY__;"meshes";"plane.obj"]]
        let scenes = geometryFiles |> HSet.ofList |> HSet.map (Loader.Assimp.load)
        let bounds = scenes |> Seq.map (fun s -> s.bounds) |> Box3d
        let sgs = scenes |> HSet.map Sg.adapter

        // Setup Lights
        let lc = emptyLightCollection
        let light1 = addSquareLight lc 5.0 true
        let t = Trafo3d.Translation(0.0, 0.0, -1.7) * (Trafo3d.Scale 0.3)
        // For plane let t = Trafo3d.Translation(0.0, 0.0, 0.5) * (Trafo3d.Scale 1.0)
        transformLight lc light1 t |> ignore

        let photometryPath = Path.combine [__SOURCE_DIRECTORY__;"photometry";"D31267AA_NE2.ldt"]
        let photometryData = Some(IntensityProfileSampler(LightMeasurementData.FromFile(photometryPath)))    
                
        {            
            lights = lc
            renderMode = RenderMode.GroundTruth
            frameCount = 1
            clear = true
            haltonSequence = HaltonSequence.init
            compareA = RenderMode.GroundTruth
            compareB = RenderMode.BaumFormFactor 
            geometryFiles = []
            scenes = sgs
            bounds = bounds
            photometryName = Some(System.IO.Path.GetFileName photometryPath)
            photometryData = photometryData
            cameraState = Render.CameraController.initial
        }

    let appThreads (state : RenderState) =
        
        let pool = ThreadPool.empty

        //match state.renderMode with 
        //| GroundTruth ->
        if state.cameraState.moving = false then
            let rec haltonUpdate() =
                proclist {
                    do! Proc.Sleep 200
                    yield GROUND_TRUTH_UPDATE
                    yield! haltonUpdate()
                }
            ThreadPool.add "haltonUpdate" (haltonUpdate()) pool
        else
            let mutable cleared = false;
            let rec clear() =
                proclist {
                    do! Proc.Sleep 200
                    if cleared = false then
                        yield GROUND_TRUTH_CLEAR
                        cleared <- true
                    yield! clear()
                }
            ThreadPool.add "clear" (clear()) pool
        //| BaumFormFactor ->             
        //    pool
            
        

    let app (runtime : Aardvark.Rendering.GL.Runtime) (form : System.Windows.Forms.Form) =
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
