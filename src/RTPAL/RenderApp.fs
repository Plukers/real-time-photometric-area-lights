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

    open Utils
    open Light
    open Aardvark.Base.RenderTask
    
    let update (s : RenderState) (a : Action) =
        match a with            
            | IMPORT ->
                Log.startTimed "importing %A" s.files
                let scenes = s.files |> HSet.ofList |> HSet.map (Loader.Assimp.load)
                let bounds = scenes |> Seq.map (fun s -> s.bounds) |> Box3d
                let sgs = scenes |> HSet.map Sg.adapter
                Log.stop()
                { s with files = []; scenes = sgs; bounds = bounds }
            | GROUND_TRUTH_UPDATE ->
                let newhs = (s.haltonSequence |> Seq.toArray).[(s.haltonSequence |> Seq.length) - 1] |> HaltonSequence.next 
                let newfc = s.frameCount + 1
                { s with haltonSequence = newhs |> Seq.ofArray; frameCount = newfc; clear = false }
            | GROUND_TRUTH_CLEAR ->
                let newhs = HaltonSequence.init
                let newfc = 0
                { s with haltonSequence = newhs; frameCount = newfc; clear = true }
            | CAMERA a -> { s with cameraState = Render.CameraController.update s.cameraState a }

    let render (m : MRenderState) (runtime : Aardvark.Rendering.GL.Runtime) =
        let normalizeTrafo (b : Box3d) =
            let size = b.Size
            let scale = 4.0 / size.NormMax

            let center = b.Center

            Trafo3d.Translation(-center) *
            Trafo3d.Scale(scale)

        let setupEffects effects sg =   
            sg
                |> Sg.effect ( List.append [
                                    toEffect DefaultSurfaces.trafo
                                    toEffect DefaultSurfaces.diffuseTexture
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

        let sceneSg = 
            m.scenes
            |> Sg.set
            |> Sg.trafo (m.bounds |> Mod.map normalizeTrafo)
            |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO, V3d.OOI, -V3d.OIO))
           


        let effectSg (clientValues : ClientValues) = 
            match m.renderMode |> Mod.force with
            | GroundTruth ->

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

                
                let signature =
                    runtime.CreateFramebufferSignature [
                        DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba32f; samples = 1 }
                        DefaultSemantic.Depth, { format = RenderbufferFormat.Depth24Stencil8; samples = 1 }
                    ]
                
                let iterationRender =
                    sceneSg
                        |> setupEffects [ toEffect GTEffect.groundTruthLighting ]
                        |> setupLights
                        |> setupCamera clientValues
                        |> Sg.uniform "HaltonSamples" (m.haltonSequence |> Mod.map Seq.toArray)
                        |> Sg.uniform "FrameCount" ( m.frameCount)
                        |> Sg.compile runtime signature
                        |> RenderTask.renderToColor clientValues.size
              
                let accumulate =
                    Sg.fullscreenQuad clientValues.size 
                        |> Sg.blendMode mode
                        |> setupEffects []
                        |> Sg.texture DefaultSemantic.DiffuseColorTexture iterationRender
                        |> Sg.compile runtime signature
                        |> renderToColorWithoutClear clientValues.size
                
                let final =
                    Sg.fullscreenQuad clientValues.size
                        |> Sg.texture DefaultSemantic.DiffuseColorTexture accumulate
                        |> Sg.effect [DefaultSurfaces.diffuseTexture |> toEffect]
                        
                final

            | BaumFormFactor ->
                sceneSg
                    |> setupEffects [ toEffect BaumFFEffect.formFactorLighting ]
                    |> setupLights 
                    |> setupCamera clientValues
        
        let frustum = Frustum.perspective 60.0 0.1 100.0 1.0
        Render.CameraController.controlledControlWithClientValues m.cameraState CAMERA
            (Mod.constant frustum) 
            (AttributeMap.ofList [ attribute "style" "width:100%; height: 100%"]) effectSg
    
    let view (runtime : Aardvark.Rendering.GL.Runtime) =
        let viewFunc (m : MRenderState) =
            let semui =
                [ 
                    { kind = Stylesheet; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
                    { kind = Script; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
                ]  

            require semui (
                    div[][
                        render m runtime
                    ]
                )
        viewFunc

    
    let initialState =     
        let files = [Path.combine [__SOURCE_DIRECTORY__;"meshes";"crytek-sponza";"sponza.obj"]]
        // let files = [Path.combine [__SOURCE_DIRECTORY__;"meshes";"plane.obj"]]
        let scenes = files |> HSet.ofList |> HSet.map (Loader.Assimp.load)
        let bounds = scenes |> Seq.map (fun s -> s.bounds) |> Box3d
        let sgs = scenes |> HSet.map Sg.adapter

        let lc = emptyLightCollection
        let light1 = addSquareLight lc 5.0 false
        let t = Trafo3d.Translation(0.0, 0.0, -1.7) * (Trafo3d.Scale 0.3)
        // For plane let t = Trafo3d.Translation(0.0, 0.0, 0.5) * (Trafo3d.Scale 1.0)
        transformLight lc light1 t |> ignore
                
        {            
            lights = lc
            renderMode = BaumFormFactor
            frameCount = 0
            clear = true
            haltonSequence = HaltonSequence.init
            files = []
            scenes = sgs
            bounds = bounds
            cameraState = Render.CameraController.initial
        }

    let appThreads (state : RenderState) =
        
        let pool = ThreadPool.empty

        match state.renderMode with 
        | GroundTruth ->
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
        | BaumFormFactor -> pool
            
        

    let app (runtime : Aardvark.Rendering.GL.Runtime) =
        {
            unpersist = Unpersist.instance
            threads = fun model -> 
                Render.CameraController.threads model.cameraState 
                |> ThreadPool.map CAMERA
                |> ThreadPool.union (appThreads model)
            initial = initialState
            update = update
            view = view runtime
        }
