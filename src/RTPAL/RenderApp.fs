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
                transact (fun _ ->
                    let last = s.haltonSequence.Value.[s.haltonSequence.Value.Length - 1]
                    s.haltonSequence.Value <- HaltonSequence.next last
                    )                
                { s with frameCount = s.frameCount + 1 }
            | GROUND_TRUTH_CLEAR ->
                transact (fun _ -> s.haltonSequence.Value <- HaltonSequence.init)
                { s with frameCount = 0 }
            | CAMERA a -> { s with cameraState = Render.CameraController.update s.cameraState a }

    let render (m : MRenderState) runtime =

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

        let setupLights sg =
            sg
                |> Light.Sg.addLightCollectionSg (m.lights |> Mod.force)
                |> Light.Sg.setLightCollectionUniforms (m.lights |> Mod.force)    
                
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

                let iterationRender =
                    sceneSg
                        |> setupEffects [ toEffect GTEffect.groundTruthLighting ]
                        |> setupLights 
                        |> Utils.HaltonSequence.addSequenceToSg (m.haltonSequence |> Mod.force)
                        |> Sg.noEvents
                        |> setupCamera clientValues
                        |> Sg.compile runtime clientValues.signature
                        |> RenderTask.renderToColor clientValues.size
                
                let fullscreenQuad =
                    Sg.draw IndexedGeometryMode.TriangleStrip
                        |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant [|V3f(-1.0,-1.0,0.0); V3f(1.0,-1.0,0.0); V3f(-1.0,1.0,0.0);V3f(1.0,1.0,0.0) |])
                        |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates (Mod.constant [|V2f.OO; V2f.IO; V2f.OI; V2f.II|])
                        |> Sg.depthTest ~~DepthTestMode.None
                        |> Sg.uniform "ViewportSize" clientValues.size

                let accumulate =
                    fullscreenQuad 
                        |> Sg.texture DefaultSemantic.DiffuseColorTexture iterationRender
                        //|> Sg.effect [ GTEffect.passThrough |> toEffect ]
                        |> Sg.effect [DefaultSurfaces.diffuseTexture |> toEffect]

                accumulate

                (*
                sceneSg
                |> Sg.effect (setupEffects [ toEffect GTEffect.groundTruthLighting ])
                |> Utils.HaltonSequence.addSequenceToSg (m.haltonSequence |> Mod.force)
                *)
        
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

        GTEffect.debugOutput

        let lc = emptyLightCollection
        let light1 = addSquareLight lc 1.0 false
        let t = Trafo3d.Translation(0.0, 0.0, -1.7) * (Trafo3d.Scale 0.3)
        // For plane let t = Trafo3d.Translation(0.0, 0.0, 0.5) * (Trafo3d.Scale 1.0)
        transformLight lc light1 t |> ignore
                
        {
            files = []
            scenes = sgs
            bounds = bounds
            lights = lc
            renderMode = GroundTruth
            frameCount = 0;
            haltonSequence = HaltonSequence.init |> Mod.init
            cameraState = Render.CameraController.initial
        }

    let appThreads (state : RenderState) =
        
        let pool = ThreadPool.empty

        match state.renderMode with 
        | GroundTruth ->
            if state.cameraState.moving = false then
                let rec haltonUpdate() =
                    proclist {
                        do! Proc.Sleep 16
                        yield GROUND_TRUTH_UPDATE
                        yield! haltonUpdate()
                    }
                ThreadPool.add "haltonUpdate" (haltonUpdate()) pool
            else
                let mutable cleared = false;
                let rec clear() =
                    proclist {
                        do! Proc.Sleep 16
                        if cleared = false then
                            yield GROUND_TRUTH_CLEAR
                            cleared <- true
                        yield! clear()
                    }
                ThreadPool.add "clear" (clear()) pool
            
        

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
