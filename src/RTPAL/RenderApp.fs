module RenderApp
    
    open Render
    open Aardvark.Base
    open Aardvark.Base.Incremental
    
    open Aardvark.SceneGraph.IO
    open Aardvark.Base.Rendering
    open Aardvark.UI
    open Aardvark.UI.Primitives

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
            | HALTON_UPDATE ->
                // CHECK if this mutation of the state is valid
                transact (fun _ ->
                    let last = s.haltonSequence.Value.[s.haltonSequence.Value.Length - 1]
                    s.haltonSequence.Value <- HaltonSequence.next last
                    )                
                s
            | CAMERA a -> { s with cameraState = CameraController.update s.cameraState a }

    let render (m : MRenderState) =

        let normalizeTrafo (b : Box3d) =
            let size = b.Size
            let scale = 4.0 / size.NormMax

            let center = b.Center

            Trafo3d.Translation(-center) *
            Trafo3d.Scale(scale)

        let sceneSg = 
            m.scenes
            |> Sg.set
            |> Sg.trafo (m.bounds |> Mod.map normalizeTrafo)
            |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO, V3d.OOI, -V3d.OIO))
            |> Sg.effect [
                toEffect DefaultSurfaces.trafo
                toEffect DefaultSurfaces.diffuseTexture
                toEffect GTEffect.groundTruthLighting
            ] 

        let sg = 
            sceneSg
            |> Light.Sg.addLightCollectionSg (m.lights |> Mod.force)
            |> Light.Sg.setLightCollectionUniforms (m.lights |> Mod.force)
            |> Utils.HaltonSequence.addSequenceToSg (m.haltonSequence |> Mod.force)           
            |> Sg.noEvents

        let frustum = Frustum.perspective 60.0 0.1 100.0 1.0
        CameraController.controlledControl m.cameraState CAMERA
            (Mod.constant frustum) 
            (AttributeMap.ofList [ attribute "style" "width:100%; height: 100%"]) sg
    
    let view (m : MRenderState) =

        let semui =
            [ 
                { kind = Stylesheet; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
                { kind = Script; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
            ]  

        require semui (
                div[][
                    render m
                ]
            )

    
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
            haltonSequence = HaltonSequence.init |> Mod.init
            cameraState = 
                {
                    view = CameraView.lookAt (6.0 * V3d.III) V3d.Zero V3d.OOI
                    dragStart = V2i.Zero
                    look = false; zoom = false; pan = false
                    forward = false; backward = false; left = false; right = false
                    moveVec = V3i.Zero
                    lastTime = None
                    orbitCenter = None
                    stash = None
                }
        }

    let appThreads (state : RenderState) =
        let pool = ThreadPool.empty
       
        let rec haltonUpdate() =
            proclist {
                do! Proc.Sleep 16
                yield HALTON_UPDATE
                yield! haltonUpdate()
            }

        ThreadPool.add "haltonUpdate" (haltonUpdate()) pool

    let app =
        {
            unpersist = Unpersist.instance
            threads = fun model -> 
                CameraController.threads model.cameraState 
                |> ThreadPool.map CAMERA
                |> ThreadPool.union (appThreads model)
            initial = initialState
            update = update
            view = view
        }
