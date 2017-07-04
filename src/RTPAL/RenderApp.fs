﻿module RenderApp
    
    open Render
    open Aardvark.Base
    open Aardvark.Base.Incremental

    open Aardvark.SceneGraph
    open Aardvark.SceneGraph.IO
    open Aardvark.Base.Rendering
    open Aardvark.UI
    open Aardvark.UI.Primitives


    let update (s : RenderState) (a : Action) =
        match a with            
            | IMPORT ->
                Log.startTimed "importing %A" s.files
                let scenes = s.files |> HSet.ofList |> HSet.map (Loader.Assimp.load)
                let bounds = scenes |> Seq.map (fun s -> s.bounds) |> Box3d
                let sgs = scenes |> HSet.map Sg.adapter
                Log.stop()
                { s with files = []; scenes = sgs; bounds = bounds }
            | CAMERA a -> { s with cameraState = CameraController.update s.cameraState a }

    let render (m : MRenderState) =

        let normalizeTrafo (b : Box3d) =
            let size = b.Size
            let scale = 4.0 / size.NormMax

            let center = b.Center

            Trafo3d.Translation(-center) *
            Trafo3d.Scale(scale)

        let sg = 
            m.scenes
            |> Sg.set
            |> Sg.trafo (m.bounds |> Mod.map normalizeTrafo)
            |> Sg.transform (Trafo3d.FromOrthoNormalBasis(V3d.IOO, V3d.OOI, -V3d.OIO))
            |> Sg.effect [
                toEffect DefaultSurfaces.trafo
                toEffect DefaultSurfaces.diffuseTexture
                toEffect DefaultSurfaces.simpleLighting
            ]       

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
        let scenes = files |> HSet.ofList |> HSet.map (Loader.Assimp.load)
        let bounds = scenes |> Seq.map (fun s -> s.bounds) |> Box3d
        let sgs = scenes |> HSet.map Sg.adapter

        {
            files = []
            scenes = sgs
            bounds = bounds
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


    let app =
        {
            unpersist = Unpersist.instance
            threads = fun model -> CameraController.threads model.cameraState |> ThreadPool.map CAMERA
            initial = initialState
            update = update
            view = view
        }
