namespace Render

open System

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Rendering
open Aardvark.Application
open Aardvark.SceneGraph
open Aardvark.UI
open Aardvark.Service

module CameraController =
    open Aardvark.Base.Incremental.Operators    
    
    type Message = CameraControllerAction

    let initial =
        {
            view = CameraView.lookAt (V3d(-1.0, 0.0, 0.0)) (V3d(1.0, 0.0, -1.0)) V3d.OOI
            dragStart = V2i.Zero
            look = false
            zoom = false
            pan = false

            moving = false

            forward = false; 
            backward = false; 
            left = false; 
            right = false
            moveVec = V3i.Zero
            orbitCenter = None
            lastTime = None

            sensitivity = 1.0
            zoomFactor = 0.01
            panFactor  = 0.01
            rotationFactor = 0.01       
            
            stash = None
        }

    let sw = System.Diagnostics.Stopwatch()
    do sw.Start()

    let withTime (model : CameraControllerState) =
        { model with lastTime = Some sw.Elapsed.TotalSeconds }

    let exp x =
        Math.Pow(Math.E, x)

    let mutable private inputCount = 0
    
    let update (model : CameraControllerState) (message : Message) =
        let nextModel = match message with
                        | Blur ->
                            { model with 
                                lastTime = None
                                moveVec = V3i.Zero
                                dragStart = V2i.Zero
                                look = false; zoom = false; pan = false                    
                                forward = false; backward = false; left = false; right = false
                            }

                        | StepTime ->
                            let now = sw.Elapsed.TotalSeconds
                            let cam = model.view

                            let cam = 
                                match model.lastTime with
                                    | Some last ->
                                        let dt = now - last

                                        let dir = 
                                            cam.Forward * float model.moveVec.Z +
                                            cam.Right * float model.moveVec.X +
                                            cam.Sky * float model.moveVec.Y

                                        if model.moveVec = V3i.Zero then
                                            printfn "useless time %A" now

                                        cam.WithLocation(model.view.Location + dir * (exp model.sensitivity) * dt)

                                    | None -> 
                                        cam


                            { model with lastTime = Some now; view = cam; }

                        | KeyDown Keys.W ->
                            if not model.forward then
                                inputCount <- inputCount + 1
                                withTime { model with forward = true; moveVec = model.moveVec + V3i.OOI; }
                            else
                                model

                        | KeyUp Keys.W ->
                            if model.forward then
                                inputCount <- inputCount - 1
                                withTime { model with forward = false; moveVec = model.moveVec - V3i.OOI; }
                            else
                                model

                        | KeyDown Keys.S ->
                            if not model.backward then
                                inputCount <- inputCount + 1
                                withTime { model with backward = true; moveVec = model.moveVec - V3i.OOI; }
                            else
                                model

                        | KeyUp Keys.S ->
                            if model.backward then
                                inputCount <- inputCount - 1
                                withTime { model with backward = false; moveVec = model.moveVec + V3i.OOI; }
                            else
                                model



                        | KeyDown Keys.A ->
                            if not model.left then
                                inputCount <- inputCount + 1
                                withTime { model with left = true; moveVec = model.moveVec - V3i.IOO; }
                            else
                                model

                        | KeyUp Keys.A ->
                            if model.left then
                                inputCount <- inputCount - 1
                                withTime { model with left = false; moveVec = model.moveVec + V3i.IOO; }
                            else
                                model


                        | KeyDown Keys.D ->
                            if not model.right then
                                inputCount <- inputCount + 1
                                withTime { model with right = true; moveVec = model.moveVec + V3i.IOO; }
                            else
                                model

                        | KeyUp Keys.D ->
                            if model.right then
                                inputCount <- inputCount - 1
                                withTime { model with right = false; moveVec = model.moveVec - V3i.IOO; }
                            else
                                model

                        | KeyDown _ | KeyUp _ ->
                            model


                        | CameraControllerAction.Down (button,pos) ->
                            let model = { model with dragStart = pos }
                            inputCount <- inputCount + 1
                            match button with
                                | MouseButtons.Left -> { model with look = true; }
                                | MouseButtons.Middle -> { model with pan = true; }
                                | MouseButtons.Right -> { model with zoom = true; }
                                | _ -> model

                        | CameraControllerAction.Up button ->
                            inputCount <- inputCount - 1
                            match button with
                                | MouseButtons.Left -> { model with look = false; }
                                | MouseButtons.Middle -> { model with pan = false; }
                                | MouseButtons.Right -> { model with zoom = false; }
                                | _ -> model

                        | CameraControllerAction.Move pos  ->
                            let cam = model.view
                            let delta = pos - model.dragStart

                            let cam =
                                if model.look then
                                    let trafo =
                                        M44d.Rotation(cam.Right, float delta.Y * -model.rotationFactor) *
                                        M44d.Rotation(cam.Sky,   float delta.X * -model.rotationFactor)

                                    let newForward = trafo.TransformDir cam.Forward |> Vec.normalize
                                    cam.WithForward newForward
                                else
                                    cam

                            let cam =
                                if model.zoom then
                                    let step = -model.zoomFactor * (cam.Forward * float delta.Y) * (exp model.sensitivity)
                                    cam.WithLocation(cam.Location + step)
                                else
                                    cam

                            let cam =
                                if model.pan then
                                    let step = model.panFactor * (cam.Down * float delta.Y + cam.Right * float delta.X) * (exp model.sensitivity)
                                    cam.WithLocation(cam.Location + step)
                                else
                                    cam

                            { model with view = cam; dragStart = pos; moving = cam <> model.view }
                   
        { nextModel with moving = nextModel.view <> model.view }

    let controlledControlWithClientValues (state : MCameraControllerState) (f : Message -> 'msg) (frustum : IMod<Frustum>) (att : AttributeMap<'msg>) (sg : Aardvark.Service.ClientValues -> ISg<'msg>) =        
        let attributes =
            AttributeMap.ofListCond [
                always (onBlur (fun _ -> f Blur))
                always (onMouseDown (fun b p -> f (CameraControllerAction.Down(b,p))))
                onlyWhen (state.look %|| state.pan %|| state.zoom) (onMouseUp (fun b p -> f (CameraControllerAction.Up b)))
                always (onKeyDown (KeyDown >> f))
                always (onKeyUp (KeyUp >> f))
                onlyWhen (state.look %|| state.pan %|| state.zoom) (onMouseMove (CameraControllerAction.Move >> f))
            ]

        let attributes = AttributeMap.union att attributes


        let cam = Mod.map2 Camera.create state.view frustum 
        Incremental.renderControlWithClientValues cam attributes sg

    let controlledControl (state : MCameraControllerState) (f : Message -> 'msg) (frustum : IMod<Frustum>) (att : AttributeMap<'msg>) (sg : ISg<'msg>) =
        controlledControlWithClientValues state f frustum att (constF sg)

    let withControls (state : MCameraControllerState) (f : Message -> 'msg) (frustum : IMod<Frustum>) (node : DomNode<'msg>) =
        let cam = Mod.map2 Camera.create state.view frustum 
        let content =
            match node.Content with
                | DomContent.Scene(scene,_) ->
                    let getState(c : Aardvark.Service.ClientInfo) =
                        let cam = cam.GetValue(c.token)
                        let cam = { cam with frustum = cam.frustum |> Frustum.withAspect (float c.size.X / float c.size.Y) }

                        {
                            viewTrafo = CameraView.viewTrafo cam.cameraView
                            projTrafo = Frustum.projTrafo cam.frustum
                        }
                    DomContent.Scene(scene,getState)
                | _ -> failwith "[Aardvark.UI] cannot added controls to none scene node"
        let attributes =
            AttributeMap.ofListCond [
                always (onBlur (fun _ -> f Blur))
                always (onMouseDown (fun b p -> f (CameraControllerAction.Down(b,p))))
                onlyWhen (state.look %|| state.pan %|| state.zoom) (onMouseUp (fun b p -> f (CameraControllerAction.Up b)))
                always (onKeyDown (KeyDown >> f))
                always (onKeyUp (KeyUp >> f))
                onlyWhen (state.look %|| state.pan %|| state.zoom) (onMouseMove (CameraControllerAction.Move >> f))
            ]

        node.WithAttributes(AttributeMap.union node.Attributes attributes)
            .WithContent(content)



    let view (state : MCameraControllerState) =
        let frustum = Frustum.perspective 60.0 0.1 100.0 1.0
        div [attribute "style" "display: flex; flex-direction: row; width: 100%; height: 100%; border: 0; padding: 0; margin: 0"] [
  
            controlledControl state id 
                (Mod.constant frustum)
                (AttributeMap.empty)
                (
                    Sg.box' C4b.Green (Box3d(-V3d.III, V3d.III))
                        |> Sg.noEvents
                        |> Sg.shader {
                            do! DefaultSurfaces.trafo
                            do! DefaultSurfaces.vertexColor
                            do! DefaultSurfaces.simpleLighting
                        }
                )
        ]


    let threads (state : CameraControllerState) =
        let pool = ThreadPool.empty
       
        let rec time() =
            proclist {
                do! Proc.Sleep 10
                yield StepTime
                yield! time()
            }

        if state.moveVec <> V3i.Zero then
            ThreadPool.add "timer" (time()) pool

        else
            pool


    let start () =
        App.start {
            unpersist = Unpersist.instance
            view = view
            threads = threads
            update = update
            initial = initial
        }
