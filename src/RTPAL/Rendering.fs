namespace Render

open EffectUtils

module Rendering = 

    open Aardvark.Base
    open Aardvark.Base.Rendering
    open Aardvark.Base.RenderTask
    open Aardvark.Base.Incremental
    open Aardvark.Rendering.GL

    open Aardvark.Data.Photometry
    
    open Aardvark.SceneGraph

    open Light
    open Utils
    
    type RenderData = {
        runtime : Aardvark.Rendering.GL.Runtime

        scenePath : IMod<string>

        view         : IMod<CameraView>
        frustum      : IMod<Frustum>
        viewportSize : IMod<V2i>

        mode            : IMod<RenderMode>
        compare         : IMod<RenderMode>
        lights          : LightCollection
        photometricData : IMod<Option<IntensityProfileSampler>>
        }

    type RenderFeedback = {
        // global
        fps : ModRef<float>

        // ground truth
        frameCount : ModRef<int>

        // compare
        compareTexture : IOutputMod<ITexture>
        }

    let private normalizeTrafo (b : Box3d) =
            let size = b.Size
            let scale = 4.0 / size.NormMax

            let center = b.Center

            Trafo3d.Translation(-center) *
            Trafo3d.Scale(scale)

    let private setupFbEffects effects sg =   
        sg
            |> Sg.effect ( List.append [
                                DefaultSurfaces.trafo |> toEffect
                                DefaultSurfaces.diffuseTexture |> toEffect
                            ] effects)

    let private setupLights (lights : LightCollection) (sg : ISg) =
        sg
            |> Light.Sg.addLightCollectionSg lights
            |> Light.Sg.setLightCollectionUniforms lights
                
    let private setupCamera (view : IMod<CameraView>) (frustum : IMod<Frustum>) (viewportSize : IMod<V2i>) sg =
        sg
            |> Sg.viewTrafo (view |> Mod.map CameraView.viewTrafo)
            |> Sg.projTrafo (frustum |> Mod.map Frustum.projTrafo)
            |> Sg.uniform "ViewportSize" viewportSize

    let private setupPhotometricData (photometricData : IMod<Option<IntensityProfileSampler>>) (sg : ISg) = 
        photometricData |> Mod.map( fun (pd : Option<IntensityProfileSampler>) ->
            printfn "LOADED NEW DATA"
            match pd with 
            | Some data ->
                sg
                    |> Sg.uniform "ProfileAddressing" (data.AddressingParameters |> Mod.init)
                    |> Sg.uniform "TextureOffsetScale" (data.ImageOffsetScale |> Mod.init)
                    |> Sg.texture Render.PhotometricLight.IntensityTexture (((PixTexture2d(PixImageMipMap(data.Image), false)) :> ITexture) |> Mod.constant)
            | None -> sg
        )
        |> Sg.dynamic

    let private setupMipMaps (runtime : IRuntime) (fb : IOutputMod<ITexture>) =

        let mutable mipmapped : Option<IBackendTexture> = None
        
        let result = 
            { new AbstractOutputMod<ITexture>() with
                member x.Create() =
                    fb.Acquire()
                            
                member x.Destroy() =
                    fb.Release()
                    match mipmapped with
                        | Some t -> 
                            runtime.DeleteTexture t
                            mipmapped <- None
                        | None -> ()

                member x.Compute(t, rt) =
                    let t = fb.GetValue(t, rt)
                    let t = unbox<IBackendTexture> t

                    let levels = 1.0 + floor (Fun.Log2 (max (float t.Size.X) (float t.Size.Y))) |> int
                    let target = 
                        match mipmapped with
                            | Some mm ->
                                if mm.Size.XY = t.Size.XY then 
                                    mm
                                else
                                    runtime.DeleteTexture mm
                                    let n = runtime.CreateTexture(t.Size.XY, t.Format, levels, 1)
                                    mipmapped <- Some n
                                    n
                            | _ ->
                                let n = runtime.CreateTexture(t.Size.XY, t.Format, levels, 1)
                                mipmapped <- Some n
                                n
                    
                    let runtime = unbox<Aardvark.Rendering.GL.Runtime> runtime
                    runtime.Context.Copy(unbox<Texture> t, 0, 0, V2i.Zero, unbox<Texture> target, 0, 0, V2i.Zero, t.Size.XY)
                    
                    runtime.GenerateMipMaps(target)
                    target :> ITexture
            }
                
        result :> IOutputMod<_>

        
    let private fbToSg (viewportSize : IMod<V2i>) fb = 
        Sg.fullscreenQuad viewportSize
            |> Sg.texture DefaultSemantic.DiffuseColorTexture fb
            |> Sg.effect [DefaultSurfaces.diffuseTexture |> toEffect]

    let private signature (runtime : Aardvark.Rendering.GL.Runtime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba32f; samples = 1 }
            DefaultSemantic.Depth, { format = RenderbufferFormat.Depth24Stencil8; samples = 1 }
        ]
     
    module GroundTruth = 

        type GroundTruthData = {
            haltonSequence : ModRef<V2d[]>
            clear : ModRef<bool>
            frameCount : ModRef<int>
            }
        
        let private renderToColorWithoutClear (size : IMod<V2i>) (task : IRenderTask) =
                let sem = (Set.singleton DefaultSemantic.Colors)
                let runtime = task.Runtime.Value
                let signature = task.FramebufferSignature.Value
            
                let fbo = runtime.CreateFramebuffer(signature, sem, size)
                
                let res = new SequentialRenderTask([|task|]) |> renderTo fbo
                sem |> Seq.map (fun k -> k, getResult k res) |> Map.ofSeq |> Map.find DefaultSemantic.Colors


        let private basicRenderTask (data : RenderData) (gtData : GroundTruthData) (sceneSg : ISg) = 
            
            let mode = 
                gtData.clear |> Mod.map ( fun c -> 
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
                    |> setupFbEffects [ 
                            EffectGT.groundTruthLighting |> toEffect 
                            EffectUtils.effectClearNaN |> toEffect
                        ]
                    |> setupLights data.lights
                    |> setupPhotometricData data.photometricData
                    |> setupCamera data.view data.frustum data.viewportSize 
                    |> Sg.uniform "HaltonSamples" gtData.haltonSequence
                    |> Sg.uniform "FrameCount" gtData.frameCount
                    |> Sg.compile data.runtime (signature data.runtime)
                    |> RenderTask.renderToColor data.viewportSize
              
            Sg.fullscreenQuad data.viewportSize
                |> Sg.blendMode mode
                |> setupFbEffects []
                |> Sg.texture DefaultSemantic.DiffuseColorTexture iterationRender
                |> Sg.compile data.runtime (signature data.runtime)

        let groundTruthFb (data : RenderData) (gtData : GroundTruthData) (sceneSg : ISg) = 
            basicRenderTask data gtData sceneSg
            |> renderToColorWithoutClear data.viewportSize
     
        let groundTruthSgAndFb (data : RenderData) (gtData : GroundTruthData) (sceneSg : ISg) = 
            let fb = groundTruthFb data gtData sceneSg
           
            let sg = Sg.fullscreenQuad data.viewportSize
                    |> Sg.effect [ EffectToneMapping.toneMap |> toEffect ]
                    |> Sg.uniform "ToneMapScale" (1.0 |> Mod.init)
                    |> Sg.texture (Sym.ofString "InputTex") (fb |> setupMipMaps data.runtime)
            
            (sg, fb)
            

        let groundTruthRenderUpdate (data : RenderData) (gtData : GroundTruthData) =
            
            let mutable prevLightTrafos : Trafo3d[] = Array.create Config.NUM_LIGHTS Trafo3d.Identity
            let lightDemandsClear = 
                data.lights.Trafos |> Mod.map (
                    fun trafos -> 
                        let clear = Array.forall2 (fun elem1 elem2 -> elem1 <> elem2) trafos prevLightTrafos
                        prevLightTrafos <- trafos        
                        clear
                    )

            let mutable prevView = Trafo3d.Identity
            let camDemandsClear =
                data.view |> Mod.map (
                    fun view ->
                        let currentView  = CameraView.viewTrafo view
                        let clear = currentView <> prevView 
                        prevView <- currentView
                        clear
                    )


            let clearRequired = 
                let required = Mod.map2 (fun l c -> l || c) lightDemandsClear camDemandsClear

                required

            // only update if the render mode is GroundTruth or Compare
            let executeUpdate =
                data.mode |> Mod.map (
                    fun mode ->
                        match mode with
                        | RenderMode.GroundTruth -> true
                        | RenderMode.Compare     -> true
                        | _ -> false
                    )
            
            let update (args : OpenTK.FrameEventArgs) =
                transact (fun _ -> 
                    
                    if executeUpdate |> Mod.force then
                    
                        let clear = clearRequired |> Mod.force 
                                        
                        if clear then
                            if not gtData.clear.Value then 
                                gtData.clear.Value <- true
                        else
                            if gtData.clear.Value then 
                                gtData.clear.Value <- false       
                                                
                        gtData.haltonSequence.Value <-
                            if clear then
                                HaltonSequence.init
                            else
                                gtData.haltonSequence.Value.[Config.NUM_SAMPLES - 1] |> HaltonSequence.next     
                                    
                        gtData.frameCount.Value <- 
                            if clear then
                                1
                            else
                                gtData.frameCount.Value + 1
                        
                        // TODO find better solution than marking outdated
                        if clear then
                            lightDemandsClear.MarkOutdated()
                            camDemandsClear.MarkOutdated()
                )

            update
        
        let initGTData =
            {
                haltonSequence = ModRef(HaltonSequence.init)
                clear = ModRef(false) 
                frameCount = ModRef(0)
            }

    module CenterPointApprox =

        let centerPointApproxRenderTask (data : RenderData) (sceneSg : ISg) = 
            sceneSg
                |> setupFbEffects [ 
                        EffectApPoint.centerPointApprox |> toEffect 
                        EffectUtils.effectClearNaN |> toEffect
                    ]
                |> setupLights data.lights
                |> setupPhotometricData data.photometricData
                |> setupCamera data.view data.frustum data.viewportSize
                |> Sg.compile data.runtime (signature data.runtime)

        let centerPointApproxFb (data : RenderData) (sceneSg : ISg) = 
            centerPointApproxRenderTask data sceneSg
            |> RenderTask.renderToColor data.viewportSize

        let centerPointApproxSgAndFb (data : RenderData) (sceneSg : ISg) = 
            let fb = centerPointApproxFb data sceneSg 
            
            let sg = Sg.fullscreenQuad data.viewportSize
                        |> Sg.effect [ EffectToneMapping.toneMap |> toEffect ]
                        |> Sg.uniform "ToneMapScale" (1.0 |> Mod.init)
                        |> Sg.texture (Sym.ofString "InputTex") (fb |> setupMipMaps data.runtime)
                        
            (sg, fb)
            

    module BaumFFApprox =

        let baumFFApproxRenderTask (data : RenderData) (sceneSg : ISg) = 
            sceneSg
                |> setupFbEffects [ 
                        EffectApBaumFF.baumFFApprox |> toEffect 
                        EffectUtils.effectClearNaN |> toEffect
                    ]
                |> setupLights data.lights
                |> setupPhotometricData data.photometricData
                |> setupCamera data.view data.frustum data.viewportSize
                |> Sg.compile data.runtime (signature data.runtime)

        let baumFFApproxFb (data : RenderData) (sceneSg : ISg) = 
            baumFFApproxRenderTask data sceneSg
            |> RenderTask.renderToColor data.viewportSize

        let baumFFApproxSgAndFb (data : RenderData) (sceneSg : ISg) = 
            let fb = baumFFApproxFb data sceneSg 
            
            let sg = Sg.fullscreenQuad data.viewportSize
                    |> Sg.effect [ EffectToneMapping.toneMap |> toEffect ]
                    |> Sg.uniform "ToneMapScale" (1.0 |> Mod.init)
                    |> Sg.texture (Sym.ofString "InputTex") (fb |> setupMipMaps data.runtime)
            
            (sg, fb)
            


    module SolidAngleApprox =

        let solidAngleApproxRenderTask (data : RenderData) (sceneSg : ISg) = 
            sceneSg
                |> setupFbEffects [ EffectApSolidAngle.solidAngleApprox |> toEffect ]
                |> setupLights data.lights
                |> setupPhotometricData data.photometricData
                |> setupCamera data.view data.frustum data.viewportSize
                |> Sg.compile data.runtime (signature data.runtime)

        let solidAngleApproxFb (data : RenderData) (sceneSg : ISg) = 
            solidAngleApproxRenderTask data sceneSg
            |> RenderTask.renderToColor data.viewportSize

        let solidAngleApproxSgAndFb (data : RenderData) (sceneSg : ISg) = 
            let fb = solidAngleApproxFb data sceneSg 

            let sg = Sg.fullscreenQuad data.viewportSize
                    |> Sg.effect [ EffectToneMapping.toneMap |> toEffect ]
                    |> Sg.uniform "ToneMapScale" (1.0 |> Mod.init)
                    |> Sg.texture (Sym.ofString "InputTex") (fb |> setupMipMaps data.runtime)
            
            // (fb |> fbToSg data.viewportSize, fb)
            (sg, fb)
 

    module Compare = 
        
        open GroundTruth
        open CenterPointApprox
        open BaumFFApprox
        open SolidAngleApprox

        let private renderToFbo (fbo : IOutputMod<IFramebuffer>) (task : IRenderTask) =
            let sem = (Set.singleton DefaultSemantic.Colors)
            let res = 
                new SequentialRenderTask([|task|]) |> renderTo fbo
        
            sem |> Seq.map (fun k -> k, getResult k res) |> Map.ofSeq |> Map.find DefaultSemantic.Colors

        let private depthSignature (runtime : Aardvark.Rendering.GL.Runtime) = 
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Depth, { format = RenderbufferFormat.DepthComponent32; samples = 1 }
            ]
                
        let private  diffSignature (runtime : Aardvark.Rendering.GL.Runtime) =
            runtime.CreateFramebufferSignature [
                DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba32f; samples = 1 }
            ]
                                        
        let diffFb (data : RenderData) (effectFbs : Map<RenderMode, IOutputMod<ITexture>>) =
                        
            Sg.fullscreenQuad data.viewportSize
                |> Sg.effect [ 
                    EffectCompare.Compute.computeError |> toEffect 
                ]
                |> Sg.texture (Sym.ofString "GTTex") (Map.find RenderMode.GroundTruth effectFbs)
                |> Sg.texture (Sym.ofString "CompTex") 
                    (
                        data.compare |> Mod.bind (fun rm ->
                            match rm with
                                | RenderMode.Compare -> Map.find RenderMode.GroundTruth effectFbs
                                | _ -> Map.find rm effectFbs
                            )
                    )
                |> Sg.compile data.runtime (diffSignature data.runtime)
                |> RenderTask.renderToColor data.viewportSize
                                
        let private depthFb (data : RenderData) (sceneSg : ISg)=
            sceneSg
                |> Sg.effect [ 
                    DefaultSurfaces.trafo |> toEffect
                    DefaultSurfaces.vertexColor |> toEffect 
                    ]
                |> setupLights data.lights                   
                |> setupCamera data.view data.frustum data.viewportSize
                |> Sg.compile data.runtime (depthSignature data.runtime)
                |> RenderTask.renderToDepth data.viewportSize

        let compareRenderTask (data : RenderData) (gtData : GroundTruthData) (sceneSg : ISg) (diffFb : IOutputMod<ITexture>) = 
            Sg.fullscreenQuad data.viewportSize
                |> Sg.effect [ 
                    // EffectOutline.outlineV |> toEffect 
                    EffectCompare.Visualize.visualize |> toEffect
                    // EffectOutline.outlineF |> toEffect 
                ]
                |> Sg.uniform "PixelSize" (data.viewportSize |> Mod.map (fun vs -> V2d(1.0 / (float) vs.X, 1.0 / (float) vs.Y)))
                |> Sg.texture (Sym.ofString "TexError") diffFb
                |> Sg.texture (Sym.ofString "TexDepth") (depthFb data sceneSg)
                |> Sg.compile data.runtime (signature data.runtime) 

        let compareFb (data : RenderData) (gtData : GroundTruthData) (sceneSg : ISg) (diffFb : IOutputMod<ITexture>) = 
            compareRenderTask data gtData sceneSg diffFb
            |> RenderTask.renderToColor data.viewportSize
     
        let compareSg (data : RenderData) (gtData : GroundTruthData) (sceneSg : ISg) (diffFb : IOutputMod<ITexture>) = 
            compareFb data gtData sceneSg diffFb
            |> fbToSg data.viewportSize
 
    module Effects = 
        open Aardvark.Application.WinForms

        open GroundTruth
        open CenterPointApprox
        open BaumFFApprox
        open SolidAngleApprox
        open Compare

        let initialRenderData (app : OpenGlApplication) (view : IMod<CameraView>) (viewportSize : V2i) (m : MRenderState) =
             {
                runtime = app.Runtime
                scenePath = m.scenePath
                view = view
                frustum = Frustum.perspective 60.0 0.1 100.0 ((float)viewportSize.X / (float)viewportSize.Y) |> Mod.init 
                viewportSize = viewportSize |> Mod.init
                lights = m.lights |> Mod.force // mod  force necessary ? 
                photometricData = m.photometryData
                mode = m.renderMode
                compare = m.compare
            }

        let fpsUpdate (feedback : RenderFeedback) =

            let mutable dTSum = 0.0 // in seconds
            let mutable updateCount = 0

            let update (args : OpenTK.FrameEventArgs) =
                transact (fun _ ->
                    if dTSum > 0.5 then
                        let avgDT = dTSum / (float)updateCount
                        
                        feedback.fps.Value <- 1.0 / avgDT
                        
                        dTSum <- 0.0
                        updateCount <- 0
                    else
                        dTSum <- dTSum + args.Time
                        updateCount <- updateCount + 1
                )

                ()

            update

        let CreateAndLinkRenderTask (data : RenderData) (gtData : GroundTruthData) =

            let sceneSg = 
                Mod.map( fun path -> path |> Utils.Assimp.loadFromFile true |> Sg.normalize) data.scenePath
                |> Sg.dynamic
                |> Sg.scale 18.0

            let (groundTruthSg, groundTruthFb)              = groundTruthSgAndFb data gtData sceneSg
            let (centerPointApproxSg, centerPointApproxFb)  = centerPointApproxSgAndFb data sceneSg
            let (baumFFApproxSg, baumFFApproxFb)            = baumFFApproxSgAndFb data sceneSg
            let (solidAngleApproxSg, solidAngleApproxFb)    = solidAngleApproxSgAndFb data sceneSg

            let effectFbs = 
                Map.empty
                |> Map.add RenderMode.GroundTruth       groundTruthFb
                |> Map.add RenderMode.CenterPointApprox centerPointApproxFb
                |> Map.add RenderMode.BaumFFApprox      baumFFApproxFb
                |> Map.add RenderMode.SolidAngleApprox  solidAngleApproxFb
                
            let diffFrameBuffer = diffFb data effectFbs

            let signature = signature data.runtime
            let tasks = 
                Map.empty
                |> Map.add RenderMode.GroundTruth (groundTruthSg |> Sg.compile data.runtime signature)
                |> Map.add RenderMode.CenterPointApprox (centerPointApproxSg |> Sg.compile data.runtime signature)
                |> Map.add RenderMode.BaumFFApprox (baumFFApproxSg |> Sg.compile data.runtime signature)
                |> Map.add RenderMode.SolidAngleApprox (solidAngleApproxSg |> Sg.compile data.runtime signature)
                |> Map.add RenderMode.Compare (compareSg data gtData sceneSg diffFrameBuffer |> Sg.compile data.runtime signature)
                
            tasks |> Map.iter (fun _ t -> t.Update(AdaptiveToken.Top, RenderToken.Empty)) // iterate over tasks initially one time to create them

            let renderTask = 
                { new AbstractRenderTask() with
                    override x.Release() = ()
                    override x.Perform(a,b,c) =
                        let m = data.mode.GetValue(a)
                        let task = tasks.[m]
                        task.Run(a,b,c)

                    override x.PerformUpdate(a,b) =
                        let m = data.mode.GetValue(a)
                        let task = tasks.[m]
                        task.Update(a,b)

                    override x.Use f = f()
                    override x.FramebufferSignature = Some signature
                    override x.Runtime = Some (data.runtime :> _)
                }

            let renderFeedback = 
                {
                    fps = ModRef(0.0)
                    frameCount = gtData.frameCount                      
                    compareTexture = diffFrameBuffer
                }

            (renderTask, renderFeedback)
