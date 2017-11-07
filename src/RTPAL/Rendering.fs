namespace Render

open EffectUtils

module Rendering = 

    open Aardvark.Base
    open Aardvark.Base.Rendering
    open Aardvark.Base.RenderTask
    open Aardvark.Base.Incremental

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

                let res = 
                    new SequentialRenderTask([|task|]) |> renderTo fbo
        
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
                    |> setupFbEffects [ EffectGT.groundTruthLighting |> toEffect ]
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
     
        let groundTruthSg (data : RenderData) (gtData : GroundTruthData) (sceneSg : ISg) = 
            Sg.fullscreenQuad data.viewportSize
            |> Sg.effect [ EffectToneMapping.toneMap |> toEffect ]
            |> Sg.uniform "ToneMapScale" (1.0 |> Mod.init)
            |> Sg.texture (Sym.ofString "InputTex") (groundTruthFb data gtData sceneSg)

        
        let groundTruthRenderTask (data : RenderData) (gtData : GroundTruthData) (sceneSg : ISg) =
            data.runtime.CompileRender((signature data.runtime), (groundTruthSg data gtData sceneSg))

        let groundTruthRenderUpdate (data : RenderData) (gtData : GroundTruthData) =

            
            let mutable prevView = Trafo3d.Identity
            let mutable prevLightTrafos : Trafo3d[] = Array.create Config.NUM_LIGHTS Trafo3d.Identity
            
            let lightDemandsClear = 
                data.lights.Trafos |> Mod.map (
                    fun trafos -> 
                        let clear = Array.forall2 (fun elem1 elem2 -> elem1 <> elem2) trafos prevLightTrafos
                        prevLightTrafos <- trafos        
                        clear
                    )

            let camDemandsClear =
                data.view |> Mod.map (
                    fun view ->
                        let currentView  = CameraView.viewTrafo view
                        let clear = currentView <> prevView 
                        prevView <- currentView
                        clear
                    )

            let clearRequired = 
                Mod.map2 (fun l c -> l || c) lightDemandsClear camDemandsClear
            
            let update (args : OpenTK.FrameEventArgs) =
                transact (fun _ -> 
                    
                    let clear = clearRequired |> Mod.force 
                                        
                    if clear then
                        if not gtData.clear.Value then 
                            gtData.clear.Value <- true
                    else
                        if gtData.clear.Value then 
                            gtData.clear.Value <- false       
                                                
                    gtData.haltonSequence.Value <-
                        if gtData.clear.Value then
                            HaltonSequence.init
                        else
                            gtData.haltonSequence.Value.[Config.NUM_SAMPLES - 1] |> HaltonSequence.next     
                                    
                    gtData.frameCount.Value <- 
                        if gtData.clear.Value then
                            1
                        else
                            gtData.frameCount.Value + 1
                        

                    // TODO find better solution than marking outdated
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
                         
    module BaumFormFactor = 
           
        let baumFormFactorRenderTask (data : RenderData) (sceneSg : ISg) = 
            sceneSg
                |> setupFbEffects [ EffectBaumFF.formFactorLighting |> toEffect ]
                |> setupLights data.lights
                |> setupCamera data.view data.frustum data.viewportSize
                |> Sg.compile data.runtime (signature data.runtime)        

        let baumFormFactorFb (data : RenderData) (sceneSg : ISg) = 
            baumFormFactorRenderTask data sceneSg
            |> RenderTask.renderToColor data.viewportSize
     
        let baumFormFactorSg (data : RenderData) (sceneSg : ISg) = 
            baumFormFactorFb data sceneSg
            |> fbToSg data.viewportSize

    module Compare = 
        
        open GroundTruth
        open BaumFormFactor
        
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
                                        
        let diffFb (data : RenderData) (gtData : GroundTruthData) (sceneSg : ISg) =

            let groundTruthFb = groundTruthFb data gtData sceneSg
            let baumFormFactorFb = baumFormFactorFb data sceneSg

            Sg.fullscreenQuad data.viewportSize
                |> Sg.effect [ 
                    EffectCompare.compare |> toEffect 
                ]
                |> Sg.texture (Sym.ofString "TexA") groundTruthFb
                |> Sg.texture (Sym.ofString "TexB") 
                    (
                        data.compare |> Mod.bind (fun rm ->
                            match rm with
                                | RenderMode.GroundTruth -> groundTruthFb
                                | RenderMode.BaumFormFactor -> baumFormFactorFb
                                | _ -> groundTruthFb
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
                    EffectOutline.outlineV |> toEffect 
                    EffectOutline.outlineF |> toEffect 
                ]
                |> Sg.uniform "PixelSize" (data.viewportSize |> Mod.map (fun vs -> V2d(1.0 / (float) vs.X, 1.0 / (float) vs.Y)))
                |> Sg.texture (Sym.ofString "Tex") diffFb
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
        open BaumFormFactor
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
                
            let diffFrameBuffer = diffFb data gtData sceneSg

            let signature = signature data.runtime
            let tasks = 
                Map.empty
                |> Map.add RenderMode.GroundTruth (groundTruthSg data gtData sceneSg |> Sg.compile data.runtime signature)
                |> Map.add RenderMode.BaumFormFactor (baumFormFactorSg data sceneSg |> Sg.compile data.runtime signature)
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
