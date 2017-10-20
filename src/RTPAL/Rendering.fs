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
    
    type SharedRenderData = {
        runtime : Aardvark.Rendering.GL.Runtime

        sceneSg : ISg

        view : IMod<CameraView>
        frustum : IMod<Frustum>
        viewportSize : IMod<V2i>

        lights : LightCollection
        photometricData : IMod<Option<IntensityProfileSampler>>
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
        Mod.map( fun (pd : Option<IntensityProfileSampler>) ->
            match pd with 
            | Some data -> 
                sg
                    |> Sg.uniform "ProfileAddressing" (data.AddressingParameters |> Mod.init)
                    |> Sg.uniform "TextureOffsetScale" (data.ImageOffsetScale |> Mod.init)
                    |> Sg.texture Render.PhotometricLight.IntensityTexture (((PixTexture2d(PixImageMipMap(data.Image), false)) :> ITexture) |> Mod.constant)
            | None -> sg
        ) photometricData
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
            haltonSequence : ResetMod<seq<V2d>>
            clear : ResetMod<bool>
            frameCount : ResetMod<int>
            }
        
        let private renderToColorWithoutClear (size : IMod<V2i>) (task : IRenderTask) =
                let sem = (Set.singleton DefaultSemantic.Colors)
                let runtime = task.Runtime.Value
                let signature = task.FramebufferSignature.Value
            
                let fbo = runtime.CreateFramebuffer(signature, sem, size)

                let res = 
                    new SequentialRenderTask([|task|]) |> renderTo fbo
        
                sem |> Seq.map (fun k -> k, getResult k res) |> Map.ofSeq |> Map.find DefaultSemantic.Colors

        let private basicRenderTask (data : SharedRenderData) (gtData : GroundTruthData) = 
            
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
                data.sceneSg
                    |> setupFbEffects [ EffectGT.groundTruthLighting |> toEffect ]
                    |> setupLights data.lights
                    |> setupPhotometricData data.photometricData
                    |> setupCamera data.view data.frustum data.viewportSize 
                    |> Sg.uniform "HaltonSamples" (gtData.haltonSequence |> Mod.map Seq.toArray)
                    |> Sg.uniform "FrameCount" ( gtData.frameCount)
                    |> Sg.compile data.runtime (signature data.runtime)
                    |> RenderTask.renderToColor data.viewportSize
              
            Sg.fullscreenQuad data.viewportSize
                |> Sg.blendMode mode
                |> setupFbEffects []
                |> Sg.texture DefaultSemantic.DiffuseColorTexture iterationRender
                |> Sg.compile data.runtime (signature data.runtime)

        let groundTruthFb (data : SharedRenderData) (gtData : GroundTruthData) = 
            basicRenderTask data gtData
            |> renderToColorWithoutClear data.viewportSize
     
        let groundTruthSg (data : SharedRenderData) (gtData : GroundTruthData) = 
            groundTruthFb data gtData
            |> fbToSg data.viewportSize
        
        let groundTruthRenderTask (data : SharedRenderData) (gtData : GroundTruthData) =
            data.runtime.CompileRender((signature data.runtime), (groundTruthSg data gtData))

        let groundTruthRenderUpdate (data : SharedRenderData) (gtData : GroundTruthData) =

            let update (args : OpenTK.FrameEventArgs) =

                let mutable prevView = Trafo3d.Identity

                transact (fun _ -> 
                    
                    let currentView = data.view |> Mod.force |> CameraView.viewTrafo
                    let mutable currentClear = gtData.clear |> Mod.force

                    if prevView <> currentView then
                        if not currentClear then 
                            currentClear <- true
                            ResetMod.Update(gtData.clear, true)
                    else
                        if currentClear then 
                            currentClear <- false
                            ResetMod.Update(gtData.clear, false)

                    prevView <- currentView
                    
                    let newhs = if currentClear then
                                    HaltonSequence.init
                                else
                                    let hs = gtData.haltonSequence |> Mod.force
                                    (hs |> Seq.toArray).[(hs |> Seq.length) - 1] |> HaltonSequence.next                
                    ResetMod.Update(gtData.haltonSequence, newhs :> seq<V2d>)
                    
                    let newfc = (gtData.frameCount |> Mod.force) + 1
                    ResetMod.Update(gtData.frameCount, newfc)
                )

            update

    module BaumFormFactor = 
           
        let baumFormFactorRenderTask (data : SharedRenderData) = 
            data.sceneSg
                |> setupFbEffects [ EffectBaumFF.formFactorLighting |> toEffect ]
                |> setupLights data.lights
                |> setupCamera data.view data.frustum data.viewportSize
                |> Sg.compile data.runtime (signature data.runtime)        

        let baumFormFactorFb (data : SharedRenderData) = 
            baumFormFactorRenderTask data
            |> RenderTask.renderToColor data.viewportSize
     
        let baumFormFactorSg (data : SharedRenderData) = 
            baumFormFactorFb data
            |> fbToSg data.viewportSize

    module Compare = 
        
        open GroundTruth
        open BaumFormFactor

        type CompareData = {
            compare : ResetMod<RenderMode>
            }

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
                                        
        let private diffFb (data : SharedRenderData) (gtData : GroundTruthData) (compData : CompareData) =
            
            Sg.fullscreenQuad data.viewportSize
                |> Sg.effect [ 
                    EffectCompare.compare |> toEffect 
                ]
                |> Sg.texture (Sym.ofString "TexA") (groundTruthFb data gtData)
                |> Sg.texture (Sym.ofString "TexB") 
                    (
                        Mod.bind (fun rm ->
                            match rm with
                                | RenderMode.GroundTruth -> (groundTruthFb data gtData)
                                | RenderMode.BaumFormFactor -> (baumFormFactorFb data)
                                | _ -> (groundTruthFb data gtData)
                            ) compData.compare
                    )
                |> Sg.compile data.runtime (diffSignature data.runtime)
                |> RenderTask.renderToColor data.viewportSize
                                
        let private depthFb (data : SharedRenderData) =
            data.sceneSg
                |> Sg.effect [ 
                    DefaultSurfaces.trafo |> toEffect
                    DefaultSurfaces.vertexColor |> toEffect 
                    ]
                |> setupLights data.lights                   
                |> setupCamera data.view data.frustum data.viewportSize
                |> Sg.compile data.runtime (depthSignature data.runtime)
                |> RenderTask.renderToDepth data.viewportSize

        let compareRenderTask (data : SharedRenderData) (gtData : GroundTruthData) (compData : CompareData) = 
            Sg.fullscreenQuad data.viewportSize
                |> Sg.effect [ 
                    EffectOutline.outlineV |> toEffect 
                    EffectOutline.outlineF |> toEffect 
                ]
                |> Sg.texture (Sym.ofString "Tex") (diffFb data gtData compData)
                |> Sg.texture (Sym.ofString "TexDepth") (depthFb data)
                |> Sg.compile data.runtime (signature data.runtime) 

        (*
        let a = (fun _ -> 
            let diff = diffFb.GetValue()
                            
            let diffPixData = runtime.Download(diff |> unbox<_>)
            let downlaoded = diffPixData.ToPixImage<float32>()
            let data = downlaoded.GetMatrix<C4f>()
            let ec = data.Elements |> Seq.fold ( fun cs c-> (C4f.White - c) + cs ) C4f.Black
                                        
            COMPUTED_ERROR (float (sqrt (ec.R * ec.R + ec.G * ec.G + ec.B  * ec.B)))
        )
        *)
                

