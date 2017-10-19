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
                
    let private setupCamera (viewTrafo : IMod<Trafo3d>) (projTrafo : IMod<Trafo3d>) (viewportSize : IMod<V2i>) sg =
        sg
            |> Sg.viewTrafo viewTrafo
            |> Sg.projTrafo projTrafo
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


    let private sceneSg = 
        Mod.map( fun path -> path |> Utils.Assimp.loadFromFile true |> Sg.normalize) m.scenePath
        |> Sg.dynamic
        |> Sg.scale 18.0 // because sponza is so small

    let private fbToSg (viewportSize : IMod<V2i>) fb= 
        Sg.fullscreenQuad viewportSize
            |> Sg.texture DefaultSemantic.DiffuseColorTexture fb
            |> Sg.effect [DefaultSurfaces.diffuseTexture |> toEffect]

    let private signature (runtime : Aardvark.Rendering.GL.Runtime)=
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba32f; samples = 1 }
            DefaultSemantic.Depth, { format = RenderbufferFormat.Depth24Stencil8; samples = 1 }
        ]

    module GroundTruth = 
        
        let private renderToColorWithoutClear (size : IMod<V2i>) (task : IRenderTask) =
                let sem = (Set.singleton DefaultSemantic.Colors)
                let runtime = task.Runtime.Value
                let signature = task.FramebufferSignature.Value
            
                let fbo = runtime.CreateFramebuffer(signature, sem, size)

                let res = 
                    new SequentialRenderTask([|task|]) |> renderTo fbo
        
                sem |> Seq.map (fun k -> k, getResult k res) |> Map.ofSeq |> Map.find DefaultSemantic.Colors

        let groundTruthRenderTask (runtime : Aardvark.Rendering.GL.Runtime) (viewTrafo : IMod<Trafo3d>) (projTrafo : IMod<Trafo3d>) (viewportSize : IMod<V2i>) (lights : LightCollection) (photometricData : IMod<Option<IntensityProfileSampler>>) = 

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
                    |> setupLights lights
                    |> setupPhotometricData photometricData
                    |> setupCamera viewTrafo projTrafo viewportSize 
                    |> Sg.uniform "HaltonSamples" (m.haltonSequence |> Mod.map Seq.toArray)
                    |> Sg.uniform "FrameCount" ( m.frameCount)
                    |> Sg.compile runtime (signature runtime)
                    |> RenderTask.renderToColor viewportSize
              
            Sg.fullscreenQuad viewportSize
                |> Sg.blendMode mode
                |> setupFbEffects []
                |> Sg.texture DefaultSemantic.DiffuseColorTexture iterationRender
                |> Sg.compile runtime (signature runtime)

        let groundTruthFb (runtime : Aardvark.Rendering.GL.Runtime) (viewTrafo : IMod<Trafo3d>) (projTrafo : IMod<Trafo3d>) (viewportSize : IMod<V2i>) (lights : LightCollection) (photometricData : IMod<Option<IntensityProfileSampler>>) = 
            groundTruthRenderTask runtime viewTrafo projTrafo viewportSize lights photometricData
            |> renderToColorWithoutClear viewportSize
     
        let groundTruthSg (runtime : Aardvark.Rendering.GL.Runtime) (viewTrafo : IMod<Trafo3d>) (projTrafo : IMod<Trafo3d>) (viewportSize : IMod<V2i>) (lights : LightCollection) (photometricData : IMod<Option<IntensityProfileSampler>>) = 
            groundTruthFb runtime viewTrafo projTrafo viewportSize lights photometricData
            |> fbToSg viewportSize

    module BaumFormFactor = 
        open RenderWindow
           
        let baumFormFactorRenderTask (data : SharedRenderData) = 
            sceneSg
                |> setupFbEffects [ EffectBaumFF.formFactorLighting |> toEffect ]
                |> setupLights data.lights
                |> setupCamera data.viewTrafo data.projTrafo data.viewportSize
                |> Sg.compile data.runtime (signature data.runtime)        

        let baumFormFactorFb (runtime : Aardvark.Rendering.GL.Runtime) (viewTrafo : IMod<Trafo3d>) (projTrafo : IMod<Trafo3d>) (viewportSize : IMod<V2i>) (lights : LightCollection) = 
            baumFormFactorRenderTask runtime viewTrafo projTrafo viewportSize lights
            |> RenderTask.renderToColor viewportSize
     
        let baumFormFactorSg (runtime : Aardvark.Rendering.GL.Runtime) (viewTrafo : IMod<Trafo3d>) (projTrafo : IMod<Trafo3d>) (viewportSize : IMod<V2i>) (lights : LightCollection) = 
            baumFormFactorFb runtime viewTrafo projTrafo viewportSize lights
            |> fbToSg viewportSize

    module Compare = 
        
        open GroundTruth
        open BaumFormFactor
        open RenderWindow

        type CompareData = {
            baseBridge : SharedRenderData
            compare : IMod<RenderMode>
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
                                        
        let private diffFb (runtime : Aardvark.Rendering.GL.Runtime) (viewTrafo : IMod<Trafo3d>) (projTrafo : IMod<Trafo3d>) (viewportSize : IMod<V2i>) (lights : LightCollection) (photometricData : IMod<Option<IntensityProfileSampler>>)=
            Sg.fullscreenQuad viewportSize
                |> Sg.effect [ 
                    EffectCompare.compare |> toEffect 
                ]
                |> Sg.texture (Sym.ofString "TexA") (groundTruthFb runtime viewTrafo projTrafo viewportSize lights photometricData)
                |> Sg.texture (Sym.ofString "TexB") 
                    (
                        Mod.bind (fun rm ->
                            match rm with
                                | RenderMode.GroundTruth -> (groundTruthFb runtime viewTrafo projTrafo viewportSize lights photometricData)
                                | RenderMode.BaumFormFactor -> (baumFormFactorFb runtime viewTrafo projTrafo viewportSize lights)
                                | _ -> (groundTruthFb runtime viewTrafo projTrafo viewportSize lights photometricData)
                            ) m.compare
                    )
                |> Sg.compile runtime (diffSignature runtime)
                |> RenderTask.renderToColor viewportSize
                                
        let private depthFb (runtime : Aardvark.Rendering.GL.Runtime) (viewTrafo : IMod<Trafo3d>) (projTrafo : IMod<Trafo3d>) (viewportSize : IMod<V2i>) (lights : LightCollection) (photometricData : IMod<Option<IntensityProfileSampler>>)=
            sceneSg
                |> Sg.effect [ 
                    DefaultSurfaces.trafo |> toEffect
                    DefaultSurfaces.vertexColor |> toEffect 
                    ]
                |> setupLights lights                   
                |> setupCamera viewTrafo projTrafo viewportSize
                |> Sg.compile runtime (depthSignature runtime)
                |> RenderTask.renderToDepth viewportSize

        let compareRenderTask (runtime : Aardvark.Rendering.GL.Runtime) (viewTrafo : IMod<Trafo3d>) (projTrafo : IMod<Trafo3d>) (viewportSize : IMod<V2i>) (lights : LightCollection) (photometricData : IMod<Option<IntensityProfileSampler>>)= 
            Sg.fullscreenQuad viewportSize
                |> Sg.effect [ 
                    EffectOutline.outlineV |> toEffect 
                    EffectOutline.outlineF |> toEffect 
                ]
                |> Sg.texture (Sym.ofString "Tex") (diffFb runtime viewTrafo projTrafo viewportSize lights photometricData)
                |> Sg.texture (Sym.ofString "TexDepth") (depthFb runtime viewTrafo projTrafo viewportSize lights photometricData)
                |> Sg.compile runtime (signature runtime) 

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
                

