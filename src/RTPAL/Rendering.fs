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

    
    open PhotometricLight
    open Light
    open Utils
    open Utils.Sg
    open RenderInterop
    
                
    let private signature (runtime : Aardvark.Rendering.GL.Runtime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba32f; samples = 1 }
            DefaultSemantic.Depth, { format = RenderbufferFormat.Depth24Stencil8; samples = 1 }
        ]

 
    module RealTime = 

        open Aardvark.Application.WinForms
        
        open EffectGT.Rendering
        open EffectApPoint.Rendering
        open EffectApMRP.Rendering
        open EffectApBaumFF.Rendering
        open EffectToneMapping.Rendering
        open EffectApStructuredSampling.Rendering
        open EffectCompare.Rendering

        open EffectFormFactor.Rendering
        open EffectSolidAngle.Rendering

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

        let CreateAndLinkRenderTask (data : RenderData) (gtData : GroundTruthData) (mrpData : MRPData) (ssData : SSData) =



            let sceneSg = 
                data.sceneSg 
                    |> Light.Sg.setLightCollectionUniforms data.lights
                    |> setupPhotometricData data.photometricData
                    |> setupCamera data.view data.projTrafo data.viewportSize

            let signature = signature data.runtime

            let groundTruthFb       = groundTruthFb data gtData signature sceneSg       |> applyTonemappingOnFb data signature
            let centerPointApproxFb = centerPointApproxFb data signature sceneSg        |> applyTonemappingOnFb data signature
            let mrpApproxFb         = mrpApproxFb data mrpData signature sceneSg        |> applyTonemappingOnFb data signature
            let baumFFApproxFb      = baumFFApproxFb data signature sceneSg             |> applyTonemappingOnFb data signature
            let ssIrrApproxFb       = ssIrrApproxFb data ssData signature sceneSg       |> applyTonemappingOnFb data signature
            let ssApproxFb          = ssApproxFb data ssData signature sceneSg          |> applyTonemappingOnFb data signature
            let ssCombinedApproxFb  = ssCombinedApproxFb data ssData signature sceneSg  |> applyTonemappingOnFb data signature

            let formFactorFb        = formFactorFb data signature sceneSg           
            let solidAngleFb        = solidAngleFb data signature sceneSg
                  
            let effectFbs = 
                Map.empty
                |> Map.add RenderMode.GroundTruth                   groundTruthFb
                |> Map.add RenderMode.CenterPointApprox             centerPointApproxFb
                |> Map.add RenderMode.MRPApprox                     mrpApproxFb
                |> Map.add RenderMode.BaumFFApprox                  baumFFApproxFb
                |> Map.add RenderMode.StructuredIrrSampling         ssIrrApproxFb
                |> Map.add RenderMode.StructuredSampling            ssApproxFb
                |> Map.add RenderMode.CombinedStructuredSampling    ssCombinedApproxFb

                |> Map.add RenderMode.FormFactor                    formFactorFb
                |> Map.add RenderMode.SolidAngle                    solidAngleFb



                
            let diffFrameBuffer = diffFb data effectFbs
            
            let tasks = 
                let mutable map = 
                    Map.empty
                    |> Map.add RenderMode.Compare (compareSg data gtData signature sceneSg diffFrameBuffer |> Sg.compile data.runtime signature)

                for kv in effectFbs do
                    map <- map |> Map.add (kv.Key) (kv.Value |> fbToSg data.viewportSize |> Sg.compile data.runtime signature)

                map

(*
            let tasks = 
                Map.empty
                |> Map.add RenderMode.GroundTruth                   (groundTruthFb       |> fbToSg data.viewportSize |> Sg.compile data.runtime signature)
                |> Map.add RenderMode.CenterPointApprox             (centerPointApproxFb |> fbToSg data.viewportSize |> Sg.compile data.runtime signature)
                |> Map.add RenderMode.MRPApprox                     (mrpApproxFb         |> fbToSg data.viewportSize |> Sg.compile data.runtime signature)
                |> Map.add RenderMode.BaumFFApprox                  (baumFFApproxFb      |> fbToSg data.viewportSize |> Sg.compile data.runtime signature)
                |> Map.add RenderMode.StructuredIrrSampling         (ssIrrApproxFb       |> fbToSg data.viewportSize |> Sg.compile data.runtime signature)
                |> Map.add RenderMode.StructuredSampling            (ssApproxFb          |> fbToSg data.viewportSize |> Sg.compile data.runtime signature)
                |> Map.add RenderMode.CombinedStructuredSampling    (ssCombinedApproxFb  |> fbToSg data.viewportSize |> Sg.compile data.runtime signature)
                |> Map.add RenderMode.FormFactor                    (formFactorFb        |> fbToSg data.viewportSize |> Sg.compile data.runtime signature)

                |> Map.add RenderMode.Compare (compareSg data gtData signature sceneSg diffFrameBuffer |> Sg.compile data.runtime signature)
    *)            
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
