namespace Render

(*
    Ground Truth Rendering Effect
    Single Bounce Path Tracing
*)
module EffectGT = 
    open System

    open FShade
    open FShade.Imperative
    open Aardvark.Base
    open Aardvark.Base.Rendering

    open Utils.HaltonSequence
    open Light.Effect
    open EffectUtils
    open PhotometricLight
    open Light
    
    type GTVertex = {
        [<Position>]        pos     : V4d
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
        [<FragCoord>]       fc      : V4d
    }        
    
    let groundTruthLighting (v : GTVertex) = 
        fragment {

            let P = v.wp.XYZ

            let t2w = v.n |> Vec.normalize |> basisFrisvad 
            let w2t = t2w |> Mat.transpose
            
            // Compute a jitter
            let jitter = (fast32Hash v.fc.XYZ).XY              

            let mutable illumination = V4d.Zero
            
            // Iterate over Samples
            for sIdx in 0 .. Config.NUM_SAMPLES - 1 do
                   
                let u1 = 
                    let x = jitter.X + uniform.HaltonSamples.[sIdx].X
                    x - Math.Floor(x)

                let u2 = 
                    let x = jitter.Y + uniform.HaltonSamples.[sIdx].Y
                    x - Math.Floor(x)

                // let m = BRDF_GGX.sampleGGX u1 u2 alpha                
                // let i = sampleHemisphere u1 u2
                let i = cosineSampleHemisphere u1 u2   

                //let pdf = i.Z / PI  

                for addr in 0 .. (Config.NUM_LIGHTS - 1) do 
                    match uniform.Lights.[addr] with
                    | -1 -> ()
                    |  _ ->                        
                        let vAddr = addr * Config.VERT_PER_LIGHT
                        let iAddr = addr * Config.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT
                        
                        for iIdx in iAddr .. Config.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT .. (iAddr + uniform.LNumPatchIndices.[addr] - 1) do

                            let mutable vt = Arr<N<Config.MAX_PATCH_SIZE>, V3d>() 
                            
                            for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                                let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                                vt.[vtc] <- w2t * (uniform.LVertices.[vtcAddr] - P)
     
                            ////////////////////////////////////////////////////////
                            
                            let hitLight = 
                                match uniform.LBaseComponents.[addr] with
                                | bt when bt = Light.LIGHT_BASE_TYPE_TRIANGLE ->                                 
                                    let t = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[1] vt.[2]
                                    t > 1e-8
                                | bt when bt = Light.LIGHT_BASE_TYPE_SQUARE -> 
                                    let t1 = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[1] vt.[2]
                                    let t2 = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[2] vt.[3]
                                    t1 > 1e-8 || t2 > 1e-8
                                | _ -> false         
                                    
                            if hitLight then
                                
                                let worldI = t2w * -i

                                let dotOut = max 1e-5 (abs (Vec.dot worldI uniform.LForwards.[addr]))
                                
                                let irr = (getPhotometricIntensity worldI uniform.LForwards.[addr]  uniform.LUps.[addr]) / (uniform.LAreas.[addr] * dotOut)

                                if irr > 0.0 then 

                                    illumination <-
                                        //let brdf = v.c / PI 
                                        illumination + irr * v.c//(brdf / pdf) * i.Z                            
                                    ()                            
                             
                            ////////////////////////////////////////////////////////
                        ()       
                ()

            illumination <- illumination / V4d(Config.NUM_SAMPLES);

            let alpha = 1.0 / (float)(uniform.FrameCount)

            return V4d(illumination.XYZ, alpha)
            }


    module Rendering = 

        open Aardvark.SceneGraph
        open Aardvark.Base.Incremental
        open Aardvark.Base.RenderTask

        open RenderInterop
        open Utils
        open Utils.Sg

        type GroundTruthData = {
            haltonSequence : ModRef<V2d[]>
            clear : ModRef<bool>
            frameCount : ModRef<int>
            updateGroundTruth : IMod<bool>
            }

        let initGTData (m : MRenderState) =
            {
                haltonSequence = ModRef(HaltonSequence.init)
                clear = ModRef(false) 
                frameCount = ModRef(0)
                updateGroundTruth = m.updateGroundTruth
            }

        let initGTData' (update : IMod<bool>) =
            {
                haltonSequence = ModRef(HaltonSequence.init)
                clear = ModRef(false) 
                frameCount = ModRef(0)
                updateGroundTruth = update
            }

        
        let private renderToColorWithoutClear (size : IMod<V2i>) (task : IRenderTask) =
                let sem = (Set.singleton DefaultSemantic.Colors)
                let runtime = task.Runtime.Value
                let signature = task.FramebufferSignature.Value
            
                let fbo = runtime.CreateFramebuffer(signature, sem, size)
                
                let res = new SequentialRenderTask([|task|]) |> renderTo fbo
                sem |> Seq.map (fun k -> k, getResult k res) |> Map.ofSeq |> Map.find DefaultSemantic.Colors


        let private basicRenderTask (data : RenderData) (gtData : GroundTruthData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            
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
                            groundTruthLighting |> toEffect 
                            EffectUtils.effectClearNaN |> toEffect
                        ]
                    |> Sg.uniform "HaltonSamples" gtData.haltonSequence
                    |> Sg.uniform "FrameCount" gtData.frameCount
                    |> Sg.compile data.runtime signature
                    |> RenderTask.renderToColor data.viewportSize
              
            Sg.fullscreenQuad data.viewportSize
                |> Sg.blendMode mode
                |> setupFbEffects []
                |> Sg.texture DefaultSemantic.DiffuseColorTexture iterationRender
                |> Sg.compile data.runtime signature

        let groundTruthFb (data : RenderData) (gtData : GroundTruthData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            basicRenderTask data gtData signature sceneSg
            |> renderToColorWithoutClear data.viewportSize
            

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
                 Mod.map2  (
                    fun update mode ->
                        if not update then
                            false
                        else
                            match mode with
                            | RenderMode.GroundTruth -> true
                            | RenderMode.Compare     -> true
                            | _ -> false
                    ) gtData.updateGroundTruth data.mode


            let update() = 
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
                