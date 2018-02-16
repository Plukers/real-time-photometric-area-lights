﻿namespace Render

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
    
    [<ReflectedDefinition>]
    let private to2d (w2t : M33d) (o : V3d) (p : V3d) = 
        let v = w2t * (p - o)
        V2d(v.X, v.Y)

    [<ReflectedDefinition>]
    let private computePointLineDistance2D (t1 : V2d) (t2 : V2d) (p : V2d) =         
        let n = (t2.Y - t1.Y) * p.X - (t2.X - t1.X) * p.Y + t2.X * t1.Y - t2.Y * t1.X
        let d = sqrt((t2.Y - t1.Y) * (t2.Y - t1.Y) + (t2.X - t1.X) * (t2.X - t1.X))
        n / d     
        
    [<ReflectedDefinition>]
    let private sampleLightSurface (t2w : M33d) (addr : int) (p : V3d) = 

        let i = p |> Vec.normalize  
        let irr = getPhotometricIntensity -(t2w * i) uniform.LForwards.[addr]  uniform.LUps.[addr]      
        let weight = 1.0 / (Vec.lengthSquared p + 1e-9)
        weight * irr * i.Z

    [<ReflectedDefinition>]
    let private signF (v : float) =
        if v > 0.0 then
            1
        elif v < 0.0 then
            -1
        else
            0
  
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

                
                let brdf = v.c / PI 
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

                            let mutable hitLight = false
                            let mutable samplePoint = vt.[0]
                            
                            match uniform.LBaseComponents.[addr] with
                            | bt when bt = Light.LIGHT_BASE_TYPE_TRIANGLE ->                                 
                                let t = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[1] vt.[2]
                                hitLight <- t > 1e-8


                                let vt02d = vt.[0] |> to2d w2t vt.[0]
                                let vt12d = vt.[1] |> to2d w2t vt.[0]
                                let vt22d = vt.[2] |> to2d w2t vt.[0]

                                let u = vt.[1] - vt.[0]
                                let v = vt.[2] - vt.[0]

                                let s = vt.[0] + u1 * u + u2 * v
                                
                                let rSign = vt02d |> computePointLineDistance2D vt12d vt22d |> signF
                                let sSign = s |> to2d w2t vt.[0] |> computePointLineDistance2D vt12d vt22d |> signF

                                if rSign * sSign > 0 then
                                    samplePoint <- s
                                else
                                    samplePoint <- vt.[0] - (s - (vt.[0] + u + v))

                            | bt when bt = Light.LIGHT_BASE_TYPE_SQUARE -> 
                                let t1 = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[1] vt.[2]
                                let t2 = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[2] vt.[3]
                                hitLight <- t1 > 1e-8 || t2 > 1e-8


                                samplePoint <- vt.[0] + u1 * (vt.[1] - vt.[0]) + u2 * (vt.[3] - vt.[0])
                            | _ -> ()  
                            

                            illumination <- illumination + brdf * sampleLightSurface t2w addr samplePoint
                            
                            if hitLight then
                                
                                let worldI = t2w * -i

                                let dotOut = max 1e-5 (abs (Vec.dot worldI uniform.LForwards.[addr]))
                                
                                let irr = (getPhotometricIntensity worldI uniform.LForwards.[addr]  uniform.LUps.[addr]) / (uniform.LAreas.[addr] * dotOut)

                                if irr > 0.0 then 

                                    illumination <- illumination + irr * v.c//(brdf / pdf) * i.Z                            
                                    ()                            
                            
                            ////////////////////////////////////////////////////////
                        ()       
                ()

            illumination <- illumination / V4d(2 * Config.NUM_SAMPLES)

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
                    |> Light.Sg.setLightCollectionUniforms data.lights
                    |> setupPhotometricData data.photometricData
                    |> setupCamera data.view data.projTrafo data.viewportSize 
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


            let update (forceClear : bool) = 
                transact (fun _ -> 
                    if executeUpdate |> Mod.force then
                                        
                        let clear = (clearRequired |> Mod.force) || forceClear
                                        
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
                