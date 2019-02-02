

(*
Ground Truth Rendering Effect
Single Bounce Path Tracing
*)
module EffectGT

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

open RenderState

type UniformScope with
    member uniform.samplingMode : GTSamplingMode  = uniform?samplingMode 
    
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
        for sIdx in 0 .. Config.Light.NUM_SAMPLES - 1 do
                   
            let u1 = 
                let x = jitter.X + uniform.HaltonSamples.[sIdx].X
                x - Math.Floor(x)

            let u2 = 
                let x = jitter.Y + uniform.HaltonSamples.[sIdx].Y
                x - Math.Floor(x)

                
            let brdf = v.c / PI 
           
            //let i = sampleHemisphere u1 u2                
            //let brdfPDF = 1.0 / PI // uniform spherical sampling

            let i = cosineSampleHemisphere u1 u2                   
            // let brdfPDF = i.Z / (2 * PI) // cosine spherical sampling
            let brdfPDF = i.Z / (2.0 * PI) // cosine spherical sampling


            for addr in 0 .. (Config.Light.NUM_LIGHTS - 1) do 
                match uniform.Lights.[addr] with
                | -1 -> ()
                |  _ ->                        
                    let vAddr = addr * Config.Light.VERT_PER_LIGHT
                    let iAddr = addr * Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT
                        
                    for iIdx in iAddr .. Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT .. (iAddr + uniform.LNumPatchIndices.[addr] - 1) do

                        let mutable vt = Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>() 
                            
                        for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                            let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                            if uniform.samplingMode = GTSamplingMode.BRDF then                                    
                                vt.[vtc] <- w2t * (uniform.LVertices.[vtcAddr] - P)
                            if uniform.samplingMode = GTSamplingMode.Light then
                                vt.[vtc] <- uniform.LVertices.[vtcAddr]
     
                        ////////////////////////////////////////////////////////

                        let mutable hitLight = false
                        let mutable samplePointDir = vt.[0]
                        let mutable samplePointDistSqrd = 1.0

                        let mutable lightPDF = 1.0
                            
                        match uniform.LBaseComponents.[addr] with
                        | bt when bt = Light.LIGHT_BASE_TYPE_TRIANGLE ->    
                            if uniform.samplingMode = GTSamplingMode.BRDF then
                                    
                                let t = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[1] vt.[2]
                                hitLight <- t > 1e-8

                            if uniform.samplingMode = GTSamplingMode.Light then
                                // TODO : Not working with direct light sampling. Fix this by time.

                                let vt02d = vt.[0] |> to2d w2t vt.[0]
                                let vt12d = vt.[1] |> to2d w2t vt.[0]
                                let vt22d = vt.[2] |> to2d w2t vt.[0]

                                let u = vt.[1] - vt.[0]
                                let v = vt.[2] - vt.[0]

                                let s = vt.[0] + u1 * u + u2 * v
                                
                                let rSign = vt02d |> computePointLineDistance2D vt12d vt22d |> signF
                                let sSign = s |> to2d w2t vt.[0] |> computePointLineDistance2D vt12d vt22d |> signF

                                let samplePoint = 
                                    if rSign * sSign > 0 then
                                        s
                                    else
                                        vt.[0] - (s - (vt.[0] + u + v))

                                samplePointDistSqrd <- Vec.lengthSquared samplePoint
                                samplePointDir <- w2t * (samplePoint |> Vec.normalize)

                        | bt when bt = Light.LIGHT_BASE_TYPE_ORTHO_QUAD -> 
                            
                            if uniform.samplingMode = GTSamplingMode.BRDF then
                                let t1 = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[1] vt.[2]
                                let t2 = rayTriangleIntersaction V3d.Zero i vt.[0] vt.[2] vt.[3]
                                hitLight <- t1 > 1e-8 || t2 > 1e-8

                            if uniform.samplingMode = GTSamplingMode.Light then
                                
                                // This generates the samples on the projected light
                                (*
                                let ex = vt.[1] - vt.[0]
                                let ey = vt.[3] - vt.[0]
                                let squad = SphericalQuad.sphQuadInit vt.[0] ex ey P
                                    
                                let samplePoint = (SphericalQuad.sphQuadSample squad u1 u2) - P
                                samplePointDistSqrd <- Vec.lengthSquared samplePoint
                                samplePointDir <- w2t * (samplePoint |> Vec.normalize)

                                lightPDF <- 1.0 / squad.S 
                                *)
                                    
                                // This generates the samples on the light

                                for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                                    let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr                                
                                    vt.[vtc] <- w2t * (uniform.LVertices.[vtcAddr] - P)

                                let samplePoint = vt.[0] + u1 * (vt.[1] - vt.[0]) + u2 * (vt.[3] - vt.[0])
                                samplePointDistSqrd <- Vec.lengthSquared samplePoint
                                samplePointDir <- samplePoint |> Vec.normalize

                                let dotOut = max 1e-9 (abs (Vec.dot (t2w * -samplePointDir) uniform.LForwards.[addr]))

                                // light pdf is only 1 / Area, but the other values are needed for a correct computation
                                lightPDF <- samplePointDistSqrd / (uniform.LAreas.[addr] * dotOut )

                                    

                                // test
                                // lightPDF <- 1.0 / (uniform.LAreas.[addr])
                                    
                        | _ -> ()  
                            

                        if uniform.samplingMode = GTSamplingMode.BRDF then                            
                            if hitLight then
                                
                                let worldI = t2w * -i

                                let dotOut = max 1e-9 (abs (Vec.dot worldI uniform.LForwards.[addr]))
                                
                                let irr = (getPhotometricIntensity worldI uniform.LForwards.[addr]  uniform.LUps.[addr]) / (uniform.LAreas.[addr] * dotOut)

                                    
                                illumination <- illumination + (brdf * i.Z * irr) / brdfPDF                          

                        if uniform.samplingMode = GTSamplingMode.Light then

                            let i = samplePointDir   

                            if i.Z >= 0.0 then

                                let worldI = t2w * -i
                                let dotOut = max 1e-9 (abs (Vec.dot worldI uniform.LForwards.[addr]))

                                let irr = (getPhotometricIntensity (t2w * -i) uniform.LForwards.[addr]  uniform.LUps.[addr]) / (uniform.LAreas.[addr] * dotOut)

                                illumination <- illumination + (brdf * irr * i.Z) / (lightPDF)

                        ////////////////////////////////////////////////////////
                    ()       
            ()

        // illumination <- illumination / V4d(Config.Light.NUM_SAMPLES)

        // let alpha = 1.0 / (float)(uniform.FrameCount)

        illumination <- illumination / V4d(Config.Light.NUM_SAMPLES * uniform.FrameCount)

        let alpha = (float)(uniform.FrameCount - 1) / (float)(uniform.FrameCount)

        return V4d(illumination.XYZ, alpha)
        }



let private SourceTex =
    sampler2d {
        texture uniform?Source
        filter Filter.MinMagLinear
        addressU FShade.WrapMode.Wrap
        addressV FShade.WrapMode.Wrap
    }

let private DestinationTex =
    sampler2d {
        texture uniform?Destination
        filter Filter.MinMagLinear
        addressU FShade.WrapMode.Wrap
        addressV FShade.WrapMode.Wrap
    }

type BlendVertex = {
    [<TexCoord>] tc : V2d
} 

let blend (v : BlendVertex) = 
    fragment {   
            
        let S = SourceTex.Sample v.tc
        let D = DestinationTex.Sample v.tc

        if uniform?clear then
            return S
        else
            let frameCount = (float)(uniform.FrameCount)
            let invFrameCount = 1.0 / frameCount

            return invFrameCount * ((frameCount - 1.0) * D + S)             
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
        samplingMode : IMod<GTSamplingMode>
        }

    let initGTData (m : MRenderState) =
        {
            haltonSequence = ModRef(HaltonSequence.init)
            clear = ModRef(false) 
            frameCount = ModRef(0)
            updateGroundTruth = m.updateGroundTruth
            samplingMode = m.gtSamplingMode
        }

    let initGTData' (update : IMod<bool>) (samplingMode : IMod<GTSamplingMode>) =
        {
            haltonSequence = HaltonSequence.init |> Mod.init
            clear = false |> Mod.init
            frameCount = 1 |> Mod.init
            updateGroundTruth = update
            samplingMode = samplingMode
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
                        SourceFactor = BlendFactor.One, 
                        DestinationFactor = BlendFactor.SourceAlpha,
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
                |> Light.Sg.addLightCollectionSg (data.lights) (data.lightData)
                |> Light.Sg.setLightCollectionUniforms data.lights
                |> setupPhotometricData data.photometricData
                |> setupCamera data.view data.projTrafo data.viewportSize 
                |> setUniformUsePhotometry data.lightData.usePhotometry
                |> setUniformDiffuseExitance data.lightData.diffuseExitance
                |> Sg.uniform "samplingMode" (gtData.samplingMode |> Mod.map (fun sm -> sm |> int))
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
            
        let mutable prevLightTrafos : Trafo3d[] = Array.create Config.Light.NUM_LIGHTS Trafo3d.Identity
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

        let mutable prevSamplingMode = gtData.samplingMode |> Mod.force
        let samplingModeDemandsClear =
            gtData.samplingMode |> Mod.map (
                fun mode ->
                    let clear = mode <> prevSamplingMode 
                    prevSamplingMode <- mode
                    clear
                )


        let clearRequired = 
            let required = Mod.map2 (fun l c -> l || c) lightDemandsClear camDemandsClear
            let required = Mod.map2 (fun r o -> r || o) required samplingModeDemandsClear
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
                            gtData.haltonSequence.Value.[Config.Light.NUM_SAMPLES - 1] |> HaltonSequence.next     
                                    
                    gtData.frameCount.Value <- 
                        if clear then
                            1
                        else
                            gtData.frameCount.Value + 1
                        
                    // TODO find better solution than marking outdated
                    if clear then
                        lightDemandsClear.MarkOutdated()
                        camDemandsClear.MarkOutdated()
                        samplingModeDemandsClear.MarkOutdated()
            )

        update
                