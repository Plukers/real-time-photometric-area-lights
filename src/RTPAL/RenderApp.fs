
module RenderApp
    
open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.Base.Incremental.Operators
open Aardvark.Base.RenderTask

open Aardvark.Application
open Aardvark.Application.WinForms

open Aardvark.Service

    
open Aardvark.UI
open Aardvark.UI.Html.SemUi
open Aardvark.UI.Combinators

open System.Windows.Forms

    
open Aardvark.SceneGraph
open Aardvark.SceneGraph.IO
open Aardvark.SceneGraph.RuntimeSgExtensions
open Aardvark.Base.Rendering

open Aardvark.Data.Photometry
    
open Aardvark.Rendering.GL
open Aardvark.UI.Chromium
open Aardvark.UI.Operators
    

open Utils
open Light
open RenderInterop
    
open EffectGT.Rendering
open EffectApMRP.Rendering
open EffectApStructuredSampling.Rendering
open EffectCompare.Rendering

open RenderState

open OfflineRenderTasks

open Rendering.Render


let setupOfflineRendering (app : OpenGlApplication) (m : MRenderState) (sceneSg : ISg) = 
    

    let viewportSize = 
        m.offlineCamera |> Mod.map(fun cam ->
            match cam with 
            | OfflineCamera.Evaluation -> V2i(2048, 2048)
            | _ -> V2i(1920, 1080)
        )

    let view = 
        m.offlineCamera |> Mod.map(fun cam ->
            match cam with 
            | OfflineCamera.Camera1 ->
                new CameraView (
                    V3d(0.0, 0.0, 1.0),
                    V3d(4.67545488218773, -6.2681695650426, 8.20436584604461),
                    V3d(-0.173032163783345, 0.549018034530654, -0.817703533107539),
                    V3d(-0.245794443827743, 0.779887273546608, 0.57563958510812),
                    V3d(0.953753092618768, 0.300591148106753, 0.0)
                )
            | OfflineCamera.Camera2 ->
                new CameraView (
                    V3d(0.0, 0.0, 1.0),
                    V3d(-0.308925339524174, -3.5006080422878, 3.42414845522788),
                    V3d(-0.382990393911838, 0.67917950957668, -0.626125827563786),
                    V3d(-0.307545724403983, 0.545389016522395, 0.77972203255876),
                    V3d(0.871053376993675, 0.491188369597566, 0.0)
                )
            | _ (* Evaluation *) ->
                CameraView.lookAt (V3d(0.0, 0.0, 5.0)) (V3d(0.0, 0.0, -1.0)) V3d.IOO
        )

    let projTrafo =
        Mod.map2(fun cam (v : V2i) ->
            match cam with 
            | OfflineCamera.Evaluation ->
                { 
                    left = -20.0
                    right = 20.0
                    bottom = -20.0
                    top = 20.0
                    near = 0.1
                    far = 20.1
                    isOrtho = true
                } 
                |> Frustum.projTrafo
            | _ ->
                Frustum.perspective 60.0 0.1 100.0 ((float)(v.X) / (float)(v.Y))
                |> Frustum.projTrafo

        ) m.offlineCamera viewportSize
        
    let renderMode = RenderMode.GroundTruth |> Mod.init
    let updateRenderMode mode = 
        transact (fun _ ->
            mode |> Mod.change renderMode 
        )
    let compareMode = RenderMode.GroundTruth |> Mod.init
    let updateCompareMode mode = 
        transact (fun _ ->
            mode |> Mod.change compareMode 
        )

    let toneMap = false |> Mod.init
    let setToneMap tm = 
        transact (fun _ ->
            tm |> Mod.change toneMap 
        )

    let toneMapScale = 0.2 |> Mod.init
    let updateToneMapScale tms = 
        transact (fun _ ->
            tms |> Mod.change toneMapScale
        )

        
    let mutable photometryName = ""
    let photometryData = None |> Mod.init
    let updatePhotometryData name = 
        let photometryPath = Path.combine [__SOURCE_DIRECTORY__;"..";"..";"photometry";name]
        let lightData = LightMeasurementData.FromFile(photometryPath)
        let data = Some(IntensityProfileSampler(lightData))
        
        transact (fun _ ->
            data |> Mod.change photometryData 
        )

        photometryName <- name
           
        
    let usePhotometry = true |> Mod.init
    let setUsePhotometry up = 
        transact (fun _ ->
            up |> Mod.change usePhotometry
        )

    let diffuseExitance = 1.0 |> Mod.init
    let setDiffuseExitance de = 
        transact (fun _ ->
            de |> Mod.change diffuseExitance
        )

    let renderLight = false |> Mod.init
    let setRenderLight rl = 
        transact (fun _ ->
            rl |> Mod.change renderLight
        )

    let skewClipPlane = false |> Mod.init
    let setSkewClipPlane s =
        transact(fun _ ->
            s |> Mod.change skewClipPlane
        )
        
    updatePhotometryData "PERLUCE_42182932.ldt"

    let lightData : Light.Sg.LightSgData = 
        {
            usePhotometry = usePhotometry
            photometricData = photometryData
            diffuseExitance = diffuseExitance
            renderLight = renderLight
        }   
        
    let renderData = 
        {
            runtime = app.Runtime
            dt = 0.0 |> Mod.init
            sceneSg = sceneSg
            view = view
            projTrafo = projTrafo 
            viewportSize = viewportSize
            mouseClickRay = Ray3d() |> Mod.constant
            lights = m.lights |> Mod.force // mod force necessary ? 
            lightData = lightData
            photometricData = photometryData
            mode = renderMode
            compare = compareMode
            toneMap = toneMap
            toneMapScale = toneMapScale
            skewClipPlane = skewClipPlane
        }
                       
    let gtData = initGTData' (true |> Mod.init) (GTSamplingMode.Light |> Mod.init)
    let mrpData = initMRPData m

    let sampleLight = false |> Mod.init
    let updateSampleLight sl = 
        transact (fun _ ->
            sl |> Mod.change sampleLight
        )

    let blendSamples = false |> Mod.init
    let updateBlendSamples bs = 
        transact (fun _ ->
            bs |> Mod.change blendSamples
        )

    let blendDistance = 0.1 |> Mod.init
    let updateBlendDistance bd = 
        transact (fun _ ->
            bd |> Mod.change blendDistance
        )
            
    let sampleCorners       = false |> Mod.init
    let sampleBarycenter    = false |> Mod.init
    let sampleClosest       = false |> Mod.init
    let sampleNorm          = false |> Mod.init
    let sampleMRP           = false |> Mod.init
    let sampleRandom        = false |> Mod.init

    let updateSamples corners barycenter closest norm mrp random = 
        transact (fun _ ->
            corners     |> Mod.change sampleCorners
            barycenter  |> Mod.change sampleBarycenter
            closest     |> Mod.change sampleClosest
            norm        |> Mod.change sampleNorm
            mrp         |> Mod.change sampleMRP
            random      |> Mod.change sampleRandom
        )

    let updateSamplesBitmask mask = 
        let corners    = (mask &&& (1 <<< 0)) <> 0
        let barycenter = (mask &&& (1 <<< 1)) <> 0
        let closest    = (mask &&& (1 <<< 2)) <> 0
        let norm       = (mask &&& (1 <<< 3)) <> 0
        let mrp        = (mask &&& (1 <<< 4)) <> 0
        let random     = (mask &&& (1 <<< 5)) <> 0
        updateSamples corners barycenter closest norm mrp random

    let numOfSRSamples = 8 |> Mod.init
    let updateRandomSampleCount count =
        transact (fun _ -> 
            count |> Mod.change numOfSRSamples
        )

    let ssData = 
        {
            sampleCorners        = sampleCorners
            sampleBarycenter     = sampleBarycenter
            sampleClosest        = sampleClosest
            sampleNorm           = sampleNorm
            sampleMRP            = sampleMRP
            sampleRandom         = sampleRandom
            sampleLight          = sampleLight
            blendSamples         = blendSamples
            blendEasing          = m.blendEasing
            blendDistance        = blendDistance
            numSRSamples         = numOfSRSamples
            SRSWeightScale       = m.SRSWeightScale.value
            TangentApproxDist    = m.TangentApproxDist.value
            SRSWeightScaleIrr    = m.SRSWeightScaleIrr.value
            TangentApproxDistIrr = m.TangentApproxDistIrr.value
            CombinedLerpValue    = m.CombinedSSWeight.value
        }

    let saData = EffectSolidAngle.Rendering.initSolidAngleData' (SolidAngleCompMethod.Square |> Mod.init) 
        
    let (renderTask, _) = Rendering.Render.CreateAndLinkRenderTask renderData gtData mrpData ssData saData
        
    let updateGroundTruth = groundTruthRenderUpdate renderData gtData


    let scSignature =
        app.Runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba32f; samples = 1 }
        ]
    
    let scColor =
        viewportSize |> Mod.map(fun v ->
            app.Runtime.CreateTexture(v, TextureFormat.Rgba32f, 1, 1, 1)
        )  

    let fbo = 
        scColor |> Mod.map(fun c ->
            app.Runtime.CreateFramebuffer(
                scSignature, 
                Map.ofList [
                    DefaultSemantic.Colors, ({ texture = c; slice = 0; level = 0 } :> IFramebufferOutput)
                ])
        )

        
    let activeTrafoLightId = 0        
        
    let numOfRotationSteps = 5
    let angle = (System.Math.PI / 2.0) / float(numOfRotationSteps - 1)

    let translations = [0.1; 1.1; 3.1; 5.1]
        
    let imageFormat = PixFileFormat.Exr 

    let createFileName step height renderModeData mode =
        
        let renderModeData =                 
            match renderModeData with
            | Some rmd -> rmd
            | None ->
                match mode with
                | RenderMode.StructuredIrrSampling -> sprintf "_%s" (EffectApStructuredSampling.Rendering.encodeSettingsForName ssData)
                | RenderMode.StructuredSampling -> sprintf "_%s" (EffectApStructuredSampling.Rendering.encodeSettingsForName ssData)
                | _ -> ""

              
        match step with
        | Some step -> 
            match height with
            | Some height ->
                printfn "Create  File Name for height %f %s" height (sprintf "%s%s_%i_h%f"  (mode.ToString()) renderModeData step height) 
                sprintf "%s%s_%i_h%f"  (mode.ToString()) renderModeData step height
            | None -> sprintf "%s%s_%i"  (mode.ToString()) renderModeData step
        | None -> 
            match height with
            | Some height ->
                printfn "Create  File Name for height %f %s" height (sprintf "%s%s_h%f"  (mode.ToString()) renderModeData height) 
                sprintf "%s%s_h%f"  (mode.ToString()) renderModeData height
            | None -> sprintf "%s%s"  (mode.ToString()) renderModeData



    let doIteration action =
        
        for h in translations do

            let t = Trafo3d.Translation(V3d(0.0, 0.0, h))
            transformLight renderData.lights activeTrafoLightId (t)

            let mutable rotation = Trafo3d.Identity
            let rotationStep = Trafo3d.Rotation(V3d(0.0, angle, 0.0));

            for r in 0 .. numOfRotationSteps - 1 do                 
                action r h

                transformLight renderData.lights activeTrafoLightId (rotationStep)
                rotation <- rotation * rotationStep

            transformLight renderData.lights activeTrafoLightId (rotation.Inverse)


            transformLight renderData.lights activeTrafoLightId (t.Inverse)

            
         
    let resultPath =  Path.combine [__SOURCE_DIRECTORY__;"..";"..";"results"]
    let photometryFiles = 
        System.IO.Directory.GetFiles (Path.combine [__SOURCE_DIRECTORY__;"..";"..";"photometry"])
        |> Array.map System.IO.Path.GetFileName 

    let nl = System.Environment.NewLine
    let writeMetaData path name data = File.writeAllText (Path.combine[path;name]) data 

    let API = {
            setRenderMode = updateRenderMode
            render = fun _ -> renderTask.Run(RenderToken.Empty, fbo |> Mod.force)
            saveImage = fun _ -> () // is overwriten per renderstep
            usePhotometry = setUsePhotometry
            setDiffuseExitance = setDiffuseExitance
            setRenderLight = setRenderLight
            setSkewClipPlane = setSkewClipPlane

            gtAPI = {
                        overwriteEstimate = fun overwrite -> updateGroundTruth overwrite
                    }
            ssAPI = {
                        setSamples = updateSamples
                        setSampleBitmask = updateSamplesBitmask
                        sampleLight = updateSampleLight 
                        setRandomSampleCount = updateRandomSampleCount
                    }                              
            evalAPI = {
                        updateEffectList = fun _ -> () // is overwriten as needed
                    }
        }

    let generateSaveImage step height path = 
        fun (_ : unit) ->            
            let heightPath = Path.combine [path; ( sprintf "h%i" (height * 10.0 |> int))]
            if not (System.IO.Directory.Exists heightPath) then
                System.IO.Directory.CreateDirectory heightPath |> ignore
            app.Runtime.Download(scColor |> Mod.force).SaveAsImage(Path.combine [heightPath; renderData.mode |> Mod.force |> createFileName (Some step) None None], imageFormat);

    let executeTask step height path api (task : TaskAPI -> unit) = 
        task { api with saveImage = generateSaveImage step height path }

    let renderOfflineTask forPhotometry (task : (TaskAPI -> unit)) = 
        async {
            if (m.offlineCamera |> Mod.force) <> OfflineCamera.Evaluation then
                setToneMap true
            else 
                setToneMap false

            if forPhotometry then
                for f in photometryFiles do
                    let dataPath =  Path.combine [resultPath; (System.IO.Path.GetFileNameWithoutExtension f)]
                    if not (System.IO.Directory.Exists dataPath) then
                        System.IO.Directory.CreateDirectory dataPath |> ignore
                    
                    updatePhotometryData f

                    doIteration (fun step height -> task |> executeTask step height dataPath API |> ignore)

            else
                doIteration (fun step height -> task |> executeTask step height resultPath API |> ignore)
        }

    let renderOfflineEvaluationTasks (tasks : (TaskAPI -> unit) list) = 
        async {

            let evaluation = (sprintf "%s_%s" "Evaluation" (System.DateTime.Now.ToString("dd-M-yyyy--HH-mm")))

            let mutable approxList = HashSet.empty<string>

            let API = { API with evalAPI = {
                                                updateEffectList = (fun _ ->
                                                                            renderMode |> Mod.force |> createFileName None None None |> approxList.Add |> ignore
                                                                    )
                                            }}
                
            let resultPath = Path.combine [resultPath; evaluation; "Data"];

            if (m.offlineCamera |> Mod.force) <> OfflineCamera.Evaluation then
                setToneMap true
            else 
                setToneMap false

            for f in photometryFiles do
                let dataPath =  Path.combine [resultPath; (System.IO.Path.GetFileNameWithoutExtension f)]
                if not (System.IO.Directory.Exists dataPath) then
                    System.IO.Directory.CreateDirectory dataPath |> ignore
                    
                updatePhotometryData f

                for t in tasks do
                    doIteration (fun step height -> t |> executeTask step height dataPath API |> ignore)      
                        

            let approxList =
                let mutable l = ""
                let mutable addedFirstApprox = false
                for approx in approxList do
                    if addedFirstApprox then
                        l <- String.concat nl [l; approx ]
                    else
                        l <- approx
                        addedFirstApprox <- true

                l

            writeMetaData resultPath "ApproximationData.txt" approxList

            let evaluateOfflineRender = m.evaluateOfflineRender |> Mod.force
            let tonemapOfflineRender  = m.tonemapOfflineRender  |> Mod.force

            let postprocess = evaluateOfflineRender || tonemapOfflineRender
                
            if postprocess then 
                let mutable scripts = ""
                if evaluateOfflineRender then scripts <- String.concat "" [scripts; "RunEvaluation;"]
                if tonemapOfflineRender then scripts <- String.concat "" [scripts; "RunCustomToneMap;"]


                let command = sprintf "/c matlab.exe -r \"evaluation='%s';heights=%A';%sexit;\" -sd \"%s\" -nodesktop -nosplash"  evaluation translations scripts (Path.combine [__SOURCE_DIRECTORY__;"..";".."])
                System.Diagnostics.Process.Start("cmd", command) |> ignore

        }
            
    let createPhotometryList =
        async {
            let mutable photometryList = sprintf "%i" (photometryFiles.Length)
                        
            for f in photometryFiles do
                photometryList <- String.concat nl [ photometryList; (System.IO.Path.GetFileNameWithoutExtension f) ]

            writeMetaData resultPath "PhotometryData.txt" photometryList
        }

    let createImageTask = 
        m.offlineRenderMode |> Mod.map (fun mode ->
            match mode with
            | OfflineRenderMode.AbstractData    -> renderOfflineTask false abstractTask
            | OfflineRenderMode.GroundTruth     -> renderOfflineTask true groundTruthTask 
            | OfflineRenderMode.PhotometryList  -> createPhotometryList
            | _ (* Approximations *)            -> renderOfflineEvaluationTasks offlineRenderTasks
                    
        )
            
    createImageTask


let setupRendering (app : OpenGlApplication) (viewportSize : V2i) (m : MRenderState) =
        
    let win = app.CreateGameWindow()
        
    win.Title <- "Render"
        
    win.Height <- viewportSize.Y
    win.Width <- viewportSize.X

        
    let sceneSg = 
            m.scenePath 
            |> Mod.map( fun path -> path |> Utils.Assimp.loadFromFile true |> Sg.normalize) 
            |> Sg.dynamic
            |> Sg.scale 20.0

        
    let offlineRenderTask = setupOfflineRendering app m sceneSg


        
    // let view = CameraView.lookAt (V3d(1.0, 0.0, 0.0)) (V3d(-1.0, 0.0, -1.0)) V3d.OOI
        
    let view = 
        CameraView.lookAt (V3d(8.0, 0.0, 3.0)) (V3d(-11.0, 0.0, -1.0)) V3d.OOI
        |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
    let projTrafo =
        Frustum.perspective 60.0 0.1 100.0 ((float)viewportSize.X / (float)viewportSize.Y)
        |> Frustum.projTrafo |> Mod.constant

    //let view =
    //    CameraView.lookAt (V3d(0.0, 0.0, 5.0)) (V3d(0.0, 0.0, -1.0)) V3d.IOO
    //    |> DefaultCameraController.control win.Mouse win.Keyboard win.Time
    //let projTrafo = 
    //    { 
    //        left = -20.0
    //        right = 20.0
    //        bottom = -20.0
    //        top = 20.0
    //        near = 0.1
    //        far = 20.1
    //    } 
    //    |> Frustum.orthoTrafo |> Mod.init
        
    let lightData : Light.Sg.LightSgData = 
        {
            usePhotometry =  m.usePhotometry
            photometricData = m.photometryData
            diffuseExitance = m.diffuseExitance.value
            renderLight = true |> Mod.init
        }   
        
    // let sceneSg = sceneSg |> Light.Sg.addLightCollectionSg (m.lights |> Mod.force) lightData

                
    let gtData = initGTData m 
    let mrpData = initMRPData m
    let ssData = initSSData m

    let saData = EffectSolidAngle.Rendering.initSolidAngleData m
        
    let dt = 0.0 |> Mod.init

    win.UpdateFrame.Add(fun args -> transact (fun _ -> dt.Value <- args.Time) )
            
    let mouseClickedAt = V2d(0) |> Mod.init

    let mouseClickRay = 
        mouseClickedAt 
        |> Mod.map (fun pos -> 
                        let nds = (pos * 2.0 - V2d.II) * V2d(1, -1)

                        let projTrafo = projTrafo |> Mod.force
                        let viewTrafo = (view |> Mod.force).ViewTrafo 

                        let a = V3d(nds.X, nds.Y, 0.0)
                        let b = V3d(nds.X, nds.Y, 1.0)
                        let c = V3d(nds, 0.0)
                        let d = V3d(nds, 1.0)

                        let near = a |> projTrafo.Backward.TransformPosProj |> viewTrafo.Backward.TransformPos
                        let far = b |> projTrafo.Backward.TransformPosProj |> viewTrafo.Backward.TransformPos

                        let rayDir = (far - near).Normalized

                        Ray3d(near, rayDir)
                    )
        

    let renderData = initialRenderData app view projTrafo viewportSize mouseClickRay m dt sceneSg lightData


    let (renderTask, renderFeedback) = Rendering.Render.CreateAndLinkRenderTask renderData gtData mrpData ssData saData
        

    win.RenderTask <- renderTask

    let rtGroundTruthRenderUpdate =
        let updateRT = groundTruthRenderUpdate renderData gtData

        let update (args : OpenTK.FrameEventArgs) =
            updateRT false

        update
        
    win.UpdateFrame.Add(rtGroundTruthRenderUpdate)
    win.UpdateFrame.Add(fpsUpdate renderFeedback)


    win.MouseUp.AddHandler (System.EventHandler<OpenTK.Input.MouseButtonEventArgs>(
                                                    fun s e -> 
                                                        if e.Button = OpenTK.Input.MouseButton.Middle then
                                                            let s = V2i(1920, 1080)
                                                            let pp = PixelPosition(e.X, e.Y, s.X, s.Y)

                                                            transact (fun _ ->
                                                                pp.NormalizedPosition |> Mod.change mouseClickedAt
                                                            )
                                                    ))


    (win, renderFeedback, offlineRenderTask)
        

let update (s : RenderState) (a : Action) =

    match a with     
        | IMPORT_SCENE p -> {s with scenePath = p}
        | IMPORT_PHOTOMETRY p ->
            if System.String.IsNullOrEmpty p = false then 
                let lmd = 
                    try
                        Some(LightMeasurementData.FromFile(p))
                    with
                    | Failure msg -> 
                        printfn "%s" msg
                        None
                
                match lmd with
                | Some v ->  
                    { s with photometryData = Some(IntensityProfileSampler(v)); photometryName = Some(System.IO.Path.GetFileName p) }
                | None -> s
            else 
                s
        | CHANGE_RENDER_MODE mode -> { s with renderMode = mode }    
        | CHANGE_OFFLINE_RENDER_MODE mode -> { s with offlineRenderMode = mode }
        | TOGGLE_OFFLINE_RENDER_EVALUATION -> { s with evaluateOfflineRender = (not s.evaluateOfflineRender) }
        | TOGGLE_OFFLINE_RENDER_TONEMAP -> { s with tonemapOfflineRender = (not s.tonemapOfflineRender) }
        | CHANGE_OFFLINE_CAMERA cam -> {s with offlineCamera = cam}
        | CHANGE_COMPARE mode -> { s with compare = mode }
        | COMPUTED_ERROR (error, brightError, darkError) -> { s with error = error; brightError = brightError; darkError = darkError }
        | OPENED_WINDOW -> s
        | UPDATE_GROUND_TRUTH update -> { s with updateGroundTruth = update }
        | SET_GT_SAMPLING_MODE samplingMode -> { s with gtSamplingMode = samplingMode }
        | SET_SOLID_ANGLE_COMP_METHOD sacm -> { s with solidAngleCompMethod = sacm }
        | CHANGE_LIGHT_TRANSFORM_MODE mode -> { s with lightTransformMode = mode }
        | TOGGLE_USE_PHOTOMETRY -> { s with usePhotometry = (not s.usePhotometry) }
        | CHANGE_DIFFUSE_EXITANCE de -> { s with diffuseExitance = Numeric.update s.diffuseExitance de }
        | TRANSLATE_LIGHT (lightID, dir) ->             
            transformLight s.lights lightID (Trafo3d.Translation(dir))
            s
        | ROTATE_LIGHT (lightID, euler) ->
            transformLight s.lights lightID (Trafo3d.Rotation(euler))
            s
        | SET_MRP_CLOSEST_WEIGHT w ->
            try
                let w = float w 
                let w = clamp 0.0 1.0 w

                let leftSum = 1.0 - w
                let otherSum = s.mrpWeights.Y + s.mrpWeights.Z
                
                let normal     = leftSum * (s.mrpWeights.Y / otherSum)
                let barycenter = leftSum * (s.mrpWeights.Z / otherSum)

                let sum = w + normal + barycenter

                { s with mrpWeights = V3d(w / sum, normal / sum, barycenter / sum) }
            with
            | :? System.FormatException -> s
        | SET_MRP_NORMAL_WEIGHT w ->
            try
                let w = float w 
                let w = clamp 0.0 1.0 w

                let leftSum = 1.0 - w
                let otherSum = s.mrpWeights.X + s.mrpWeights.Z
                
                let closest    = leftSum * (s.mrpWeights.X / otherSum)
                let barycenter = leftSum * (s.mrpWeights.Z / otherSum)

                let sum = closest + w + barycenter

                { s with mrpWeights = V3d(closest / sum, w / sum, barycenter / sum) }
            with
            | :? System.FormatException -> s
        | SET_MRP_BARYCENTER_WEIGHT w ->
            try
                let w = float w 
                let w = clamp 0.0 1.0 w

                let leftSum = 1.0 - w
                let otherSum = s.mrpWeights.X + s.mrpWeights.Y
                
                let closest = leftSum * (s.mrpWeights.X / otherSum)
                let normal  = leftSum * (s.mrpWeights.Y / otherSum)

                let sum = closest + normal + w

                { s with mrpWeights = V3d(closest / sum, normal / sum, w / sum) }
            with
            | :? System.FormatException -> s
        | TOGGLE_SAMPLE_CORNERS -> { s with sampleCorners = (not s.sampleCorners) }
        | TOGGLE_SAMPLE_BARYCENTER -> { s with sampleBarycenter = (not s.sampleBarycenter) }
        | TOGGLE_SAMPLE_CLOSEST -> { s with sampleClosest = (not s.sampleClosest) }
        | TOGGLE_SAMPLE_NORM -> { s with sampleNorm = (not s.sampleNorm) }
        | TOGGLE_SAMPLE_MRP -> { s with sampleMRP = (not s.sampleMRP) }
        | TOGGLE_SAMPLE_RND -> { s with sampleRandom = (not s.sampleRandom) }
        | TOGGLE_BLEND_SAMPLES -> { s with blendSamples = (not s.blendSamples) }
        | TOGGLE_SAMPLE_LIGHT -> { s with sampleLight = (not s.sampleLight) }
        | TOGGLE_BLEND_EASING -> { s with blendEasing = (not s.blendEasing) }
        | CHANGE_BLEND_DIST bd -> { s with blendDistance = Numeric.update s.blendDistance bd}
        | CHANGE_SRS_SAMPLE_NUM nss -> { s with numOfSRSamples = Numeric.update s.numOfSRSamples nss}
        | CHANGE_SRS_WEIGHT_SCALE srss -> { s with SRSWeightScale = Numeric.update s.SRSWeightScale srss }
        | CHANGE_TANGENT_APPROX_DIST tad -> { s with TangentApproxDist = Numeric.update s.TangentApproxDist tad }
        | CHANGE_SRS_WEIGHT_SCALE_IRR srss -> { s with SRSWeightScaleIrr = Numeric.update s.SRSWeightScaleIrr srss}
        | CHANGE_TANGENT_APPROX_DIST_IRR tad -> { s with TangentApproxDistIrr = Numeric.update s.TangentApproxDistIrr tad }
        | CHANGE_COMBINED_WEIGHT t -> { s with CombinedSSWeight = Numeric.update s.CombinedSSWeight t }

        | TOGGLE_SKEW_CLIP_PLANE -> { s with skewClipPlane = (not s.skewClipPlane) }

        | TOGGLE_TONEMAPPING -> { s with toneMap = (not s.toneMap) }
        | CHANGE_TONEMAP_SCALE tms -> { s with toneMapScale = Numeric.update s.toneMapScale tms}

        | RENDER_IMAGES createImageTask ->    
            createImageTask |> Mod.force |> Async.Start
            s

let openFileDialog (form : System.Windows.Forms.Form) =
    let mutable final = ""

    let action : System.Action = 
        new System.Action( fun () -> 
            let d = new System.Windows.Forms.OpenFileDialog()
            if d.ShowDialog() = DialogResult.OK then
                final <- d.FileName
        ) 

    form.Invoke action |> ignore
    final
        
let view (app : OpenGlApplication) (form : System.Windows.Forms.Form) =
        
    let viewFunc (m : MRenderState) =
            
        let (win, renderFeedback, offlineRenderTask) = setupRendering app (V2i(1920, 1080)) m
            
        let openGameWindowAction : System.Action = 
            new System.Action( fun () -> 
                win.Run()
            ) 

        let computeError = (fun _ -> 
            
            let comp = renderFeedback.compareTexture.GetValue()
                            
            let compPixData = app.Runtime.Download(comp |> unbox<_>)
            let downloaded = compPixData.ToPixImage<float32>()
            let data = downloaded.GetMatrix<C4f>()
            //let ec = data.Elements |> Seq.fold ( fun (cs : double) c -> (double c.R) + cs) 0.0

            let mutable ec : double = 0.0

            let mutable brightEcCount = 0
            let mutable brightEc : double = 0.0

            let mutable darkEcCount = 0
            let mutable darkEc : double = 0.0
                
            data.Elements |> Seq.iter ( fun c ->
                ec <- ec + (double c.R)
                    
                if c.G > 0.0f then
                    brightEc <- brightEc + (double c.R)
                    brightEcCount <- brightEcCount + 1
                else
                    darkEc <- darkEc + (double c.R)
                    darkEcCount <- darkEcCount + 1

                )

            let ec = Fun.Sqrt(ec / (double data.Count))

            let brightEc = Fun.Sqrt(brightEc / (double brightEcCount))
            let darkEc = Fun.Sqrt(darkEc / (double darkEcCount))
                 

            COMPUTED_ERROR (ec, brightEc, darkEc)
                                                    
        )
            
        let toggleBox (state : IMod<bool>) (toggle : 'msg) =

            let attributes = 
                amap {
                        yield "type" => "checkbox"
                        yield onChange (fun _ -> toggle)

                        let! check = state
                        if check then
                        yield "checked" => "checked"
                }
                    
            Incremental.input (AttributeMap.ofAMap attributes)
            
        // view
        let semui =
            [ 
                { kind = Stylesheet; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.css" }
                { kind = Script; name = "semui"; url = "https://cdn.jsdelivr.net/semantic-ui/2.2.6/semantic.min.js" }
            ]   

        let activeTrafoLightId = 0                   // TODO make changeable
        let translationStepSize = 0.1                // TODO make changeable
        let rotationStepSize = System.Math.PI / 18.0 // TODO make changelable

        body [attribute "style" "display: flex; flex-direction: row; width: 100%; height: 100%; border: 0; margin: 0; padding: 1rem;"] [

            require semui (
                    div [] [

                        div [ clazz "ui stackable equal width grid" ] [

                            div [ clazz "column" ][ 
                                    button [ clazz "ui button" ; onClick (fun () -> 
                                            form.BeginInvoke openGameWindowAction |> ignore
                                            OPENED_WINDOW
                                        )] [text "Open Window"]

                                    button [ clazz "ui button" ; onClick (fun () -> 
                                            IMPORT_SCENE (openFileDialog form)
                                        )] [text "Load Object"] 

                                    button [ clazz "ui button" ; onClick (fun () -> 
                                            IMPORT_PHOTOMETRY (openFileDialog form)
                                        )] [text "Load Photometric Data"]        


                                                
                                ]
                        ]

                        div [ clazz "ui stackable two column vertically divided grid container" ] [
                            div [ clazz "row" ] [
                                div [ clazz "column" ] [ 
                                    div [ clazz "ui segment" ] [
                                        Incremental.div (AttributeMap.ofList []) (
                                            alist {     
                                                let! sceneName = m.scenePath
                                                yield p[] [ text ("Loaded Object: " + System.IO.Path.GetFileName(sceneName)) ]

                                                let! photometryName = m.photometryName
                                                match photometryName with
                                                | Some name -> yield p[] [ text ("Loaded Photometry: " + name)]
                                                | None -> ()

                                                let! fps = renderFeedback.fps
                                                yield p [] [ text ("FPS: " + (sprintf "%.2f" fps))]
                                            })

                                        div [ clazz "ui divider"] []

                                        div [ clazz "ui buttons"] [
                                            button [ clazz "ui button"; onClick (fun () -> CHANGE_LIGHT_TRANSFORM_MODE Translate) ] [ text "Translate" ]
                                            div [ clazz "or" ] []
                                            button [ clazz "ui button"; onClick (fun () -> CHANGE_LIGHT_TRANSFORM_MODE Rotate) ] [ text "Rotate" ]
                                        ]

                                            

                                        Incremental.div (AttributeMap.ofList [clazz "ui icon buttons"]) (
                                            alist {
                                                let! mode = m.lightTransformMode
                                                    
                                                yield   button [clazz "ui button"; onClick (fun () -> 
                                                                match mode with 
                                                                | Translate ->
                                                                    TRANSLATE_LIGHT (activeTrafoLightId, V3d(0.0, translationStepSize, 0.0)) 
                                                                | Rotate ->
                                                                    ROTATE_LIGHT (activeTrafoLightId, V3d(0.0, 0.0, rotationStepSize)) 
                                                            )] [
                                                            i [ clazz "arrow left icon"][]
                                                        ]
                                                yield   button [clazz "ui button"; onClick (fun () -> 
                                                                match mode with 
                                                                | Translate ->
                                                                    TRANSLATE_LIGHT (activeTrafoLightId, V3d(-translationStepSize, 0.0, 0.0))
                                                                | Rotate ->
                                                                    ROTATE_LIGHT (activeTrafoLightId, V3d(0.0, rotationStepSize, 0.0)) 
                                                            )] [
                                                            i [ clazz "arrow down icon"][]
                                                        ]
                                                yield   button [clazz "ui button"; onClick (fun () -> 
                                                                match mode with 
                                                                | Translate ->
                                                                    TRANSLATE_LIGHT (activeTrafoLightId, V3d(translationStepSize, 0.0, 0.0))
                                                                | Rotate ->
                                                                    ROTATE_LIGHT (activeTrafoLightId, V3d(0.0, -rotationStepSize, 0.0))
                                                            )] [
                                                            i [ clazz "arrow up icon"][]
                                                        ]
                                                yield   button [clazz "ui button"; onClick (fun () -> 
                                                                match mode with 
                                                                | Translate ->
                                                                    TRANSLATE_LIGHT (activeTrafoLightId, V3d(0.0, -translationStepSize, 0.0))
                                                                | Rotate ->
                                                                    ROTATE_LIGHT (activeTrafoLightId, V3d(0.0, 0.0, -rotationStepSize)) 
                                                            )] [
                                                            i [ clazz "arrow right icon"][]
                                                        ]
                                            }
                                        )
                                        text " "
                                        Incremental.div (AttributeMap.ofList [clazz "ui icon buttons"]) (
                                            alist {      
                                                let! mode = m.lightTransformMode

                                                match mode with
                                                | Translate ->
                                                    yield   button [clazz "ui button"; onClick (fun () -> 
                                                                TRANSLATE_LIGHT (activeTrafoLightId, V3d(0.0, 0.0, -translationStepSize)) 
                                                            )] [
                                                                i [ clazz "chevron down icon"][]
                                                            ]
                                                    yield   button [clazz "ui button"; onClick (fun () -> 
                                                                TRANSLATE_LIGHT (activeTrafoLightId, V3d(0.0, 0.0, translationStepSize)) 
                                                            )] [
                                                                i [ clazz "chevron up icon"][]
                                                            ]
                                                | Rotate -> ()
                                            })  

                                        div [ clazz "ui divider"] []

                                        toggleBox m.toneMap TOGGLE_TONEMAPPING    
                                        text "Tonemapping"
                                        br[]                                                
                                        div [clazz "ui input"] [ Numeric.view' [InputBox] m.toneMapScale |> UI.map CHANGE_TONEMAP_SCALE ]
                                        br[]

                                        toggleBox m.usePhotometry TOGGLE_USE_PHOTOMETRY      
                                        text "Use Photometry"                                                    
                                        br[] 

                                        text "Diffuse Exitance"
                                        div [clazz "ui input"] [ Numeric.view' [InputBox] m.diffuseExitance |> UI.map CHANGE_DIFFUSE_EXITANCE ]

                                    ]

                                    div [ clazz "ui segment" ] [

                                        p [style "font-weight: bold;"] [ 
                                            text ("Offline Rendering")
                                        ]         
                                        
                                        p [] [
                                            dropDown m.offlineRenderMode (fun mode -> CHANGE_OFFLINE_RENDER_MODE mode)
                                        ]

                                        p [] [
                                            text "Camera"                                                    
                                            br[] 
                                            dropDown m.offlineCamera (fun cam -> CHANGE_OFFLINE_CAMERA cam)
                                        ]

                                        toggleBox m.evaluateOfflineRender TOGGLE_OFFLINE_RENDER_EVALUATION      
                                        text "Evaluate (requires Matlab)"                                                    
                                        br[] 

                                        toggleBox m.tonemapOfflineRender TOGGLE_OFFLINE_RENDER_TONEMAP      
                                        text "Tonemap (requires Matlab)"                                                    
                                        br[]                                             

                                        button [ clazz "ui button" ; onClick (fun () -> 
                                            RENDER_IMAGES (offlineRenderTask)
                                        )] [text "Render Offline"]    

                                        // m.boxScale |> Mod.map string |> Incremental.text
                                    ] 
                                ]  

                                div [ clazz "column" ] [ 

                                    div [ clazz "ui segment" ] [
                                        Incremental.div (AttributeMap.ofList []) (
                                            alist {
                                                yield dropDown m.renderMode (fun mode -> CHANGE_RENDER_MODE mode)
                                            }
                                        )

                                        div [ clazz "ui divider"] []

                                        Incremental.div (AttributeMap.ofList []) (
                                            alist {
                                                let! mode = m.renderMode

                                                if mode = RenderMode.GroundTruth || mode = RenderMode.Compare then

                                                    let! updateGT = m.updateGroundTruth

                                                    yield p[][
                                                        if updateGT then
                                                            yield button [ clazz "ui button" ; onClick (fun () -> 
                                                                            UPDATE_GROUND_TRUTH false
                                                                        )] [text "Pause Update"]
                                                        else 
                                                            yield button [ clazz "ui button" ; onClick (fun () -> 
                                                                            UPDATE_GROUND_TRUTH true
                                                                        )] [text "Continue Update"]
                                                    ]

                                                    yield p [] [
                                                        yield text "Sampling Mode"
                                                        yield br[]
                                                        yield dropDown m.gtSamplingMode (fun mode -> SET_GT_SAMPLING_MODE mode)
                                                    ]
                                                        
                                                    let! fc = renderFeedback.frameCount   
                                                    yield p [] [ text ("Num Samples: " + string (fc * Config.Light.NUM_SAMPLES))]

                                                    if updateGT then
                                                        let! fps = renderFeedback.fps
                                                        yield p [] [ text ("Samples/Second: " + (sprintf "%.2f" (fps * (float)Config.Light.NUM_SAMPLES)))]

                                                                                                               
                                            }
                                        )

                                        Incremental.div (AttributeMap.ofList []) (
                                            alist {
                                                let! mode = m.renderMode

                                                if mode = RenderMode.Compare then
                                                    yield div [ clazz "ui divider"] []
                                        
                                                    let! c = m.compare

                                                    if c = RenderMode.Compare then
                                                        yield p [ clazz "ui label red" ] [ text ("Render mode Compare cannot be compared.") ]
                                                        yield div [ clazz "ui divider"] []
                         
                                                    yield p [] [ text ("Compare Ground Truth with")]
                                                    yield p [] [ dropDown m.compare (fun mode -> CHANGE_COMPARE mode) ]
                                                                                                
                                            }
                                        )

                                        Incremental.div (AttributeMap.ofList []) (
                                            alist {
                                                let! mode = m.renderMode

                                                if mode = RenderMode.DelaunayIrradianceSampling || mode = RenderMode.DelaunayNoFlipIrradianceSampling then
                                                    yield div [ clazz "ui divider"] []                                        

                                                    yield toggleBox m.sampleMRP TOGGLE_SKEW_CLIP_PLANE       
                                                    yield text "Skew Clip Plane"                                                    
                                                    yield br[]   
                                                                                                
                                            }
                                        )
                                                                                                    

                                        Incremental.div (AttributeMap.ofList []) (
                                            alist {
                                                let! mode = m.renderMode

                                                if mode = RenderMode.SolidAngle then
                                                    yield p [] [
                                                        yield text "Solid Angle Compuation Method"
                                                        yield br[]
                                                        yield dropDown m.solidAngleCompMethod (fun mode -> SET_SOLID_ANGLE_COMP_METHOD mode)
                                                    ]
                                                                                                
                                            }
                                        )

                                        Incremental.div (AttributeMap.ofList []) (
                                            alist {
                                                let! mode = m.renderMode
                                                let! c = m.compare

                                                let ssActive = mode = RenderMode.StructuredSampling || mode = RenderMode.StructuredIrrSampling
                                                let ssCompActive = c = RenderMode.StructuredSampling || c = RenderMode.StructuredIrrSampling

                                                if ssActive || (mode = RenderMode.Compare && ssCompActive) then    
                                                                                                                
                                                    yield p [] [     
                                                        yield toggleBox m.sampleCorners TOGGLE_SAMPLE_CORNERS 
                                                        yield text "Sample Corners"                                                                                               
                                                        yield br[]
                                                            
                                                        yield toggleBox m.sampleBarycenter TOGGLE_SAMPLE_BARYCENTER  
                                                        yield text "Sample Barycenter"                                                      
                                                        yield br[]

                                                        yield toggleBox m.sampleClosest TOGGLE_SAMPLE_CLOSEST  
                                                        yield text "Sample Closest"                     
                                                        yield br[]
                                                            
                                                        yield toggleBox m.sampleNorm TOGGLE_SAMPLE_NORM        
                                                        yield text "Sample Norm"                                                    
                                                        yield br[]        
                                                            
                                                        yield toggleBox m.sampleMRP TOGGLE_SAMPLE_MRP       
                                                        yield text "Sample MRP"                                                    
                                                        yield br[]   

                                                        yield toggleBox m.sampleRandom TOGGLE_SAMPLE_RND      
                                                        yield text "Sample Random"                                                    
                                                        yield br[]  

                                                            
                                                        yield toggleBox m.sampleLight TOGGLE_SAMPLE_LIGHT      
                                                        yield text "Sample Light"                                                    
                                                        yield br[]  
                                                            
                                                    ]

                                                    let! sampleRandom = m.sampleRandom

                                                    if sampleRandom then
                                                        yield p [] [
                                                            yield text "Samples"
                                                            yield div [clazz "ui input"] [ Numeric.view' [InputBox] m.numOfSRSamples |> UI.map CHANGE_SRS_SAMPLE_NUM ]
                                                            yield br[] 
                                                        ]

                                                    //let! blendSamples = m.blendSamples

                                                    //yield p[] [
                                                    //    yield toggleBox m.blendSamples TOGGLE_BLEND_SAMPLES      
                                                    //    yield text "Blend Samples"                                                    
                                                    //    yield br[]  

                                                            

                                                    //    if blendSamples  then
                                                    //        yield toggleBox m.blendEasing TOGGLE_BLEND_EASING      
                                                    //        yield text "Eased Blending"                                                    
                                                    //        yield br[]  

                                                    //        yield text "Blend Distance"
                                                    //        yield div [clazz "ui input"] [ Numeric.view' [InputBox] m.blendDistance |> UI.map CHANGE_BLEND_DIST ]
                                                                
                                                    //]
                                                        
                                                    //if mode = RenderMode.StructuredSampling || c = RenderMode.StructuredSampling then
                                                    //    yield p [] [   
                                                    //        yield p [style "font-weight: bold;"] [ 
                                                    //            text ("Structured Sampling")
                                                    //        ]
                                                                
                                                    //        yield text "Scale factor"
                                                    //        yield div [clazz "ui input"] [ Numeric.view' [InputBox] m.SRSWeightScale |> UI.map CHANGE_SRS_WEIGHT_SCALE ]
                                                    //        yield br[] 

                                                    //        yield text "Tangent Approx Dist"
                                                    //        yield div [clazz "ui input"] [ Numeric.view' [InputBox] m.TangentApproxDist |> UI.map CHANGE_TANGENT_APPROX_DIST ]
                                                    //        yield br[] 
                                                    //    ]

                                                    //if mode = RenderMode.StructuredIrrSampling || c = RenderMode.StructuredIrrSampling then
                                                    //    yield p [] [   
                                                    //        yield p [style "font-weight: bold;"] [ 
                                                    //            text ("Structured Irradiance Sampling")
                                                    //        ]
                                                                
                                                    //        yield text "Scale factor"
                                                    //        yield div [clazz "ui input"] [ Numeric.view' [InputBox] m.SRSWeightScaleIrr |> UI.map CHANGE_SRS_WEIGHT_SCALE_IRR ]
                                                    //        yield br[] 

                                                    //        yield text "Tangent Approx Dist"
                                                    //        yield div [clazz "ui input"] [ Numeric.view' [InputBox] m.TangentApproxDistIrr |> UI.map CHANGE_TANGENT_APPROX_DIST_IRR ]
                                                    //        yield br[] 
                                                    //    ]                                                            
                                                        
                                            }
                                        )

                                        Incremental.div (AttributeMap.ofList []) (
                                            alist {
                                                let! mode = m.renderMode

                                                if mode = RenderMode.Compare then
                                        
                                                    yield div [ clazz "ui divider"] []

                                                    let! error = m.error
                                                    let! brightError = m.brightError
                                                    let! darkError = m.darkError
                                                        
                                                    yield p [style "font-weight: bold;"] [ 
                                                        text ("Error: " + (sprintf "%.5f" error))
                                                        ]

                                                    yield p [] [ 
                                                        text ("Bright: " + (sprintf "%.5f" brightError)) 
                                                        br []
                                                        text ("Dark: " + (sprintf "%.5f" darkError)) 
                                                        ]
                                                    yield p [] [ button [clazz "ui button" ; onClick (fun () -> computeError())] [text "Compute Error"] ]
                                            }
                                        )

                                    ]
                                ]                                  
                            ]
                        ]
                    ]
                        
                )
        ]

    viewFunc

    
let initialState =     

    // Load geometry
    // let geometryFile = Path.combine [__SOURCE_DIRECTORY__;"meshes";"crytek-sponza";"sponza.obj"]
    let geometryFile = Path.combine [__SOURCE_DIRECTORY__;"meshes";"plane.obj"]

    // Setup Lights
    let lc = emptyLightCollection
    let light1 = addRectangleLight lc 1.0 1.0
                
    match light1 with
    | Some lightId ->             
        // let t = Trafo3d.Translation(-8.0, 0.0, -5.0)        
        let t = Trafo3d.Translation(-3.0, 0.0, 0.6)
        transformLight lc lightId t |> ignore
    | None -> ()
        
    let photometryPath = Path.combine [__SOURCE_DIRECTORY__;"..";"..";"photometry";"PERLUCE_42182932.LDT"]
    let lightData = LightMeasurementData.FromFile(photometryPath)
        
        
    let photometryData = Some(IntensityProfileSampler(lightData))    

         

    // initial state
    {            
        lights = lc
        renderMode = RenderMode.DelaunayIrradianceSampling
        updateGroundTruth = true
        usePhotometry = true
        diffuseExitance = {
                            value   = 10.0
                            min     = 0.0
                            max     = 1000.0
                            step    = 1.0
                            format  = "{0:F1}"
                            }  
        offlineRenderMode = OfflineRenderMode.Approximations
        evaluateOfflineRender = true
        tonemapOfflineRender = false
        offlineCamera = OfflineCamera.Evaluation
        gtSamplingMode = GTSamplingMode.Light
        solidAngleCompMethod = SolidAngleCompMethod.Square
        compare = RenderMode.DelaunayIrradianceSampling 
        error = 0.0
        brightError = 0.0
        darkError = 0.0
        scenePath = geometryFile
        photometryName = Some(System.IO.Path.GetFileName photometryPath)
        photometryData = photometryData
        lightTransformMode = Translate
        mrpWeights    = V3d(1.0/3.0, 1.0/3.0, 1.0/3.0)
        sampleCorners    = false
        sampleBarycenter = false
        sampleClosest    = false
        sampleNorm       = false
        sampleMRP        = false
        sampleRandom     = true
        sampleLight      = false
        blendSamples     = false
        blendEasing      = false
        skewClipPlane = true
        blendDistance = {
                            value   = 0.1
                            min     = 0.0
                            max     = 2.0
                            step    = 0.05
                            format  = "{0:F3}"
                            }
        numOfSRSamples   = {
                            value   = 24.0
                            min     = 0.0
                            max     = (float) Config.Light.SS_LIGHT_SAMPLES_ALL_LIGHT
                            step    = 1.0
                            format  = "{0:0}"
                            }
        SRSWeightScale = {
                            value   = 0.0
                            min     = 0.0
                            max     = 1000.0
                            step    = 0.1
                            format  = "{0:F3}"
                            }
        TangentApproxDist = {
                                value   = 1.0
                                min     = 0.0
                                max     = 1000.0
                                step    = 0.5
                                format  = "{0:F3}"
                                }
        SRSWeightScaleIrr = {
                            value   = 0.0
                            min     = 0.0
                            max     = 1000.0
                            step    = 0.1
                            format  = "{0:F3}"
                            }
        TangentApproxDistIrr = {
                                value   = 1.0
                                min     = 0.0
                                max     = 1000.0
                                step    = 0.5
                                format  = "{0:F3}"
                                }    
        CombinedSSWeight = {
                                value   = 0.5
                                min     = 0.0
                                max     = 1.0
                                step    = 0.1
                                format  = "{0:F3}"
                                }              
        toneMap = false
        toneMapScale     = {
                                value   = 0.2
                                min     = 1e-3
                                max     = 10.0
                                step    = 0.001
                                format  = "{0:F3}"
                            }
    }
       

let app (app : OpenGlApplication) (form : System.Windows.Forms.Form) =
    {
        unpersist = Unpersist.instance
        threads = fun model -> ThreadPool.empty
        initial = initialState
        update = update
        view = view app form
    }
