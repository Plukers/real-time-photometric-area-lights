module OfflineRenderTasks

open RenderState

type GroundTruthAPI = 
    {
        // If false, ray tracing passes are accumulated
        overwriteEstimate : bool -> unit
    }

type StructuredSamplingAPI =
    {
        // Use : corners barycenter closest norm mrp random
        setSamples : bool -> bool -> bool -> bool -> bool -> bool -> unit

        // Input bitmask
        // Mask 543210: 0-corners 1-barycenter 2-closest 3-norm 4-mrp 5-random
        setSampleBitmask : int -> unit

        // If true, samples are handled as if they were importance sampled from the subtended solid angle of the light
        sampleLight : bool -> unit
            
        // Numbers of random samples to be used
        setRandomSampleCount : int -> unit
    }   

type EvaluationAPI = {
        updateEffectList : unit -> unit
    }
            
type TaskAPI = 
    {
        // Change the shader
        setRenderMode : RenderMode -> unit

        // Render an image with the active shader
        render : unit -> unit

        // Save the rendered image
        saveImage : unit -> unit

        // Use photometry or diffuse emitter
        usePhotometry : bool -> unit

        setSkewClipPlane : bool -> unit

        setDiffuseExitance : float -> unit

        setRenderLight : bool -> unit

        gtAPI : GroundTruthAPI
        ssAPI : StructuredSamplingAPI
        evalAPI : EvaluationAPI
    }



        
let abstractTask =
    (fun (api : TaskAPI) ->
        api.setRenderMode RenderMode.FormFactor
        api.render ()
        api.saveImage ()

        api.setRenderMode RenderMode.SolidAngle
        api.render ()
        api.saveImage ()
    )

let groundTruthTask =
    (fun (api : TaskAPI) ->

        api.setRenderMode RenderMode.GroundTruth
                        
        api.gtAPI.overwriteEstimate true
        for _ in 1 .. (25000 / Config.Light.NUM_SAMPLES) do
            api.render ()
            api.gtAPI.overwriteEstimate false
                            
        api.saveImage ()
    )

    
let offlineRenderTasks : ((TaskAPI -> unit) list) = 

    let taskMap = Map.empty

    let addTask (name : string) (task : TaskAPI -> unit) taskMap =
        (name, taskMap |> Map.add name task)                         
        
        
    let (delaunay, taskMap) =   
        taskMap 
        |> addTask  "Delaunay"
                    (fun (api : TaskAPI) ->
                        api.setRenderMode RenderMode.DelaunayIrradianceSampling

                        api.setSkewClipPlane true

                        api.render ()
                        api.saveImage () 
                        api.evalAPI.updateEffectList ()

                        api.setRenderMode RenderMode.DelaunayNoFlipIrradianceSampling

                        api.render ()
                        api.saveImage () 
                        api.evalAPI.updateEffectList ()
                    )

    let (delaunayWithLight, taskMap) =  
        taskMap 
        |> addTask  "DelaunayWithLight"
                    (fun (api : TaskAPI) ->
                        api.setRenderLight true

                        api.setRenderMode RenderMode.DelaunayIrradianceSampling

                        api.setSkewClipPlane true
                        api.render ()
                        api.saveImage () 
                        api.evalAPI.updateEffectList ()

                        api.setSkewClipPlane false
                        api.render ()
                        api.saveImage () 
                        api.evalAPI.updateEffectList ()
                    )

    let (structuredSampling, taskMap) = 
        taskMap 
        |> addTask  "StructuredSampling"
                    (fun (api : TaskAPI) ->
                        api.setRenderMode RenderMode.StructuredSampling

                        for mask in 1 .. 31 do
                            api.ssAPI.setSampleBitmask mask
                            api.render ()
                            api.saveImage () 
                            api.evalAPI.updateEffectList ()
                    )

    let (structuredSamplingRandom, taskMap) =   
        taskMap 
        |> addTask  "StructuredSamplingRandom"
                    (fun (api : TaskAPI) ->
                        api.setRenderMode RenderMode.StructuredSampling

                        api.ssAPI.setSamples false false false false false true
                                
                        for n in [30; 60] do
                            api.ssAPI.setRandomSampleCount n                                
                            api.render ()
                            api.saveImage () 
                            api.evalAPI.updateEffectList ()                            
                    )

    let (compare, taskMap) =    
        taskMap 
        |> addTask  "Compare"
                    (fun (api : TaskAPI) ->
                            
                        api.setRenderLight true
                        api.setDiffuseExitance 5.0            

                        api.usePhotometry true
                                    
                        api.setRenderMode RenderMode.StructuredSampling

                        api.ssAPI.setSamples true true false false false false
                        api.ssAPI.sampleLight true
                                
                        api.render ()
                        api.saveImage () 
                        api.evalAPI.updateEffectList ()

                        api.setRenderMode RenderMode.GroundTruth
                        
                        api.gtAPI.overwriteEstimate true
                        for _ in 1 .. (20000 / Config.Light.NUM_SAMPLES) do
                            api.render ()
                            api.gtAPI.overwriteEstimate false
                            
                        api.saveImage ()
                        api.evalAPI.updateEffectList ()                                

                    )
             
    let tasks = [ delaunay; structuredSamplingRandom ]

    taskMap
    |> Map.filter (fun key _ -> tasks |> List.contains key) 
    |> Map.toList
    |> List.map snd

