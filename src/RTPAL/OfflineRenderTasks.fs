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


        
    let delaunay =   
                    (fun (api : TaskAPI) ->
                        api.setRenderLight true

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

    let delaunayWithLight =  
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

    let structuredSampling = 
                    (fun (api : TaskAPI) ->
                        api.setRenderMode RenderMode.StructuredSampling

                        for mask in 1 .. 31 do
                            api.ssAPI.setSampleBitmask mask
                            api.render ()
                            api.saveImage () 
                            api.evalAPI.updateEffectList ()
                    )

    let centerPointApprox = 
                    (fun (api : TaskAPI) ->
                        api.setRenderMode RenderMode.CenterPointApprox

                        api.render ()
                        api.saveImage () 
                        api.evalAPI.updateEffectList ()
                    )

    let structuredSampling = 
                    (fun (api : TaskAPI) ->
                        api.setRenderMode RenderMode.StructuredSampling
                        api.setRenderLight true
                        
                        api.ssAPI.setSamples false false false false false true
                        
                        api.ssAPI.sampleLight false 
                        for n in [16; 32; 64; 128] do
                            api.ssAPI.setRandomSampleCount n                                
                            api.render ()
                            api.saveImage () 
                            api.evalAPI.updateEffectList () 
                    )

    let structuredSamplingRandom =  
                    (fun (api : TaskAPI) ->
                        api.setRenderMode RenderMode.StructuredSampling

                        api.ssAPI.setSamples false false false false false true
  
                        api.ssAPI.sampleLight false 
                        for n in [40] do
                            api.ssAPI.setRandomSampleCount n                                
                            api.render ()
                            api.saveImage () 
                            api.evalAPI.updateEffectList ()   
                    )

    let compare =    
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
             
    let tasks = [ centerPointApprox; delaunay; structuredSamplingRandom; ]
    //let tasks = [ structuredSamplingRandom ]

    tasks

