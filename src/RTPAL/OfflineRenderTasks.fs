namespace Render


module OfflineRenderTasks =

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
            setUsePhotometry : bool -> unit

            gtAPI : GroundTruthAPI
            ssAPI : StructuredSamplingAPI
            evalAPI : EvaluationAPI
        }

    
    let offlineRenderTasks : Map<string, (TaskAPI -> unit)> = 
        let taskMap = Map.empty

        let taskMap = taskMap |> Map.add 
                            "AbstractData"
                            (fun (api : TaskAPI) ->
                                api.setRenderMode RenderMode.FormFactor
                                api.render ()
                                api.saveImage ()

                                api.setRenderMode RenderMode.SolidAngle
                                api.render ()
                                api.saveImage ()
                            )

        let taskMap = taskMap |> Map.add 
                            "GroundTruth"
                            (fun (api : TaskAPI) ->

                                api.setRenderMode RenderMode.GroundTruth
                        
                                api.gtAPI.overwriteEstimate true
                                for _ in 1 .. (800 / Config.Light.NUM_SAMPLES) do
                                    api.render ()
                                    api.gtAPI.overwriteEstimate false
                            
                                api.saveImage ()
                            )
                            
        let taskMap = taskMap |> Map.add 
                            "CenterPoint"
                            (fun (api : TaskAPI) ->
                                api.setRenderMode RenderMode.CenterPointApprox
                                api.render ()
                                api.saveImage () 
                                api.evalAPI.updateEffectList ()
                            )

        let taskMap = taskMap |> Map.add 
                            "BaumFormFactor"
                            (fun (api : TaskAPI) ->
                                api.setRenderMode RenderMode.BaumFFApprox
                                api.render ()
                                api.saveImage () 
                                api.evalAPI.updateEffectList ()
                            )

        let taskMap = taskMap |> Map.add 
                            "MRP"
                            (fun (api : TaskAPI) ->
                                api.setRenderMode RenderMode.MRPApprox
                                api.render ()
                                api.saveImage () 
                                api.evalAPI.updateEffectList ()
                            )

        let taskMap = taskMap |> Map.add 
                            "Delaunay"
                            (fun (api : TaskAPI) ->
                                api.setRenderMode RenderMode.DelaunayIrradianceSampling
                                api.render ()
                                api.saveImage () 
                                api.evalAPI.updateEffectList ()
                            )

        let taskMap = taskMap |> Map.add 
                            "StructuredSampling"
                            (fun (api : TaskAPI) ->
                                api.setRenderMode RenderMode.StructuredSampling

                                for mask in 1 .. 31 do
                                    api.ssAPI.setSampleBitmask mask
                                    api.render ()
                                    api.saveImage () 
                                    api.evalAPI.updateEffectList ()
                            )

        let taskMap = taskMap |> Map.add 
                            "StructuredSamplingRandom"
                            (fun (api : TaskAPI) ->
                                api.setRenderMode RenderMode.StructuredSampling

                                api.ssAPI.setSamples false false false false false true
                                
                                for n in [16; 64; 128; 380] do
                                    api.ssAPI.setRandomSampleCount n                                
                                    api.render ()
                                    api.saveImage () 
                                    api.evalAPI.updateEffectList ()
                            
                            )

        let taskMap = taskMap |> Map.add 
                            "StructuredLuminanceSampling"
                            (fun (api : TaskAPI) ->
                                api.setRenderMode RenderMode.StructuredIrrSampling

                                for mask in 1 ..  7 do //31 do
                                    api.ssAPI.setSampleBitmask mask
                                    api.render ()
                                    api.saveImage () 
                                    api.evalAPI.updateEffectList ()
                            )

        let taskMap = taskMap |> Map.add 
                            "StructuredLuminanceSamplingRandom"
                            (fun (api : TaskAPI) ->
                                api.setRenderMode RenderMode.StructuredIrrSampling

                                api.ssAPI.setSamples false false false false false true

                                for n in [16; 64; 128; 380] do
                                    api.ssAPI.setRandomSampleCount n                                
                                    api.render ()
                                    api.saveImage () 
                                    api.evalAPI.updateEffectList ()
                            )


        taskMap

