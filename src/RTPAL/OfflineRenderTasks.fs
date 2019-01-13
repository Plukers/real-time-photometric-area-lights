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
            usePhotometry : bool -> unit

            setSkewClipPlane : bool -> unit

            setDiffuseExitance : float -> unit

            setRenderLight : bool -> unit

            gtAPI : GroundTruthAPI
            ssAPI : StructuredSamplingAPI
            evalAPI : EvaluationAPI
        }

    
    let offlineRenderTasks : (Map<string, (TaskAPI -> unit)> * string list) = 
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
                                for _ in 1 .. (25000 / Config.Light.NUM_SAMPLES) do
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

                                api.setSkewClipPlane true
                                api.render ()
                                api.saveImage () 
                                api.evalAPI.updateEffectList ()

                                //api.setSkewClipPlane false
                                //api.render ()
                                //api.saveImage () 
                                //api.evalAPI.updateEffectList ()
                            )

        let taskMap = taskMap |> Map.add 
                            "DelaunayWithLight"
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
                            "StructuredSamplingLDR"
                            (fun (api : TaskAPI) ->
                                api.setRenderMode RenderMode.StructuredSampling

                                api.ssAPI.setSamples true true false false false false
                                api.render ()
                                api.saveImage () 
                                api.evalAPI.updateEffectList ()
                            )

        let taskMap = taskMap |> Map.add 
                            "StructuredSamplingRandom"
                            (fun (api : TaskAPI) ->
                                api.setRenderMode RenderMode.StructuredSampling

                                api.ssAPI.setSamples false false false false false true
                                
                                for n in [5; 16; 24] do
                                    api.ssAPI.setRandomSampleCount n                                
                                    api.render ()
                                    api.saveImage () 
                                    api.evalAPI.updateEffectList ()
                            
                            )

        let taskMap = taskMap |> Map.add 
                            "StructuredSamplingSmall"
                            (fun (api : TaskAPI) ->
                                api.setRenderMode RenderMode.StructuredSampling

                                api.ssAPI.setSamples false false false false false true
                                
                                for n in [5] do
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
                                api.setRenderLight true   
                                api.usePhotometry true
                                
                                api.setRenderMode RenderMode.StructuredSampling

                                api.ssAPI.sampleLight true

                                api.ssAPI.setSamples false false false false false true

                                for n in [16; 24; 32; 40; 48; 56; 64; 128; 256] do
                                    api.ssAPI.setRandomSampleCount n                                
                                    api.render ()
                                    api.saveImage () 
                                    api.evalAPI.updateEffectList ()
                            )

        let taskMap = taskMap |> Map.add 
                            "Compare"
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

        let taskMap = taskMap |> Map.add 
                            "Custom"
                            (fun (api : TaskAPI) ->

                                api.setRenderMode RenderMode.GroundTruth
                        
                                api.gtAPI.overwriteEstimate true
                                for _ in 1 .. (20000 / Config.Light.NUM_SAMPLES) do
                                    api.render ()
                                    api.gtAPI.overwriteEstimate false
                            
                                api.saveImage ()
                            )


        (taskMap, [ "Delaunay"; "StructuredSamplingLDR"; "StructuredSamplingRandom" ])
        //(taskMap, [ "DelaunayWithLight"])

