namespace Render


module OfflineRenderTasks =

    type GroundTruthAPI = 
        {
            overwriteEstimate : bool -> unit
        }

    type TaskAPI = 
        {
            setRenderMode : RenderMode -> unit
            render : unit -> unit
            saveImage : unit -> unit

            gtAPI : GroundTruthAPI
        }

    
    let offlineRenderTasks : Map<string, (TaskAPI -> unit)> = 
        let taskMap = Map.empty

        let taskMap = taskMap |> Map.add "AbstractData"
                            (fun (api : TaskAPI) ->
                                api.setRenderMode RenderMode.FormFactor
                                api.render ()
                                api.saveImage ()

                                api.setRenderMode RenderMode.SolidAngle
                                api.render ()
                                api.saveImage ()
                            )

        let taskMap = taskMap |> Map.add "GroundTruth"
                            (fun (api : TaskAPI) ->

                                api.setRenderMode RenderMode.GroundTruth
                        
                                api.gtAPI.overwriteEstimate true
                                for _ in 1 .. (50000 / Config.Light.NUM_SAMPLES) do
                                    api.render ()
                                    api.gtAPI.overwriteEstimate false
                            
                                api.saveImage ()
                            )

        let taskMap = taskMap |> Map.add "Delaunay"
                            (fun (api : TaskAPI) ->
                                api.setRenderMode RenderMode.DelaunayIrradianceSampling
                                api.render ()
                                api.saveImage () 
                            )


        taskMap

