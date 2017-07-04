namespace Render

module Light = 
    open Aardvark.Base
    open Aardvark.Base.Incremental

    // Represents a polygon light
    type Light = {
        Vertices      : Arr<N<Config.VERT_PER_LIGHT>, V3d>
        NumOfVertices : IMod<int>
        Intensity     : ModRef<double>
        TwoSided      : ModRef<bool>
        Trafo         : ModRef<Trafo3d>
    } 

    module Effect =
        open FShade
        open Aardvark.Base.Rendering

        type UniformScope with
            member uniform.Lights           : Arr<N<Config.VERT_ALL_LIGHT>, V3d> = uniform?Lights
            member uniform.NumOfVertices    : Arr<N<Config.NUM_LIGHTS>,     int> = uniform?NumOfVertices
            member uniform.Intensities      : Arr<N<Config.NUM_LIGHTS>,  double> = uniform?Intensities
            member uniform.TwoSided         : Arr<N<Config.NUM_LIGHTS>,    bool> = uniform?TwoSided
            member uniform.NumberOfLights   :                                int = uniform?NumberOfLights

    module Sg = 
        open Aardvark.SceneGraph

        let setLightUniforms (l : Light []) sg =
  
            let lightVertices =
                let vert = 
                    [
                        for light in l do
                            for v in light.Vertices do
                               yield v
                    ]

                Mod.constant (List.toArray vert)
                
            let proj f = 
                l |> Array.map f 

            let collect (ms : IMod<'a>[]) : IMod<'a[]> =
                Mod.custom ( fun a -> 
                    [|
                        for m in ms do
                            let v = m.GetValue a
                            yield v
                    |] 
                )                 

            sg
                |> Sg.uniform "Lights"         ( lightVertices                                          )
                |> Sg.uniform "NumOfVertices"  ( proj (fun x -> x.NumOfVertices)        |> Mod.constant )
                |> Sg.uniform "TwoSided"       ( proj (fun x -> x.TwoSided  :> IMod<_>) |> collect      )
                |> Sg.uniform "Intensities"    ( proj (fun x -> x.Intensity :> IMod<_>) |> collect      )
                |> Sg.uniform "NumberOfLights" (Config.NUM_LIGHTS                       |> Mod.constant )
