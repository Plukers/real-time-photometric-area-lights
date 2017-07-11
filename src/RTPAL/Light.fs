namespace Render

module Light = 
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Rendering.Text.PathSegment
    open Aardvark.Base.Camera

    type LightCollection = {
        Lights      : ModRef<    int[]> // Size: Config.NUM_LIGHTS      Holds light indices. -1 means no light
        Vertices    : ModRef<    V3d[]> // Size: Config.VERT_ALL_LIGHT
        NumVertices : ModRef<    int[]> // Size: Config.NUM_LIGHTS
        Indices     : ModRef<    int[]> // Size: Config.MAX_IDX_BUFFER_SIZE_ALL_LIGHT
        NumIndices  : ModRef<    int[]> // Size: Config.NUM_LIGHTS
        Forwards    : ModRef<    V3d[]> // Size: Config.NUM_LIGHTS.     Direction the light is facing, corresponding to normal. Only one normal is needed because a light is a plane
        Ups         : ModRef<    V3d[]> // Size: Config.NUM_LIGHTS.     The up direction of the light, has to be orthonormal to Forward
        Intensities : ModRef< double[]> // Size: Config.NUM_LIGHTS.
        TwoSided    : ModRef<   bool[]> // Size: Config.NUM_LIGHTS.
        Trafos      : ModRef<Trafo3d[]> // Size: Config.NUM_LIGHTS.
        NumLights   : ModRef<    int  > //                              Specifies the number of lights. Indicates the next free index in Lights
        NextFree    : ModRef<    int  > //                              Indicates the next free address in Lights array, -1 indicates no free space
        IDCounter   : ModRef<    int  > //                              Counts the IDs of the lights, holds the next free ID
    }

    let emptyLightCollection = {    
        Lights      =               -1 |> Array.create Config.NUM_LIGHTS                    |> Mod.init
        Vertices    =         V3d.Zero |> Array.create Config.VERT_ALL_LIGHT                |> Mod.init
        NumVertices =                0 |> Array.create Config.NUM_LIGHTS                    |> Mod.init
        Indices     =                0 |> Array.create Config.MAX_IDX_BUFFER_SIZE_ALL_LIGHT |> Mod.init
        NumIndices  =                0 |> Array.create Config.NUM_LIGHTS                    |> Mod.init
        Forwards    =         V3d.Zero |> Array.create Config.NUM_LIGHTS                    |> Mod.init
        Ups         =         V3d.Zero |> Array.create Config.NUM_LIGHTS                    |> Mod.init                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
        Intensities =              0.0 |> Array.create Config.NUM_LIGHTS                    |> Mod.init
        TwoSided    =            false |> Array.create Config.NUM_LIGHTS                    |> Mod.init
        Trafos      = Trafo3d.Identity |> Array.create Config.NUM_LIGHTS                    |> Mod.init
        NumLights   =                0 |>                                                      Mod.init    
        NextFree    =                0 |>                                                      Mod.init  
        IDCounter   =                0 |>                                                      Mod.init  
    }

    // Registers a new light and returns the ID of the new light
    // Register means: Update NumLights, Lights, NextFree, IDCounter
    // If light could not be registers (no space left) -1 is returned
    let private registerLight (lc : LightCollection) = 

        // fetch next free storage address in light collection
        let cAddr  = (lc.NextFree |> Mod.force)

        if cAddr = -1 then 
            -1
        else
        
            // get the ID of the light and update it for the next
            let lightID    = (lc.IDCounter |> Mod.force)
            transact(fun() -> lightID + 1 |> Mod.change lc.IDCounter) 
            
            // add the index of the light to the lights array
            let lights = (lc.Lights |> Mod.force)
            lights.[cAddr] <- lightID;
            transact(fun() -> lights |> Mod.change lc.Lights)

            // update the number of lights
            let numLights = (lc.NumLights |> Mod.force)
            transact(fun() -> numLights + 1 |> Mod.change lc.NumLights)

            let nextFree = 
                let mutable found = -1

                for i in 0 .. lights.Length - 1 do
                    if Array.get lights i = -1 then
                        found <- i

                found
            transact(fun() -> nextFree |> Mod.change lc.NextFree)



            lightID

    // Adds a new square light to the given light collection
    // Returns the updated light collection and the id of the added light
    // If the light could not be added because there is no space left, the index -1 is returned
    let addSquareLight (intensity : float) (twoSided : bool) (lc : LightCollection) =

        let lightID = registerLight lc

        if lightID = -1 then 
            (lc, -1)
        else
       

            (lc, lightID)

    // Data of all ights
    (*
    let private lightsVertices   = V3d.Zero |> Array.create Config.VERT_ALL_LIGHT
    let private lightNumVertices = 0        |> Array.create Config.NUM_LIGHTS
    let private lightsIndices    = 0        |> Array.create Config.VERT_ALL_LIGHT
    let private lightNumIndices  = 0        |> Array.create Config.NUM_LIGHTS
    *)
    type LightStructure = {
        Vertices      : ModRef<V3d[]> //Arr<N<Config.VERT_PER_LIGHT>, V3d>
        NumOfVertices : IMod<int>
        Forward       : V3d // direction the light is facing, corresponding to normal. Only one normal is needed because a light is a plane
        Up            : V3d // the up direction of the light, has to be orthonormal to Forward
    }

    // Represents a polygon light
    // A light is plane with a shape
    type Light = {
        BaseStruct    : LightStructure
        TransStruct   : LightStructure
        IdxArray      : Arr<N<Config.MAX_IDX_BUFFER_SIZE_PER_LIGHT>, int>
        NumOfIndices  : IMod<int>
        Intensity     : ModRef<double>
        TwoSided      : ModRef<bool>
        Trafo         : ModRef<Trafo3d>
    } 

    let squareLight (intensity : float) (twoSided : bool) = 
        let vertices = Arr<N<Config.VERT_PER_LIGHT>, V3d>()
        vertices.[0] <- V3d(0.0, -0.5, -0.5)
        vertices.[1] <- V3d(0.0,  0.5, -0.5)
        vertices.[2] <- V3d(0.0,  0.5,  0.5)
        vertices.[3] <- V3d(0.0, -0.5,  0.5)

        let indices = Arr<N<Config.MAX_IDX_BUFFER_SIZE_PER_LIGHT>, int>()
        indices.[0] <- 0
        indices.[1] <- 1
        indices.[2] <- 2
        indices.[3] <- 0
        indices.[4] <- 2
        indices.[5] <- 3

        let structure = {
            Vertices      = vertices
            NumOfVertices = 4 |> Mod.init
            Forward       = V3d(1.0, 0.0, 0.0)
            Up            = V3d(0.0, 0.0, 1.0)
        }

        {
            BaseStruct    = structure
            TransStruct   = structure
            IdxArray      = indices
            NumOfIndices  = 6 |> Mod.init
            Intensity     = intensity |> Mod.init
            TwoSided      = twoSided |> Mod.init
            Trafo         = Trafo3d.Identity |> Mod.init
        } 

    module Effect =
        open FShade
        open Aardvark.Base.Rendering

        type UniformScope with
            member uniform.Lights           : Arr<N<Config.VERT_ALL_LIGHT>, V3d> = uniform?Lights
            member uniform.LightIdx         : Arr<N<Config.VERT_ALL_LIGHT>, V3d> = uniform?LightIdx
            member uniform.Forward          : Arr<N<Config.NUM_LIGHTS>,     V3d> = uniform?Forward
            member uniform.Up               : Arr<N<Config.NUM_LIGHTS>,     V3d> = uniform?Up
            member uniform.NumOfVertices    : Arr<N<Config.NUM_LIGHTS>,     int> = uniform?NumOfVertices
            member uniform.Intensities      : Arr<N<Config.NUM_LIGHTS>,  double> = uniform?Intensities
            member uniform.TwoSided         : Arr<N<Config.NUM_LIGHTS>,    bool> = uniform?TwoSided
            member uniform.NumberOfLights   :                                int = uniform?NumberOfLights

    module Sg = 
        open System
        open Aardvark.Base
        open Aardvark.Base.Rendering
        open Aardvark.SceneGraph

        let createLightSg ( light : Light ) sg =

            let lightGeometry = 
                IndexedGeometry(
                    Mode = IndexedGeometryMode.TriangleList,
                    IndexArray = light.IdxArray.ToArray(light.IdxArray.Length),
                    IndexedAttributes =
                        SymDict.ofList [-
                            DefaultSemantic.Positions, light.BaseStruct.Vertices.ToArray(light.BaseStruct.Vertices.Length) :> Array
                            DefaultSemantic.Colors, [| 
                                    for i in 1 .. light.BaseStruct.Vertices.Length do
                                        yield C4b.White
                                |] :> Array
                            DefaultSemantic.Normals, [| 
                                    for i in 1 .. light.BaseStruct.Vertices.Length do
                                        yield light.BaseStruct.Forward
                                |] :> Array
                        ]
                )
            
            let lightSg = lightGeometry 
                            |> Sg.ofIndexedGeometry 
                            |> Sg.trafo light.Trafo
                            |> Sg.effect [
                                    DefaultSurfaces.trafo |> toEffect
                                    DefaultSurfaces.vertexColor |> toEffect
                                ]
                            |> sg
                            
            lightSg  
                               

        let createCollectiveLightSg ( lights : Light[] ) sg =
            for l in lights do
                createLightSg l sg
            sg

        let setLightUniforms (lights : Light []) sg =
  
  (*
            let vert = lights |> Array.map (fun l -> l.Intensity :> IMod<_>)

            let va = vert |> Mod.mapN (fun xx -> xx |> Array.ofSeq)


            let vert = lights |> Array.map (fun l -> l.BaseStruct.Vertices :> IMod<_>)

            let va = vert |> Mod.mapN (fun xx -> xx |> Array.ofSeq)
            
            BaseStruct
*)

            let lightsVertices =
                let vert = 
                    [
                        for l in lights do
                            for v in l.TransStruct.Vertices do
                               yield v
                    ]

                Mod.constant (List.toArray vert)
                
            let proj f = 
                lights |> Array.map f 

            let collect (ms : IMod<'a>[]) : IMod<'a[]> =
                Mod.custom ( fun a -> 
                    [|
                        for m in ms do
                            let v = m.GetValue a
                            yield v
                    |] 
                )                 

            sg
                |> Sg.uniform "Lights"         ( lightsVertices                                          )
                |> Sg.uniform "NumOfVertices"  ( proj (fun x -> x.TransStruct.NumOfVertices)        |> Mod.constant )
                |> Sg.uniform "TwoSided"       ( proj (fun x -> x.TwoSided  :> IMod<_>) |> collect      )
                |> Sg.uniform "Intensities"    ( proj (fun x -> x.Intensity :> IMod<_>) |> collect      )
                |> Sg.uniform "NumberOfLights" (Config.NUM_LIGHTS                       |> Mod.constant )
