namespace Render

module Light = 
    open System.Collections
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Rendering.Text.PathSegment
    open Aardvark.Base.Camera

    open Utils.PoissonSequence

    [<ReflectedDefinition>] 
    let LIGHT_BASE_TYPE_TRIANGLE = 3

    [<ReflectedDefinition>] 
    let LIGHT_BASE_TYPE_ORTHO_QUAD = 4

    type LightCollection = {
        Lights            : ModRef<       int[]> // Size: Config.NUM_LIGHTS    
        BaseComponents    : ModRef<       int[]> // Size: Config.NUM_LIGHTS   
        Vertices          : ModRef<       V3d[]> // Size: Config.VERT_ALL_LIGHT  Modified as defined by the corresponding trafo.
        NumVertices       : ModRef<       int[]> // Size: Config.NUM_LIGHTS
        RenderIndices     : ModRef<       int[]> // Size: Config.MAX_IDX_BUFFER_SIZE_ALL_LIGHT     Define triangles such that the light can be rendered
        NumRenderIndices  : ModRef<       int[]> // Size: Config.NUM_LIGHTS
        PatchIndices      : ModRef<       int[]> // Size: Config.MAX_IDX_BUFFER_SIZE_ALL_LIGHT     Define triangles or squares for the illumination evaluation
        NumPatchIndices   : ModRef<       int[]> // Size: Config.NUM_LIGHTS
        SamplePoints      : ModRef<       V3d[]> // Size: Config.NUM_ALL_SS_LIGHT_SAMPLES
        Forwards          : ModRef<       V3d[]> // Size: Config.NUM_LIGHTS.     Direction the light is facing, corresponding to normal. Only one normal is needed because a light is a plane.  Modified as defined by the corresponding trafo.
        Ups               : ModRef<       V3d[]> // Size: Config.NUM_LIGHTS.     The up direction of the light, has to be orthonormal to Forward. Modified as defined by the corresponding trafo.
        Areas             : ModRef<    double[]> // Size: Config.NUM_LIGHTS.
        Trafos            : ModRef<   Trafo3d[]> // Size: Config.NUM_LIGHTS.
        NextFreeAddr      : ModRef< Option<int>> //                              Indicates the next free address in Lights array, -1 indicates no free space.
        IDCounter         : ModRef<       int  > //                              Counts the IDs of the lights, holds the next free ID.
        IDToAddr          :   cmap<  int, int  > //                              Maps light IDs to Addr.
    } 

    let emptyLightCollection = {    
        Lights            =                          -1 |> Array.create Config.NUM_LIGHTS                            |> Mod.init
        BaseComponents    =    LIGHT_BASE_TYPE_TRIANGLE |> Array.create Config.NUM_LIGHTS                            |> Mod.init
        Vertices          =                    V3d.Zero |> Array.create Config.VERT_ALL_LIGHT                        |> Mod.init
        NumVertices       =                           0 |> Array.create Config.NUM_LIGHTS                            |> Mod.init
        RenderIndices     =                           0 |> Array.create Config.MAX_RENDER_IDX_BUFFER_SIZE_ALL_LIGHT  |> Mod.init
        NumRenderIndices  =                           0 |> Array.create Config.NUM_LIGHTS                            |> Mod.init
        PatchIndices      =                           0 |> Array.create Config.MAX_PATCH_IDX_BUFFER_SIZE_ALL_LIGHT   |> Mod.init
        NumPatchIndices   =                           0 |> Array.create Config.NUM_LIGHTS                            |> Mod.init
        SamplePoints      =                    V3d.Zero |> Array.create Config.SS_LIGHT_SAMPLES_ALL_LIGHT            |> Mod.init
        Forwards          =                    V3d.Zero |> Array.create Config.NUM_LIGHTS                            |> Mod.init
        Ups               =                    V3d.Zero |> Array.create Config.NUM_LIGHTS                            |> Mod.init  
        Areas             =                         0.0 |> Array.create Config.NUM_LIGHTS                            |> Mod.init
        Trafos            =            Trafo3d.Identity |> Array.create Config.NUM_LIGHTS                            |> Mod.init 
        NextFreeAddr      =               Option.Some 0 |>                                                              Mod.init
        IDCounter         =                           0 |>                                                              Mod.init  
        IDToAddr          =            new cmap<int, int>()
    }

    // Registers a new light and returns the ID of the new light
    // Register means: Updates Lights, NextFree, IDCounter, IDToAddr
    // returns Option of ( array ID, light ID)
    // The collection needs an empty slot
    let private registerLight (lc : LightCollection) = 

        // fetch next free storage address in light collection
        let nextAddr  = lc.NextFreeAddr.Value

        match nextAddr with
        | Some addr ->
            // get the ID of the light and update it for the next
            let lightID    = lc.IDCounter.Value
            lc.IDCounter.Value <- lc.IDCounter.Value + 1
            
            // add the index of the light to the lights array
            lc.Lights.Value.[addr] <- lightID

            lc.NextFreeAddr.Value <-
                let mutable foundAddr = Option.None

                for i in 0 .. lc.Lights.Value.Length - 1 do
                    if Array.get lc.Lights.Value i <> -1 then
                        foundAddr <- Option.Some i

                foundAddr

            lc.IDToAddr.Add(lightID, addr)

            Option.Some (addr, lightID)
        | None ->
            Option.None
        

    // Adds vertices to the light container at the specified address
    // The collection needs an empty slot
    let private addVertices (lc : LightCollection) addr (vertices : V3d[]) =
        vertices.CopyTo(lc.Vertices.Value, addr * Config.VERT_PER_LIGHT)
        lc.NumVertices.Value.[addr] <- vertices.Length

    // Adds render indices to the light container at the specified address
    // The collection needs an empty slot
    let private addRenderIndices (lc : LightCollection) addr (indices : int[]) =
        indices.CopyTo(lc.RenderIndices.Value, addr * Config.MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT)
        lc.NumRenderIndices.Value.[addr] <- indices.Length

    // Adds eval indices to the light container at the specified address
    // The collection needs an empty slot
    let private addPatchIndices (lc : LightCollection) addr (indices : int[]) =
        indices.CopyTo(lc.PatchIndices.Value, addr * Config.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT)
        lc.NumPatchIndices.Value.[addr] <- indices.Length

    // Adds sample points to the light container at the specified address
    // The collection needs an empty slot
    let private addSamplePoints (lc : LightCollection) addr (samplePoints : V3d[]) =
        samplePoints.CopyTo(lc.SamplePoints.Value, addr * Config.SS_LIGHT_SAMPLES_PER_LIGHT)

    // Computes the area of a polygon light
    let private computeArea (vertices : V3d[]) (indices : int[]) (numIndices : int) = 

        let mutable area = 0.0

        for i in 0 .. 3 .. numIndices do
            let i0 = indices.[i]
            let i1 = indices.[i + 1]
            let i2 = indices.[i + 2]

            area <- area + Vec.length (Vec.cross (vertices.[i0] - vertices.[i1]) (vertices.[i2] - vertices.[i1])) / 2.0
            ()

        area

    // Adds a new triangle light to the given light collection
    // Returns the updated light collection and the id of the added light
    // If the light could not be added because there is no space left, no id is returned
    [<System.Obsolete("Some shaders do not support triangle lights at the moment.")>]
    let addTriangleLight (lc : LightCollection) = 
        match lc.NextFreeAddr.Value  with
        | Some nfa ->
            let mutable returnID = Option.None;

            transact (fun _ ->

                match registerLight lc with
                | Some (addr, lightID) ->
                    returnID <- Option.Some lightID

                    let vertices = [|
                            V3d(0.0, -0.5, -0.5)
                            V3d(0.0,  0.5, -0.5)
                            V3d(0.0,  0.0,  1.0)
                        |] 

                    addVertices lc addr vertices
                
                    addRenderIndices lc addr [| 0; 1; 2 |]

                    lc.BaseComponents.Value.[addr] <- LIGHT_BASE_TYPE_TRIANGLE
                    addPatchIndices lc addr [| 0; 1; 2 |]

                    let samplePoints = OfflineStructuredSamplePoints.Triangle.samples 

                    addSamplePoints lc addr (samplePoints |> List.toArray)

                    let vAddr = addr * Config.VERT_PER_LIGHT
                    let iAddr = addr * Config.MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT

                    lc.Forwards.Value.[addr]    <- V3d(1, 0, 0)
                    lc.Ups.Value.[addr]         <- V3d(0, 0, 1)
                    lc.Areas.Value.[addr]       <- computeArea lc.Vertices.Value.[vAddr .. (vAddr + Config.VERT_PER_LIGHT - 1)] lc.RenderIndices.Value.[iAddr .. (iAddr + Config.MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT - 1)] lc.NumRenderIndices.Value.[addr]

                    lc.Trafos.Value.[addr]      <- Trafo3d.Identity
                | None -> ()       
            )
                        
            returnID

        | None -> Option.None


    let private addOrthoganlQuadrilateralLight (lc : LightCollection) vertices =
        match lc.NextFreeAddr.Value with
        | Some nfa -> 
            let mutable returnID = Option.None;
            
            transact (fun _ ->

                match registerLight lc with
                | Some (addr, lightID) ->
                    returnID <- Option.Some lightID

                    addVertices lc addr vertices
                
                    addRenderIndices lc addr [| 0; 1; 2; 0; 2; 3 |]

                    lc.BaseComponents.Value.[addr] <- LIGHT_BASE_TYPE_ORTHO_QUAD
                    addPatchIndices lc addr [| 0; 1; 2; 3 |]

                    let samplePoints = OfflineStructuredSamplePoints.Square.samples 

                    addSamplePoints lc addr (samplePoints |> List.toArray)

                    let vAddr = addr * Config.VERT_PER_LIGHT
                    let iAddr = addr * Config.MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT

                    lc.Forwards.Value.[addr]    <- V3d(1, 0, 0)
                    lc.Ups.Value.[addr]         <- V3d(0, 0, 1)
                    lc.Areas.Value.[addr]       <- computeArea lc.Vertices.Value.[vAddr .. (vAddr + Config.VERT_PER_LIGHT - 1)] lc.RenderIndices.Value.[iAddr .. (iAddr + Config.MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT - 1)] lc.NumRenderIndices.Value.[addr]

                    lc.Trafos.Value.[addr]      <- Trafo3d.Identity
                | None -> ()       
            )
                        
            returnID

        | None -> Option.None
   
    // Adds a new square light to the given light collection
    // Returns the updated light collection and the id of the added light
    // If the light could not be added because there is no space left, no id is returned
    let addSquareLight (lc : LightCollection) =
        [|
            V3d(0.0, -0.5, -0.5)
            V3d(0.0,  0.5, -0.5)
            V3d(0.0,  0.5,  0.5)
            V3d(0.0, -0.5,  0.5)
        |] |> addOrthoganlQuadrilateralLight lc

    // Adds a new rectangle light to the given light collection
    // Returns the updated light collection and the id of the added light
    // If the light could not be added because there is no space left, no id is returned
    let addRectangleLight (lc : LightCollection) width height =
        [|
            V3d(0.0, -(width / 2.0), -(height / 2.0))
            V3d(0.0,  (width / 2.0), -(height / 2.0))
            V3d(0.0,  (width / 2.0),  (height / 2.0))
            V3d(0.0, -(width / 2.0),  (height / 2.0))
        |] |> addOrthoganlQuadrilateralLight lc
        

    // Transforms a given light with the given trafo
    let transformLight (lc : LightCollection) lightID (trafo : Trafo3d) =
        let addr = lc.IDToAddr.Item lightID

        let vAddr = addr * Config.VERT_PER_LIGHT
        let iAddr = addr * Config.MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT
        let sAddr = addr * Config.SS_LIGHT_SAMPLES_PER_LIGHT

        let update array updateFunc = 
            array |> Array.mapi (fun i v -> if i = addr then (updateFunc v) else v)

        transact (fun _ -> 
            
            let vertexTrafo = lc.Trafos.Value.[addr].Forward * trafo.Forward * lc.Trafos.Value.[addr].Backward
            
            lc.Vertices.Value <-
                lc.Vertices.Value |> Array.mapi (
                    fun i v -> 
                        if vAddr <= i && i < (vAddr + Config.VERT_PER_LIGHT) then
                            V3d(vertexTrafo * V4d(v, 1.0))
                        else
                            v
                    )

            lc.SamplePoints.Value <-
                lc.SamplePoints.Value |> Array.mapi (
                    fun i sp -> 
                        if sAddr <= i && i < (sAddr + Config.SS_LIGHT_SAMPLES_PER_LIGHT) then
                            V3d(vertexTrafo * V4d(sp, 1.0))
                        else
                            sp
                    )
            
            let transformDir dir =
                dir
                |> Mat.transformDir lc.Trafos.Value.[addr].Backward
                |> Mat.transformDir trafo.Forward 
                |> Mat.transformDir lc.Trafos.Value.[addr].Forward

            lc.Forwards.Value <- update lc.Forwards.Value (fun forward -> transformDir forward |> Vec.normalize)
            lc.Ups.Value <- update lc.Ups.Value (fun up -> transformDir up |> Vec.normalize)
                  
            lc.Areas.Value <- update lc.Areas.Value (fun _ -> computeArea lc.Vertices.Value.[vAddr .. (vAddr + Config.VERT_PER_LIGHT - 1)] lc.RenderIndices.Value.[iAddr .. (iAddr + Config.MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT - 1)] lc.NumRenderIndices.Value.[addr])
            
            lc.Trafos.Value <- update lc.Trafos.Value (fun t -> trafo * t)
           )
        
        

    module Effect =
        open FShade
        open Aardvark.Base.Rendering

        type UniformScope with
            member uniform.Lights             : Arr<N<Config.NUM_LIGHTS>,                           int> = uniform?Lights
            member uniform.LBaseComponents    : Arr<N<Config.NUM_LIGHTS>,                           int> = uniform?LBaseComponents
            member uniform.LVertices          : Arr<N<Config.VERT_ALL_LIGHT>,                       V3d> = uniform?LVertices
            member uniform.LNumVertices       : Arr<N<Config.NUM_LIGHTS>,                           int> = uniform?LNumVertices
            member uniform.LRenderIndices     : Arr<N<Config.MAX_RENDER_IDX_BUFFER_SIZE_ALL_LIGHT>, int> = uniform?LRenderIndices
            member uniform.LNumRenderIndices  : Arr<N<Config.NUM_LIGHTS>,                           int> = uniform?LNumRenderIndices
            member uniform.LPatchIndices      : Arr<N<Config.MAX_PATCH_IDX_BUFFER_SIZE_ALL_LIGHT>,  int> = uniform?LPatchIndices
            member uniform.LNumPatchIndices   : Arr<N<Config.NUM_LIGHTS>,                           int> = uniform?LNumPatchIndices
            member uniform.LSamplePoints      : Arr<N<Config.SS_LIGHT_SAMPLES_ALL_LIGHT>,           V3d> = uniform?LSamplePoints
            member uniform.LForwards          : Arr<N<Config.NUM_LIGHTS>,                           V3d> = uniform?LForwards
            member uniform.LUps               : Arr<N<Config.NUM_LIGHTS>,                           V3d> = uniform?LUps
            member uniform.LAreas             : Arr<N<Config.NUM_LIGHTS>,                        double> = uniform?LAreas
            member uniform.LBases             : Arr<N<Config.NUM_LIGHTS>,                          M33d> = uniform?LBases

    module Sg = 
        open System
        open Aardvark.Base.Rendering
        open Aardvark.SceneGraph

        let addLightCollectionSg ( lc : LightCollection ) sg =

            let lightSgList = [

                for addr in 0 .. lc.Lights.Value.Length - 1 do
                    if Array.get lc.Lights.Value addr <> -1 then

                        let vAddr = addr * Config.VERT_PER_LIGHT
                        let iAddr = addr * Config.MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT

                        let lightGeometry =
                            IndexedGeometry(
                                Mode = IndexedGeometryMode.TriangleList,
                                IndexArray = (Array.sub lc.RenderIndices.Value iAddr lc.NumRenderIndices.Value.[addr] :> Array),
                                IndexedAttributes =
                                    SymDict.ofList [
                                        DefaultSemantic.Positions, 
                                            (Array.sub lc.Vertices.Value vAddr lc.NumVertices.Value.[addr]) 
                                            |> Array.map (fun v -> // use original vertex positions
                                                 V3d(
                                                    lc.Trafos.Value.[addr].Backward * V4d(v, 1.0)
                                                 )
                                                )                                            
                                            :> Array
                                        DefaultSemantic.Colors, [| 
                                                for i in 1 .. lc.NumVertices.Value.[addr] do
                                                    yield C4b.White
                                            |] :> Array
                                        DefaultSemantic.Normals, [| 
                                                for i in 1 .. lc.NumVertices.Value.[addr] do
                                                    yield lc.Forwards.Value.[addr]
                                            |]
                                            |> Array.map (fun n -> // use original vertex normals
                                                  Mat.transformDir lc.Trafos.Value.[addr].Backward n
                                                  |> Vec.normalize
                                                )
                                            :> Array
                                    ]
                            )
            
                        let lightTrafo = Mod.map (fun (trafos : Trafo3d[]) -> 
                            trafos.[addr]) lc.Trafos
                           
                        let lightSg = lightGeometry 
                                        |> Sg.ofIndexedGeometry 
                                        |> Sg.trafo lightTrafo
                                        |> Sg.effect [
                                                DefaultSurfaces.trafo |> toEffect
                                                DefaultSurfaces.vertexColor |> toEffect
                                            ]
                        (*
                        let lightCoordSysSg = 
                            [
                                yield IndexedGeometryPrimitives.wireframeCone
                                    V3d.Zero V3d.OOI 0.3 0.05 10 C4b.Green 

                                yield IndexedGeometryPrimitives.wireframeCone
                                    V3d.Zero V3d.IOO 0.3 0.05 10 C4b.Red

                                yield IndexedGeometryPrimitives.wireframeCone
                                    V3d.Zero V3d.OIO 0.3 0.05 10 C4b.Blue
                            ]
                            |> List.map Sg.ofIndexedGeometry
                            |> Sg.group'
                            |> Sg.trafo lightTrafo
                            |> Sg.effect [
                                    DefaultSurfaces.trafo |> toEffect
                                    DefaultSurfaces.vertexColor |> toEffect
                                ]
                        *)

                        yield [lightSg(*; lightCoordSysSg*)] |> Sg.group'
            ]
            
            Sg.group (sg :: lightSgList) :> ISg

        let setLightCollectionUniforms ( lc : LightCollection ) sg =

            sg
                |> Sg.uniform "Lights"            lc.Lights
                |> Sg.uniform "LBaseComponents"   lc.BaseComponents
                |> Sg.uniform "LVertices"         lc.Vertices
                |> Sg.uniform "LNumVertices"      lc.NumVertices
                |> Sg.uniform "LRenderIndices"    lc.RenderIndices
                |> Sg.uniform "LNumRenderIndices" lc.NumRenderIndices
                |> Sg.uniform "LPatchIndices"     lc.PatchIndices
                |> Sg.uniform "LNumPatchIndices"  lc.NumPatchIndices
                |> Sg.uniform "LSamplePoints"     lc.SamplePoints
                |> Sg.uniform "LForwards"         lc.Forwards
                |> Sg.uniform "LUps"              lc.Ups
                |> Sg.uniform "LAreas"            lc.Areas
