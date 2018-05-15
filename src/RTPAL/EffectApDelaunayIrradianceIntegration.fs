﻿namespace Render

module EffectApDelaunayIrradianceIntegration = 
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open FShade

    open Light.Effect
    open EffectUtils
    open PhotometricLight

    [<Literal>]
    let MAX_EDGES = 10

    [<Literal>]
    let MAX_FACES = 5


    [<Literal>]
    let CASE_CORNER = 0 

    [<Literal>]
    let CASE_EDGE = 1 

    [<Literal>]
    let CASE_INSIDE = 2 

    [<Literal>]
    let NUM_CASE = 3 // num of possible cases

    [<Literal>]
    let MAX_EDGES_ALL = 30 // MAX_EDGES * NUM_CASE

    [<Literal>]
    let MAX_FACES_ALL = 15 // MAX_FACES * NUM_CASE


    type UniformScope with
        member uniform.quadVertices         : Arr<N<MAX_EDGES_ALL>, V4i>  = uniform?quadVertices 
        member uniform.quadNeighbourEdges   : Arr<N<MAX_EDGES_ALL>, V4i>  = uniform?quadNeighbourEdges
        member uniform.quadMeta             : Arr<N<MAX_EDGES_ALL>, V2i>  = uniform?quadMeta 
        member uniform.quadFaces            : Arr<N<MAX_FACES_ALL>, V4i>  = uniform?quadFaces 
        member uniform.quadStack            : Arr<N<MAX_EDGES_ALL>, int>  = uniform?quadStack
        member uniform.quadStackPointer     : Arr<N<NUM_CASE>, int>       = uniform?quadStackPointer

    [<ReflectedDefinition>]
    let private IDHash a b c = 
    
        let (a, b, c) =
            if a < b && a < c then
                (a, b, c)
            elif b < a && b < c then
                (b, c, a)
            else
                (c, a, b)
                
        (a <<< 20) ||| (b <<< 10) ||| c

    module QUAD_DATA =

        [<ReflectedDefinition>]
        let getInitVertexData case =             
            let d = Arr<N<MAX_EDGES>, V4i>()
            for i in 0 .. MAX_EDGES - 1 do
                d.[i] <- uniform.quadVertices.[MAX_EDGES * case + i]
            d

        [<ReflectedDefinition>]
        let getInitNeighbourEdgeData case =             
            let d = Arr<N<MAX_EDGES>, V4i>()
            for i in 0 .. MAX_EDGES - 1 do
                d.[i] <- uniform.quadNeighbourEdges.[MAX_EDGES * case + i]
            d

        [<ReflectedDefinition>]
        let getInitMetaData case =             
            let d = Arr<N<MAX_EDGES>, V2i>()
            for i in 0 .. MAX_EDGES - 1 do
                d.[i] <- uniform.quadMeta.[MAX_EDGES * case + i]
            d
            
        [<ReflectedDefinition>]
        let getInitFaceData case =             
            let d = Arr<N<MAX_FACES>, V4i>()
            for i in 0 .. MAX_FACES - 1 do
                d.[i] <- uniform.quadFaces.[MAX_FACES * case + i]
            d

        [<ReflectedDefinition>]
        let getInitStackData case =             
            let s = Arr<N<MAX_EDGES>, int>()
            for i in 0 .. MAX_EDGES - 1 do
                s.[i] <- uniform.quadStack.[MAX_EDGES * case + i]
            (s, uniform.quadStackPointer.[case])
            

        module ALL =

            (*

            v0 is always the special point (e.g. closest)
            The other points follow in counter clockwise order

            CASE_CORNER

             v3               e2                   v2
                +--------------------------------+   
                |                              --|   
                |                            -/  |   
                |                          -/    |   
                |                        -/      |   
                |                      -/        |   
                |                   --/          |   
                |                 -/             |   
                |               -/               | e1
             e3 |             -/  e4             |   
                |          --/                   |   
                |        -/                      |   
                |      -/                        |   
                |    -/                          |   
                |  -/                            |   
                |-/                              |   
                +--------------------------------+   
              v0                e0                 v1
            

            CASE_EDGE

             v3               e2                   v2
                +--------------------------------+   
                |-                             - |   
                | \                           /  |   
                |  \                         /   |   
                |   \                       /    |   
                |    \                     /     |   
                |     \                   /      |   
                |      \                 /       |   
                |       \ e6           -/        | e1
             e3 |        \            / e5       |   
                |         \          /           |   
                |          \        /            |   
                |           \      /             |   
                |            \    /              |   
                |             \  /               |   
                |              \/                |   
                +--------------------------------+   
             v4         e4     v0        e0        v1


            CASE_INSIDE
                                                     
               v4             e2                 v3  
                +--------------------------------+   
                |--                            --|   
                |  \-                        -/  |   
                |    \-                    -/    |   
                |      \-e7           e6 -/      |   
                |        \-            -/        |   
                |          \--      --/          |   
                |             \-  -/             |   
                |               \- v0            | e1
             e3 |             -/  \-             |   
                |          --/      \--          |   
                |        -/            \-        |   
                |      -/ e4          e5 \-      |   
                |    -/                    \-    |   
                |  -/                        \-  |   
                |-/                            \-|   
                +--------------------------------+   
              v1             e0                   v2 

            *)

            // vertices
            // vertex0 (v0), opposite0 (o0), vertex1 (v1), opposite1 (o1)
            let V = Arr<N<MAX_EDGES_ALL>, V4i>([
                                            // CASE_CORNER
                                                // outer
                                                V4i(0, -1, 1, 2) // 0
                                                V4i(1, -1, 2, 0) // 1
                                                V4i(2, -1, 3, 0) // 2
                                                V4i(3, -1, 0, 2) // 3
                                                
                                                // inner
                                                V4i(0, 1, 2, 3) // 4

                                                // fill up
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)

                                            // CASE_EDGE
                                                // outer
                                                V4i(0, -1, 1, 2) // 0
                                                V4i(1, -1, 2, 0) // 1
                                                V4i(2, -1, 3, 0) // 2
                                                V4i(3, -1, 4, 0) // 3
                                                V4i(4, -1, 0, 3) // 4

                                                // inner
                                                V4i(0, 1, 2, 3) // 5
                                                V4i(0, 2, 3, 4) // 6

                                                // fill up
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)

                                            // CASE_INSIDE
                                                // outer
                                                V4i(1, -1, 2, 0) // 0
                                                V4i(2, -1, 3, 0) // 1
                                                V4i(3, -1, 4, 0) // 2
                                                V4i(4, -1, 1, 0) // 3

                                                // inner
                                                V4i(0, 4, 1, 2) // 4
                                                V4i(0, 1, 2, 3) // 5
                                                V4i(0, 2, 3, 4) // 6
                                                V4i(0, 3, 4, 1) // 7

                                                // fill up
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)
                                            ])

            // edges
            // v0 -> o0, o0 -> v1, v1 -> o1, o1 -> v0
            let E = Arr<N<MAX_EDGES_ALL>, V4i>([
                                            // CASE_CORNER
                                                // outer
                                                V4i(-1, -1, 1, 4) // 0
                                                V4i(-1, -1, 4, 0) // 1
                                                V4i(-1, -1, 3, 4) // 2
                                                V4i(-1, -1, 4, 2) // 3
                                                
                                                // 
                                                V4i(0, 1, 2, 3) // 4

                                                // fill up
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)
                                                
                                            // CASE_EDGE
                                                // outer
                                                V4i(-1, -1, 1, 5) // 0
                                                V4i(-1, -1, 5, 0) // 1
                                                V4i(-1, -1, 6, 5) // 2
                                                V4i(-1, -1, 4, 6) // 3
                                                V4i(-1, -1, 6, 3) // 4

                                                // inner
                                                V4i(0, 1, 2, 6) // 5
                                                V4i(5, 2, 3, 4) // 6

                                                // fill up
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)

                                            // CASE_INSIDE
                                                // outer
                                                V4i(-1, -1, 5, 4) // 0
                                                V4i(-1, -1, 6, 5) // 1
                                                V4i(-1, -1, 7, 6) // 2
                                                V4i(-1, -1, 4, 7) // 3

                                                // inner
                                                V4i(7, 3, 0, 5) // 4
                                                V4i(4, 0, 1, 6) // 5
                                                V4i(5, 1, 2, 7) // 6
                                                V4i(6, 2, 3, 4) // 7

                                                // fill up
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)
                                            ])

            // meta    
            // inside, marked
            // 1 = true, 0 = false
            let M = Arr<N<MAX_EDGES_ALL>, V2i>([
                                            // CASE_CORNER
                                                // outer
                                                V2i(0, 0) // 0
                                                V2i(0, 0) // 1
                                                V2i(0, 0) // 2
                                                V2i(0, 0) // 3
                                                
                                                // inner
                                                V2i(1, 1) // 4

                                                // fill up
                                                V2i(-1, -1) 
                                                V2i(-1, -1)
                                                V2i(-1, -1) 
                                                V2i(-1, -1)
                                                V2i(-1, -1)
                                                
                                            // CASE_EDGE
                                                // outer
                                                V2i(0, 0) // 0
                                                V2i(0, 0) // 1
                                                V2i(0, 0) // 2
                                                V2i(0, 0) // 3
                                                V2i(0, 0) // 4

                                                // inner
                                                V2i(1, 1) // 5
                                                V2i(1, 1) // 6

                                                // fill up
                                                V2i(-1, -1) 
                                                V2i(-1, -1)
                                                V2i(-1, -1)

                                            // CASE_INSIDE
                                                // outer
                                                V2i(0, 0) // 0
                                                V2i(0, 0) // 1
                                                V2i(0, 0) // 2
                                                V2i(0, 0) // 3

                                                // inner
                                                V2i(1, 1) // 4
                                                V2i(1, 1) // 5
                                                V2i(1, 1) // 6
                                                V2i(1, 1) // 7

                                                // fill up
                                                V2i(-1, -1)
                                                V2i(-1, -1)
                                            ])
            
            // faces
            // (IDHash v0 v1 v2), v0, v1, v2
            // v0 should be the smallest ID, v0, v1, v2 ordered counter clockwise
            let F = Arr<N<MAX_FACES_ALL>, V4i>([
                                            // CASE_CORNER
                                                V4i((IDHash 0 1 2), 0, 1, 2) 
                                                V4i((IDHash 0 2 3), 0, 2, 3) 

                                                // fill 
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)
                                                
                                            // CASE_EDGE
                                                V4i((IDHash 0 1 2), 0, 1, 2) 
                                                V4i((IDHash 0 2 3), 0, 2, 3) 
                                                V4i((IDHash 0 3 4), 0, 3, 4) 

                                                // fill 
                                                V4i(-1, -1, -1, -1)
                                                V4i(-1, -1, -1, -1)

                                            // CASE_INSIDE
                                                V4i((IDHash 0 1 2), 0, 1, 2) 
                                                V4i((IDHash 0 2 3), 0, 2, 3) 
                                                V4i((IDHash 0 3 4), 0, 3, 4) 
                                                V4i((IDHash 0 4 1), 0, 4, 1) 

                                                // fill up
                                                V4i(-1, -1, -1, -1)
                                            ])

            // stack of marked edges
            let S = Arr<N<MAX_EDGES_ALL>, int>([
                                            // CASE_CORNER
                                                4 // 0
                                                -1;-1;-1;-1;-1;-1;-1;-1;-1
                                                
                                            // CASE_EDGE
                                                5 // 0
                                                6 // 1 
                                                -1;-1;-1;-1;-1;-1;-1;-1

                                            // CASE_INSIDE
                                                4 // 0
                                                5 // 1
                                                6 // 2 
                                                7 // 3
                                                -1;-1;-1;-1;-1;-1
                                            ])

            // stack pointer
            let SP = Arr<N<NUM_CASE>, int>([
                                            // CASE_CORNER
                                                1

                                            // CASE_EDGE
                                                2

                                            // CASE_INSIDE
                                                4
                                            ])

    type Vertex = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
        [<FragCoord>]       fc      : V4d
    }  

    [<ReflectedDefinition>]
    let private sampleIrr (t2w : M33d) (addr : int) (p : V3d) = 
    
        let i = p |> Vec.normalize  
        let iw = t2w * -i
 
        let dotOut = max 1e-9 (abs (Vec.dot iw uniform.LForwards.[addr]))
        // let invDistSquared = 1.0 / (Vec.lengthSquared p + 1e-9)

        getPhotometricIntensity iw uniform.LForwards.[addr]  uniform.LUps.[addr] / (uniform.LAreas.[addr] * dotOut)

    // true if flip, false otherwise
    [<ReflectedDefinition>]
    let private localDelaunayFlipTest (a : V3d) (b : V3d) (c : V3d) (d : V3d) = (Vec.cross (b - c) (d - c) |> Vec.dot (a - c)) < 0.0     


    let delaunyIrrIntegration (v : Vertex) = 
        fragment {

            ////////////////////////////////////////////////////////

            let P = v.wp.XYZ

            let t2w = v.n |> Vec.normalize |> basisFrisvad 
            let w2t = t2w |> Mat.transpose

            let brdf = v.c / PI 

            let mutable illumination = V4d.Zero * (uniform.dT * 1e-256 * 0.0)
            
            ////////////////////////////////////////////////////////

            for addr in 0 .. (Config.Light.NUM_LIGHTS - 1) do 
                match uniform.Lights.[addr] with
                    | -1 -> ()
                    |  _ ->    
                        
                        let vAddr = addr * Config.Light.VERT_PER_LIGHT
                        let iAddr = addr * Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT

                        ////////////////////////////////////////////////////////

                        let l2w = M33dFromCols  (V3d.Cross((uniform.LUps.[addr]), (uniform.LForwards.[addr]))) uniform.LUps.[addr] uniform.LForwards.[addr]
                            
                        let w2l = l2w |> Mat.transpose

                        let t2l = w2l * t2w

                        let l2t = l2w * w2t

                        ////////////////////////////////////////////////////////

                        for iIdx in iAddr .. Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT .. (iAddr + uniform.LNumPatchIndices.[addr] - 1) do
                            
                            let vt = Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>() 
                            
                            for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                                let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                                vt.[vtc] <- w2t * (uniform.LVertices.[vtcAddr] - P)

                            ////////////////////////////////////////////////////////

                            let (clippedVa, clippedVc) = clipPatch(V3d.Zero, V3d.OOI, vt, uniform.LBaseComponents.[addr])

                            if clippedVc <> 0 then

                                let eps = 1e-9

                                let lightPlaneN = w2t * uniform.LForwards.[addr] |> Vec.normalize   

                                // find closest point limited to upper hemisphere
                                let t = (- clippedVa.[0]) |> Vec.dot lightPlaneN
                                let mutable closestPoint = t * (-lightPlaneN)
                                                    
                                if (Vec.dot closestPoint V3d.OOI) < 0.0 then
                                    let newDir = V3d(closestPoint.X, closestPoint.Y, 0.0) |> Vec.normalize
                                    closestPoint <- linePlaneIntersection V3d.Zero newDir (clippedVa.[0]) lightPlaneN
                                    
                                let insideLightPlane = (Vec.length closestPoint) < eps
                                
                                if not insideLightPlane then

                                                                        
                                    let (closestPoint, CLAMP_POLYGON_RESULT, clampP0Id, clampP1ID) = clampPointToPolygon clippedVa clippedVc closestPoint t2l

                                    ////////////////////////////////////////
                                    // create triangulation

                                    let (case, v1Idx) =
                                        if CLAMP_POLYGON_RESULT = CLAMP_POLYGON_RESULT_POINT then
                                            (CASE_CORNER, clampP0Id)
                                        elif CLAMP_POLYGON_RESULT = CLAMP_POLYGON_RESULT_LINE then
                                            (CASE_EDGE, clampP1ID)
                                        else (*CLAMP_POLYGON_RESULT =  CLAMP_POLYGON_RESULT_NONE *) 
                                            (CASE_INSIDE, 0)
                                     
                                     
                                    // XYZ -> Spherical coords; W -> luminance
                                    let vertices = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_TWO>, V4d>() 

                                    let mutable vc = clippedVc
                                    if case <> CASE_CORNER then 
                                        vertices.[0] <- V4d(closestPoint |> Vec.normalize, (sampleIrr t2w  addr closestPoint))
                                        vc <- clippedVc + 1 
                                    

                                    for i in 0 .. clippedVc - 1 do 
                                        vertices.[i + 1] <- V4d(clippedVa.[v1Idx + i % clippedVc] |> Vec.normalize, (sampleIrr t2w  addr clippedVa.[v1Idx + i % clippedVc]))
                                    

                                    let delVertexData   = QUAD_DATA.getInitVertexData case
                                    let delNEdgeData    = QUAD_DATA.getInitNeighbourEdgeData case
                                    let delMetaData     = QUAD_DATA.getInitMetaData case
                                    let delFaceData     = QUAD_DATA.getInitFaceData case
                                    let (delStack, delSP) = QUAD_DATA.getInitStackData case
                                    let mutable delSP = delSP

                                    ////////////////////////////////////////
                                    // transform to a Delaunay triangulation

                                    while delSP <> 0 do

                                        // get edge Id
                                        let eId = delStack.[delSP]

                                        // unmark edge
                                        delMetaData.[eId] <- V2i(delMetaData.[eId].X, 0)
                                        delSP <- delSP - 1
                                        
                                        let eVertices = delVertexData.[eId]
                                        let eNEdges = delNEdgeData.[eId]

                                        // test if edge is locally delaunay
                                        let notLD = localDelaunayFlipTest (vertices.[eVertices.X].XYZ) (vertices.[eVertices.Y].XYZ) (vertices.[eVertices.Z].XYZ) (vertices.[eVertices.W].XYZ)

                                        if notLD then
                                            // flip edge

                                            // left shift vertices and edges of edge to flip
                                            delVertexData.[eId] <- V4i(eVertices.Y, eVertices.Z, eVertices.W, eVertices.X)
                                            delNEdgeData.[eId] <- V4i(eNEdges.Y, eNEdges.Z, eNEdges.W, eNEdges.X)

                                            // adopt faces
                                            let ef0Id = IDHash (eVertices.X) (eVertices.Y) (eVertices.Z)
                                            let ef1Id = IDHash (eVertices.Z) (eVertices.W) (eVertices.X)

                                            for f in 0 .. MAX_FACES - 1 do
                                                let face = delFaceData.[f]

                                                if face.X = ef0Id then
                                                    let fId = IDHash (delVertexData.[eId].X) (delVertexData.[eId].Y) (delVertexData.[eId].Z)
                                                    delFaceData.[f] <- V4i(fId, (delVertexData.[eId].X), (delVertexData.[eId].Y), (delVertexData.[eId].Z))

                                                if face.X = ef1Id then
                                                    let fId = IDHash (delVertexData.[eId].Z) (delVertexData.[eId].W) (delVertexData.[eId].X)
                                                    delFaceData.[f] <- V4i(fId, (delVertexData.[eId].Z), (delVertexData.[eId].W), (delVertexData.[eId].X))

                                            
                                            // adapt neighbour edges
                                            for ne in 0 .. 3 do
                                                let (neId, otherEdgesOfE, otherOppositeV) = 
                                                    match ne with
                                                    | 0 -> (eNEdges.X, eNEdges.ZW, eVertices.W)
                                                    | 1 -> (eNEdges.Y, eNEdges.ZW, eVertices.W)
                                                    | 2 -> (eNEdges.Z, eNEdges.XY, eVertices.Y)
                                                    | _ -> (eNEdges.W, eNEdges.XY, eVertices.Y)

                                                    
                                                if eId = delNEdgeData.[neId].X then
                                                    delNEdgeData.[neId] <- V4i(otherEdgesOfE.X, eId, delNEdgeData.[neId].Z, delNEdgeData.[neId].W)
                                                    delVertexData.[neId] <- V4i(delVertexData.[neId].X, otherOppositeV, delVertexData.[neId].Z, delVertexData.[neId].W)
                                                elif eId = delNEdgeData.[neId].Y then
                                                    delNEdgeData.[neId] <- V4i(eId, otherEdgesOfE.Y, delNEdgeData.[neId].Z, delNEdgeData.[neId].W)
                                                    delVertexData.[neId] <- V4i(delVertexData.[neId].X, otherOppositeV, delVertexData.[neId].Z, delVertexData.[neId].W)
                                                elif eId = delNEdgeData.[neId].Z then
                                                    delNEdgeData.[neId] <- V4i(delNEdgeData.[neId].X, delNEdgeData.[neId].Y, otherEdgesOfE.X, eId)
                                                    delVertexData.[neId] <- V4i(delVertexData.[neId].X, delVertexData.[neId].Y, delVertexData.[neId].Z, otherOppositeV)
                                                else (* eId = neNEdges.W *)
                                                    delNEdgeData.[neId] <- V4i(delNEdgeData.[neId].X, delNEdgeData.[neId].Y, eId, otherEdgesOfE.Y)
                                                    delVertexData.[neId] <- V4i(delVertexData.[neId].X, delVertexData.[neId].Y, delVertexData.[neId].Z, otherOppositeV)

                                                // if inside and not marked add to stack
                                                if delMetaData.[neId].X = 1 && delMetaData.[neId].Y = 0 then
                                                    delMetaData.[neId] <- V2i(1, 1)
                                                    delSP <- delSP + 1
                                                    delStack.[delSP] <- neId


                                    ////////////////////////////////////////
                                    // integrate

                                    for f in 0 .. MAX_FACES - 1 do
                                        let face = delFaceData.[f]

                                        if face.X <> -1 then

                                            let sum = (vertices.[face.Y].W + vertices.[face.Z].W + vertices.[face.W].W) / 3.0

                                            let area =  computeSolidAngle (vertices.[face.Y].XYZ) (vertices.[face.Z].XYZ) (vertices.[face.W].XYZ)

                                            illumination <- illumination + sum * area


                                    
                           
                            ////////////////////////////////////////////////////////
                        

            return V4d(illumination.XYZ, v.c.W)

        }


    module Rendering =

        open Aardvark.SceneGraph

        open RenderInterop
        open Utils
        open Utils.Sg
        open Aardvark.Base.Incremental

        let delIrrIntApproxRenderTask (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            sceneSg
                |> setupFbEffects [ 
                        delaunyIrrIntegration |> toEffect
                    ]
                |> Light.Sg.setLightCollectionUniforms data.lights
                |> setupPhotometricData data.photometricData
                |> setupCamera data.view data.projTrafo data.viewportSize 
                |> Sg.uniform "quadVertices"        (QUAD_DATA.ALL.V  |> Mod.init)
                |> Sg.uniform "quadNeighbourEdges"  (QUAD_DATA.ALL.E  |> Mod.init)
                |> Sg.uniform "quadMeta"            (QUAD_DATA.ALL.M  |> Mod.init)
                |> Sg.uniform "quadFaces"           (QUAD_DATA.ALL.F  |> Mod.init)
                |> Sg.uniform "quadStack"           (QUAD_DATA.ALL.S  |> Mod.init)
                |> Sg.uniform "quadStackPointer"    (QUAD_DATA.ALL.SP |> Mod.init)
                |> setUniformDT data.dt
                |> setUniformUsePhotometry data.usePhotometry
                |> Sg.compile data.runtime signature

        let delIrrIntApproxFb (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            delIrrIntApproxRenderTask data signature sceneSg
            |> RenderTask.renderToColor data.viewportSize