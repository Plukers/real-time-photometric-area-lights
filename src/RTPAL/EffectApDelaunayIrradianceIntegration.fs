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
    let CASE_CORNER = 1 

    [<Literal>]
    let CASE_CORNER_OFFSET = 0 


    [<Literal>]
    let CASE_EDGE = 2 

    [<Literal>]
    let CASE_EDGE_OFFSET = 1 


    [<Literal>]
    let CASE_INSIDE = 4

    [<Literal>]
    let CASE_INSIDE_OFFSET = 2 

    [<Literal>]
    let NUM_CASE = 3 // num of possible cases

    [<Literal>]
    let MAX_EDGES_ALL = 30 // MAX_EDGES * NUM_CASE

    [<Literal>]
    let MAX_FACES_ALL = 15 // MAX_FACES * NUM_CASE


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
            let V = [|
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
                    |]

            // edges
            // v0 -> o0, o0 -> v1, v1 -> o1, o1 -> v0
            let E = [|
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
                    |]

            // meta    
            // inside, marked
            // 1 = true, 0 = false
            let M = [|
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
                    |]
            
            // faces
            // (IDHash v0 v1 v2), v0, v1, v2
            // v0 should be the smallest ID, v0, v1, v2 ordered counter clockwise
            let F = [|
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
                    |]

            // stack of marked edges
            let S = [|
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
                    |]

            // stack pointer
            let SP = [|
                    // CASE_CORNER
                        0

                    // CASE_EDGE
                        1

                    // CASE_INSIDE
                        3
                    |]

        [<ReflectedDefinition>]
        let getInitVertexData caseOffset =             
            let d = Arr<N<MAX_EDGES>, V4i>()
            for i in 0 .. MAX_EDGES - 1 do
                d.[i] <- ALL.V.[MAX_EDGES * caseOffset + i]
            d

        [<ReflectedDefinition>]
        let getInitNeighbourEdgeData caseOffset =             
            let d = Arr<N<MAX_EDGES>, V4i>()
            for i in 0 .. MAX_EDGES - 1 do
                d.[i] <- ALL.E.[MAX_EDGES * caseOffset + i]
            d

        [<ReflectedDefinition>]
        let getInitMetaData caseOffset =             
            let d = Arr<N<MAX_EDGES>, V2i>()
            for i in 0 .. MAX_EDGES - 1 do
                d.[i] <- ALL.M.[MAX_EDGES * caseOffset + i]
            d

        [<ReflectedDefinition>]
        let getInitFaceData caseOffset =             
            let d = Arr<N<MAX_FACES>, V4i>()
            for i in 0 .. MAX_FACES - 1 do
                d.[i] <- ALL.F.[MAX_FACES * caseOffset + i]
            d

        [<ReflectedDefinition>]
        let getInitStackData caseOffset =             
            let s = Arr<N<MAX_EDGES>, int>()
            for i in 0 .. MAX_EDGES - 1 do
                s.[i] <- ALL.S.[MAX_EDGES * caseOffset + i]
            (s, ALL.SP.[caseOffset])


    type Vertex = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
    }  

    [<ReflectedDefinition>]
    let private sampleIrr (t2w : M33d) (addr : int) (p : V3d) = 

        //let i = p |> Vec.normalize  
        //let iw = t2w * -i
 
        //let dotOut = max 1e-9 (abs (Vec.dot iw uniform.LForwards.[addr]))
        //let invDistSquared = 1.0 / (Vec.lengthSquared p + 1e-9)

        //// simplified
        //let irr = getPhotometricIntensity iw uniform.LForwards.[addr]  uniform.LUps.[addr]
        //let weight = i.Z * invDistSquared  

        //V2d(irr * weight, weight * dotOut)

        let i = p |> Vec.normalize  
        let iw = t2w * -i
 
        let dotOut = max 1e-9 (abs (Vec.dot iw uniform.LForwards.[addr]))
        let invDistSquared = 1.0 / (Vec.lengthSquared p + 1e-9)

        // simplified
        let irr = getPhotometricIntensity iw uniform.LForwards.[addr]  uniform.LUps.[addr] / (uniform.LAreas.[addr] * dotOut)
        let weight = i.Z  

        V2d(irr * weight, weight)

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

                                    let (caseOffset, v1Idx) =
                                        if CLAMP_POLYGON_RESULT = CLAMP_POLYGON_RESULT_POINT then
                                            (CASE_CORNER_OFFSET, clampP0Id)
                                        elif CLAMP_POLYGON_RESULT = CLAMP_POLYGON_RESULT_LINE then
                                            (CASE_EDGE_OFFSET, clampP1ID)
                                        else (*CLAMP_POLYGON_RESULT =  CLAMP_POLYGON_RESULT_NONE *) 
                                            (CASE_INSIDE_OFFSET, 0)
                                     
                                     
                                    // XYZ -> Spherical coords; 
                                    let verticesNormalized = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_TWO>, V3d>() 

                                    // X -> luminance, Y -> Weight
                                    let funVal = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_TWO>, V2d>() 


                                    let mutable vc = clippedVc
                                    let mutable offset = 0
                                    if caseOffset <> CASE_CORNER_OFFSET then 
                                        verticesNormalized.[0] <- closestPoint |> Vec.normalize
                                        funVal.[0]   <- sampleIrr t2w addr closestPoint
                                        vc <- vc + 1 
                                        offset <- 1

                                    // is the point in front of the light or behind?
                                    // if behind, iterate the light vertices backwards for counter clockwise order
                                    if Vec.dot (uniform.LForwards.[addr]) (t2w *(closestPoint |> Vec.normalize)) < 0.0 then 
                                    
                                        for i in 0 .. clippedVc - 1 do 
                                            verticesNormalized.[i + offset] <- clippedVa.[(v1Idx + i) % clippedVc] |> Vec.normalize
                                            funVal.[i + offset]   <- sampleIrr t2w  addr clippedVa.[(v1Idx + i) % clippedVc]
                                            
                                    else
                                        let mutable j = 0
                                        for i in clippedVc - 1 .. -1 .. 0 do 
                                            verticesNormalized.[j + offset] <- clippedVa.[(v1Idx + i) % clippedVc] |> Vec.normalize
                                            funVal.[j + offset]   <- sampleIrr t2w  addr clippedVa.[(v1Idx + i) % clippedVc]
                                                
                                            j <- j + 1

                                    let delVertexData         = QUAD_DATA.getInitVertexData caseOffset
                                    let delNEdgeData          = QUAD_DATA.getInitNeighbourEdgeData caseOffset
                                    let delMetaData           = QUAD_DATA.getInitMetaData caseOffset
                                    let delFaceData           = QUAD_DATA.getInitFaceData caseOffset
                                    let (delStack, delSPInit) = QUAD_DATA.getInitStackData caseOffset
                                    let mutable delSP = delSPInit

                                    ////////////////////////////////////////
                                    // transform to a Delaunay triangulation

                                    let mutable oneNotLd = false
                                    
                                    while delSP >= 0 do

                                        // get edge Id
                                        let eId = delStack.[delSP]

                                        // unmark edge
                                        delMetaData.[eId] <- V2i(delMetaData.[eId].X, 0)
                                        delSP <- delSP - 1
                                        
                                        let eVertices = delVertexData.[eId]
                                        let eNEdges = delNEdgeData.[eId]

                                        // test if edge is locally delaunay
                                            // true if flip, false otherwise
    
                                        let notLD = 
                                            let a = verticesNormalized.[eVertices.X].XYZ
                                            let b = verticesNormalized.[eVertices.Y].XYZ
                                            let c = verticesNormalized.[eVertices.Z].XYZ
                                            let d = verticesNormalized.[eVertices.W].XYZ
                                            
                                            (Vec.dot (a - c) (Vec.cross (b - c) (d - c))) < 0.0     

                                        oneNotLd <- oneNotLd || notLD
                                        
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

                                    
                                    let mutable patchIllumination = 0.0
                                    let mutable weightSum = 0.0

                                    for f in 0 .. MAX_FACES - 1 do
                                        let face = delFaceData.[f]

                                        if face.X <> -1 then
                                            let area = computeSphericalExcess (verticesNormalized.[face.Y]) (verticesNormalized.[face.Z]) (verticesNormalized.[face.W])

                                            patchIllumination <- patchIllumination + area * (funVal.[face.Y].X + funVal.[face.Z].X + funVal.[face.W].X) / 3.0
                                            weightSum <- weightSum + area * (funVal.[face.Y].Y + funVal.[face.Z].Y + funVal.[face.W].Y) / 3.0


                                    let L =
                                        if weightSum > 0.0 then
                                            patchIllumination / weightSum
                                        else 
                                            0.0

                                    
                                    for l in 0 .. Config.Light.MAX_PATCH_SIZE_PLUS_ONE - 1 do
                                            if l < clippedVc then
                                                // Project polygon light onto sphere
                                                clippedVa.[l] <- Vec.normalize clippedVa.[l]
                                                
                                    let I = abs (baumFormFactor(clippedVa, clippedVc)) / (2.0) // should be divided by 2 PI, but PI is already in the brdf
                                        
                                    illumination <- illumination + L * brdf * I //* scale // * i.Z  
                                    

                            ////////////////////////////////////////////////////////
                        

            return V4d(illumination.XYZ, v.c.W)
            
        }

    module Debug =

        open System

        open Aardvark.Base
        open Aardvark.Base.Incremental
        open Aardvark.Base.Rendering

        open Aardvark.SceneGraph

        open Light

        open EffectUtils


        let private slerp (p0 : V3d) (p1 : V3d) (t : float) = 

            let t = clamp 0.0 1.0 t

            let omega = Vec.dot p0 p1 |> Math.Acos
            let sinOmega = Math.Sin omega 

            let a = (Math.Sin ((1.0 - t) * omega)) / sinOmega
            let b = (Math.Sin (t * omega)) / sinOmega

            a * p0 + b * p1

        
        let private pointSg (radius : float) color trafo = 
            IndexedGeometryPrimitives.solidSubdivisionSphere (Sphere3d(V3d.Zero, radius)) 6 color
            |> Sg.ofIndexedGeometry
            |> Sg.trafo trafo
            |> Sg.effect [
                    DefaultSurfaces.trafo |> toEffect
                    DefaultSurfaces.vertexColor |> toEffect
                ]

        let private arcSg (p0 : V3d) (p1 : V3d) (color : C4b) trafo =

            let arcSegments = seq {
                                let mutable lastPoint = p0
                                for t in 0.0 .. 0.01 .. 1.0 do
                                    let nextPoint = slerp p0 p1 t
                                        
                                    yield (Line3d(lastPoint, nextPoint), color)
                                    lastPoint <- nextPoint

                                }

            IndexedGeometryPrimitives.lines arcSegments
                |> Sg.ofIndexedGeometry
                |> Sg.trafo trafo
                |> Sg.effect [
                        DefaultSurfaces.trafo |> toEffect
                        DefaultSurfaces.vertexColor |> toEffect
                    ]
            
        let M33dFromCols (c1 : V3d) (c2 : V3d) (c3 : V3d) =
            M33d(c1.X, c2.X, c3.X, c1.Y, c2.Y, c3.Y, c1.Z, c2.Z, c3.Z)

        let basisFrisvad (n : V3d) = 
            let c1 = V3d(
                        1.0 - (n.X  * n.X) / (1.0 + n.Z),
                        (-n.X * n.Y) / (1.0 + n.Z),
                        -n.X
                        )

            let c2 = V3d(
                        (-n.X * n.Y) / (1.0 + n.Z),
                        1.0 - (n.Y  * n.Y) / (1.0 + n.Z),
                        -n.Y
                        )

            let c3 = n
            
            M33dFromCols c1 c2 c3

        let v = {
            wp = V4d.Zero
            n = V3d.OOI
            c = V4d.One 
        }
           
            
        let delaunyScene (lc : Light.LightCollection) = 

            let lc = 
                adaptive {
                    let! lights = lc.Lights
                    let! patchIndices = lc.PatchIndices
                    let! vertices = lc.Vertices
                    let! baseComponents = lc.BaseComponents
                    let! forwards = lc.Forwards
                    let! ups = lc.Ups

                    return (lights, patchIndices, vertices, baseComponents, forwards, ups)

                }

            lc |> Mod.map (fun lc ->

                let (lights, lPatchIndices, lVertices, lBaseComponents, lForwards, lUps) = lc

                printfn "Light Vertices : %A" lVertices 

                ////////////////////////////////////////////////////////
                // SHADER START

                let P = v.wp.XYZ

                let t2w = v.n |> Vec.normalize |> basisFrisvad 
                let w2t = t2w |> Mat.transpose

                let brdf = v.c / PI 

                let mutable illumination = V4d.Zero
            
                ////////////////////////////////////////////////////////

                let addr = 0
 
                let vAddr = addr * Config.Light.VERT_PER_LIGHT
                let iAddr = addr * Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT

                ////////////////////////////////////////////////////////

                let l2w = M33dFromCols  (V3d.Cross((lUps.[addr]), (lForwards.[addr]))) lUps.[addr] lForwards.[addr]
                            
                let w2l = l2w |> Mat.transpose

                let t2l = w2l * t2w

                let l2t = l2w * w2t

                ////////////////////////////////////////////////////////

                let iIdx = 0

                let vt = Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>() 
                            
                for vtc in 0 .. lBaseComponents.[addr] - 1 do
                    let vtcAddr = lPatchIndices.[iIdx + vtc] + vAddr
                    vt.[vtc] <- w2t * (lVertices.[vtcAddr] - P)

                ////////////////////////////////////////////////////////

                let (clippedVa, clippedVc) = clipPatch(V3d.Zero, V3d.OOI, vt, lBaseComponents.[addr])

                if clippedVc <> 0 then

                    let eps = 1e-9

                    let lightPlaneN = w2t * lForwards.[addr] |> Vec.normalize   

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

                        let (caseOffset, v1Idx) =
                            if CLAMP_POLYGON_RESULT = CLAMP_POLYGON_RESULT_POINT then
                                printfn "CASE_CORNER"
                                (CASE_CORNER_OFFSET, clampP0Id)
                            elif CLAMP_POLYGON_RESULT = CLAMP_POLYGON_RESULT_LINE then
                                printfn "CASE_EDGE"
                                (CASE_EDGE_OFFSET, clampP1ID)
                            else (*CLAMP_POLYGON_RESULT =  CLAMP_POLYGON_RESULT_NONE *) 
                                printfn "CASE_INSIDE"
                                (CASE_INSIDE_OFFSET, 0)
                                     
                                     
                        // XYZ -> Spherical coords; 
                        let verticesNormalized = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_TWO>, V3d>() 

                        let vertices = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_TWO>, V3d>() 

                        // X -> luminance, Y -> Weight
                        // let funVal = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_TWO>, V2d>() 


                        let mutable vc = clippedVc
                        let mutable offset = 0
                        if caseOffset <> CASE_CORNER_OFFSET then 
                            vertices.[0] <- closestPoint
                            verticesNormalized.[0] <- closestPoint |> Vec.normalize
                            // funVal.[0]   <- sampleIrr t2w addr closestPoint
                            vc <- vc + 1 
                            offset <- 1

                        // is the point in front of the light or behind?
                        // if behind, iterate the light vertices backwards for counter clockwise order
                        if Vec.dot (lForwards.[addr]) (t2w *(closestPoint |> Vec.normalize)) < 0.0 then 
                                    
                            for i in 0 .. clippedVc - 1 do 
                                vertices.[i + offset] <- clippedVa.[(v1Idx + i) % clippedVc] 
                                verticesNormalized.[i + offset] <- clippedVa.[(v1Idx + i) % clippedVc] |> Vec.normalize
                                // funVal.[i + offset]   <- sampleIrr t2w  addr clippedVa.[(v1Idx + i) % clippedVc]
                                            
                        else
                            let mutable j = 0
                            for i in clippedVc - 1 .. -1 .. 0 do 
                                vertices.[j + offset] <- clippedVa.[(v1Idx + i) % clippedVc] 
                                verticesNormalized.[j + offset] <- clippedVa.[(v1Idx + i) % clippedVc] |> Vec.normalize
                                // funVal.[j + offset]   <- sampleIrr t2w  addr clippedVa.[(v1Idx + i) % clippedVc]
                                                
                                j <- j + 1

                        let delVertexData     = QUAD_DATA.getInitVertexData caseOffset
                        let delNEdgeData      = QUAD_DATA.getInitNeighbourEdgeData caseOffset
                        let delMetaData       = QUAD_DATA.getInitMetaData caseOffset
                        let delFaceData       = QUAD_DATA.getInitFaceData caseOffset
                        let (delStack, delSPInit) = QUAD_DATA.getInitStackData caseOffset
                        let mutable delSP = delSPInit

                        ////////////////////////////////////////
                        // transform to a Delaunay triangulation

                        let mutable oneNotLd = false
                                    
                        while delSP >= 0 do

                            // get edge Id
                            let eId = delStack.[delSP]

                            // unmark edge
                            delMetaData.[eId] <- V2i(delMetaData.[eId].X, 0)
                            delSP <- delSP - 1
                                        
                            let eVertices = delVertexData.[eId]
                            let eNEdges = delNEdgeData.[eId]

                            // test if edge is locally delaunay
                                // true if flip, false otherwise
    
                            let notLD = 
                                let a = verticesNormalized.[eVertices.X].XYZ
                                let b = verticesNormalized.[eVertices.Y].XYZ
                                let c = verticesNormalized.[eVertices.Z].XYZ
                                let d = verticesNormalized.[eVertices.W].XYZ
                                            
                                (Vec.dot (a - c) (Vec.cross (b - c) (d - c))) < 0.0     

                            oneNotLd <- oneNotLd || notLD
                                        
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

                        ////////////////////////////////////////////////////////

                        let mutable areaSum = 0.0

                        printfn "Faces: "  

                        for f in 0 .. MAX_FACES - 1 do
                            let face = delFaceData.[f]

                            if face.X <> -1 then
                                
                                            
                                let area = computeSphericalExcess (verticesNormalized.[face.Y]) (verticesNormalized.[face.Z]) (verticesNormalized.[face.W])

                                printfn " - %A - Area: %A" face area

                                areaSum <- areaSum + area 


                        printfn "Area Sum: %A" areaSum

                        let getTrafo pos = Trafo3d.Translation pos |> Mod.init
                        
                        let mutable sg = Sg.empty

                        sg <- Sg.group' [sg; pointSg 0.98 (C4b(255, 255, 255, 100)) (Trafo3d.Identity |> Mod.init)]

                        for i in 0 .. vc - 1 do 
                            sg <- Sg.group' [sg; pointSg 0.001 C4b.Red (getTrafo (verticesNormalized.[i]))]
            
                        
                        printfn "Vertices %A : %A" vc verticesNormalized

                        printfn "Edges: "
                        for i in 0 .. MAX_EDGES - 1 do         
                            if delVertexData.[i].X <> -1 && delVertexData.[i].Z <> -1 then 
                                let edgeStart = verticesNormalized.[delVertexData.[i].X]
                                let edgeEnd   = verticesNormalized.[delVertexData.[i].Z]

                                printfn " - %A -> %A" edgeStart edgeEnd

                                sg <- Sg.group' [sg; arcSg edgeStart edgeEnd C4b.Red (Trafo3d.Identity |> Mod.init)]

                        sg

                    else
                        
                        Sg.empty
                else 

                    Sg.empty

            )


    module Rendering =

        open Aardvark.SceneGraph

        open RenderInterop
        open Utils
        open Utils.Sg
        open Aardvark.Base.Incremental


        let delIrrIntApproxRenderTask (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 

            
            //let sceneSg = 
            //    [
            //        sceneSg
            //        Debug.delaunyScene data.lights |> Sg.dynamic
            //    ]
            //    |> Sg.group'
            
            
            sceneSg
                |> setupFbEffects [ 
                        delaunyIrrIntegration |> toEffect
                    ]
                |> Light.Sg.setLightCollectionUniforms data.lights
                |> setupPhotometricData data.photometricData
                |> setupCamera data.view data.projTrafo data.viewportSize 
                |> setUniformDT data.dt
                |> setUniformUsePhotometry data.usePhotometry
                |> Sg.compile data.runtime signature



        let delIrrIntApproxFb (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            delIrrIntApproxRenderTask data signature sceneSg
            |> RenderTask.renderToColor data.viewportSize
