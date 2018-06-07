namespace Render

module EffectApDelaunayIrradianceIntegration = 
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open FShade

    open Light.Effect
    open EffectUtils
    open PhotometricLight


    [<Literal>]
    let MAX_EDGES = 13

    [<Literal>]
    let MAX_FACES = 10


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
    let MAX_EDGES_ALL = 39 // MAX_EDGES * NUM_CASE

    [<Literal>]
    let MAX_FACES_ALL = 30 // MAX_FACES * NUM_CASE


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
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            // outer
                            | 0 -> yield V4i(0, -1, 1, 2) 
                            | 1 -> yield V4i(1, -1, 2, 0) 
                            | 2 -> yield V4i(2, -1, 3, 0) 
                            | 3 -> yield V4i(3, -1, 0, 2) 
                            
                            // inner
                            | 4 -> yield V4i(0, 1, 2, 3)
                            
                            // fill up
                            | _ -> yield V4i(-1)


                    // CASE_EDGE
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            // outer
                            | 0 -> yield V4i(0, -1, 1, 2) 
                            | 1 -> yield V4i(1, -1, 2, 0) 
                            | 2 -> yield V4i(2, -1, 3, 0) 
                            | 3 -> yield V4i(3, -1, 4, 0) 
                            | 4 -> yield V4i(4, -1, 0, 3) 
                            
                            // inner
                            | 5 -> yield V4i(0, 1, 2, 3) 
                            | 6 -> yield V4i(0, 2, 3, 4) 
                            
                            // fill up
                            | _ -> yield V4i(-1)

                    // CASE_INSIDE
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            // outer
                            | 0 -> yield V4i(1, -1, 2, 0) // 0
                            | 1 -> yield V4i(2, -1, 3, 0) // 1
                            | 2 -> yield V4i(3, -1, 4, 0) // 2
                            | 3 -> yield V4i(4, -1, 1, 0) // 3
                            
                            // inner
                            | 4 -> yield V4i(0, 4, 1, 2) // 4
                            | 5 -> yield V4i(0, 1, 2, 3) // 5
                            | 6 -> yield V4i(0, 2, 3, 4) // 6
                            | 7 -> yield V4i(0, 3, 4, 1) // 7
                            
                            // fill up
                            | _ -> yield V4i(-1)
                    |]

            // edges
            // v0 -> o0, o0 -> v1, v1 -> o1, o1 -> v0
            let E = [|
                    // CASE_CORNER
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            // outer
                            | 0 -> yield V4i(-1, -1, 1, 4) 
                            | 1 -> yield V4i(-1, -1, 4, 0) 
                            | 2 -> yield V4i(-1, -1, 3, 4) 
                            | 3 -> yield V4i(-1, -1, 4, 2) 
                            
                            // inner
                            | 4 -> yield V4i(0, 1, 2, 3)
                            
                            // fill up
                            | _ -> yield V4i(-1)
                                                
                    // CASE_EDGE
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            // outer
                            | 0 -> yield V4i(-1, -1, 1, 5)
                            | 1 -> yield V4i(-1, -1, 5, 0)
                            | 2 -> yield V4i(-1, -1, 6, 5)
                            | 3 -> yield V4i(-1, -1, 4, 6)
                            | 4 -> yield V4i(-1, -1, 6, 3)
                            
                            // inner
                            | 5 -> yield V4i(0, 1, 2, 6)
                            | 6 -> yield V4i(5, 2, 3, 4)
                            
                            // fill up
                            | _ -> yield V4i(-1)

                    // CASE_INSIDE
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            // outer
                            | 0 -> yield V4i(-1, -1, 5, 4) 
                            | 1 -> yield V4i(-1, -1, 6, 5) 
                            | 2 -> yield V4i(-1, -1, 7, 6) 
                            | 3 -> yield V4i(-1, -1, 4, 7) 
                            
                            // inner
                            | 4 -> yield V4i(7, 3, 0, 5)
                            | 5 -> yield V4i(4, 0, 1, 6)
                            | 6 -> yield V4i(5, 1, 2, 7)
                            | 7 -> yield V4i(6, 2, 3, 4)
                            
                            // fill up
                            | _ -> yield V4i(-1)
                    |]

            // meta    
            // inside, marked
            // 1 = true, 0 = false
            let M = [|
                    // CASE_CORNER
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            // outer
                            | 0 -> yield V2i(0, 0) 
                            | 1 -> yield V2i(0, 0) 
                            | 2 -> yield V2i(0, 0) 
                            | 3 -> yield V2i(0, 0) 
                            
                            // inner
                            | 4 -> yield V2i(1, 0) 
                            
                            // fill up
                            | _ -> yield V2i(-1) 
                                                
                    // CASE_EDGE
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            // outer
                            | 0 -> yield V2i(0, 0) 
                            | 1 -> yield V2i(0, 0) 
                            | 2 -> yield V2i(0, 0) 
                            | 3 -> yield V2i(0, 0) 
                            | 4 -> yield V2i(0, 0) 
                            
                            // inner
                            | 5 -> yield V2i(1, 0)
                            | 6 -> yield V2i(1, 0)
                            
                            // fill up
                            | _ -> yield V2i(-1) 

                    // CASE_INSIDE
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            // outer
                            | 0 -> yield V2i(0, 0)
                            | 1 -> yield V2i(0, 0)
                            | 2 -> yield V2i(0, 0)
                            | 3 -> yield V2i(0, 0)
                            
                            // inner
                            | 4 -> yield V2i(1, 0) 
                            | 5 -> yield V2i(1, 0) 
                            | 6 -> yield V2i(1, 0) 
                            | 7 -> yield V2i(1, 0) 
                            
                            // fill up
                            | _ -> yield V2i(-1) 
                    |]
            
            // faces vertices
            // v0, v1, v2, (IDHash v0 v1 v2)
            // v0 should be the smallest ID, v0, v1, v2 ordered counter clockwise
            let FV = [|
                    // CASE_CORNER
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2)) 
                            | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3)) 
                            
                            // fill 
                            | _ -> yield V4i(-1)
                                                
                    // CASE_EDGE
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2)) 
                            | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3)) 
                            | 2 -> yield V4i(0, 3, 4, (IDHash 0 3 4)) 
                            
                            // fill 
                            | _ -> yield V4i(-1)

                    // CASE_INSIDE
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2)) 
                            | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3)) 
                            | 2 -> yield V4i(0, 3, 4, (IDHash 0 3 4)) 
                            | 3 -> yield V4i(0, 4, 1, (IDHash 0 4 1)) 
                            
                            // fill up
                            | _ -> yield V4i(-1)
                    |]

            let FE = [|
                    // CASE_CORNER
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i(0, 1, 4) 
                            | 1 -> yield V3i(4, 2, 3) 
                            
                            // fill 
                            | _ -> yield V3i(-1)
                                                
                    // CASE_EDGE
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i(0, 1, 5) 
                            | 1 -> yield V3i(5, 2, 6) 
                            | 2 -> yield V3i(6, 3, 4) 
                            
                            // fill 
                            | _ -> yield V3i(-1)

                    // CASE_INSIDE
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i(4, 0, 5) 
                            | 1 -> yield V3i(5, 1, 6) 
                            | 2 -> yield V3i(6, 2, 7) 
                            | 3 -> yield V3i(7, 3, 4) 
                            
                            // fill up
                            | _ -> yield V3i(-1)
                    |]

            let NEXT_FREE_EDGE_ADDR = [| 5; 7; 8 |]

            let NEXT_FREE_FACE_ADDR = [| 2; 3; 4 |]

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
        let getInitFaceVertexData caseOffset =             
            let d = Arr<N<MAX_FACES>, V4i>()
            for i in 0 .. MAX_FACES - 1 do
                d.[i] <- ALL.FV.[MAX_FACES * caseOffset + i]
            d

        [<ReflectedDefinition>]
        let getInitFaceEdgeData caseOffset =             
            let d = Arr<N<MAX_FACES>, V3i>()
            for i in 0 .. MAX_FACES - 1 do
                d.[i] <- ALL.FE.[MAX_FACES * caseOffset + i]
            d

        [<ReflectedDefinition>]
        let getInitFreeEdgeAddr caseOffset = ALL.NEXT_FREE_EDGE_ADDR.[caseOffset]

        [<ReflectedDefinition>]
        let getInitFreeFaceAddr caseOffset = ALL.NEXT_FREE_FACE_ADDR.[caseOffset]

    type Vertex = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
    }  

    module DataMutation =

        [<ReflectedDefinition>]
        let flipEdge (vertices : Arr<N<MAX_EDGES>, V4i>) (edges : Arr<N<MAX_EDGES>, V4i>) (meta : Arr<N<MAX_EDGES>, V2i>) (faceVertices : Arr<N<MAX_FACES>, V4i>) (faceEdges : Arr<N<MAX_FACES>, V3i>) (stack : Arr<N<MAX_EDGES>, int>) (sp : int) eId =

            let originalVertices = vertices.[eId]
            let originalEdges = edges.[eId]

            // unmrak edge
            meta.[eId] <- V2i(meta.[eId].X, 0)

            // flip edge
            vertices.[eId] <- V4i(originalVertices.Y, originalVertices.Z, originalVertices.W, originalVertices.X)
            edges.[eId] <- V4i(originalEdges.Y, originalEdges.Z, originalEdges.W, originalEdges.X)

            // adopt faces
            let f0Id = IDHash (originalVertices.X) (originalVertices.Y) (originalVertices.Z)
            let f1Id = IDHash (originalVertices.Z) (originalVertices.W) (originalVertices.X)

            for f in 0 .. MAX_FACES - 1 do
                if faceVertices.[f].W = f0Id then
                    faceVertices.[f] <- V4i((vertices.[eId].X), (vertices.[eId].Y), (vertices.[eId].Z), (IDHash (vertices.[eId].X) (vertices.[eId].Y) (vertices.[eId].Z)))
                    faceEdges.[f] <- V3i((edges.[eId].X), (edges.[eId].Y), eId)

                if faceVertices.[f].W = f1Id then
                    faceVertices.[f] <- V4i((vertices.[eId].Z), (vertices.[eId].W), (vertices.[eId].X), (IDHash (vertices.[eId].Z) (vertices.[eId].W) (vertices.[eId].X)))
                    faceEdges.[f] <- V3i((edges.[eId].Z), (edges.[eId].W), eId)
              
             
            let mutable sp = sp

            // adapt neighbour edges
            for ne in 0 .. 3 do
                let (neId, oppositeEdges, oppositeVertex) = 
                    match ne with
                    | 0 -> (originalEdges.X, originalEdges.ZW, originalVertices.W)
                    | 1 -> (originalEdges.Y, originalEdges.ZW, originalVertices.W)
                    | 2 -> (originalEdges.Z, originalEdges.XY, originalVertices.Y)
                    | _ -> (originalEdges.W, originalEdges.XY, originalVertices.Y)

                                                    
                if eId = edges.[neId].X then
                    edges.[neId] <- V4i(oppositeEdges.X, eId, edges.[neId].Z, edges.[neId].W)
                    vertices.[neId] <- V4i(vertices.[neId].X, oppositeVertex, vertices.[neId].Z, vertices.[neId].W)
                elif eId = edges.[neId].Y then
                    edges.[neId] <- V4i(eId, oppositeEdges.Y, edges.[neId].Z, edges.[neId].W)
                    vertices.[neId] <- V4i(vertices.[neId].X, oppositeVertex, vertices.[neId].Z, vertices.[neId].W)
                elif eId = edges.[neId].Z then
                    edges.[neId] <- V4i(edges.[neId].X, edges.[neId].Y, oppositeEdges.X, eId)
                    vertices.[neId] <- V4i(vertices.[neId].X, vertices.[neId].Y, vertices.[neId].Z, oppositeVertex)
                else (* eId = edges.[neId].W *)
                    edges.[neId] <- V4i(edges.[neId].X, edges.[neId].Y, eId, oppositeEdges.Y)
                    vertices.[neId] <- V4i(vertices.[neId].X, vertices.[neId].Y, vertices.[neId].Z, oppositeVertex)

                // if inside and not marked -> mark it
                if meta.[neId].X = 1 && meta.[neId].Y = 0 then
                    sp <- sp + 1
                    stack.[sp] <- neId
                    meta.[neId] <- V2i(1, 1)


            (vertices, edges, meta, faceVertices, faceEdges, stack, sp)


        // Inserts vertex with Id = vId into face with faceId. FaceId is position in Array, not faceHash
        [<ReflectedDefinition>]
        let insertVertexIntoFace (vertices : Arr<N<MAX_EDGES>, V4i>) (edges : Arr<N<MAX_EDGES>, V4i>) (meta : Arr<N<MAX_EDGES>, V2i>) (faceVertices : Arr<N<MAX_FACES>, V4i>) (faceEdges : Arr<N<MAX_FACES>, V3i>) nextFreeEdgeAddr nextFreeFaceAddr faceId vId =
        
            let fV = faceVertices.[faceId]
            let fE = faceEdges.[faceId]


            let mutable nextFreeFaceAddr = nextFreeFaceAddr

            for e in 0 .. 2 do
                let newEdgeId = nextFreeEdgeAddr + e

                // insert new edge
                vertices.[newEdgeId] <- V4i(fV.[(e + 0) % 3], fV.[(e + 1) % 3], vId, fV.[(e + 2) % 3])
                edges.[newEdgeId] <- V4i(fE.[(e + 0) % 3], nextFreeEdgeAddr + (e + 1) % 3, nextFreeEdgeAddr + (e + 2) % 3, fE.[(e + 2) % 3])
                meta.[newEdgeId] <- V2i(1, 0)

                // insert new face
                faceVertices.[nextFreeFaceAddr] <- V4i(vertices.[newEdgeId].X, vertices.[newEdgeId].Y, vertices.[newEdgeId].Z, (IDHash vertices.[newEdgeId].X vertices.[newEdgeId].Y vertices.[newEdgeId].Z))
                faceEdges.[nextFreeFaceAddr] <- V3i(edges.[newEdgeId].X, edges.[newEdgeId].Y, newEdgeId)
                nextFreeFaceAddr <- nextFreeFaceAddr + 1

                // update outside edge
                let currentOpposite =
                    match (e + 2) % 3 with
                    | 0 -> fV.X
                    | 1 -> fV.Y
                    | _ -> fV.Z


                let updateEdgeId = 
                    match e with
                    | 0 -> fE.X
                    | 1 -> fE.Y
                    | _ -> fE.Z

                if vertices.[updateEdgeId].Y = currentOpposite then                
                    vertices.[updateEdgeId] <- V4i(vertices.[updateEdgeId].X, vId, vertices.[updateEdgeId].Z, vertices.[updateEdgeId].W)
                    edges.[updateEdgeId] <- V4i(nextFreeEdgeAddr + (e + 1) % 3, newEdgeId, edges.[updateEdgeId].Z, edges.[updateEdgeId].W)

                else (* vertices.[updateEdgeId].W = currentOpposite *)
                    vertices.[updateEdgeId] <- V4i(vertices.[updateEdgeId].X, vertices.[updateEdgeId].Y, vertices.[updateEdgeId].Z, vId)
                    edges.[updateEdgeId] <- V4i(edges.[updateEdgeId].X, edges.[updateEdgeId].Y, nextFreeEdgeAddr + (e + 1) % 3, newEdgeId)

                // flip new edge
                if meta.[updateEdgeId].X = 1 && meta.[updateEdgeId].Y = 0 then
                    meta.[updateEdgeId] <- V2i(1, 1)
    
            // remove face where vertex was inserted
            faceVertices.[faceId] <- V4i(-1)

            (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr + 3, nextFreeFaceAddr)


        [<ReflectedDefinition>]
        let private mapLocalToGlobalId freedAddr nextFreeEdgeAddr edgeLocalId =
            match edgeLocalId with
            | 0 -> freedAddr
            | _ -> nextFreeEdgeAddr + edgeLocalId - 1
       
        // Splits edgy edgeId by inseting vertex with Id = vId 
        [<ReflectedDefinition>]
        let spliteEdge (vertices : Arr<N<MAX_EDGES>, V4i>) (edges : Arr<N<MAX_EDGES>, V4i>) (meta : Arr<N<MAX_EDGES>, V2i>) (faceVertices : Arr<N<MAX_FACES>, V4i>) (faceEdges : Arr<N<MAX_FACES>, V3i>) nextFreeEdgeAddr nextFreeFaceAddr edgeId vId =
        
            let fhash1 = IDHash (vertices.[edgeId].X) (vertices.[edgeId].Y) (vertices.[edgeId].Z)
            let fhash2 = IDHash (vertices.[edgeId].Z) (vertices.[edgeId].W) (vertices.[edgeId].X)
            
            let splitEdgeV = Arr<N<4>, int>([| vertices.[edgeId].X; vertices.[edgeId].Y; vertices.[edgeId].Z; vertices.[edgeId].W |])
            let splitEdgeE = Arr<N<4>, int>([| edges.[edgeId].X; edges.[edgeId].Y; edges.[edgeId].Z; edges.[edgeId].W |])


            let mutable nextFreeFaceAddrDynamic = nextFreeFaceAddr

            for eIdx in 0 .. 3 do
                if splitEdgeV.[eIdx] <> -1 then
                    
                    
                    
                    let thisGlobalId = eIdx |> mapLocalToGlobalId edgeId nextFreeEdgeAddr


                    ///////////////////////////////////////////////////
                    // Insert new Edge

                    vertices.[thisGlobalId] <- V4i(splitEdgeV.[eIdx], splitEdgeV.[(eIdx + 1) % 4], vId, splitEdgeV.[(eIdx + 3) % 4])

                    let mutable eO1IsEmpty = false
                    let mutable eO2IsEmpty = false

                    let eO1 =
                        if splitEdgeV.[(eIdx + 1) % 4] = -1 then
                            eO1IsEmpty <- true
                            V2i(-1, -1)
                        else
                            V2i(splitEdgeE.[eIdx], ((eIdx + 1) % 4) |> mapLocalToGlobalId edgeId nextFreeEdgeAddr)

                    let eO2 =
                        if splitEdgeV.[(eIdx + 3) % 4] = -1 then
                            eO2IsEmpty <- true
                            V2i(-1, -1)
                        else
                            V2i(((eIdx + 3) % 4) |> mapLocalToGlobalId edgeId nextFreeEdgeAddr, splitEdgeE.[(eIdx + 3) % 4])

                    edges.[thisGlobalId] <- V4i(eO1.X, eO1.Y, eO2.X, eO2.Y)
                    
                    meta.[thisGlobalId] <- if eO1IsEmpty || eO2IsEmpty then V2i(0, 0) else V2i(1, 0)
                    


                    if not eO1IsEmpty then

                        ///////////////////////////////////////////////////
                        // Update Existing Edge

                        if vertices.[splitEdgeE.[eIdx]].Y = splitEdgeV.[(eIdx + 2) % 4] then
                            vertices.[splitEdgeE.[eIdx]] <- V4i(vertices.[splitEdgeE.[eIdx]].X, vId, vertices.[splitEdgeE.[eIdx]].Z, vertices.[splitEdgeE.[eIdx]].W)
                            edges.[splitEdgeE.[eIdx]] <- V4i(((eIdx + 1) % 4) |> mapLocalToGlobalId edgeId nextFreeEdgeAddr, thisGlobalId, edges.[splitEdgeE.[eIdx]].Z, edges.[splitEdgeE.[eIdx]].W)
                        else
                            vertices.[splitEdgeE.[eIdx]] <- V4i(vertices.[splitEdgeE.[eIdx]].X, vertices.[splitEdgeE.[eIdx]].Y, vertices.[splitEdgeE.[eIdx]].Z, vId)
                            edges.[splitEdgeE.[eIdx]] <- V4i(edges.[splitEdgeE.[eIdx]].X, edges.[splitEdgeE.[eIdx]].Y, ((eIdx + 1) % 4) |> mapLocalToGlobalId edgeId nextFreeEdgeAddr, thisGlobalId)

                        ///////////////////////////////////////////////////
                        // Insert new Face

                        faceVertices.[nextFreeFaceAddrDynamic] <- V4i(splitEdgeV.[eIdx], splitEdgeV.[(eIdx + 1) % 4], vId, IDHash splitEdgeV.[eIdx] splitEdgeV.[(eIdx + 1) % 4] vId)
                        faceEdges.[nextFreeFaceAddrDynamic] <- V3i(splitEdgeE.[eIdx], ((eIdx + 1) % 4) |> mapLocalToGlobalId edgeId nextFreeEdgeAddr, thisGlobalId)
                        nextFreeFaceAddrDynamic <- nextFreeFaceAddrDynamic + 1

            for i in 0 .. nextFreeFaceAddr - 1 do
                if faceVertices.[i].W = fhash1 || faceVertices.[i].W = fhash2 then
                    faceVertices.[i] <- V4i(-1)

            (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr + 3, nextFreeFaceAddrDynamic)


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
                                let epm = 1e-5
                                let epb = 1e-3

                                let lightPlaneN = w2t * uniform.LForwards.[addr] |> Vec.normalize   

                                // find closest point limited to upper hemisphere
                                let t = (- clippedVa.[0]) |> Vec.dot lightPlaneN
                                let mutable closestPoint = t * (-lightPlaneN)
                                                    
                                if (Vec.dot closestPoint V3d.OOI) < 0.0 then
                                    let newDir = V3d(closestPoint.X, closestPoint.Y, 0.0) |> Vec.normalize
                                    closestPoint <- linePlaneIntersection V3d.Zero newDir (clippedVa.[0]) lightPlaneN
                                    
                                let insideLightPlane = (Vec.length closestPoint) < eps
                                
                                if not insideLightPlane then

                                    
                                    let (closestPointClamped, CLAMP_POLYGON_RESULT, clampP0Id, clampP1ID) = clampPointToPolygonP1 clippedVa clippedVc closestPoint

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
                                    let vertices = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_THREE>, V3d>() 
                                     
                                    // XYZ -> Spherical coords; 
                                    let verticesNormalized = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_THREE>, V3d>() 

                                    // X -> luminance, Y -> Weight
                                    let funVal = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_THREE>, V2d>() 


                                    let mutable vc = clippedVc
                                    let mutable offset = 0
                                    if caseOffset <> CASE_CORNER_OFFSET then 
                                        vertices.[0] <- closestPointClamped |> Vec.normalize
                                        verticesNormalized.[0] <- closestPointClamped |> Vec.normalize
                                        funVal.[0]   <- sampleIrr t2w addr closestPointClamped
                                        vc <- vc + 1 
                                        offset <- 1

                                    // is the point in front of the light or behind?
                                    // if behind, iterate the light vertices backwards for counter clockwise order
                                    if Vec.dot (uniform.LForwards.[addr]) (t2w *(closestPointClamped |> Vec.normalize)) < 0.0 then 
                                    
                                        for i in 0 .. clippedVc - 1 do 
                                            vertices.[i + offset] <- clippedVa.[(v1Idx + i) % clippedVc]
                                            verticesNormalized.[i + offset] <- clippedVa.[(v1Idx + i) % clippedVc] |> Vec.normalize
                                            funVal.[i + offset]   <- sampleIrr t2w  addr clippedVa.[(v1Idx + i) % clippedVc]
                                            
                                    else
                                        let mutable j = 0
                                        for i in clippedVc - 1 .. -1 .. 0 do 
                                            vertices.[j + offset] <- clippedVa.[(v1Idx + i) % clippedVc]
                                            verticesNormalized.[j + offset] <- clippedVa.[(v1Idx + i) % clippedVc] |> Vec.normalize
                                            funVal.[j + offset]   <- sampleIrr t2w  addr clippedVa.[(v1Idx + i) % clippedVc]
                                                
                                            j <- j + 1

                                    ////////////////////////////////////////
                                    // load inital data

                                    let mutable delVertexData       = caseOffset |> QUAD_DATA.getInitVertexData
                                    let mutable delNEdgeData        = caseOffset |> QUAD_DATA.getInitNeighbourEdgeData 
                                    let mutable delMetaData         = caseOffset |> QUAD_DATA.getInitMetaData 
                                    let mutable delFaceVertexData   = caseOffset |> QUAD_DATA.getInitFaceVertexData 
                                    let mutable delFaceEdgeData     = caseOffset |> QUAD_DATA.getInitFaceEdgeData 

                                    let mutable delNextFreeEdgeAddr = caseOffset |> QUAD_DATA.getInitFreeEdgeAddr
                                    let mutable delNextFreeFaceAddr = caseOffset |> QUAD_DATA.getInitFreeFaceAddr

                                    ////////////////////////////////////////
                                    // insert additional verticex = MRP
                                    
                                    let closestPointDir = closestPoint |> Vec.normalize

                                    // intersect normal with plane
                                    let mutable up = V3d.OOI
                                                                    
                                    if abs(Vec.dot up lightPlaneN) < eps then
                                        up <- up + (epb * closestPointDir) |> Vec.normalize     
                                    else
                                        let abovePlane = if (Vec.dot V3d.OOI closestPoint) < 0.0 && (Vec.dot closestPoint lightPlaneN) < 0.0 then false else true
                                        if abovePlane then
                                            if (Vec.dot up lightPlaneN) > 0.0 then
                                                up <- up + (abs(Vec.dot up lightPlaneN) + epb) * (-lightPlaneN) |> Vec.normalize
                                    
                                    
                                    let normPlanePoint = linePlaneIntersection V3d.Zero up (clippedVa.[0]) lightPlaneN // tangent space

                                    let mrpDir = (closestPointDir + (normPlanePoint |> Vec.normalize)) |> Vec.normalize
                                    let mrp = linePlaneIntersection V3d.Zero mrpDir (clippedVa.[0]) lightPlaneN

                                    let (mrpClamped, CLAMP_POLYGON_RESULT, clampP0Id, clampP1Id) =   clampPointToPolygonP3 vertices vc mrp 
                                    let mrpClampedDir = mrpClamped |> Vec.normalize

                                    match CLAMP_POLYGON_RESULT with
                                    | CLAMP_POLYGON_RESULT_NONE -> 

                                        if Vec.dot mrpClampedDir (closestPointClamped |> Vec.normalize) < (1.0 - 1e-8) then

                                            let mutable splitEdge = false
                                            let mutable param = 0

                                            let mutable foundFace = false
                        
                                            for f in 0 .. MAX_FACES - 1 do
                                                if not foundFace && delFaceVertexData.[f].W <> -1 then 
                            
                                                    let face = delFaceVertexData.[f]
                                                                                        
                                                    let dotZN = Vec.cross (verticesNormalized.[face.X]) (verticesNormalized.[face.Z]) |> Vec.normalize |> Vec.dot mrpClampedDir 
                                                    let dotYN = Vec.cross (verticesNormalized.[face.Z]) (verticesNormalized.[face.Y]) |> Vec.normalize |> Vec.dot mrpClampedDir 
                                                    let dotXN = Vec.cross (verticesNormalized.[face.Y]) (verticesNormalized.[face.X]) |> Vec.normalize |> Vec.dot mrpClampedDir 

                                                    if dotXN >= 0.0 && dotYN >= 0.0 && dotZN >= 0.0 then 
                                                        
                                                        foundFace <- true

                                                        let x = dotXN < eps
                                                        let y = dotYN < eps
                                                        let z = dotZN < eps

                                                        if (x && y) || (x && z) || (y && z) then
                                                            ()
                                                        elif x || y || z then

                                                            splitEdge <- true

                                                            param  <- 
                                                                if x then delFaceEdgeData.[f].X
                                                                elif y then delFaceEdgeData.[f].Y
                                                                else delFaceEdgeData.[f].Z
                                        
                                                        else
                                                            param <- f

                                                 

                                            verticesNormalized.[vc] <- mrpClampedDir
                                            funVal.[vc]   <- sampleIrr t2w addr mrpClamped
                                            vc <- vc + 1 

                                            if splitEdge then
                                                let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = (vc - 1) |> DataMutation.spliteEdge delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData delNextFreeEdgeAddr delNextFreeFaceAddr param
                                                delVertexData       <- vertices
                                                delNEdgeData        <- edges
                                                delMetaData         <- meta
                                                delFaceVertexData   <- faceVertices
                                                delFaceEdgeData     <- faceEdges
                                                delNextFreeEdgeAddr <- nextFreeEdgeAddr
                                                delNextFreeFaceAddr <- nextFreeFaceAddr

                                            else
                                                let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = (vc - 1) |> DataMutation.insertVertexIntoFace delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData delNextFreeEdgeAddr delNextFreeFaceAddr param
                                                delVertexData       <- vertices
                                                delNEdgeData        <- edges
                                                delMetaData         <- meta
                                                delFaceVertexData   <- faceVertices
                                                delFaceEdgeData     <- faceEdges
                                                delNextFreeEdgeAddr <- nextFreeEdgeAddr
                                                delNextFreeFaceAddr <- nextFreeFaceAddr
                                    
                                    | CLAMP_POLYGON_RESULT_LINE -> 

                                        let mutable edgeToSplit = 0

                                        let mutable foundEdge = false
                        
                                        for e in 0 .. MAX_EDGES - 1 do
                                            if not foundEdge then 

                                                if (delVertexData.[e].X = clampP0Id && delVertexData.[e].Z = clampP1Id) || (delVertexData.[e].X = clampP1Id && delVertexData.[e].Z = clampP0Id) then
                                                    foundEdge <- true
                                                    edgeToSplit <- e
                            

                                        verticesNormalized.[vc] <- mrpClampedDir
                                        funVal.[vc]   <- sampleIrr t2w addr mrpClamped
                                        vc <- vc + 1 

                                        let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = (vc - 1) |> DataMutation.spliteEdge delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData delNextFreeEdgeAddr delNextFreeFaceAddr edgeToSplit
                                        delVertexData       <- vertices
                                        delNEdgeData        <- edges
                                        delMetaData         <- meta
                                        delFaceVertexData   <- faceVertices
                                        delFaceEdgeData     <- faceEdges
                                        delNextFreeEdgeAddr <- nextFreeEdgeAddr
                                        delNextFreeFaceAddr <- nextFreeFaceAddr
                                    
                                                                         
                                    | _ (* CLAMP_POLYGON_RESULT_POINT *) -> ()
                                      

                                    ////////////////////////////////////////
                                    // execute edge flip algorithm

                                    let mutable stack = Arr<N<MAX_EDGES>, int>()
                                    let mutable SP = -1

                                    for i in 0 .. MAX_EDGES - 1 do
                                        if delMetaData.[i].X = 1 then
                                            SP <- SP + 1
                                            stack.[SP] <- i
                                            delMetaData.[i] <- V2i(1, 1)


                                    ////////////////////////////////////////
                                    // transform to a Delaunay triangulation
                                                                        
                                    while SP >= 0 do

                                        // get edge Id
                                        let eId = stack.[SP]

                                        // unmark edge
                                        delMetaData.[eId] <- V2i(delMetaData.[eId].X, 0)
                                        SP <- SP - 1
                                        
                                        let eVertices = delVertexData.[eId]

                                        // test if edge is locally delaunay
                                            // true if flip, false otherwise
    
                                        let notLD = 
                                            let a = verticesNormalized.[delVertexData.[eId].X].XYZ
                                            let b = verticesNormalized.[delVertexData.[eId].Y].XYZ
                                            let c = verticesNormalized.[delVertexData.[eId].Z].XYZ
                                            let d = verticesNormalized.[delVertexData.[eId].W].XYZ
                                            
                                            (Vec.dot (a - c) (Vec.cross (b - c) (d - c))) < 0.0     
                                                                                    
                                        if notLD then
                                            // flip edge

                                            let (vertices, edges, meta, faceVertices, faceEdges, updatedStack, sp) = eId |> DataMutation.flipEdge delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData stack SP
                                            delVertexData       <- vertices
                                            delNEdgeData        <- edges
                                            delMetaData         <- meta
                                            delFaceVertexData   <- faceVertices
                                            delFaceEdgeData     <- faceEdges
                                            stack               <- updatedStack
                                            SP                  <- sp
                                    
                                    ////////////////////////////////////////
                                    // integrate

                                    
                                    let mutable patchIllumination = 0.0
                                    let mutable weightSum = 0.0

                                    for f in 0 .. MAX_FACES - 1 do
                                        let face = delFaceVertexData.[f]

                                        if face.W <> -1 then
                                            let area = computeSphericalExcess (verticesNormalized.[face.X]) (verticesNormalized.[face.Y]) (verticesNormalized.[face.Z])

                                            patchIllumination <- patchIllumination + area * (funVal.[face.X].X + funVal.[face.Y].X + funVal.[face.Z].X) / 3.0
                                            weightSum <- weightSum + area * (funVal.[face.X].Y + funVal.[face.Y].Y + funVal.[face.Z].Y) / 3.0


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
                        

            return V4d(0)// V4d(illumination.XYZ, v.c.W)
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
                    let epm = 1e-5
                    let epb = 1e-3

                    let lightPlaneN = w2t * lForwards.[addr] |> Vec.normalize   

                    // find closest point limited to upper hemisphere
                    let t = (- clippedVa.[0]) |> Vec.dot lightPlaneN
                    let mutable closestPoint = t * (-lightPlaneN)
                                                    
                    if (Vec.dot closestPoint V3d.OOI) < 0.0 then
                        let newDir = V3d(closestPoint.X, closestPoint.Y, 0.0) |> Vec.normalize
                        closestPoint <- linePlaneIntersection V3d.Zero newDir (clippedVa.[0]) lightPlaneN
                                    
                    let insideLightPlane = (Vec.length closestPoint) < eps
                                
                    if not insideLightPlane then

                        let closestPointDir = closestPoint |> Vec.normalize
                                                            
                        let (closestPointClamped, CLAMP_POLYGON_RESULT, clampP0Id, clampP1ID) = clampPointToPolygon clippedVa clippedVc closestPoint t2l

                                                                        
                        // let (closestPoint, CLAMP_POLYGON_RESULT, clampP0Id, clampP1ID) = clampPointToPolygon clippedVa clippedVc closestPoint t2l

                        let (caseOffset, v1Idx) =
                            if CLAMP_POLYGON_RESULT = CLAMP_POLYGON_RESULT_POINT then
                                (CASE_CORNER_OFFSET, clampP0Id)
                            elif CLAMP_POLYGON_RESULT = CLAMP_POLYGON_RESULT_LINE then
                                (CASE_EDGE_OFFSET, clampP1ID)
                            else (*CLAMP_POLYGON_RESULT =  CLAMP_POLYGON_RESULT_NONE *) 
                                (CASE_INSIDE_OFFSET, 0)
                                     
                        // XYZ -> Spherical coords; 
                        let vertices = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_THREE>, V3d>() 
                                     
                        // XYZ -> Spherical coords; 
                        let verticesNormalized = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_THREE>, V3d>() 


                        let mutable vc = clippedVc
                        let mutable offset = 0
                        if caseOffset <> CASE_CORNER_OFFSET then 
                            vertices.[0] <- closestPointClamped |> Vec.normalize
                            verticesNormalized.[0] <- closestPointClamped |> Vec.normalize
                            vc <- vc + 1 
                            offset <- 1

                        // is the point in front of the light or behind?
                        // if behind, iterate the light vertices backwards for counter clockwise order
                        if Vec.dot (lForwards.[addr]) (t2w *(closestPointClamped |> Vec.normalize)) < 0.0 then 
                                    
                            for i in 0 .. clippedVc - 1 do 
                                vertices.[i + offset] <- clippedVa.[(v1Idx + i) % clippedVc]
                                verticesNormalized.[i + offset] <- clippedVa.[(v1Idx + i) % clippedVc] |> Vec.normalize
                                            
                        else
                            let mutable j = 0
                            for i in clippedVc - 1 .. -1 .. 0 do 
                                vertices.[j + offset] <- clippedVa.[(v1Idx + i) % clippedVc]
                                verticesNormalized.[j + offset] <- clippedVa.[(v1Idx + i) % clippedVc] |> Vec.normalize
                                                
                                j <- j + 1

                        ////////////////////////////////////////
                        // load inital data

                        let mutable delVertexData       = caseOffset |> QUAD_DATA.getInitVertexData
                        let mutable delNEdgeData        = caseOffset |> QUAD_DATA.getInitNeighbourEdgeData 
                        let mutable delMetaData         = caseOffset |> QUAD_DATA.getInitMetaData 
                        let mutable delFaceVertexData   = caseOffset |> QUAD_DATA.getInitFaceVertexData 
                        let mutable delFaceEdgeData     = caseOffset |> QUAD_DATA.getInitFaceEdgeData 

                        let mutable delNextFreeEdgeAddr = caseOffset |> QUAD_DATA.getInitFreeEdgeAddr
                        let mutable delNextFreeFaceAddr = caseOffset |> QUAD_DATA.getInitFreeFaceAddr

                        ////////////////////////////////////////
                        // insert additional verticex = MRP
                                    
                        let closestPointDir = closestPoint |> Vec.normalize

                        // intersect normal with plane
                        let mutable up = V3d.OOI
                                                                    
                        if abs(Vec.dot up lightPlaneN) < eps then
                            up <- up + (epb * closestPointDir) |> Vec.normalize     
                        else
                            let abovePlane = if (Vec.dot V3d.OOI closestPoint) < 0.0 && (Vec.dot closestPoint lightPlaneN) < 0.0 then false else true
                            if abovePlane then
                                if (Vec.dot up lightPlaneN) > 0.0 then
                                    up <- up + (abs(Vec.dot up lightPlaneN) + epb) * (-lightPlaneN) |> Vec.normalize
                                    
                                    
                        let normPlanePoint = linePlaneIntersection V3d.Zero up (clippedVa.[0]) lightPlaneN // tangent space

                        let mrpDir = (closestPointDir + (normPlanePoint |> Vec.normalize)) |> Vec.normalize
                        let mrp = linePlaneIntersection V3d.Zero mrpDir (clippedVa.[0]) lightPlaneN

                        let (mrpClamped, CLAMP_POLYGON_RESULT, clampP0Id, clampP1Id) =   clampPointToPolygonP3 vertices vc mrp 
                        let mrpClampedDir = mrpClamped |> Vec.normalize

                        match CLAMP_POLYGON_RESULT with
                        | CLAMP_POLYGON_RESULT_NONE -> 

                            if Vec.dot mrpClampedDir (closestPointClamped |> Vec.normalize) < (1.0 - 1e-8) then

                                let mutable splitEdge = false
                                let mutable param = 0

                                let mutable foundFace = false
                        
                                for f in 0 .. MAX_FACES - 1 do
                                    if not foundFace && delFaceVertexData.[f].W <> -1 then 
                            
                                        let face = delFaceVertexData.[f]
                                                                                        
                                        let dotZN = Vec.cross (verticesNormalized.[face.X]) (verticesNormalized.[face.Z]) |> Vec.normalize |> Vec.dot mrpClampedDir 
                                        let dotYN = Vec.cross (verticesNormalized.[face.Z]) (verticesNormalized.[face.Y]) |> Vec.normalize |> Vec.dot mrpClampedDir 
                                        let dotXN = Vec.cross (verticesNormalized.[face.Y]) (verticesNormalized.[face.X]) |> Vec.normalize |> Vec.dot mrpClampedDir 

                                        if dotXN >= 0.0 && dotYN >= 0.0 && dotZN >= 0.0 then 
                                                        
                                            foundFace <- true

                                            let x = dotXN < eps
                                            let y = dotYN < eps
                                            let z = dotZN < eps

                                            if (x && y) || (x && z) || (y && z) then
                                                ()
                                            elif x || y || z then

                                                splitEdge <- true

                                                param  <- 
                                                    if x then delFaceEdgeData.[f].X
                                                    elif y then delFaceEdgeData.[f].Y
                                                    else delFaceEdgeData.[f].Z
                                        
                                            else
                                                param <- f

                                                 

                                verticesNormalized.[vc] <- mrpClampedDir
                                vc <- vc + 1 

                                if splitEdge then
                                    let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = (vc - 1) |> DataMutation.spliteEdge delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData delNextFreeEdgeAddr delNextFreeFaceAddr param
                                    delVertexData       <- vertices
                                    delNEdgeData        <- edges
                                    delMetaData         <- meta
                                    delFaceVertexData   <- faceVertices
                                    delFaceEdgeData     <- faceEdges
                                    delNextFreeEdgeAddr <- nextFreeEdgeAddr
                                    delNextFreeFaceAddr <- nextFreeFaceAddr

                                else
                                    let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = (vc - 1) |> DataMutation.insertVertexIntoFace delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData delNextFreeEdgeAddr delNextFreeFaceAddr param
                                    delVertexData       <- vertices
                                    delNEdgeData        <- edges
                                    delMetaData         <- meta
                                    delFaceVertexData   <- faceVertices
                                    delFaceEdgeData     <- faceEdges
                                    delNextFreeEdgeAddr <- nextFreeEdgeAddr
                                    delNextFreeFaceAddr <- nextFreeFaceAddr
                                    
                        | CLAMP_POLYGON_RESULT_LINE -> 

                            let mutable edgeToSplit = 0

                            let mutable foundEdge = false
                        
                            for e in 0 .. MAX_EDGES - 1 do
                                if not foundEdge then 

                                    if (delVertexData.[e].X = clampP0Id && delVertexData.[e].Z = clampP1Id) || (delVertexData.[e].X = clampP1Id && delVertexData.[e].Z = clampP0Id) then
                                        foundEdge <- true
                                        edgeToSplit <- e
                            

                            verticesNormalized.[vc] <- mrpClampedDir
                            vc <- vc + 1 

                            let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = (vc - 1) |> DataMutation.spliteEdge delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData delNextFreeEdgeAddr delNextFreeFaceAddr edgeToSplit
                            delVertexData       <- vertices
                            delNEdgeData        <- edges
                            delMetaData         <- meta
                            delFaceVertexData   <- faceVertices
                            delFaceEdgeData     <- faceEdges
                            delNextFreeEdgeAddr <- nextFreeEdgeAddr
                            delNextFreeFaceAddr <- nextFreeFaceAddr
                                    
                                                                         
                        | _ (* CLAMP_POLYGON_RESULT_POINT *) -> ()
                                      

                        ////////////////////////////////////////
                        // execute edge flip algorithm

                        let mutable stack = Arr<N<MAX_EDGES>, int>()
                        let mutable SP = -1

                        for i in 0 .. MAX_EDGES - 1 do
                            if delMetaData.[i].X = 1 then
                                SP <- SP + 1
                                stack.[SP] <- i
                                delMetaData.[i] <- V2i(1, 1)


                        ////////////////////////////////////////
                        // transform to a Delaunay triangulation
                                                                        
                        while SP >= 0 do

                            // get edge Id
                            let eId = stack.[SP]

                            // unmark edge
                            delMetaData.[eId] <- V2i(delMetaData.[eId].X, 0)
                            SP <- SP - 1
                                        
                            let eVertices = delVertexData.[eId]

                            // test if edge is locally delaunay
                                // true if flip, false otherwise
    
                            let notLD = 
                                let a = verticesNormalized.[delVertexData.[eId].X].XYZ
                                let b = verticesNormalized.[delVertexData.[eId].Y].XYZ
                                let c = verticesNormalized.[delVertexData.[eId].Z].XYZ
                                let d = verticesNormalized.[delVertexData.[eId].W].XYZ
                                            
                                (Vec.dot (a - c) (Vec.cross (b - c) (d - c))) < 0.0     
                                                                                    
                            if notLD then
                                // flip edge

                                let (vertices, edges, meta, faceVertices, faceEdges, updatedStack, sp) = eId |> DataMutation.flipEdge delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData stack SP
                                delVertexData       <- vertices
                                delNEdgeData        <- edges
                                delMetaData         <- meta
                                delFaceVertexData   <- faceVertices
                                delFaceEdgeData     <- faceEdges
                                stack               <- updatedStack
                                SP                  <- sp

                        ////////////////////////////////////////////////////////

                        let mutable areaSum = 0.0

                        printfn "Faces: "  

                        for f in 0 .. MAX_FACES - 1 do
                            let face = delFaceVertexData.[f]

                            if face.W <> -1 then
                                
                                            
                                let area = computeSphericalExcess (verticesNormalized.[face.X]) (verticesNormalized.[face.Y]) (verticesNormalized.[face.Z])

                                printfn " - %A - Area: %A" face area

                                areaSum <- areaSum + area 


                        printfn "Area Sum: %A" areaSum

                        let getTrafo pos = Trafo3d.Translation pos |> Mod.init
                        
                        let mutable sg = Sg.empty

                        sg <- Sg.group' [sg; pointSg 0.98 (C4b(255, 255, 255, 100)) (Trafo3d.Identity |> Mod.init)]

                        for i in 0 .. vc - 1 do 
                            match i with
                            | 0 ->  sg <- Sg.group' [sg; pointSg 0.005 C4b.Blue (getTrafo (verticesNormalized.[i]))]
                            | v when v = (vc - 1) -> sg <- Sg.group' [sg; pointSg 0.005 C4b.Green (getTrafo (verticesNormalized.[i]))]
                            | _ -> sg <- Sg.group' [sg; pointSg 0.001 C4b.Red (getTrafo (verticesNormalized.[i]))]
                            
            
                        
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


    module Test = 
        open NUnit.Framework
        open FsUnit


        module FlipTestMockup =

            module Before = 

                let V = Arr<N<MAX_EDGES>, V4i>([| 
                                                    for i in 0 .. MAX_EDGES - 1 do
                                                        match i with
                                                        | 0 -> yield V4i( 0, 1, 2, 3)
                                                        | 1 -> yield V4i( 0,-1, 1, 2)
                                                        | 2 -> yield V4i( 1,-1, 2, 0)
                                                        | 3 -> yield V4i( 2,-1, 3, 0)
                                                        | 4 -> yield V4i( 3,-1, 0, 2)
                                                        | _ -> yield V4i(-1)
                                                |])

                let E = Arr<N<MAX_EDGES>, V4i>([| 
                                                    for i in 0 .. MAX_EDGES - 1 do
                                                        match i with
                                                        | 0 -> yield V4i( 1, 2, 3, 4)
                                                        | 1 -> yield V4i(-1,-1, 2, 0)
                                                        | 2 -> yield V4i(-1,-1, 0, 1)
                                                        | 3 -> yield V4i(-1,-1, 4, 0)
                                                        | 4 -> yield V4i(-1,-1, 0, 3)
                                                        | _ -> yield V4i(-1)
                                                |])

                let M = Arr<N<MAX_EDGES>, V2i>([| 
                                                    for i in 0 .. MAX_EDGES - 1 do
                                                        match i with
                                                        | 0 -> yield V2i(1, 1)
                                                        | 1 -> yield V2i(0, 0)
                                                        | 2 -> yield V2i(1, 0)
                                                        | 3 -> yield V2i(1, 0)
                                                        | 4 -> yield V2i(0, 0)
                                                        | _ -> yield V2i(-1)
                                                |])

                let FV = Arr<N<MAX_FACES>, V4i>([| 
                                                    for i in 0 .. MAX_FACES - 1 do
                                                        match i with
                                                        | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2))
                                                        | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3))
                                                        | _ -> yield V4i(-1)
                                                |])

                let FE = Arr<N<MAX_FACES>, V3i>([| 
                                                    for i in 0 .. MAX_FACES - 1 do
                                                        match i with
                                                        | 0 -> yield V3i( 1, 2, 0)
                                                        | 1 -> yield V3i( 0, 3, 4)
                                                        | _ -> yield V3i(-1)
                                                |])


                                            
                let STACK = Arr<N<MAX_EDGES>, int>()

                let SP = -1
            
            module After = 

                let V = Arr<N<MAX_EDGES>, V4i>([| 
                                                    for i in 0 .. MAX_EDGES - 1 do
                                                        match i with
                                                        | 0 -> yield V4i( 1, 2, 3, 0)
                                                        | 1 -> yield V4i( 0,-1, 1, 3)
                                                        | 2 -> yield V4i( 1,-1, 2, 3)
                                                        | 3 -> yield V4i( 2,-1, 3, 1)
                                                        | 4 -> yield V4i( 3,-1, 0, 1)
                                                        | _ -> yield V4i(-1)
                                                |])

                let E = Arr<N<MAX_EDGES>, V4i>([| 
                                                    for i in 0 .. MAX_EDGES - 1 do
                                                        match i with
                                                        | 0 -> yield V4i( 2, 3, 4, 1)
                                                        | 1 -> yield V4i(-1,-1, 0, 4)
                                                        | 2 -> yield V4i(-1,-1, 3, 0)
                                                        | 3 -> yield V4i(-1,-1, 0, 2)
                                                        | 4 -> yield V4i(-1,-1, 1, 0)
                                                        | _ -> yield V4i(-1)
                                                |])

                let M = Arr<N<MAX_EDGES>, V2i>([| 
                                                    for i in 0 .. MAX_EDGES - 1 do
                                                        match i with
                                                        | 0 -> yield V2i(1, 0)
                                                        | 1 -> yield V2i(0, 0)
                                                        | 2 -> yield V2i(1, 1)
                                                        | 3 -> yield V2i(1, 1)
                                                        | 4 -> yield V2i(0, 0)
                                                        | _ -> yield V2i(-1)
                                                |])

                let FV = Arr<N<MAX_FACES>, V4i>([| 
                                                    for i in 0 .. MAX_FACES - 1 do
                                                        match i with
                                                        | 0 -> yield V4i(1, 2, 3, (IDHash 1 2 3))
                                                        | 1 -> yield V4i(3, 0, 1, (IDHash 3 0 1))
                                                        | _ -> yield V4i(-1)
                                                |])

                let FE = Arr<N<MAX_FACES>, V3i>([| 
                                                    for i in 0 .. MAX_FACES - 1 do
                                                        match i with
                                                        | 0 -> yield V3i( 2, 3, 0)
                                                        | 1 -> yield V3i( 4, 1, 0)
                                                        | _ -> yield V3i(-1)
                                                |])

                let STACK = Arr<N<MAX_EDGES>, int>([|
                                                    2
                                                    3
                                                |])

                let SP = 1
        
        [<Test>]
        let ``Flip Edge``() = 

            let (vertices, edges, meta, faceVertices, faceEdges, stack, sp) = 0 |> DataMutation.flipEdge (FlipTestMockup.Before.V) (FlipTestMockup.Before.E) (FlipTestMockup.Before.M) (FlipTestMockup.Before.FV) (FlipTestMockup.Before.FE) (FlipTestMockup.Before.STACK) (FlipTestMockup.Before.SP)


            Assert.Multiple( fun _ ->
                vertices       |> should equal (FlipTestMockup.After.V)
                edges          |> should equal (FlipTestMockup.After.E)
                meta           |> should equal (FlipTestMockup.After.M)
                faceVertices   |> should equal (FlipTestMockup.After.FV)
                faceEdges      |> should equal (FlipTestMockup.After.FE)
                stack          |> should equal (FlipTestMockup.After.STACK)
                sp             |> should equal (FlipTestMockup.After.SP)
            )

        module InsertVertexMockup =

            module Before = 

                let V = Arr<N<MAX_EDGES>, V4i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i( 0,-1, 1, 2)
                                                    | 1 -> yield V4i( 1,-1, 2, 0)
                                                    | 2 -> yield V4i( 2,-1, 0, 1)
                                                    | _ -> yield V4i(-1)
                                            |])

                let E = Arr<N<MAX_EDGES>, V4i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i(-1,-1, 1, 2)
                                                    | 1 -> yield V4i(-1,-1, 2, 0)
                                                    | 2 -> yield V4i(-1,-1, 0, 1)
                                                    | _ -> yield V4i(-1)
                                            |])

                let M = Arr<N<MAX_EDGES>, V2i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V2i(1, 0)
                                                    | 1 -> yield V2i(1, 0)
                                                    | 2 -> yield V2i(0, 0)
                                                    | _ -> yield V2i(-1)
                                            |])

                let FV = Arr<N<MAX_FACES>, V4i>([| 
                            for i in 0 .. MAX_FACES - 1 do
                                match i with
                                | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2))
                                | _ -> yield V4i(-1)
                        |])

                let FE = Arr<N<MAX_FACES>, V3i>([| 
                            for i in 0 .. MAX_FACES - 1 do
                                match i with
                                | 0 -> yield V3i( 0, 1, 2)
                                | _ -> yield V3i(-1)
                        |])
                    
                let nextFreeFaceAddr = 1
                let nextFreeEdgeAddr = 3
            
            module After = 

                let V = Arr<N<MAX_EDGES>, V4i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i( 0,-1, 1, 3)
                                                    | 1 -> yield V4i( 1,-1, 2, 3)
                                                    | 2 -> yield V4i( 2,-1, 0, 3)
                                                    | 3 -> yield V4i( 0, 1, 3, 2)
                                                    | 4 -> yield V4i( 1, 2, 3, 0)
                                                    | 5 -> yield V4i( 2, 0, 3, 1)
                                                    | _ -> yield V4i(-1)
                                            |])

                let E = Arr<N<MAX_EDGES>, V4i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i(-1,-1, 4, 3)
                                                    | 1 -> yield V4i(-1,-1, 5, 4)
                                                    | 2 -> yield V4i(-1,-1, 3, 5)
                                                    | 3 -> yield V4i( 0, 4, 5, 2)
                                                    | 4 -> yield V4i( 1, 5, 3, 0)
                                                    | 5 -> yield V4i( 2, 3, 4, 1)
                                                    | _ -> yield V4i(-1)
                                            |])

                let M = Arr<N<MAX_EDGES>, V2i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V2i(1, 1)
                                                    | 1 -> yield V2i(1, 1)
                                                    | 2 -> yield V2i(0, 0)
                                                    | 3 -> yield V2i(1, 0)
                                                    | 4 -> yield V2i(1, 0)
                                                    | 5 -> yield V2i(1, 0)
                                                    | _ -> yield V2i(-1)
                                            |])

                let FV = Arr<N<MAX_FACES>, V4i>([| 
                                                for i in 0 .. MAX_FACES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i(-1)
                                                    | 1 -> yield V4i(0, 1, 3, (IDHash 0 1 3))
                                                    | 2 -> yield V4i(1, 2, 3, (IDHash 1 2 3))
                                                    | 3 -> yield V4i(2, 0, 3, (IDHash 2 0 3))
                                                    | _ -> yield V4i(-1)
                                            |])

                let FE = Arr<N<MAX_FACES>, V3i>([| 
                                                for i in 0 .. MAX_FACES - 1 do
                                                    match i with
                                                    | 0 -> yield V3i( 0, 1, 2)
                                                    | 1 -> yield V3i( 0, 4, 3)
                                                    | 2 -> yield V3i( 1, 5, 4)
                                                    | 3 -> yield V3i( 2, 3, 5)
                                                    | _ -> yield V3i(-1)
                                            |])

                let nextFreeFaceAddr = 4
                let nextFreeEdgeAddr = 6
        
        [<Test>]
        let ``Insert Vertex``() = 

            let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = 3 |> DataMutation.insertVertexIntoFace (InsertVertexMockup.Before.V) (InsertVertexMockup.Before.E) (InsertVertexMockup.Before.M) (InsertVertexMockup.Before.FV) (InsertVertexMockup.Before.FE) (InsertVertexMockup.Before.nextFreeEdgeAddr) (InsertVertexMockup.Before.nextFreeFaceAddr) 0 


            Assert.Multiple( fun _ ->
                vertices         |> should equal (InsertVertexMockup.After.V)
                edges            |> should equal (InsertVertexMockup.After.E)
                meta             |> should equal (InsertVertexMockup.After.M)
                faceVertices     |> should equal (InsertVertexMockup.After.FV)
                faceEdges        |> should equal (InsertVertexMockup.After.FE)
                nextFreeEdgeAddr |> should equal (InsertVertexMockup.After.nextFreeEdgeAddr)
                nextFreeFaceAddr |> should equal (InsertVertexMockup.After.nextFreeFaceAddr)
            )
        
        module SplitInsideEdgeMockup =

            module Before = 

                let V = Arr<N<MAX_EDGES>, V4i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i( 0, 1, 2, 3)
                                                    | 1 -> yield V4i( 0,-1, 1, 2)
                                                    | 2 -> yield V4i( 1,-1, 2, 0)
                                                    | 3 -> yield V4i( 2,-1, 3, 0)
                                                    | 4 -> yield V4i( 3,-1, 0, 2)
                                                    | _ -> yield V4i(-1)
                                            |])

                let E = Arr<N<MAX_EDGES>, V4i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i( 1, 2, 3, 4)
                                                    | 1 -> yield V4i(-1,-1, 2, 0)
                                                    | 2 -> yield V4i(-1,-1, 0, 1)
                                                    | 3 -> yield V4i(-1,-1, 4, 0)
                                                    | 4 -> yield V4i(-1,-1, 0, 3)
                                                    | _ -> yield V4i(-1)
                                            |])

                let M = Arr<N<MAX_EDGES>, V2i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V2i(1, 1)
                                                    | 1 -> yield V2i(0, 0)
                                                    | 2 -> yield V2i(0, 0)
                                                    | 3 -> yield V2i(0, 0)
                                                    | 4 -> yield V2i(0, 0)
                                                    | _ -> yield V2i(-1)
                                            |])

                let FV = Arr<N<MAX_FACES>, V4i>([| 
                                                for i in 0 .. MAX_FACES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2))
                                                    | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3))
                                                    | _ -> yield V4i(-1)
                                            |])

                let FE = Arr<N<MAX_FACES>, V3i>([| 
                                                for i in 0 .. MAX_FACES - 1 do
                                                    match i with
                                                    | 0 -> yield V3i( 1, 2, 0)
                                                    | 1 -> yield V3i( 0, 3, 4)
                                                    | _ -> yield V3i(-1)
                                            |])
                    
                let nextFreeFaceAddr = 2
                let nextFreeEdgeAddr = 5

            module After = 

                let V = Arr<N<MAX_EDGES>, V4i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i( 0, 1, 4, 3)
                                                    | 1 -> yield V4i( 0,-1, 1, 4)
                                                    | 2 -> yield V4i( 1,-1, 2, 4)
                                                    | 3 -> yield V4i( 2,-1, 3, 4)
                                                    | 4 -> yield V4i( 3,-1, 0, 4)
                                                    | 5 -> yield V4i( 1, 2, 4, 0)
                                                    | 6 -> yield V4i( 2, 3, 4, 1)
                                                    | 7 -> yield V4i( 3, 0, 4, 2)
                                                    | _ -> yield V4i(-1)
                                            |])

                let E = Arr<N<MAX_EDGES>, V4i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i( 1, 5, 7, 4)
                                                    | 1 -> yield V4i(-1,-1, 5, 0)
                                                    | 2 -> yield V4i(-1,-1, 6, 5)
                                                    | 3 -> yield V4i(-1,-1, 7, 6)
                                                    | 4 -> yield V4i(-1,-1, 0, 7)
                                                    | 5 -> yield V4i( 2, 6, 0, 1)
                                                    | 6 -> yield V4i( 3, 7, 5, 2)
                                                    | 7 -> yield V4i( 4, 0, 6, 3)
                                                    | _ -> yield V4i(-1)
                                            |])

                let M = Arr<N<MAX_EDGES>, V2i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V2i(1, 0)
                                                    | 1 -> yield V2i(0, 0)
                                                    | 2 -> yield V2i(0, 0)
                                                    | 3 -> yield V2i(0, 0)
                                                    | 4 -> yield V2i(0, 0)
                                                    | 5 -> yield V2i(1, 0)
                                                    | 6 -> yield V2i(1, 0)
                                                    | 7 -> yield V2i(1, 0)
                                                    | _ -> yield V2i(-1)
                                            |])

                let FV = Arr<N<MAX_FACES>, V4i>([| 
                                                for i in 0 .. MAX_FACES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i(-1)
                                                    | 1 -> yield V4i(-1)
                                                    | 2 -> yield V4i(0, 1, 4, (IDHash 0 1 4))
                                                    | 3 -> yield V4i(1, 2, 4, (IDHash 1 2 4))
                                                    | 4 -> yield V4i(2, 3, 4, (IDHash 2 3 4))
                                                    | 5 -> yield V4i(3, 0, 4, (IDHash 3 0 4))
                                                    | _ -> yield V4i(-1)
                                            |])

                let FE = Arr<N<MAX_FACES>, V3i>([| 
                                                for i in 0 .. MAX_FACES - 1 do
                                                    match i with
                                                    | 0 -> yield V3i( 1, 2, 0)
                                                    | 1 -> yield V3i( 0, 3, 4)
                                                    | 2 -> yield V3i( 1, 5, 0)
                                                    | 3 -> yield V3i( 2, 6, 5)
                                                    | 4 -> yield V3i( 3, 7, 6)
                                                    | 5 -> yield V3i( 4, 0, 7)
                                                    | _ -> yield V3i(-1)
                                            |])
                    
                let nextFreeFaceAddr = 6
                let nextFreeEdgeAddr = 8
             
        [<Test>]
        let ``Split Inside Edge``() = 

            let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = 4 |> DataMutation.spliteEdge (SplitInsideEdgeMockup.Before.V) (SplitInsideEdgeMockup.Before.E) (SplitInsideEdgeMockup.Before.M) (SplitInsideEdgeMockup.Before.FV) (SplitInsideEdgeMockup.Before.FE) (SplitInsideEdgeMockup.Before.nextFreeEdgeAddr) (SplitInsideEdgeMockup.Before.nextFreeFaceAddr) 0 


            Assert.Multiple( fun _ ->
                vertices         |> should equal (SplitInsideEdgeMockup.After.V)
                edges            |> should equal (SplitInsideEdgeMockup.After.E)
                meta             |> should equal (SplitInsideEdgeMockup.After.M)
                faceVertices     |> should equal (SplitInsideEdgeMockup.After.FV)
                faceEdges        |> should equal (SplitInsideEdgeMockup.After.FE)
                nextFreeEdgeAddr |> should equal (SplitInsideEdgeMockup.After.nextFreeEdgeAddr)
                nextFreeFaceAddr |> should equal (SplitInsideEdgeMockup.After.nextFreeFaceAddr)
            )     

        module SplitBorderEdgeMockup =

            module Before = 

                let V = Arr<N<MAX_EDGES>, V4i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i(1, -1, 2, 0) 
                                                    | 1 -> yield V4i(2, -1, 3, 0) 
                                                    | 2 -> yield V4i(3, -1, 4, 0) 
                                                    | 3 -> yield V4i(4, -1, 1, 0) 
                                                    | 4 -> yield V4i(0, 4, 1, 2)
                                                    | 5 -> yield V4i(0, 1, 2, 3)
                                                    | 6 -> yield V4i(0, 2, 3, 4)
                                                    | 7 -> yield V4i(0, 3, 4, 1)  
                                                    | _ -> yield V4i(-1)
                                            |])

                let E = Arr<N<MAX_EDGES>, V4i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i(-1, -1, 5, 4) 
                                                    | 1 -> yield V4i(-1, -1, 6, 5) 
                                                    | 2 -> yield V4i(-1, -1, 7, 6) 
                                                    | 3 -> yield V4i(-1, -1, 4, 7) 
                                                    | 4 -> yield V4i(7, 3, 0, 5)
                                                    | 5 -> yield V4i(4, 0, 1, 6)
                                                    | 6 -> yield V4i(5, 1, 2, 7)
                                                    | 7 -> yield V4i(6, 2, 3, 4)
                                                    | _ -> yield V4i(-1)
                                            |])

                let M = Arr<N<MAX_EDGES>, V2i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V2i(0, 0)
                                                    | 1 -> yield V2i(0, 0)
                                                    | 2 -> yield V2i(0, 0)
                                                    | 3 -> yield V2i(0, 0)
                                                    | 4 -> yield V2i(1, 0) 
                                                    | 5 -> yield V2i(1, 0) 
                                                    | 6 -> yield V2i(1, 0) 
                                                    | 7 -> yield V2i(1, 0) 
                                                    | _ -> yield V2i(-1) 
                                            |])

                let FV = Arr<N<MAX_FACES>, V4i>([| 
                                                for i in 0 .. MAX_FACES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2)) 
                                                    | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3)) 
                                                    | 2 -> yield V4i(0, 3, 4, (IDHash 0 3 4)) 
                                                    | 3 -> yield V4i(0, 4, 1, (IDHash 0 4 1)) 
                                                    | _ -> yield V4i(-1)
                                            |])

                let FE = Arr<N<MAX_FACES>, V3i>([| 
                                                for i in 0 .. MAX_FACES - 1 do
                                                    match i with
                                                    | 0 -> yield V3i(4, 0, 5) 
                                                    | 1 -> yield V3i(5, 1, 6) 
                                                    | 2 -> yield V3i(6, 2, 7) 
                                                    | 3 -> yield V3i(7, 3, 4) 
                                                    | _ -> yield V3i(-1)
                                            |])
                    
                let nextFreeFaceAddr = 4
                let nextFreeEdgeAddr = 8

            module After = 

                let V = Arr<N<MAX_EDGES>, V4i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i(1, -1, 2, 0) 
                                                    | 1 -> yield V4i(2, -1, 3, 0) 
                                                    | 2 -> yield V4i(3, -1, 5, 0)  
                                                    | 3 -> yield V4i(4, -1, 1, 0) 
                                                    | 4 -> yield V4i(0, 4, 1, 2)
                                                    | 5 -> yield V4i(0, 1, 2, 3)
                                                    | 6 -> yield V4i(0, 2, 3, 5)
                                                    | 7 -> yield V4i(0, 5, 4, 1) 
                                                    | 9 -> yield V4i(4, 0, 5, -1) 
                                                    | 10 -> yield V4i(0, 3, 5, 4)  
                                                    | _ -> yield V4i(-1)
                                            |])

                let E = Arr<N<MAX_EDGES>, V4i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i(-1, -1, 5, 4) 
                                                    | 1 -> yield V4i(-1, -1, 6, 5) 
                                                    | 2 -> yield V4i(-1, -1,10, 6)
                                                    | 3 -> yield V4i(-1, -1, 4, 7) 
                                                    | 4 -> yield V4i(7, 3, 0, 5)
                                                    | 5 -> yield V4i(4, 0, 1, 6)
                                                    | 6 -> yield V4i(5, 1, 2, 10)
                                                    | 7 -> yield V4i(10, 9, 3, 4)
                                                    | 9 -> yield V4i( 7,10, -1, -1) 
                                                    | 10 -> yield V4i(6, 2, 9, 7)
                                                    | _ -> yield V4i(-1)
                                            |])

                let M = Arr<N<MAX_EDGES>, V2i>([| 
                                                for i in 0 .. MAX_EDGES - 1 do
                                                    match i with
                                                    | 0 -> yield V2i(0, 0)
                                                    | 1 -> yield V2i(0, 0)
                                                    | 2 -> yield V2i(0, 0)
                                                    | 3 -> yield V2i(0, 0)
                                                    | 4 -> yield V2i(1, 0) 
                                                    | 5 -> yield V2i(1, 0) 
                                                    | 6 -> yield V2i(1, 0) 
                                                    | 7 -> yield V2i(1, 0) 
                                                    | 9 -> yield V2i(0, 0) 
                                                    | 10 -> yield V2i(1, 0) 
                                                    | _ -> yield V2i(-1) 
                                            |])

                let FV = Arr<N<MAX_FACES>, V4i>([| 
                                                for i in 0 .. MAX_FACES - 1 do
                                                    match i with
                                                    | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2)) 
                                                    | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3)) 
                                                    | 2 -> yield V4i(-1) 
                                                    | 3 -> yield V4i(0, 4, 1, (IDHash 0 4 1)) 
                                                    | 4 -> yield V4i(4, 0, 5, (IDHash 4 0 5)) 
                                                    | 5 -> yield V4i(0, 3, 5, (IDHash 0 3 5)) 
                                                    | _ -> yield V4i(-1)
                                            |])

                let FE = Arr<N<MAX_FACES>, V3i>([| 
                                                for i in 0 .. MAX_FACES - 1 do
                                                    match i with
                                                    | 0 -> yield V3i(4, 0, 5) 
                                                    | 1 -> yield V3i(5, 1, 6) 
                                                    | 2 -> yield V3i(6, 2, 7) 
                                                    | 3 -> yield V3i(7, 3, 4) 
                                                    | 4 -> yield V3i(7, 10, 9) 
                                                    | 5 -> yield V3i(6, 2, 10) 
                                                    | _ -> yield V3i(-1)
                                            |])
                    
                let nextFreeFaceAddr = 6
                let nextFreeEdgeAddr = 11
             
        [<Test>]
        let ``Split Border Edge``() = 

            let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = 5 |> DataMutation.spliteEdge (SplitBorderEdgeMockup.Before.V) (SplitBorderEdgeMockup.Before.E) (SplitBorderEdgeMockup.Before.M) (SplitBorderEdgeMockup.Before.FV) (SplitBorderEdgeMockup.Before.FE) (SplitBorderEdgeMockup.Before.nextFreeEdgeAddr) (SplitBorderEdgeMockup.Before.nextFreeFaceAddr) 2


            Assert.Multiple( fun _ ->
                vertices         |> should equal (SplitBorderEdgeMockup.After.V)
                edges            |> should equal (SplitBorderEdgeMockup.After.E)
                meta             |> should equal (SplitBorderEdgeMockup.After.M)
                faceVertices     |> should equal (SplitBorderEdgeMockup.After.FV)
                faceEdges        |> should equal (SplitBorderEdgeMockup.After.FE)
                nextFreeEdgeAddr |> should equal (SplitBorderEdgeMockup.After.nextFreeEdgeAddr)
                nextFreeFaceAddr |> should equal (SplitBorderEdgeMockup.After.nextFreeFaceAddr)
            )     

        
    module Rendering =

        open Aardvark.SceneGraph

        open RenderInterop
        open Utils
        open Utils.Sg
        open Aardvark.Base.Incremental


        let delIrrIntApproxRenderTask (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 

            
            let sceneSg = 
                [
                    sceneSg
                    Debug.delaunyScene data.lights |> Sg.dynamic
                ]
                |> Sg.group'
            
            
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
