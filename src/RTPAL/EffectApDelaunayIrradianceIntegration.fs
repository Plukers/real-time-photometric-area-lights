namespace Render

module EffectApDelaunayIrradianceIntegration = 
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open FShade

    open Light.Effect
    open EffectUtils
    open PhotometricLight

    open Config.Delaunay

    open EffectApDelaunayDataHandling
    open EffectApDelaunayDataMutation

   

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


            let (EDGES, META) = transformEdgeCollectionToCompactCollection V E M 3
            let FACES = transformFaceollectionToCompactCollection FV FE 3

        [<ReflectedDefinition>][<Inline>]
        let getInitEdgeData (edgeArray : Arr<N<MAX_EDGES_HALF>, V4i>) caseOffset = 
            for i in 0 .. MAX_EDGES_HALF - 1 do
                edgeArray.[i] <- ALL.EDGES.[MAX_EDGES_HALF * caseOffset + i]

        [<ReflectedDefinition>][<Inline>]
        let getInitEdgeData' caseOffset =
            let edgeArray = Arr<N<MAX_EDGES_HALF>, V4i>()
            for i in 0 .. MAX_EDGES_HALF - 1 do
                edgeArray.[i] <- ALL.EDGES.[MAX_EDGES_HALF * caseOffset + i]
            edgeArray

        [<ReflectedDefinition>][<Inline>]
        let getInitMetaData caseOffset = ALL.META.[caseOffset]

        [<ReflectedDefinition>][<Inline>]
        let getInitFaceData (faceArray : Arr<N<MAX_FACES_HALF>, V4i>) caseOffset = 
            for i in 0 .. MAX_FACES_HALF - 1 do
                faceArray.[i] <- ALL.FACES.[MAX_FACES_HALF * caseOffset + i]

        [<ReflectedDefinition>][<Inline>]
        let getInitFaceData' caseOffset = 
            let faceArray = Arr<N<MAX_FACES_HALF>, V4i>()
            for i in 0 .. MAX_FACES_HALF - 1 do
                faceArray.[i] <- ALL.FACES.[MAX_FACES_HALF * caseOffset + i]
            faceArray
        
        [<ReflectedDefinition>]
        let getInitFreeEdgeAddr caseOffset = ALL.NEXT_FREE_EDGE_ADDR.[caseOffset]

        [<ReflectedDefinition>]
        let getInitFreeFaceAddr caseOffset = ALL.NEXT_FREE_FACE_ADDR.[caseOffset]

    type Vertex = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
    }  

   
    [<ReflectedDefinition>][<Inline>]
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

    let delaunyIrrIntegration useSecondSpecialPoint (v : Vertex) = 
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

                                    
                                    let (closestPointClamped, CLAMP_POLYGON_RESULT, clampP0Id, clampP1ID) = clampPointToPolygonP1 clippedVa 0 clippedVc closestPoint

                                    ////////////////////////////////////////
                                    // create triangulation

                                    let mutable caseOffset = CASE_INSIDE_OFFSET
                                    let mutable v1Idx = 0

                                    if CLAMP_POLYGON_RESULT = CLAMP_POLYGON_RESULT_POINT then
                                        caseOffset <- CASE_CORNER_OFFSET
                                        v1Idx <- clampP0Id
                                    elif CLAMP_POLYGON_RESULT = CLAMP_POLYGON_RESULT_LINE then
                                        caseOffset <- CASE_EDGE_OFFSET
                                        v1Idx <- clampP1ID
                                    else
                                        ()

                                     
                                    // XYZ -> Spherical coords; 
                                    let vertices = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_THREE>, V3d>() 
                                     
                                    // X -> luminance, Y -> Weight
                                    let funVal = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_THREE>, V2d>() 


                                    let mutable vc = clippedVc
                                    let mutable offset = 0
                                    if caseOffset <> CASE_CORNER_OFFSET then 
                                        vertices.[0] <- closestPointClamped |> Vec.normalize
                                        funVal.[0]   <- sampleIrr t2w addr closestPointClamped
                                        vc <- vc + 1 
                                        offset <- 1

                                    // is the point in front of the light or behind?
                                    // if behind, iterate the light vertices backwards for counter clockwise order
                                    if Vec.dot (uniform.LForwards.[addr]) (t2w *(closestPointClamped |> Vec.normalize)) < 0.0 then 
                                    
                                        for i in 0 .. clippedVc - 1 do 
                                            vertices.[i + offset] <- clippedVa.[(v1Idx + i) % clippedVc]
                                            funVal.[i + offset]   <- sampleIrr t2w  addr clippedVa.[(v1Idx + i) % clippedVc]
                                            
                                    else
                                        let mutable j = 0
                                        for i in clippedVc - 1 .. -1 .. 0 do 
                                            vertices.[j + offset] <- clippedVa.[(v1Idx + i) % clippedVc]
                                            funVal.[j + offset]   <- sampleIrr t2w  addr clippedVa.[(v1Idx + i) % clippedVc]
                                                
                                            j <- j + 1

                                    ////////////////////////////////////////
                                    // load inital data
                                    

                                    let delEdgeData = Arr<N<MAX_EDGES_HALF>, V4i>()
                                    let delFaceData = Arr<N<MAX_FACES_HALF>, V4i>()

                                    QUAD_DATA.getInitEdgeData delEdgeData caseOffset
                                    QUAD_DATA.getInitFaceData delFaceData caseOffset
                                    
                                    let mutable delMetaData = QUAD_DATA.getInitMetaData caseOffset
                                    

                                    let mutable delNextFreeEdgeAddr =  QUAD_DATA.getInitFreeEdgeAddr caseOffset
                                    let mutable delNextFreeFaceAddr =  QUAD_DATA.getInitFreeFaceAddr caseOffset



                                    ////////////////////////////////////////
                                    // execute edge flip algorithm

                                    let stack = Arr<N<MAX_EDGES>, int>()
                                    let mutable SP = -1
                                    
                                    for i in 0 .. MAX_EDGES - 1 do
                                        if edgeIsInside delMetaData i then
                                            SP <- SP + 1
                                            stack.[SP] <- i
                                            delMetaData <- markEdge delMetaData i
                                    

                                    ////////////////////////////////////////
                                    // transform to a Delaunay triangulation
                                    while SP >= 0 do

                                        // get edge Id
                                        let eId = stack.[SP]
                                        SP <- SP - 1

                                        // unmark edge
                                        delMetaData <- unmarkEdge delMetaData eId
                                        
                                        // test if edge is locally delaunay
                                            // true if flip, false otherwise
                                
                                        let a = vertices.[ readVertexId delEdgeData eId 0].XYZ |> Vec.normalize
                                        let b = vertices.[ readVertexId delEdgeData eId 1].XYZ |> Vec.normalize
                                        let c = vertices.[ readVertexId delEdgeData eId 2].XYZ |> Vec.normalize
                                        let d = vertices.[ readVertexId delEdgeData eId 3].XYZ |> Vec.normalize
                                        let notLD = (Vec.dot (a - c) (Vec.cross (b - c) (d - c))) < 0.0   
                                        
                                        if notLD then
                                            // flip edge

                                            let (meta, sp) = flipEdge delEdgeData delMetaData delFaceData stack SP eId

                                            SP <- sp
                                            delMetaData <- meta

                                    ////////////////////////////////////////////////////////
                                    // integrate

                                    
                                    let mutable patchIllumination = 0.0
                                    let mutable weightSum = 0.0

                                    for f in 0 .. MAX_FACES - 1 do
                                        
                                        if not (faceIsEmpty delFaceData f) then
                                            let area = computeSphericalExcess (vertices.[readFaceVertexId delFaceData f 0] |> Vec.normalize) (vertices.[readFaceVertexId delFaceData f 1] |> Vec.normalize) (vertices.[readFaceVertexId delFaceData f 2] |> Vec.normalize)

                                            patchIllumination <- patchIllumination + area * (funVal.[0 |> readFaceVertexId delFaceData f].X + funVal.[1 |> readFaceVertexId delFaceData f].X + funVal.[2 |> readFaceVertexId delFaceData f].X) / 3.0
                                            weightSum <- weightSum + area * (funVal.[readFaceVertexId delFaceData f 0].Y + funVal.[readFaceVertexId delFaceData f 1].Y + funVal.[readFaceVertexId delFaceData f 2].Y) / 3.0


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
                                                                                
                        let (closestPointClamped, CLAMP_POLYGON_RESULT, clampP0Id, clampP1ID) = clampPointToPolygonP1 clippedVa 0 clippedVc closestPoint

                                                                        
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
                            vertices.[0] <- closestPointClamped
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
                        (*
                        let delEdgeData = Arr<N<MAX_EDGES_HALF>, V4i>()
                        let delFaceData = Arr<N<MAX_FACES_HALF>, V4i>()

                        caseOffset |> QUAD_DATA.getInitEdgeData delEdgeData
                        caseOffset |> QUAD_DATA.getInitFaceData delFaceData
                        *)

                        let delEdgeData = Arr<N<MAX_EDGES_HALF>, V4i>()
                        for i in 0 .. MAX_EDGES_HALF - 1 do
                            delEdgeData.[i] <- QUAD_DATA.ALL.EDGES.[MAX_EDGES_HALF * caseOffset + i]

                        
                        let delFaceData = Arr<N<MAX_FACES_HALF>, V4i>()
                        for i in 0 .. MAX_FACES_HALF - 1 do
                            delFaceData.[i] <- QUAD_DATA.ALL.FACES.[MAX_FACES_HALF * caseOffset + i]
  
                        let mutable delMetaData = QUAD_DATA.ALL.META.[caseOffset]

                        let mutable delNextFreeEdgeAddr = caseOffset |> QUAD_DATA.getInitFreeEdgeAddr
                        let mutable delNextFreeFaceAddr = caseOffset |> QUAD_DATA.getInitFreeFaceAddr



                        ////////////////////////////////////////
                        // execute edge flip algorithm

                        let mutable stack = Arr<N<MAX_EDGES>, int>()
                        let mutable SP = 0
                                    
                        for i in 0 .. MAX_EDGES - 1 do
                            if i |> edgeIsInside delMetaData then
                                SP <- SP + 1
                                stack.[SP - 1] <- i
                                delMetaData <- i |> markEdge delMetaData
                                    
                        SP <- SP - 1

                        ////////////////////////////////////////
                        // transform to a Delaunay triangulation
                                                                        
                        while SP >= 0 do

                            // get edge Id
                            let eId = stack.[SP]
                            SP <- SP - 1

                            // unmark edge
                            delMetaData <- eId |> unmarkEdge delMetaData
                                        
                            // test if edge is locally delaunay
                                // true if flip, false otherwise

                            let a' = 0 |> readVertexId delEdgeData eId
                            let b' = 1 |> readVertexId delEdgeData eId
                            let c' = 2 |> readVertexId delEdgeData eId
                            let d' = 3 |> readVertexId delEdgeData eId
                                
                            let a = vertices.[a'].XYZ |> Vec.normalize
                            let b = vertices.[b'].XYZ |> Vec.normalize
                            let c = vertices.[c'].XYZ |> Vec.normalize
                            let d = vertices.[d'].XYZ |> Vec.normalize
                            let notLD = (Vec.dot (a - c) (Vec.cross (b - c) (d - c))) < 0.0   
                                        
                            if notLD then
                                // flip edge

                                let (meta, sp) = eId |>  flipEdge delEdgeData delMetaData delFaceData stack SP 

                                SP <- sp
                                delMetaData <- meta

                        ////////////////////////////////////////////////////////

                        let mutable areaSum = 0.0

                        printfn "Faces: "  

                        for f in 0 .. MAX_FACES - 1 do

                             if not (faceIsEmpty delFaceData f) then

                                let area = computeSphericalExcess (vertices.[0 |> readFaceVertexId delFaceData f] |> Vec.normalize) (vertices.[1 |> readFaceVertexId delFaceData f] |> Vec.normalize) (vertices.[2 |> readFaceVertexId delFaceData f] |> Vec.normalize)

                                printfn " - %A - Area: %A" f area

                                areaSum <- areaSum + area 


                        printfn "Area Sum: %A" areaSum

                        let getTrafo pos = Trafo3d.Translation pos |> Mod.init
                        
                        let mutable sg = Sg.empty

                        sg <- Sg.group' [sg; pointSg 0.12 (C4b(255, 255, 255, 100)) (Trafo3d.Identity |> Mod.init)]

                        for i in 0 .. vc - 1 do 
                            match i with
                            | 0 ->  sg <- Sg.group' [sg; pointSg 0.002 C4b.Blue (getTrafo (verticesNormalized.[i] * 0.14))]
                            // | v when v = (vc - 1) -> sg <- Sg.group' [sg; pointSg 0.002 C4b.Green (getTrafo (verticesNormalized.[i] * 0.14))]
                            | _ -> sg <- Sg.group' [sg; pointSg 0.001 C4b.Red (getTrafo (verticesNormalized.[i] * 0.14))]
                            
            
                        
                        printfn "Vertices %A : %A" vc verticesNormalized

                        printfn "Edges: "
                        for i in 0 .. MAX_EDGES - 1 do         
                            if (0 |> readVertexId delEdgeData i) <> NONE && (2 |> readVertexId delEdgeData i) <> NONE then 
                                let edgeStart = verticesNormalized.[0 |> readVertexId delEdgeData i]
                                let edgeEnd   = verticesNormalized.[2 |> readVertexId delEdgeData i] 

                                printfn " - %A -> %A" edgeStart edgeEnd


                                sg <- Sg.group' [sg; arcSg edgeStart edgeEnd C4b.Red (Trafo3d.Scale 0.14 |> Mod.init)]

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

            (*
            let sceneSg = 
                [
                    sceneSg
                    Debug.delaunyScene data.lights |> Sg.dynamic
                ]
                |> Sg.group'
            *)
            
            sceneSg
                |> setupFbEffects [ 
                        delaunyIrrIntegration false |> toEffect
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
