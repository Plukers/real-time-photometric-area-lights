namespace Render

module EffectApDelaunayIrradianceIntegration = 
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open FShade

    open Light.Effect
    open EffectUtils
    open PhotometricLight

    open Config.Delaunay

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

                                    let mutable delVertexData       = caseOffset |> QUAD_DATA.getInitVertexData
                                    let mutable delNEdgeData        = caseOffset |> QUAD_DATA.getInitNeighbourEdgeData 
                                    let mutable delMetaData         = caseOffset |> QUAD_DATA.getInitMetaData 
                                    let mutable delFaceVertexData   = caseOffset |> QUAD_DATA.getInitFaceVertexData 
                                    let mutable delFaceEdgeData     = caseOffset |> QUAD_DATA.getInitFaceEdgeData 

                                    let mutable delNextFreeEdgeAddr = caseOffset |> QUAD_DATA.getInitFreeEdgeAddr
                                    let mutable delNextFreeFaceAddr = caseOffset |> QUAD_DATA.getInitFreeFaceAddr

                                    ////////////////////////////////////////
                                    // insert additional verticex = MRP

                                    if useSecondSpecialPoint then
                                    
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

                                        let (mrpClamped, CLAMP_POLYGON_RESULT, clampP0Id, clampP1Id) =   
                                            if CLAMP_POLYGON_RESULT <> CLAMP_POLYGON_RESULT_NONE then
                                                clampPointToPolygonP3 vertices 0 vc mrp 
                                            else
                                                clampPointToPolygonP3 vertices 1 vc mrp 

                                        let mrpClampedDir = mrpClamped |> Vec.normalize

                                        match CLAMP_POLYGON_RESULT with
                                        | CLAMP_POLYGON_RESULT_NONE -> 

                                            if Vec.dot mrpClampedDir (closestPointClamped |> Vec.normalize) < (1.0 - 1e-9) then

                                                let mutable splitEdge = false
                                                let mutable param = 0

                                                let mutable foundFace = false
                        
                                                for f in 0 .. MAX_FACES - 1 do
                                                    if not foundFace && delFaceVertexData.[f].W <> -1 then 
                            
                                                        let face = delFaceVertexData.[f]
                                                                                        
                                                        let dotZN = Vec.cross (vertices.[face.X]) (vertices.[face.Z]) |> Vec.normalize |> Vec.dot mrpClampedDir 
                                                        let dotYN = Vec.cross (vertices.[face.Z]) (vertices.[face.Y]) |> Vec.normalize |> Vec.dot mrpClampedDir 
                                                        let dotXN = Vec.cross (vertices.[face.Y]) (vertices.[face.X]) |> Vec.normalize |> Vec.dot mrpClampedDir 

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

                                                vertices.[vc] <- mrpClamped
                                                funVal.[vc]   <- sampleIrr t2w addr mrpClamped
                                                
                                                vc <- vc + 1 

                                                if splitEdge then
                                                    let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = (vc - 1) |> spliteEdge delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData delNextFreeEdgeAddr delNextFreeFaceAddr param
                                                    delVertexData       <- vertices
                                                    delNEdgeData        <- edges
                                                    delMetaData         <- meta
                                                    delFaceVertexData   <- faceVertices
                                                    delFaceEdgeData     <- faceEdges
                                                    delNextFreeEdgeAddr <- nextFreeEdgeAddr
                                                    delNextFreeFaceAddr <- nextFreeFaceAddr

                                                else
                                                    let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = (vc - 1) |> insertVertexIntoFace delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData delNextFreeEdgeAddr delNextFreeFaceAddr param
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
                            

                                            vertices.[vc] <- mrpClamped
                                            funVal.[vc]   <- sampleIrr t2w addr mrpClamped
                                            
                                            vc <- vc + 1 

                                            let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = (vc - 1) |> spliteEdge delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData delNextFreeEdgeAddr delNextFreeFaceAddr edgeToSplit
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
                                        
                                        // test if edge is locally delaunay
                                            // true if flip, false otherwise
                                
                                        let a = vertices.[delVertexData.[eId].X].XYZ |> Vec.normalize
                                        let b = vertices.[delVertexData.[eId].Y].XYZ |> Vec.normalize
                                        let c = vertices.[delVertexData.[eId].Z].XYZ |> Vec.normalize
                                        let d = vertices.[delVertexData.[eId].W].XYZ |> Vec.normalize
                                        let notLD = (Vec.dot (a - c) (Vec.cross (b - c) (d - c))) < 0.0   
                                        
                                        if notLD then
                                            // flip edge
                                            (*
                                            let (vertices, edges, meta, faceVertices, faceEdges, updatedStack, sp) = 
                                                 flipEdge delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData stack SP eId
                                            delVertexData       <- vertices
                                            delNEdgeData        <- edges
                                            delMetaData         <- meta
                                            delFaceVertexData   <- faceVertices
                                            delFaceEdgeData     <- faceEdges
                                            stack               <- updatedStack
                                            SP                  <- sp
                                            *) 

                                            SP <- flipEdge2 delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData stack SP eId

                                            //let originalVertices = vertices.[eId]
                                            //let originalEdges = edges.[eId]

                                            //// unmrak edge
                                            //meta.[eId] <- V2i(meta.[eId].X, 0)

                                            //// flip edge
                                            //vertices.[eId] <- V4i(originalVertices.Y, originalVertices.Z, originalVertices.W, originalVertices.X)
                                            //edges.[eId] <- V4i(originalEdges.Y, originalEdges.Z, originalEdges.W, originalEdges.X)

                                            (*
                                            let originalVertices = delVertexData.[eId]
                                            let originalEdges = delNEdgeData.[eId]

                                            // unmrak edge
                                            delMetaData.[eId] <- V2i(delMetaData.[eId].X, 0)

                                            // flip edge
                                            delVertexData.[eId] <- V4i(originalVertices.Y, originalVertices.Z, originalVertices.W, originalVertices.X)
                                            delNEdgeData.[eId] <- V4i(originalEdges.Y, originalEdges.Z, originalEdges.W, originalEdges.X)

                                            // adopt faces
                                            let f0Id = IDHash (originalVertices.X) (originalVertices.Y) (originalVertices.Z)
                                            let f1Id = IDHash (originalVertices.Z) (originalVertices.W) (originalVertices.X)

                                            for f in 0 .. MAX_FACES - 1 do
                                                if delFaceVertexData.[f].W = f0Id then
                                                    delFaceVertexData.[f] <- V4i((delVertexData.[eId].X), (delVertexData.[eId].Y), (delVertexData.[eId].Z), (IDHash (delVertexData.[eId].X) (delVertexData.[eId].Y) (delVertexData.[eId].Z)))
                                                    delFaceEdgeData.[f] <- V3i((delNEdgeData.[eId].X), (delNEdgeData.[eId].Y), eId)

                                                if delFaceVertexData.[f].W = f1Id then
                                                    delFaceVertexData.[f] <- V4i((delVertexData.[eId].Z), (delVertexData.[eId].W), (delVertexData.[eId].X), (IDHash (delVertexData.[eId].Z) (delVertexData.[eId].W) (delVertexData.[eId].X)))
                                                    delFaceEdgeData.[f] <- V3i((delNEdgeData.[eId].Z), (delNEdgeData.[eId].W), eId)
              
             

                                            // adapt neighbour edges
                                            for ne in 0 .. 3 do
                                                let (neId, oppositeEdges, oppositeVertex) = 
                                                    match ne with
                                                    | 0 -> (originalEdges.X, originalEdges.ZW, originalVertices.W)
                                                    | 1 -> (originalEdges.Y, originalEdges.ZW, originalVertices.W)
                                                    | 2 -> (originalEdges.Z, originalEdges.XY, originalVertices.Y)
                                                    | _ -> (originalEdges.W, originalEdges.XY, originalVertices.Y)

                                                    
                                                if eId = delNEdgeData.[neId].X then
                                                    delNEdgeData.[neId] <- V4i(oppositeEdges.X, eId, delNEdgeData.[neId].Z, delNEdgeData.[neId].W)
                                                    delVertexData.[neId] <- V4i(delVertexData.[neId].X, oppositeVertex, delVertexData.[neId].Z, delVertexData.[neId].W)
                                                elif eId = delNEdgeData.[neId].Y then
                                                    delNEdgeData.[neId] <- V4i(eId, oppositeEdges.Y, delNEdgeData.[neId].Z, delNEdgeData.[neId].W)
                                                    delVertexData.[neId] <- V4i(delVertexData.[neId].X, oppositeVertex, delVertexData.[neId].Z, delVertexData.[neId].W)
                                                elif eId = delNEdgeData.[neId].Z then
                                                    delNEdgeData.[neId] <- V4i(delNEdgeData.[neId].X, delNEdgeData.[neId].Y, oppositeEdges.X, eId)
                                                    delVertexData.[neId] <- V4i(delVertexData.[neId].X, delVertexData.[neId].Y, delVertexData.[neId].Z, oppositeVertex)
                                                else (* eId = delNEdgeData.[neId].W *)
                                                    delNEdgeData.[neId] <- V4i(delNEdgeData.[neId].X, delNEdgeData.[neId].Y, eId, oppositeEdges.Y)
                                                    delVertexData.[neId] <- V4i(delVertexData.[neId].X, delVertexData.[neId].Y, delVertexData.[neId].Z, oppositeVertex)

                                                // if inside and not marked -> mark it
                                                if delMetaData.[neId].X = 1 && delMetaData.[neId].Y = 0 then
                                                    SP <- SP + 1
                                                    stack.[SP] <- neId
                                                    delMetaData.[neId] <- V2i(1, 1)
                                                *)
                                    ////////////////////////////////////////
                                    // integrate

                                    
                                    let mutable patchIllumination = 0.0
                                    let mutable weightSum = 0.0

                                    for f in 0 .. MAX_FACES - 1 do
                                        let face = delFaceVertexData.[f]

                                        if face.W <> -1 then
                                            let area = computeSphericalExcess (vertices.[face.X] |> Vec.normalize) (vertices.[face.Y] |> Vec.normalize) (vertices.[face.Z] |> Vec.normalize)

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
                        

            return V4d(illumination.XYZ, v.c.W)
        }
(*
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

                        let (mrpClamped, CLAMP_POLYGON_RESULT, clampP0Id, clampP1Id) =   
                            if CLAMP_POLYGON_RESULT <> CLAMP_POLYGON_RESULT_NONE then
                                clampPointToPolygonP3 vertices 0 vc mrp 
                            else
                                clampPointToPolygonP3 vertices 1 vc mrp 

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
                                    let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = (vc - 1) |> spliteEdge delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData delNextFreeEdgeAddr delNextFreeFaceAddr param
                                    delVertexData       <- vertices
                                    delNEdgeData        <- edges
                                    delMetaData         <- meta
                                    delFaceVertexData   <- faceVertices
                                    delFaceEdgeData     <- faceEdges
                                    delNextFreeEdgeAddr <- nextFreeEdgeAddr
                                    delNextFreeFaceAddr <- nextFreeFaceAddr

                                else
                                    let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = (vc - 1) |> insertVertexIntoFace delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData delNextFreeEdgeAddr delNextFreeFaceAddr param
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

                            let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = (vc - 1) |> spliteEdge delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData delNextFreeEdgeAddr delNextFreeFaceAddr edgeToSplit
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

                                let (vertices, edges, meta, faceVertices, faceEdges, updatedStack, sp) = eId |> flipEdge delVertexData delNEdgeData delMetaData delFaceVertexData delFaceEdgeData stack SP
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

                        sg <- Sg.group' [sg; pointSg 0.12 (C4b(255, 255, 255, 100)) (Trafo3d.Identity |> Mod.init)]

                        for i in 0 .. vc - 1 do 
                            match i with
                            | 0 ->  sg <- Sg.group' [sg; pointSg 0.002 C4b.Blue (getTrafo (verticesNormalized.[i] * 0.14))]
                            | v when v = (vc - 1) -> sg <- Sg.group' [sg; pointSg 0.002 C4b.Green (getTrafo (verticesNormalized.[i] * 0.14))]
                            | _ -> sg <- Sg.group' [sg; pointSg 0.001 C4b.Red (getTrafo (verticesNormalized.[i] * 0.14))]
                            
            
                        
                        printfn "Vertices %A : %A" vc verticesNormalized

                        printfn "Edges: "
                        for i in 0 .. MAX_EDGES - 1 do         
                            if delVertexData.[i].X <> -1 && delVertexData.[i].Z <> -1 then 
                                let edgeStart = verticesNormalized.[delVertexData.[i].X]
                                let edgeEnd   = verticesNormalized.[delVertexData.[i].Z] 

                                printfn " - %A -> %A" edgeStart edgeEnd


                                sg <- Sg.group' [sg; arcSg edgeStart edgeEnd C4b.Red (Trafo3d.Scale 0.14 |> Mod.init)]

                        sg

                    else
                        
                        Sg.empty
                else 

                    Sg.empty

            )
*)
  
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
