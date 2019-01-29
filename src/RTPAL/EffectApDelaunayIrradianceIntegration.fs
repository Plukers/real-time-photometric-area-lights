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
    open EffectApDelaunayGenInitTriangulation
   
    module QUAD_DATA =
      

        module ALL =        
            let (EDGES, META, FACES) = genInitTriangulation 4

        [<ReflectedDefinition>][<Inline>]
        let getInitEdgeData caseOffset = 
            let edgeArray = Arr<N<MAX_EDGES_HALF>, V4i>() 
            for i in 0 .. MAX_EDGES_HALF - 1 do
                edgeArray.[i] <- ALL.EDGES.[MAX_EDGES_HALF * caseOffset + i]
            edgeArray

        [<ReflectedDefinition>][<Inline>]
        let getInitEdgeData' caseOffset =
            let edgeArray = Arr<N<MAX_EDGES_HALF>, V4i>()
            for i in 0 .. MAX_EDGES_HALF - 1 do
                edgeArray.[i] <- ALL.EDGES.[MAX_EDGES_HALF * caseOffset + i]
            edgeArray

        [<ReflectedDefinition>][<Inline>]
        let getInitMetaData caseOffset = ALL.META.[caseOffset]

        [<ReflectedDefinition>][<Inline>]
        let getInitFaceData caseOffset = 
            let faceArray = Arr<N<MAX_FACES_HALF>, V4i>()
            for i in 0 .. MAX_FACES_HALF - 1 do
                faceArray.[i] <- ALL.FACES.[MAX_FACES_HALF * caseOffset + i]
            faceArray

        [<ReflectedDefinition>][<Inline>]
        let getInitFaceData' caseOffset = 
            let faceArray = Arr<N<MAX_FACES_HALF>, V4i>()
            for i in 0 .. MAX_FACES_HALF - 1 do
                faceArray.[i] <- ALL.FACES.[MAX_FACES_HALF * caseOffset + i]
            faceArray
        
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
                            
                            //let vt = Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>() 
                            
                            //for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                            //    let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                            //    vt.[vtc] <- w2t * (uniform.LVertices.[vtcAddr] - P)

                            ////////////////////////////////////////////////////////


                            // linePlaneIntersection (lineO : V3d) (lineDir : V3d) (planeP : V3d) (planeN : V3d)
                            let mutable clipNormal = V3d.OOI
                            if uniform.skewClipPlane && abs(Vec.dot (uniform.LForwards.[addr]) clipNormal) < (1.0 - 1e-8) then
                                let heightCut = 0.1
                                let translate = V2d(-uniform.LForwards.[addr].Y, uniform.LForwards.[addr].X)

                                let planeP0 = linePlaneIntersection (V3d(translate, heightCut)) (V3d(uniform.LForwards.[addr].XY, 0.0) |> Vec.normalize) uniform.LVertices.[0] uniform.LForwards.[addr]
                                let planeP1 = linePlaneIntersection (V3d(-translate, heightCut)) (V3d(uniform.LForwards.[addr].XY, 0.0) |> Vec.normalize) uniform.LVertices.[0] uniform.LForwards.[addr]

                                clipNormal <- Vec.cross planeP0 planeP1 |> Vec.normalize

                                if clipNormal.Z < 0.0 then
                                    clipNormal <- -clipNormal

                            ////////////////////////////////////////////////////////

                            //let (clippedVa, clippedVc) = clipPatch(V3d.Zero, V3d.OOI, vt, uniform.LBaseComponents.[addr])
                            //let (clippedVa, clippedVc) = clipPatchTS(uniform.LVertices, uniform.LBaseComponents.[addr], P, w2t)
                            let (clippedVa, clippedVc) = clipPatchTSwN(clipNormal, uniform.LVertices, uniform.LBaseComponents.[addr], P, w2t)
                            

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

                                    let behindLight = Vec.dot (uniform.LForwards.[addr]) (t2w *(clippedVa.[0] |> Vec.normalize)) > 0.0
                                    
                                    let (closestPointClamped, CLAMP_POLYGON_RESULT, clampP0Id, clampP1ID) = clampPointToPolygonP1 clippedVa 0 clippedVc behindLight closestPoint

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

                                    for i in 0 .. clippedVc - 1 do 
                                        vertices.[i + offset] <- clippedVa.[(v1Idx + i) % clippedVc]
                                        funVal.[i + offset]   <- sampleIrr t2w  addr clippedVa.[(v1Idx + i) % clippedVc]

                                    ////////////////////////////////////////
                                    // load inital data
                                    
                                    //let delEdgeData = QUAD_DATA.getInitEdgeData caseOffset
                                    let delEdgeData = Arr<N<MAX_EDGES_HALF>, V4i>() 
                                    for i in 0 .. MAX_EDGES_HALF - 1 do
                                        delEdgeData.[i] <- QUAD_DATA.ALL.EDGES.[MAX_EDGES_HALF * caseOffset + i]

                                    //let delFaceData = QUAD_DATA.getInitFaceData caseOffset
                                    let delFaceData = Arr<N<MAX_FACES_HALF>, V4i>()
                                    for i in 0 .. MAX_FACES_HALF - 1 do
                                        delFaceData.[i] <- QUAD_DATA.ALL.FACES.[MAX_FACES_HALF * caseOffset + i]
                                    
                                    let mutable delMetaData = QUAD_DATA.getInitMetaData caseOffset


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

                                            //let (meta, sp) = flipEdge delEdgeData delMetaData delFaceData stack SP eId

                                            let edges = delEdgeData
                                            let meta = delMetaData
                                            let faces = delFaceData

                                            let mutable sp = SP
                                            let mutable meta = meta

                                            // adapt neighbour edges
                                            for ne in 0 .. 3 do

                                                let neId = ne |> readEdgeId edges eId

                                                // TODO optimize this with a for loop as soon as it is guaranteed to work
                                                match ne with
                                                | 0 -> 
                                                    if 2 |> compareIndices edges neId OPPOSITE_0 eId then
                                                        writeOppositeEdgeIds edges neId OPPOSITE_0 (eId) (3 |> readEdgeId edges eId) 
                                                        writeVertexId edges neId OPPOSITE_0 (3 |> readVertexId edges eId)
                                                    else
                                                        writeOppositeEdgeIds edges neId OPPOSITE_1 (eId) (3 |> readEdgeId edges eId) 
                                                        writeVertexId edges neId OPPOSITE_1 (3 |> readVertexId edges eId)

                                                | 2 ->
                                                    if 0 |> compareIndices edges neId OPPOSITE_0 eId then
                                                        writeOppositeEdgeIds edges neId OPPOSITE_0 (eId) (1 |> readEdgeId edges eId) 
                                                        writeVertexId edges neId OPPOSITE_0 ( 1 |> readVertexId edges eId)
                                                    else
                                                        writeOppositeEdgeIds edges neId OPPOSITE_1 (eId) (1 |> readEdgeId edges eId)    
                                                        writeVertexId edges neId OPPOSITE_1 ( 1 |> readVertexId edges eId)

                                                | 1 ->
                                                    if 0 |> compareIndices edges neId OPPOSITE_0 eId then
                                                        writeOppositeEdgeIds edges neId OPPOSITE_0 (2 |> readEdgeId edges eId) (eId) 
                                                        writeVertexId edges neId OPPOSITE_0 (3 |> readVertexId edges eId)
                                                    else
                                                        writeOppositeEdgeIds edges neId OPPOSITE_1 (2 |> readEdgeId edges eId) (eId)  
                                                        writeVertexId edges neId OPPOSITE_1 (3 |> readVertexId edges eId)

                                                | 3 ->
                                                    if 2 |> compareIndices edges neId OPPOSITE_0 eId then
                                                        writeOppositeEdgeIds edges neId OPPOSITE_0 (0 |> readEdgeId edges eId) (eId)
                                                        writeVertexId edges neId OPPOSITE_0 (1 |> readVertexId edges eId)
                                                    else
                                                        writeOppositeEdgeIds edges neId OPPOSITE_1 (0 |> readEdgeId edges eId) (eId)
                                                        writeVertexId edges neId OPPOSITE_1 (1 |> readVertexId edges eId)
                
                                                | _ -> ()

                                                // if inside and not marked -> mark it
                                                if edgeIsInside meta neId && not (edgeIsMarked meta neId) then
                                                    meta <- markEdge meta neId

                                                    sp <- sp + 1
                                                    stack.[sp] <- neId
                    

                                            // get face vertices of unflipped edge
                                            let f0vertices = eId |> getFaceVerticesOfEdge edges OPPOSITE_0
                                            let f1vertices = eId |> getFaceVerticesOfEdge edges OPPOSITE_1

                                            // flip edge and unmark edge
                                            eId |> leftShiftVerticesAndEdgesByOne edges
                                            meta <- eId |> unmarkEdge meta

                                            // update faces
                                            for f in 0 .. MAX_FACES - 1 do

                                                if f0vertices |> verticesAreFromFace faces f then
                                                    eId |> getFaceVerticesOfEdge edges OPPOSITE_0 |> writeFaceVertexIdsCombined faces f
                                                    writeFaceEdgeIds faces f (0 |> readEdgeId edges eId) (1 |> readEdgeId edges eId) eId

                                                if f1vertices |> verticesAreFromFace faces f then
                                                    eId |> getFaceVerticesOfEdge edges OPPOSITE_1 |> writeFaceVertexIdsCombined faces f
                                                    writeFaceEdgeIds faces f (2 |> readEdgeId edges eId) (3 |> readEdgeId edges eId) eId


                                            SP <- sp
                                            delMetaData <- meta

                                    ////////////////////////////////////////////////////////
                                    // integrate

                                    
                                    let mutable patchIllumination = 0.0
                                    let mutable weightSum = 0.0

                                    for f in 0 .. MAX_FACES - 1 do
                                        
                                        if not (faceIsEmpty delFaceData f) then
                                            let area = computeSphericalExcess (vertices.[readFaceVertexId delFaceData f 0] |> Vec.normalize) (vertices.[readFaceVertexId delFaceData f 1] |> Vec.normalize) (vertices.[readFaceVertexId delFaceData f 2] |> Vec.normalize)

                                            patchIllumination <- patchIllumination + area * (funVal.[readFaceVertexId delFaceData f 0].X + funVal.[readFaceVertexId delFaceData f 1].X + funVal.[readFaceVertexId delFaceData f 2].X) / 3.0
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

                //let vt = Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>() 
                            
                //for vtc in 0 .. lBaseComponents.[addr] - 1 do
                //    let vtcAddr = lPatchIndices.[iIdx + vtc] + vAddr
                //    vt.[vtc] <- w2t * (lVertices.[vtcAddr] - P)

                ////////////////////////////////////////////////////////

                // let (clippedVa, clippedVc) = clipPatch(V3d.Zero, V3d.OOI, vt, lBaseComponents.[addr])
                let (clippedVa, clippedVc) = clipPatchTS(uniform.LVertices, uniform.LBaseComponents.[addr], P, w2t)

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

                        let behindLight = Vec.dot (lForwards.[addr]) (t2w *(clippedVa.[0] |> Vec.normalize)) > 0.0
                                                                                
                        let (closestPointClamped, CLAMP_POLYGON_RESULT, clampP0Id, clampP1ID) = clampPointToPolygonP1 clippedVa 0 clippedVc behindLight closestPoint

                                                                        
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
                        if not behindLight then 
                                    
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

                        //let mutable delNextFreeEdgeAddr = caseOffset |> QUAD_DATA.getInitFreeEdgeAddr
                        //let mutable delNextFreeFaceAddr = caseOffset |> QUAD_DATA.getInitFreeFaceAddr



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
                        
                        for f in 0 .. MAX_FACES - 1 do

                             if not (faceIsEmpty delFaceData f) then

                                let area = computeSphericalExcess (vertices.[0 |> readFaceVertexId delFaceData f] |> Vec.normalize) (vertices.[1 |> readFaceVertexId delFaceData f] |> Vec.normalize) (vertices.[2 |> readFaceVertexId delFaceData f] |> Vec.normalize)
                                
                                areaSum <- areaSum + area 
                                
                        let getTrafo pos = Trafo3d.Translation pos |> Mod.init
                        
                        let mutable sg = Sg.empty

                        sg <- Sg.group' [sg; pointSg 0.12 (C4b(255, 255, 255, 100)) (Trafo3d.Identity |> Mod.init)]

                        for i in 0 .. vc - 1 do 
                            match i with
                            | 0 ->  sg <- Sg.group' [sg; pointSg 0.002 C4b.Blue (getTrafo (verticesNormalized.[i] * 0.14))]
                            // | v when v = (vc - 1) -> sg <- Sg.group' [sg; pointSg 0.002 C4b.Green (getTrafo (verticesNormalized.[i] * 0.14))]
                            | _ -> sg <- Sg.group' [sg; pointSg 0.001 C4b.Red (getTrafo (verticesNormalized.[i] * 0.14))]
                            
            
                        for i in 0 .. MAX_EDGES - 1 do         
                            if (0 |> readVertexId delEdgeData i) <> NONE && (2 |> readVertexId delEdgeData i) <> NONE then 
                                let edgeStart = verticesNormalized.[0 |> readVertexId delEdgeData i]
                                let edgeEnd   = verticesNormalized.[2 |> readVertexId delEdgeData i] 
                                

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
                |> Light.Sg.addLightCollectionSg (data.lights) (data.lightData)
                |> Light.Sg.setLightCollectionUniforms data.lights
                |> setupPhotometricData data.photometricData
                |> setupCamera data.view data.projTrafo data.viewportSize 
                |> setUniformDT data.dt
                |> setUniformUsePhotometry data.lightData.usePhotometry
                |> setUniformSkewClipPlane data.skewClipPlane
                |> setUniformDiffuseExitance data.lightData.diffuseExitance                |> Light.Sg.addLightCollectionSg (data.lights) (data.lightData)
                |> Sg.compile data.runtime signature



        let delIrrIntApproxFb (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            delIrrIntApproxRenderTask data signature sceneSg
            |> RenderTask.renderToColor data.viewportSize


        
