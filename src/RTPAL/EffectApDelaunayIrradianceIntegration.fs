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

        let initEdgeDataSlice caseOffset = Arr<N<MAX_EDGES_HALF>, V4i>(ALL.EDGES.[MAX_EDGES_HALF * caseOffset .. MAX_EDGES_HALF * caseOffset + MAX_EDGES_HALF - 1])
            


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

    let delaunyIrrIntegration applyFlipping (v : Vertex) = 
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
                            let (clippedVa, clippedVc) = clipPatchTSwN uniform.LVertices uniform.LBaseComponents.[addr] clipNormal P w2t
                            

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

                                    //let delFaceData = QUAD_DATA.getInitFaceData caseOffset
                                    let delFaceData = Arr<N<MAX_FACES_HALF>, V4i>()
                                    for i in 0 .. MAX_FACES_HALF - 1 do
                                        delFaceData.[i] <- QUAD_DATA.ALL.FACES.[MAX_FACES_HALF * caseOffset + i]
                                    
                                    let mutable delMetaData = QUAD_DATA.getInitMetaData caseOffset

                                    if applyFlipping then
                                        ////////////////////////////////////////
                                        // load inital data
                                    
                                        //let delEdgeData = QUAD_DATA.getInitEdgeData caseOffset
                                        let delEdgeData = Arr<N<MAX_EDGES_HALF>, V4i>() 
                                        for i in 0 .. MAX_EDGES_HALF - 1 do
                                            delEdgeData.[i] <- QUAD_DATA.ALL.EDGES.[MAX_EDGES_HALF * caseOffset + i]




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
                                                        

                                            let notLD = 
                                                if a - c |> Vec.length < 1e-6 || b - c |> Vec.length < 1e-6 || d - c |> Vec.length < 1e-6 then
                                                    false
                                                else
                                                    (Vec.dot (a - c) ((Vec.cross (b - c) (d - c)))) < 0.0   
                                        
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
        
    let delaunyIrrDebug (v : Vertex) = 
        fragment {

            ////////////////////////////////////////////////////////

            let P = v.wp.XYZ

            let t2w = v.n |> Vec.normalize |> basisFrisvad 
            let w2t = t2w |> Mat.transpose

            let brdf = v.c / PI 

            let mutable illumination = V4d.Zero * (uniform.dT * 1e-256 * 0.0)
            let mutable flipCount = 0
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
                            let dotOut = Vec.dot (uniform.LForwards.[addr]) ((P - uniform.LCenters.[addr])  |> Vec.normalize) |> clamp -1.0 1.0
                            
                            if abs dotOut > 1e-3 then
                                // no super small dotouts

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
                                let (clippedVa, clippedVc) = clipPatchTSwN ( if dotOut > 0.0 then uniform.LVertices else uniform.LVerticesInverse) uniform.LBaseComponents.[addr] clipNormal P w2t
                           

                                if clippedVc <> 0 then

                                    let eps = 1e-9

                                    let lightPlaneN = w2t * uniform.LForwards.[addr] |> Vec.normalize   

                                    // find closest point limited to upper hemisphere
                                    let t = (- clippedVa.[0]) |> Vec.dot lightPlaneN
                                    let mutable closestPoint = t * (-lightPlaneN)
                                                    
                                    if (Vec.dot closestPoint V3d.OOI) < 0.0 then
                                        let newDir = V3d(closestPoint.X, closestPoint.Y, 0.0) |> Vec.normalize
                                        closestPoint <- linePlaneIntersection V3d.Zero newDir (clippedVa.[0]) lightPlaneN
                                    
                                     
                                    // vertices: XYZ -> Spherical coords; W -> FunValue
                                    let (vertices, caseOffset, offset) = fillVertexArray clippedVa clippedVc t2w uniform.LForwards.[addr] uniform.LUps.[addr] uniform.LAreas.[addr] closestPoint
                                     

                                    ////////////////////////////////////////
                                    // load inital data
                                                                                                               
                                    //let delEdgeData = QUAD_DATA.getInitEdgeData caseOffset
                                    let mutable delEdgeData = QUAD_DATA.initEdgeDataSlice CASE_CORNER_OFFSET 
                                    if caseOffset = CASE_EDGE_OFFSET then
                                        delEdgeData <- QUAD_DATA.initEdgeDataSlice CASE_EDGE_OFFSET 
                                    else if caseOffset = CASE_INSIDE_OFFSET then
                                        delEdgeData <- QUAD_DATA.initEdgeDataSlice CASE_INSIDE_OFFSET 

                                    //for i in 0 .. MAX_EDGES_HALF - 1 do
                                    //    delEdgeData.[i] <- QUAD_DATA.ALL.EDGES.[MAX_EDGES_HALF * caseOffset + i]

                                    
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
                                        let c = vertices.[ readVertexId delEdgeData eId 2].XYZ  
                                        let notLD = (Vec.dot ((vertices.[ readVertexId delEdgeData eId 0].XYZ) - c) ((Vec.cross ((vertices.[ readVertexId delEdgeData eId 1].XYZ) - c) ((vertices.[ readVertexId delEdgeData eId 3].XYZ) - c)))) < 0.0 
                                        
                                        if notLD then
                                            // flip edge
                                            flipCount <- flipCount + 1

                                            let (meta, sp) = cheapFlipEdge delEdgeData delMetaData stack SP eId
                                            //let (meta, sp) = flipEdge delEdgeData delMetaData delFaceData stack SP eId

                                            SP <- sp
                                            delMetaData <- meta

                                    ////////////////////////////////////////////////////////
                                    // integrate
                                    
                                    let patchIllumination = computeIlluminationCheap delEdgeData vertices caseOffset
                                                                            
                                    illumination <- illumination + patchIllumination * brdf 
                                    

                            ////////////////////////////////////////////////////////
            let mutable color = V4d(1) + illumination * 1e-8;

            if flipCount = 0 then
                color <- V4d.OOOI
            else if flipCount = 1 then 
                color <- V4d.IOOI
            else if flipCount = 2 then 
                color <- V4d.OIOI
            else if flipCount = 3 then 
                color <- V4d.OOII
            else if flipCount = 4 then 
                color <- V4d.IIOI
            else if flipCount = 5 then 
                color <- V4d.OIII

            //return color
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
                    let! baseComponents = lc.BaseComponents
                    let! areas = lc.Areas
                    let! vertices = lc.Vertices
                    let! verticesInverse = lc.VerticesInverse
                    let! centers = lc.Centers
                    let! forwards = lc.Forwards
                    let! ups = lc.Ups

                    return (lights, patchIndices,  Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>(vertices),  Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>(verticesInverse), baseComponents, centers, forwards, ups, areas)

                }

            lc |> Mod.map (fun lc ->

                let (lights, lPatchIndices, lVertices, lVerticesInverse, lBaseComponents, lCenters, lForwards, lUps, lAreas) = lc
                
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

                ////////////////////////////////////////////////////////

                let dotOut = Vec.dot (lForwards.[addr]) ((P - lCenters.[addr])  |> Vec.normalize) |> clamp -1.0 1.0

                
                let (clippedVa, clippedVc) = clipPatchTSwN ( if dotOut > 0.0 then lVertices else lVerticesInverse) lBaseComponents.[addr] V3d.OOI P w2t

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

                        // vertices: XYZ -> Spherical coords; W -> FunValue
                        let (vertices, caseOffset, offset) = fillVertexArray clippedVa clippedVc t2w lForwards.[addr] lUps.[addr] lAreas.[addr] closestPoint

                        for i in 0 .. vertices.Length - 1 do
                            vertices.[i] <- V4d(vertices.[i].XYZ, 1.0)

                        ////////////////////////////////////////
                        // load inital data
                        //let delEdgeData = QUAD_DATA.getInitEdgeData caseOffset
                        let mutable delEdgeData = QUAD_DATA.initEdgeDataSlice CASE_CORNER_OFFSET 
                        if caseOffset = CASE_EDGE_OFFSET then
                            delEdgeData <- QUAD_DATA.initEdgeDataSlice CASE_EDGE_OFFSET 
                        else if caseOffset = CASE_INSIDE_OFFSET then
                            delEdgeData <- QUAD_DATA.initEdgeDataSlice CASE_INSIDE_OFFSET 
                            
                                    
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
                            let c = vertices.[ readVertexId delEdgeData eId 2].XYZ  
                            let notLD = (Vec.dot ((vertices.[ readVertexId delEdgeData eId 0].XYZ) - c) ((Vec.cross ((vertices.[ readVertexId delEdgeData eId 1].XYZ) - c) ((vertices.[ readVertexId delEdgeData eId 3].XYZ) - c)))) < 0.0 
                                        
                            if notLD then
                                // flip edge
                                let (meta, sp) = cheapFlipEdge delEdgeData delMetaData stack SP eId

                                SP <- sp
                                delMetaData <- meta

                        ////////////////////////////////////////////////////////

                        let patchIllumination = computeIlluminationCheap delEdgeData vertices caseOffset

                        printfn "PatchIllumination: %f" patchIllumination
                                
                        let getTrafo pos = Trafo3d.Translation pos |> Mod.constant
                        
                        let mutable sg = Sg.empty

                        // center
                        sg <- Sg.group' [sg; pointSg 0.12 (C4b(255, 255, 255, 100)) (Trafo3d.Identity |> Mod.init)]

                        // closet
                        sg <- Sg.group' [sg; pointSg 0.002 C4b.Blue (getTrafo (vertices.[0].XYZ * 0.14))]

                        for v in vertices |> List.ofSeq |> List.tail do 
                            if not <| V4d.ApproxEqual(v, (V4d(0)), 1e-8) then
                                sg <- Sg.group' [sg; pointSg 0.001 C4b.Red (getTrafo (v.XYZ * 0.14))]
                            
            
                        for i in 0 .. MAX_EDGES - 1 do         
                            if (0 |> readVertexId delEdgeData i) <> NONE && (2 |> readVertexId delEdgeData i) <> NONE then 
                                let edgeStart = vertices.[0 |> readVertexId delEdgeData i].XYZ
                                let edgeEnd   = vertices.[2 |> readVertexId delEdgeData i].XYZ
                                

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


        let internal setupScene (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) applyFlip = 
   
            
            let sceneSg = 
                [
                    sceneSg
                    Debug.delaunyScene data.lights |> Sg.dynamic
                ]
                |> Sg.group
            
           
            sceneSg
                |> setupFbEffects [ 
                        if applyFlip then
                            yield delaunyIrrIntegration true |> toEffect
                        else
                            yield delaunyIrrDebug |> toEffect
                    ]
                |> Light.Sg.addLightCollectionSg (data.lights) (data.lightData)
                |> Light.Sg.setLightCollectionUniforms data.lights
                |> setupPhotometricData data.photometricData
                |> setupCamera data.view data.projTrafo data.viewportSize 
                |> setUniformDT data.dt
                |> setUniformUsePhotometry data.lightData.usePhotometry
                |> setUniformSkewClipPlane data.skewClipPlane
                |> setUniformDiffuseExitance data.lightData.diffuseExitance                
                |> Light.Sg.addLightCollectionSg (data.lights) (data.lightData)
                |> Sg.compile data.runtime signature



        let internal setupApproxFb (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) applyFlip = 
            setupScene data signature sceneSg applyFlip
            |> RenderTask.renderToColor data.viewportSize

            
        let delIrrIntNoFlipApproxFb (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            false |> setupApproxFb data signature sceneSg

        let delIrrIntApproxFb (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            true |> setupApproxFb data signature sceneSg
