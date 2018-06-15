namespace Render


module EffectApDelaunayDataHandling = 

    open Aardvark.Base
    open FShade

    open Config.Delaunay
        
    let NONE = 0x000000FF

    let private ID_BIT_MASK = uint32 0x000000FF 
        
    let private BIT_PER_ID = 8

    [<ReflectedDefinition>][<Inline>]
    let private getIdFromInt pos value =
        int (((uint32 value &&& (ID_BIT_MASK <<< pos * BIT_PER_ID)) >>> (pos * BIT_PER_ID)))

    [<ReflectedDefinition>][<Inline>]
    let private setIdInInt pos value id =
        int (((uint32 value &&& (~~~(ID_BIT_MASK <<< pos * BIT_PER_ID))) ||| (uint32 id <<< pos * BIT_PER_ID)))

    [<ReflectedDefinition>][<Inline>]
    let private setTwoIdsInInt pos value id0 id1 =
        int (((uint32 value &&& (~~~((ID_BIT_MASK <<< (pos + 1) * BIT_PER_ID) ||| (ID_BIT_MASK <<< pos * BIT_PER_ID)))) ||| ((uint32 id1 <<< (pos + 1) * BIT_PER_ID) ||| (uint32 id0 <<< pos * BIT_PER_ID))))
        
    [<ReflectedDefinition>][<Inline>]
    let private offsetId offset id = 
        id <<< (BIT_PER_ID * offset)

    [<ReflectedDefinition>][<Inline>]
    let genIntFromV3i (v : V3i) =
        int ((uint32 (offsetId 0 v.X) ||| uint32 (offsetId 1 v.Y) ||| uint32 (offsetId 2 v.Z)))

    [<ReflectedDefinition>][<Inline>]
    let genIntFromV4i (v : V4i) =
        (offsetId 0 v.X) ||| (offsetId 1 v.Y) ||| (offsetId 2 v.Z) ||| (offsetId 3 v.W)

    [<ReflectedDefinition>][<Inline>]
    let private leftShiftWithRotation24Bit shift v =
        int (((uint32 v <<< BIT_PER_ID * shift) ||| (uint32 v >>> BIT_PER_ID * (3 - shift))) &&& uint32 0x00FFFFFF)

    [<ReflectedDefinition>][<Inline>]
    let private leftShiftWithRotation32Bit shift v =
        int ((uint32 v <<< BIT_PER_ID * shift) ||| (uint32 v >>> BIT_PER_ID * (4 - shift)))

    [<ReflectedDefinition>][<Inline>]
    let private rightShiftWithRotation32Bit shift v =
        int ((uint32 v >>> BIT_PER_ID * shift) ||| (uint32 v <<< BIT_PER_ID * (4 - shift)))

    //////////////////////////////////////////////////////////////////////////////////////////
    // Edge 

    let OPPOSITE_0 = 1
    let OPPOSITE_1 = 3

    // Vertex Handling

    [<ReflectedDefinition>][<Inline>]
    let readVertexId (edges : Arr<N<MAX_EDGES_HALF>, V4i>) eId vPos =
        getIdFromInt vPos (if eId % 2 = 0 then edges.[eId / 2].X else edges.[eId / 2].Z)

    [<ReflectedDefinition>][<Inline>]
    let writeVertexId (edges : Arr<N<MAX_EDGES_HALF>, V4i>) eId vPos vertexId =
        edges.[eId / 2] <- 
            if eId % 2 = 0 then 
                V4i(setIdInInt vPos (edges.[eId / 2].X) vertexId, edges.[eId / 2].Y, edges.[eId / 2].Z, edges.[eId / 2].W)
            else 
                V4i(edges.[eId / 2].X, edges.[eId / 2].Y, setIdInInt vPos (edges.[eId / 2].Z) vertexId, edges.[eId / 2].W)
 
    // Neighbor Edge Handling

    [<ReflectedDefinition>][<Inline>]
    let readEdgeId (edges : Arr<N<MAX_EDGES_HALF>, V4i>) eId ePos =
        getIdFromInt ePos (if eId % 2 = 0 then edges.[eId / 2].Y else edges.[eId / 2].W)

    [<ReflectedDefinition>][<Inline>]
    let writeEdgeId (edges : Arr<N<MAX_EDGES_HALF>, V4i>) eId ePos edgeId =
        edges.[eId / 2] <-
            if eId % 2 = 0 then 
                V4i(edges.[eId / 2].X, setIdInInt ePos (edges.[eId / 2].Y) edgeId, edges.[eId / 2].Z, edges.[eId / 2].W)
            else 
                V4i(edges.[eId / 2].X, edges.[eId / 2].Y, edges.[eId / 2].Z, setIdInInt ePos (edges.[eId / 2].W) edgeId)

    // Meta Handling

    let private META_BIT_MASK = uint32 0x00000001

    let private MARKED_OFFSET = 0
    let private INSIDE_OFFSET = 1

    [<ReflectedDefinition>][<Inline>]
    let edgeIsInside meta eId = 
        uint32 meta &&& (META_BIT_MASK <<< ((eId * 2) + INSIDE_OFFSET)) <> uint32 0 

    // TODO maybe replace meta with a meta ref, check shader code
    [<ReflectedDefinition>][<Inline>]
    let makeEdgeInside meta eId =
        int ((uint32 meta ||| (META_BIT_MASK <<< ((eId * 2) + INSIDE_OFFSET))))
            
    [<ReflectedDefinition>][<Inline>]
    let edgeIsMarked meta eId = 
        uint32 meta &&& (META_BIT_MASK <<< ((eId * 2) + MARKED_OFFSET)) <> uint32 0 

    // TODO maybe replace meta with a meta ref, check shader code
    [<ReflectedDefinition>][<Inline>]
    let markEdge meta eId =
        int ((uint32 meta ||| (META_BIT_MASK <<< ((eId * 2) + MARKED_OFFSET))))

    // TODO maybe replace meta with a meta ref, check shader code
    [<ReflectedDefinition>][<Inline>]
    let unmarkEdge meta eId =
        int ((uint32 meta &&& (~~~(META_BIT_MASK <<< ((eId * 2) + MARKED_OFFSET)))))


    //////////////////////////////////////////////////////////////////////////////////////////
    // Face 

    // Vertex Handling

    [<ReflectedDefinition>][<Inline>]
    let readFaceVertexId (faces : Arr<N<MAX_FACES_HALF>, V4i>) fId vPos =
        // getIdFromInt vPos (if fId % 2 = 0 then faces.[fId / 2].X else faces.[fId / 2].Z)
        if fId % 2 = 0 then
            getIdFromInt vPos (faces.[fId / 2].X)
        else
            getIdFromInt vPos (faces.[fId / 2].Z)

    [<ReflectedDefinition>][<Inline>]
    let writeFaceVertexIdsCombined (faces : Arr<N<MAX_FACES_HALF>, V4i>) fId vertices =
        faces.[fId / 2] <-
            if fId % 2 = 0 then 
                V4i(vertices, faces.[fId / 2].Y, faces.[fId / 2].Z, faces.[fId / 2].W)
            else 
                V4i(faces.[fId / 2].X, faces.[fId / 2].Y, vertices, faces.[fId / 2].W)

    [<ReflectedDefinition>][<Inline>]
    let writeFaceVertexIds (faces : Arr<N<MAX_FACES_HALF>, V4i>) fId v0 v1 v2 =
        writeFaceVertexIdsCombined faces fId (int (uint32 (offsetId 0 v0) ||| uint32 (offsetId 1 v1) ||| uint32 (offsetId 2 v2)))

    // Neighbor Edge Handling

    [<ReflectedDefinition>][<Inline>]
    let readFaceEdgeId (faces : Arr<N<MAX_FACES_HALF>, V4i>) fId ePos =
        getIdFromInt ePos (if fId % 2 = 0 then faces.[fId / 2].Y else faces.[fId / 2].W)

    [<ReflectedDefinition>][<Inline>]
    let writeFaceEdgeIds (faces : Arr<N<MAX_FACES_HALF>, V4i>) fId e0 e1 e2 =
        faces.[fId / 2] <- 
            if fId % 2 = 0 then
                V4i(faces.[fId / 2].X, (offsetId 0 e0) ||| (offsetId 1 e1) ||| (offsetId 2 e2), faces.[fId / 2].Z, faces.[fId / 2].W)
            else
                V4i(faces.[fId / 2].X, faces.[fId / 2].Y, faces.[fId / 2].Z, (offsetId 0 e0) ||| (offsetId 1 e1) ||| (offsetId 2 e2))

    [<ReflectedDefinition>][<Inline>]
    let faceIsEmpty (faces : Arr<N<MAX_FACES_HALF>, V4i>) fId =
        (if fId % 2 = 0 then faces.[fId / 2].X else faces.[fId / 2].Z) = 0x00FFFFFF
            


    //////////////////////////////////////////////////////////////////////////////////////////
    // Misc Handling

    [<ReflectedDefinition>][<Inline>]
    let private compareVerticesPermutations24Bit v0 v1 =
        v0 = v1 || (leftShiftWithRotation24Bit 1 v0) = v1 || (leftShiftWithRotation24Bit 2 v0) = v1

    [<ReflectedDefinition>][<Inline>]
    let verticesAreFromFace (faces : Arr<N<MAX_FACES_HALF>, V4i>) fId vertices =
        compareVerticesPermutations24Bit (if fId % 2 = 0 then  faces.[fId / 2].X else faces.[fId / 2].Z) vertices

    [<ReflectedDefinition>][<Inline>]
    let leftShiftVerticesAndEdgesByOne (edges : Arr<N<MAX_EDGES_HALF>, V4i>) eId =
        edges.[eId / 2] <-
            if eId % 2 = 0 then 
                V4i(rightShiftWithRotation32Bit 1 (edges.[eId / 2].X), rightShiftWithRotation32Bit 1 (edges.[eId / 2].Y), edges.[eId / 2].Z, edges.[eId / 2].W)
            else 
                V4i(edges.[eId / 2].X, edges.[eId / 2].Y, rightShiftWithRotation32Bit 1 (edges.[eId / 2].Z), rightShiftWithRotation32Bit 1 (edges.[eId / 2].W))


    [<ReflectedDefinition>][<Inline>]
    let writeOppositeEdgeIds (edges : Arr<N<MAX_EDGES_HALF>, V4i>) eId opposite edgeToId edgeFromId =
        edges.[eId / 2] <-
            if eId % 2 = 0 then 
                V4i(edges.[eId / 2].X, setTwoIdsInInt (opposite - 1) (edges.[eId / 2].Y) edgeToId edgeFromId, edges.[eId / 2].Z, edges.[eId / 2].W)
            else 
                V4i(edges.[eId / 2].X, edges.[eId / 2].Y, edges.[eId / 2].Z, setTwoIdsInInt (opposite - 1) (edges.[eId / 2].W) edgeToId edgeFromId)

    [<ReflectedDefinition>][<Inline>]
    let compareIndices (edges : Arr<N<MAX_EDGES_HALF>, V4i>) e0 pos0 e1 pos1 = 
        getIdFromInt pos0 (if e0 % 2 = 0 then edges.[e0 / 2].X else edges.[e0 / 2].Z) = getIdFromInt pos1 (if e1 % 2 = 0 then edges.[e1 / 2].X else edges.[e1 / 2].Z) 

    // Use OPPOSITE_0 and OPPOSITE_1 as opposite parameter
    [<ReflectedDefinition>][<Inline>]
    let getFaceVerticesOfEdge (edges : Arr<N<MAX_EDGES_HALF>, V4i>) opposite eId  = 
        if opposite = OPPOSITE_0 then 
            (if eId % 2 = 0 then edges.[eId / 2].X else edges.[eId / 2].Z) &&& 0x00FFFFFF
        else
            (((if eId % 2 = 0 then edges.[eId / 2].X else edges.[eId / 2].Z) &&& 0xFFFF0000) >>> (BIT_PER_ID * 2)) + (((if eId % 2 = 0 then edges.[eId / 2].X else edges.[eId / 2].Z) &&& 0x000000FF) <<< (BIT_PER_ID * 2))
            

    //////////////////////////////////////////////////////////////////////////////////////////
    // Data Import

    let generateCompactEdgeV4i (v0 : V4i) (e0 : V4i) (v1 : V4i) (e1 : V4i) =
        V4i(genIntFromV4i v0, genIntFromV4i e0, genIntFromV4i v1, genIntFromV4i e1) 

    let generateCompactFaceV4i (fv0 : V3i) (fe0 : V3i) (fv1 : V3i) (fe1 : V3i) =
        V4i(genIntFromV3i fv0, genIntFromV3i fe0, genIntFromV3i fv1, genIntFromV3i fe1) 

    let private  replaceMinusOneWithNone v = 
        if v = -1 then NONE else v

    let private replaceMinusOneWithNoneInV4i (v : V4i) =
        V4i(replaceMinusOneWithNone (v.X), replaceMinusOneWithNone (v.Y), replaceMinusOneWithNone (v.Z), replaceMinusOneWithNone (v.W))

    let private replaceMinusOneWithNoneInV3i (v : V3i) =
        V3i(replaceMinusOneWithNone (v.X), replaceMinusOneWithNone (v.Y), replaceMinusOneWithNone (v.Z))

    let transformEdgesToCompactRepresentation (V : V4i array) (E : V4i array) (M : V2i array) = 
            
        let edges = [|
                        for i in 0 .. 2 .. MAX_EDGES - 2 do
                            yield generateCompactEdgeV4i (replaceMinusOneWithNoneInV4i (V.[i])) (replaceMinusOneWithNoneInV4i (E.[i])) (replaceMinusOneWithNoneInV4i (V.[i + 1])) (replaceMinusOneWithNoneInV4i (E.[i + 1]))
                    |]
            
        let mutable meta = 0x00000000

        for i in 0 .. MAX_EDGES - 1 do

            if M.[i].X = 1 then
                meta <- makeEdgeInside meta i

            if M.[i].Y = 1 then
                meta <- markEdge meta i

        (edges, meta)

    let transformEdgeCollectionToCompactCollection (V : V4i array) (E : V4i array) (M : V2i array) numOfCollectionElements =

        let mutable edgeList : V4i array = Array.empty
        let mutable metaList : int array = Array.empty

        for i in 0 .. numOfCollectionElements - 1 do

            let s = i * MAX_EDGES
            let e = i * MAX_EDGES + MAX_EDGES - 1
            let (e, m) = transformEdgesToCompactRepresentation (V.[s .. e]) (E.[s .. e]) (M.[s .. e])

            edgeList <- Array.concat [ edgeList ; e ]
            metaList <- Array.concat [ metaList ; [| m |] ]


        (edgeList, metaList)
        
    let transformFacesToCompactRepresentation (FV : V4i array) (FE : V3i array) =
        [|
            for i in 0 .. 2 .. MAX_FACES - 2 do
                yield generateCompactFaceV4i (replaceMinusOneWithNoneInV3i (FV.[i].XYZ)) (replaceMinusOneWithNoneInV3i (FE.[i] )) (replaceMinusOneWithNoneInV3i (FV.[i + 1].XYZ)) (replaceMinusOneWithNoneInV3i (FE.[i + 1]))
        |]

    let transformFaceollectionToCompactCollection (FV : V4i array) (FE : V3i array) numOfCollectionElements =

        let mutable faceList : V4i array = Array.empty

        for i in 0 .. numOfCollectionElements - 1 do

            let s = i * MAX_FACES
            let e = i * MAX_FACES + MAX_FACES - 1
            let f = transformFacesToCompactRepresentation (FV.[s .. e]) (FE.[s .. e])

            faceList <- Array.concat [ faceList ; f ]

        faceList

    module Test =
        
        open NUnit.Framework
        open FsUnit

        [<Test>]
        let ``Get Id from Int``() = 
            Assert.Multiple( fun _ ->
                0x000000AA |> getIdFromInt 0 |> should equal 0xAA
                0x000000AA |> getIdFromInt 1 |> should equal 0x00
                0x00AABB00 |> getIdFromInt 2 |> should equal 0xAA
                0xFFAABB00 |> getIdFromInt 3 |> should equal 0xFF
            )

        [<Test>]
        let ``Set Id in Int``() = 
            Assert.Multiple( fun _ ->
                0xBB |> setIdInInt 0 0x000000AA |> should equal 0x000000BB
                0xFF |> setIdInInt 1 0x000000AA |> should equal 0x0000FFAA
                0xCC |> setIdInInt 2 0x00AABB00 |> should equal 0x00CCBB00
                0xDD |> setIdInInt 3 0xFFAABB00 |> should equal 0xDDAABB00
            )

        [<Test>]
        let ``Set Two Ids in Int``() = 
            Assert.Multiple( fun _ ->
                setTwoIdsInInt 0 0x000000AA 0xBB 0xBB |> should equal 0x0000BBBB
                setTwoIdsInInt 1 0x000000AA 0xFF 0xAA |> should equal 0x00AAFFAA
                setTwoIdsInInt 2 0x00AABB00 0xCC 0xBB |> should equal 0xBBCCBB00
            )

        [<Test>]
        let ``Offset Id``() = 
            Assert.Multiple( fun _ ->
                0x000000AA |> offsetId 0 |> should equal 0x000000AA
                0x000000AA |> offsetId 1 |> should equal 0x0000AA00
                0x00AABB00 |> offsetId 2 |> should equal 0xBB000000
            )

        [<Test>]
        let ``Gen Int From V3i``() = 
            Assert.Multiple( fun _ ->
                V3i(0xAA, 0x02, 0x23) |> genIntFromV3i |> should equal 0x002302AA
            )

        [<Test>]
        let ``Gen Int From V4i``() = 
            Assert.Multiple( fun _ ->
                V4i(0xAA, 0x02, 0x23, 0x67) |> genIntFromV4i |> should equal 0x672302AA
            )

        [<Test>]
        let ``Left Shift  with Rotation 24Bit``() = 
            Assert.Multiple( fun _ ->
                0x000000AA |> leftShiftWithRotation24Bit 0 |> should equal 0x000000AA
                0x000000AA |> leftShiftWithRotation24Bit 2 |> should equal 0x00AA0000
                0x000000AA |> leftShiftWithRotation24Bit 3 |> should equal 0x000000AA
                0x00AA00AA |> leftShiftWithRotation24Bit 3 |> should equal 0x00AA00AA
            )

        [<Test>]
        let ``Left Shift  with Rotation 32Bit``() = 
            Assert.Multiple( fun _ ->
                0x000000AA |> leftShiftWithRotation32Bit 0 |> should equal 0x000000AA
                0x000000AA |> leftShiftWithRotation32Bit 1 |> should equal 0x0000AA00
                0x000000AA |> leftShiftWithRotation32Bit 2 |> should equal 0x00AA0000
                0x000000AA |> leftShiftWithRotation32Bit 3 |> should equal 0xAA000000
                0xAA0000AA |> leftShiftWithRotation32Bit 3 |> should equal 0xAAAA0000
            )

        [<Test>]
        let ``Right Shift With Rotation 32Bit``() =
            Assert.Multiple( fun _ ->
                0x000000AA |> rightShiftWithRotation32Bit 0 |> should equal 0x000000AA
                0x000000AA |> rightShiftWithRotation32Bit 1 |> should equal 0xAA000000
                0x000000AA |> rightShiftWithRotation32Bit 2 |> should equal 0x00AA0000
                0x000000AA |> rightShiftWithRotation32Bit 3 |> should equal 0x0000AA00
                0xAA0000AA |> rightShiftWithRotation32Bit 3 |> should equal 0x0000AAAA
            )

        let private getMockupEdgeArray () =
            Arr<N<MAX_EDGES_HALF>, V4i>([|
                                        yield V4i(V4i(0, 1, 2, 3) |> genIntFromV4i, V4i(00, 11, 22, 33) |> genIntFromV4i, V4i(4, 5, 6, 7) |> genIntFromV4i, V4i(44, 55, 66, 77) |> genIntFromV4i) 
                                        yield V4i(V4i(8, 9, 10, 11) |> genIntFromV4i, V4i(88, 99, 100, 111) |> genIntFromV4i, V4i(12, 13, 14, 15) |> genIntFromV4i, V4i(122, 133, 144, 155) |> genIntFromV4i) 
                                        yield V4i(V4i(0, 0, 2, 3) |> genIntFromV4i, V4i(00, 11, 22, 33) |> genIntFromV4i, V4i(4, 4, 4, 7) |> genIntFromV4i, V4i(44, 55, 66, 77) |> genIntFromV4i) 
                                    |])

        [<Test>]
        let ``Read Vertex Id``() = 
            let edges = getMockupEdgeArray ()

            Assert.Multiple( fun _ ->
                0 |> readVertexId edges 0 |> should equal 0
                1 |> readVertexId edges 1 |> should equal 5
                2 |> readVertexId edges 2 |> should equal 10
                3 |> readVertexId edges 3 |> should equal 15
            )

        [<Test>]
        let ``Write Vertex Id``() = 
            let edges = getMockupEdgeArray ()

            12 |> writeVertexId edges 0 0 
            12 |> writeVertexId edges 1 1 
            12 |> writeVertexId edges 2 2 
            12 |> writeVertexId edges 3 3 

            Assert.Multiple( fun _ ->
                readVertexId edges 0 0 |> should equal 12
                readVertexId edges 1 1 |> should equal 12
                readVertexId edges 2 2 |> should equal 12
                readVertexId edges 3 3 |> should equal 12
            )

        [<Test>]
        let ``Read Edge Id``() = 
            let edges = getMockupEdgeArray ()

            Assert.Multiple( fun _ ->
                0 |> readEdgeId edges 0 |> should equal 0
                1 |> readEdgeId edges 1 |> should equal 55
                2 |> readEdgeId edges 2 |> should equal 100
                3 |> readEdgeId edges 3 |> should equal 155
            )

        [<Test>]
        let ``Write Edge Id``() = 
            let edges = getMockupEdgeArray ()

            12 |> writeEdgeId edges 0 0 
            12 |> writeEdgeId edges 1 1 
            12 |> writeEdgeId edges 2 2 
            12 |> writeEdgeId edges 3 3 

            Assert.Multiple( fun _ ->
                readEdgeId edges 0 0 |> should equal 12
                readEdgeId edges 1 1 |> should equal 12
                readEdgeId edges 2 2 |> should equal 12
                readEdgeId edges 3 3 |> should equal 12
            )

        [<Test>]
        let ``Edge Is Inside``() =

            let meta = 0x00000880

            Assert.Multiple( fun _ ->
                edgeIsInside meta 3 |> should equal true
                edgeIsInside meta 1 |> should equal false
                edgeIsInside meta 5 |> should equal true
                edgeIsInside meta 6 |> should equal false
            )

        [<Test>]
        let ``Make Edge Inside``() =

            let mutable meta = 0x0

            meta <- makeEdgeInside meta 4
            meta <- makeEdgeInside meta 6

            Assert.Multiple( fun _ ->
                edgeIsInside meta 4 |> should equal true
                edgeIsInside meta 1 |> should equal false
                edgeIsInside meta 6 |> should equal true
                edgeIsInside meta 3 |> should equal false
            )

        [<Test>]
        let ``Edge Is Marked``() =

            let meta = 0x00000440

            Assert.Multiple( fun _ ->
                edgeIsMarked meta 3 |> should equal true
                edgeIsMarked meta 1 |> should equal false
                edgeIsMarked meta 5 |> should equal true
                edgeIsMarked meta 6 |> should equal false
            )

        [<Test>]
        let ``Mark Edge``() =

            let mutable meta = 0x0

            meta <- markEdge meta 4
            meta <- markEdge meta 6

            Assert.Multiple( fun _ ->
                edgeIsMarked meta 4 |> should equal true
                edgeIsMarked meta 1 |> should equal false
                edgeIsMarked meta 6 |> should equal true
                edgeIsMarked meta 3 |> should equal false
            )

        [<Test>]
        let ``Unmark Edge``() =

            let mutable meta = 0x00000440

            meta <- unmarkEdge meta 3
            meta <- unmarkEdge meta 5

            Assert.Multiple( fun _ ->
                edgeIsMarked meta 3 |> should equal false
                edgeIsMarked meta 1 |> should equal false
                edgeIsMarked meta 5 |> should equal false
                edgeIsMarked meta 3 |> should equal false
            )

        let private getMockupFaceArray () =
            Arr<N<MAX_FACES_HALF>, V4i>([|
                                        yield V4i(-1)
                                        yield V4i(-1)
                                        yield V4i(-1)
                                        yield V4i(V3i(0, 1, 2) |> genIntFromV3i, V3i(00, 11, 22) |> genIntFromV3i, V3i(4, 5, 6) |> genIntFromV3i, V3i(44, 55, 66) |> genIntFromV3i) 
                                        yield V4i(V3i(8, 9, 10) |> genIntFromV3i, V3i(88, 99, 100) |> genIntFromV3i, V3i(12, 13, 14) |> genIntFromV3i, V3i(122, 133, 144) |> genIntFromV3i) 
                                    |])

        [<Test>]
        let ``Read Face Vertex Id``() =                
            let faces = getMockupFaceArray ()

            Assert.Multiple( fun _ ->
                0 |> readFaceVertexId faces 6 |> should equal 0
                1 |> readFaceVertexId faces 7 |> should equal 5
                2 |> readFaceVertexId faces 8 |> should equal 10
                1 |> readFaceVertexId faces 9 |> should equal 13
            )

        [<Test>]
        let ``Write Face Vertex Ids Combined``() = 
            let faces = getMockupFaceArray ()

            0x00AABBCC |> writeFaceVertexIdsCombined faces 6 
            0x00AABBCC |> writeFaceVertexIdsCombined faces 7 
            0x00AABBCC |> writeFaceVertexIdsCombined faces 8 
            0x00AABBCC |> writeFaceVertexIdsCombined faces 9 

            Assert.Multiple( fun _ ->
                0 |> readFaceVertexId faces 6 |> should equal 0xCC
                1 |> readFaceVertexId faces 7 |> should equal 0xBB
                2 |> readFaceVertexId faces 8 |> should equal 0xAA
                1 |> readFaceVertexId faces 9 |> should equal 0xBB
            )

        [<Test>]
        let ``Write Face Vertex Ids``() = 
            let faces = getMockupFaceArray ()

            writeFaceVertexIds faces 6 0xCC 0xBB 0xAA
            writeFaceVertexIds faces 7 0xCC 0xBB 0xAA
            writeFaceVertexIds faces 8 0xCC 0xBB 0xAA
            writeFaceVertexIds faces 9 0xCC 0xBB 0xAA

            Assert.Multiple( fun _ ->
                0 |> readFaceVertexId faces 6 |> should equal 0xCC
                1 |> readFaceVertexId faces 7 |> should equal 0xBB
                2 |> readFaceVertexId faces 8 |> should equal 0xAA
                1 |> readFaceVertexId faces 9 |> should equal 0xBB
            )
        
        [<Test>]
        let ``Read Face Edge Id``() = 
            let faces = getMockupFaceArray ()

            Assert.Multiple( fun _ ->
                0 |> readFaceEdgeId faces 6 |> should equal 0
                1 |> readFaceEdgeId faces 7 |> should equal 55
                2 |> readFaceEdgeId faces 8 |> should equal 100
                1 |> readFaceEdgeId faces 9 |> should equal 133
            )

        [<Test>]
        let ``Write Face Edge Ids``() = 
            let faces = getMockupFaceArray ()

            writeFaceEdgeIds faces 6 0xCC 0xBB 0xAA
            writeFaceEdgeIds faces 7 0xCC 0xBB 0xAA
            writeFaceEdgeIds faces 8 0xCC 0xBB 0xAA
            writeFaceEdgeIds faces 9 0xCC 0xBB 0xAA

            Assert.Multiple( fun _ ->
                0 |> readFaceEdgeId faces 6 |> should equal 0xCC
                1 |> readFaceEdgeId faces 7 |> should equal 0xBB
                2 |> readFaceEdgeId faces 8 |> should equal 0xAA
                1 |> readFaceEdgeId faces 9 |> should equal 0xBB
            )
        
        [<Test>]
        let ``Compare Vertices Permutations 24Bit``() =
            
            Assert.Multiple( fun _ ->
                compareVerticesPermutations24Bit 0x00AABBCC 0x00AABBCC |> should equal true
                compareVerticesPermutations24Bit 0x00CCBBAA 0x00BBAACC |> should equal true
                compareVerticesPermutations24Bit 0x00CCBBAA 0x00AACCBB |> should equal true
                compareVerticesPermutations24Bit 0x00AABBCC 0x00CCBBAA |> should equal false
                compareVerticesPermutations24Bit 0x00CCBBAA 0x00CCAABB |> should equal false
            )

        [<Test>]
        let ``Vertices Are From Face``() =
            let faces = getMockupFaceArray ()

            Assert.Multiple( fun _ ->
                verticesAreFromFace faces 6 (V3i(0, 1, 2) |> genIntFromV3i) |> should equal true
                verticesAreFromFace faces 6 (V3i(1, 2, 0) |> genIntFromV3i) |> should equal true
                verticesAreFromFace faces 6 (V3i(1, 2, 3) |> genIntFromV3i) |> should equal false
            )

        [<Test>]
        let ``Leftshift Vertices And Edges By One``() =
            let edges = getMockupEdgeArray ()


            leftShiftVerticesAndEdgesByOne edges 2   
            let shiftResult2 = V2i(V4i(9, 10, 11, 8) |> genIntFromV4i, V4i(99, 100, 111, 88) |> genIntFromV4i)

            leftShiftVerticesAndEdgesByOne edges 3
            let shiftResult3 = V2i(V4i(13, 14, 15, 12) |> genIntFromV4i, V4i(133, 144, 155, 122) |> genIntFromV4i)

            Assert.Multiple( fun _ ->
                edges.[1].XY |> should equal shiftResult2
                edges.[1].ZW |> should equal shiftResult3
            )

        [<Test>]
        let ``Write Opposite Edge Ids``() =
            let edges = getMockupEdgeArray ()

            writeOppositeEdgeIds edges 2 OPPOSITE_0 0xAA 0xBB 
            writeOppositeEdgeIds edges 3 OPPOSITE_1 0xAA 0xBB 

            Assert.Multiple( fun _ ->
                readEdgeId edges 2 0 |> should equal 0xAA
                readEdgeId edges 2 1 |> should equal 0xBB
                readEdgeId edges 3 2 |> should equal 0xAA
                readEdgeId edges 3 3 |> should equal 0xBB
            )
        
        [<Test>]
        let ``Compare Indices``() =
            let edges = getMockupEdgeArray ()
            
            Assert.Multiple( fun _ ->
                compareIndices edges 0 0 4 1 |> should equal true
                compareIndices edges 1 3 5 3 |> should equal true
                compareIndices edges 0 0 3 3 |> should equal false
                compareIndices edges 1 1 3 1 |> should equal false
            )

        [<Test>]
        let ``Get Face Vertices Of Edge``() =
            let edges = getMockupEdgeArray ()

            Assert.Multiple( fun _ ->
                0 |> getFaceVerticesOfEdge edges OPPOSITE_0 |> should equal 0x020100
                3 |> getFaceVerticesOfEdge edges OPPOSITE_0 |> should equal 0x0E0D0C
                4 |> getFaceVerticesOfEdge edges OPPOSITE_1 |> should equal 0x000302
                5 |> getFaceVerticesOfEdge edges OPPOSITE_1 |> should equal 0x040704
            )

        [<Test>]
        let ``Generate Compact Edge V4i``() =
            let edge = generateCompactEdgeV4i (V4i(0x1, 0x2, 0x3, 0x4)) (V4i(0x5, 0x6, 0x7, 0x8)) (V4i(0x9, 0xA, 0xB, 0xC)) (V4i(0xD, 0xE, 0xF, 0x0))

            edge |> should equal (V4i(0x04030201, 0x08070605, 0x0C0B0A09, 0x000F0E0D))

        [<Test>]
        let ``Generate Compact Face V4i``() =
            let face = generateCompactFaceV4i (V3i(0x2, 0x3, 0x4)) (V3i(0x6, 0x7, 0x8)) (V3i(0xA, 0xB, 0xC)) (V3i(0xE, 0xF, 0x0))

            face |> should equal (V4i(0x00040302, 0x00080706, 0x000C0B0A, 0x00000F0E))


        [<Test>]
        let ``Transform Edges To Compact Representation``() =

            let V = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i( 0, 1, 2, 3)
                            | 1 -> yield V4i( 0,-1, 1, 2)
                            | 2 -> yield V4i( 1,-1, 2, 0)
                            | 3 -> yield V4i( 2,-1, 3, 0)
                            | 4 -> yield V4i( 3,-1, 0, 2)
                            | _ -> yield V4i(-1)
                    |]

            let E = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i( 1, 2, 3, 4)
                            | 1 -> yield V4i(-1,-1, 2, 0)
                            | 2 -> yield V4i(-1,-1, 0, 1)
                            | 3 -> yield V4i(-1,-1, 4, 0)
                            | 4 -> yield V4i(-1,-1, 0, 3)
                            | _ -> yield V4i(-1)
                    |]

            let M = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V2i(1, 1)
                            | 1 -> yield V2i(0, 0)
                            | 2 -> yield V2i(1, 0)
                            | 3 -> yield V2i(1, 0)
                            | 4 -> yield V2i(0, 0)
                            | _ -> yield V2i(-1)
                    |]

            let (edges, meta) = transformEdgesToCompactRepresentation V E M
            
            let afterEdges = [|
                                for i in 0 .. MAX_EDGES_HALF - 1 do
                                    match i with
                                    | 0 -> yield V4i(0x03020100, 0x04030201, 0x0201FF00, 0x0002FFFF)
                                    | 1 -> yield V4i(0x0002FF01, 0x0100FFFF, 0x0003FF02, 0x0004FFFF)
                                    | 2 -> yield V4i(0x0200FF03, 0x0300FFFF, 0xFFFFFFFF, 0xFFFFFFFF)
                                    | _ -> yield V4i(0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF)
                            |]

            let afterMeta = 0x000000A3

            Assert.Multiple( fun _ ->
                edges |> should equal afterEdges
                meta |> should equal afterMeta
            )

        [<Test>]
        let ``Transform Faces To Compact Representation``() =
            
            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(0, 1, 2, 12)
                            | 1 -> yield V4i(0, 2, 3, 23)
                            | _ -> yield V4i(-1)
                    |]

            let FE = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i( 1, 2, 0)
                            | 1 -> yield V3i( 0, 3, 4)
                            | _ -> yield V3i(-1)
                    |]


            let faces = transformFacesToCompactRepresentation FV FE
            
            let afterFaces = [|
                                for i in 0 .. MAX_FACES_HALF - 1 do
                                    match i with
                                    | 0 -> yield V4i( 0x00020100, 0x00000201, 0x00030200, 0x00040300)
                                    | _ -> yield V4i(0x00FFFFFF, 0x00FFFFFF, 0x00FFFFFF, 0x00FFFFFF)
                            |]

            faces |> should equal afterFaces
