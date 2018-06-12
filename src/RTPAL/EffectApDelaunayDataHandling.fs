namespace Render


module DataHandling = 
    open Aardvark.Base
    open FShade

    open Config.Delaunay
        
    let NONE = 0x000000FF

    let private ID_BIT_MASK = uint32 0x000000FF 
        
    let private BIT_PER_ID = 8

    [<ReflectedDefinition>][<Inline>]
    let private getIdFromInt pos value =
        ((uint32 value &&& (ID_BIT_MASK <<< pos * BIT_PER_ID)) >>> (pos * BIT_PER_ID)) |> int

    [<ReflectedDefinition>][<Inline>]
    let private setIdInInt pos value id =
        ((uint32 value &&& (~~~(ID_BIT_MASK <<< pos * BIT_PER_ID))) ||| (uint32 id <<< pos * BIT_PER_ID)) |> int

    [<ReflectedDefinition>][<Inline>]
    let private setTwoIdsInInt pos value id0 id1 =
        ((uint32 value &&& (~~~((ID_BIT_MASK <<< (pos + 1) * BIT_PER_ID) ||| (ID_BIT_MASK <<< pos * BIT_PER_ID)))) ||| ((uint32 id1 <<< (pos + 1) * BIT_PER_ID) ||| (uint32 id0 <<< pos * BIT_PER_ID))) |> int

    [<ReflectedDefinition>][<Inline>]
    let private offsetId offset id = 
        id <<< (BIT_PER_ID * offset)

    [<ReflectedDefinition>][<Inline>]
    let genIntFromV3i (v : V3i) =
        (uint32 (v.X |> offsetId 0) ||| uint32 (v.Y |> offsetId 1) ||| uint32 (v.Z |> offsetId 2)) |> int

    [<ReflectedDefinition>][<Inline>]
    let genIntFromV4i (v : V4i) =
        (v.X |> offsetId 0) ||| (v.Y |> offsetId 1) ||| (v.Z |> offsetId 2) ||| (v.W |> offsetId 3)

    [<ReflectedDefinition>][<Inline>]
    let private leftShiftWithRotation24Bit shift v =
        int (((uint32 v <<< BIT_PER_ID * shift) ||| (uint32 v >>> BIT_PER_ID * (3 - shift))) &&& uint32 0x00FFFFFF)

    [<ReflectedDefinition>][<Inline>]
    let private leftShiftWithRotation32Bit shift v =
        int ((uint32 v <<< BIT_PER_ID * shift) ||| (uint32 v >>> BIT_PER_ID * (4 - shift)))

    //////////////////////////////////////////////////////////////////////////////////////////
    // Edge 

    let OPPOSITE_0 = 0
    let OPPOSITE_1 = 2

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
        (uint32 meta ||| (META_BIT_MASK <<< ((eId * 2) + INSIDE_OFFSET))) |> int
            
    [<ReflectedDefinition>][<Inline>]
    let edgeIsMarked meta eId = 
        uint32 meta &&& (META_BIT_MASK <<< ((eId * 2) + MARKED_OFFSET)) <> uint32 0 

    // TODO maybe replace meta with a meta ref, check shader code
    [<ReflectedDefinition>][<Inline>]
    let markEdge meta eId =
        (uint32 meta ||| (META_BIT_MASK <<< ((eId * 2) + MARKED_OFFSET))) |> int

    // TODO maybe replace meta with a meta ref, check shader code
    [<ReflectedDefinition>][<Inline>]
    let unmarkEdge meta eId =
        (uint32 meta &&& (~~~(META_BIT_MASK <<< ((eId * 2) + MARKED_OFFSET)))) |> int


    //////////////////////////////////////////////////////////////////////////////////////////
    // Face 

    // Vertex Handling

    [<ReflectedDefinition>][<Inline>]
    let readFaceVertexId (faces : Arr<N<MAX_FACES_HALF>, V4i>) fId vPos =
        getIdFromInt vPos (if fId % 2 = 0 then faces.[fId / 2].X else faces.[fId / 2].Z)

    [<ReflectedDefinition>][<Inline>]
    let writeFaceVertexIdsCombined (faces : Arr<N<MAX_FACES_HALF>, V4i>) fId vertices =
        faces.[fId / 2] <-
            if fId % 2 = 0 then 
                V4i(vertices, faces.[fId / 2].Y, faces.[fId / 2].Z, faces.[fId / 2].W)
            else 
                V4i(faces.[fId / 2].X, faces.[fId / 2].Y, vertices, faces.[fId / 2].W)

    [<ReflectedDefinition>][<Inline>]
    let writeFaceVertexIds (faces : Arr<N<MAX_FACES_HALF>, V4i>) fId v0 v1 v2 =
        writeFaceVertexIdsCombined faces fId ((uint32 (v0 |> offsetId 0) ||| uint32 (v1 |> offsetId 1) ||| uint32 (v2 |> offsetId 2)) |> int)

    // Neighbor Edge Handling

    [<ReflectedDefinition>][<Inline>]
    let readFaceEdgeId (faces : Arr<N<MAX_FACES_HALF>, V4i>) fId ePos =
        getIdFromInt ePos (if fId % 2 = 0 then faces.[fId / 2].Y else faces.[fId / 2].W)

    [<ReflectedDefinition>][<Inline>]
    let writeFaceEdgeIds (faces : Arr<N<MAX_FACES_HALF>, V4i>) fId e0 e1 e2 =
        faces.[fId / 2] <- 
            if fId % 2 = 0 then
                V4i(faces.[fId / 2].X, (e0 |> offsetId 0) ||| (e1 |> offsetId 1) ||| (e2 |> offsetId 2), faces.[fId / 2].Z, faces.[fId / 2].W)
            else
                V4i(faces.[fId / 2].X, faces.[fId / 2].Y, faces.[fId / 2].Z, (e0 |> offsetId 0) ||| (e1 |> offsetId 1) ||| (e2 |> offsetId 2))

    //////////////////////////////////////////////////////////////////////////////////////////
    // Misc Handling

    [<ReflectedDefinition>][<Inline>]
    let private compareVerticesPermutations24Bit v0 v1 =
        v0 = v1 || (v0 |> leftShiftWithRotation24Bit 1) = v1 || (v0 |> leftShiftWithRotation24Bit 2) = v1

    [<ReflectedDefinition>][<Inline>]
    let verticesAreFromFace (faces : Arr<N<MAX_FACES_HALF>, V4i>) fId vertices =
        compareVerticesPermutations24Bit (if fId % 2 = 0 then  faces.[fId / 2].X else faces.[fId / 2].Z) vertices

    [<ReflectedDefinition>][<Inline>]
    let leftShiftVerticesAndEdgesByOne (edges : Arr<N<MAX_EDGES_HALF>, V4i>) eId =
        edges.[eId / 2] <-
            if eId % 2 = 0 then 
                V4i(edges.[eId / 2].X |> leftShiftWithRotation32Bit 1, edges.[eId / 2].Y |> leftShiftWithRotation32Bit 1, edges.[eId / 2].Z, edges.[eId / 2].W)
            else 
                V4i(edges.[eId / 2].X, edges.[eId / 2].Y, edges.[eId / 2].Z |> leftShiftWithRotation32Bit 1, edges.[eId / 2].W |> leftShiftWithRotation32Bit 1)


    [<ReflectedDefinition>][<Inline>]
    let writeOppositeEdgeIds (edges : Arr<N<MAX_EDGES_HALF>, V4i>) eId opposite edgeToId edgeFromId =
        edges.[eId / 2] <-
            if eId % 2 = 0 then 
                V4i(edges.[eId / 2].X, setTwoIdsInInt opposite (edges.[eId / 2].Y) edgeToId edgeFromId, edges.[eId / 2].Z, edges.[eId / 2].W)
            else 
                V4i(edges.[eId / 2].X, edges.[eId / 2].Y, edges.[eId / 2].Z, setTwoIdsInInt opposite (edges.[eId / 2].Y) edgeToId edgeFromId)

    [<ReflectedDefinition>][<Inline>]
    let compareIndices (edges : Arr<N<MAX_EDGES_HALF>, V4i>) e0 pos0 e1 pos1 = 
        getIdFromInt (if e0 % 2 = 0 then edges.[e0 / 2].X else edges.[e0 / 2].Z) pos0 = getIdFromInt (if e1 % 2 = 0 then edges.[e1 / 2].X else edges.[e1 / 2].Z) pos1

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
        V4i(v0 |> genIntFromV4i, e0 |> genIntFromV4i, v1 |> genIntFromV4i, e1 |> genIntFromV4i) 


    let generateCompactFaceV4i (fv0 : V3i) (fe0 : V3i) (fv1 : V3i) (fe1 : V3i) =
        V4i(fv0 |> genIntFromV3i, fe0 |> genIntFromV3i, fv1 |> genIntFromV3i, fe1 |> genIntFromV3i) 

    let private  replaceMinusOneWithNone v = 
        if v = -1 then NONE else v

    let private replaceMinusOneWithNoneInV4i (v : V4i) =
        V4i(v.X |> replaceMinusOneWithNone, v.Y |> replaceMinusOneWithNone, v.Z |> replaceMinusOneWithNone, v.W |> replaceMinusOneWithNone)

    let private replaceMinusOneWithNoneInV3i (v : V3i) =
        V3i(v.X |> replaceMinusOneWithNone, v.Y |> replaceMinusOneWithNone, v.Z |> replaceMinusOneWithNone)

    let transformEdgesToCompactRepresentation (V :  Arr<N<MAX_EDGES>, V4i>) (E :  Arr<N<MAX_EDGES>, V4i>) (M :  Arr<N<MAX_EDGES>, V2i>) = 
            
        let edges = Arr<N<MAX_EDGES_HALF>, V4i>([|
                                                    for i in 0 .. 2 .. MAX_EDGES - 1 do
                                                        yield generateCompactEdgeV4i (V.[i] |> replaceMinusOneWithNoneInV4i) (E.[i] |> replaceMinusOneWithNoneInV4i) (V.[i + 1] |> replaceMinusOneWithNoneInV4i) (E.[i + 1] |> replaceMinusOneWithNoneInV4i)
            
                                                |])
            
        let mutable meta = 0x00000000

        for i in 0 .. MAX_EDGES - 1 do

            if M.[i].X = 1 then
                meta <- i |> makeEdgeInside meta

            if M.[i].Y = 1 then
                meta <- i |> markEdge meta

        (edges, meta)

    let transformFacesToCompactRepresentation (FV : Arr<N<MAX_FACES>, V4i>) (FE : Arr<N<MAX_FACES>, V3i>) =

        Arr<N<MAX_FACES_HALF>, V4i>([|
                                    for i in 0 .. 2 .. MAX_FACES - 1 do
                                        yield generateCompactFaceV4i (FV.[i].XYZ |> replaceMinusOneWithNoneInV3i) (FE.[i] |> replaceMinusOneWithNoneInV3i) (FV.[i + 1].XYZ |> replaceMinusOneWithNoneInV3i) (FE.[i + 1] |> replaceMinusOneWithNoneInV3i)
            
                                |])


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

        let private getMockupEdgeArray () =
            Arr<N<MAX_EDGES_HALF>, V4i>([|
                                        yield V4i(V4i(0, 1, 2, 3) |> genIntFromV4i, V4i(00, 11, 22, 33) |> genIntFromV4i, V4i(4, 5, 6, 7) |> genIntFromV4i, V4i(44, 55, 66, 77) |> genIntFromV4i) 
                                        yield V4i(V4i(8, 9, 10, 11) |> genIntFromV4i, V4i(88, 99, 100, 111) |> genIntFromV4i, V4i(12, 13, 14, 15) |> genIntFromV4i, V4i(122, 133, 144, 155) |> genIntFromV4i) 
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
            Arr<N<MAX_EDGES_HALF>, V4i>([|
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
            leftShiftVerticesAndEdgesByOne edges 3

            Assert.Multiple( fun _ ->
                edges.[2].XY |> should equal (V2i(V4i(9, 10, 11, 8) |> genIntFromV4i, V4i(99, 100, 111, 88) |> genIntFromV4i))
                edges.[2].ZW |> should equal (V2i(V4i(13, 14, 15, 12) |> genIntFromV4i, V4i(133, 144, 155, 122) |> genIntFromV4i))
            )