namespace Tests

module DelaunayDataMutation = 

open NUnit.Framework
open FsUnit
open NUnit.Framework.Constraints


    module FlipTestMockup =

        module Before = 

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

            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2))
                            | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3))
                            | _ -> yield V4i(-1)
                    |]

            let FE = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i( 1, 2, 0)
                            | 1 -> yield V3i( 0, 3, 4)
                            | _ -> yield V3i(-1)
                    |]


                                            
            let STACK = Arr<N<MAX_EDGES>, int>()

            let SP = -1
            
        module After = 

            let V = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i( 1, 2, 3, 0)
                            | 1 -> yield V4i( 0,-1, 1, 3)
                            | 2 -> yield V4i( 1,-1, 2, 3)
                            | 3 -> yield V4i( 2,-1, 3, 1)
                            | 4 -> yield V4i( 3,-1, 0, 1)
                            | _ -> yield V4i(-1)
                    |]

            let E = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i( 2, 3, 4, 1)
                            | 1 -> yield V4i(-1,-1, 0, 4)
                            | 2 -> yield V4i(-1,-1, 3, 0)
                            | 3 -> yield V4i(-1,-1, 0, 2)
                            | 4 -> yield V4i(-1,-1, 1, 0)
                            | _ -> yield V4i(-1)
                    |]

            let M = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V2i(1, 0)
                            | 1 -> yield V2i(0, 0)
                            | 2 -> yield V2i(1, 1)
                            | 3 -> yield V2i(1, 1)
                            | 4 -> yield V2i(0, 0)
                            | _ -> yield V2i(-1)
                    |]

            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(1, 2, 3, (IDHash 1 2 3))
                            | 1 -> yield V4i(3, 0, 1, (IDHash 3 0 1))
                            | _ -> yield V4i(-1)
                    |]

            let FE = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i( 2, 3, 0)
                            | 1 -> yield V3i( 4, 1, 0)
                            | _ -> yield V3i(-1)
                    |]

            let STACK = Arr<N<MAX_EDGES>, int>([|
                                                2
                                                3
                                            |])

            let SP = 1
        
    [<Test>]
    let ``Flip Edge``() = 

        let (edges, meta) = EffectApDelaunayDataHandling.transformEdgesToCompactRepresentation (FlipTestMockup.Before.V) (FlipTestMockup.Before.E) (FlipTestMockup.Before.M)
        let mutable meta = meta
        let faces = EffectApDelaunayDataHandling.transformFacesToCompactRepresentation (FlipTestMockup.Before.FV) (FlipTestMockup.Before.FE)
        let stack = FlipTestMockup.Before.STACK
        let mutable sp = FlipTestMockup.Before.SP

        let edges = Arr<N<MAX_EDGES_HALF>, V4i>(edges)
        let faces = Arr<N<MAX_FACES_HALF>, V4i>(faces)

        let (newMeta, newSp) = 0 |> flipEdge edges meta faces stack sp
        meta <- newMeta
        sp <- newSp


        let (afterEdges, afterMeta) = EffectApDelaunayDataHandling.transformEdgesToCompactRepresentation (FlipTestMockup.After.V) (FlipTestMockup.After.E) (FlipTestMockup.After.M)
        let afterFaces = EffectApDelaunayDataHandling.transformFacesToCompactRepresentation (FlipTestMockup.After.FV) (FlipTestMockup.After.FE)
        let afterStack = FlipTestMockup.After.STACK
        let afterSp = FlipTestMockup.After.SP

        let afterEdges = Arr<N<MAX_EDGES_HALF>, V4i>(afterEdges)
        let afterFaces = Arr<N<MAX_FACES_HALF>, V4i>(afterFaces)

        Assert.Multiple( fun _ ->
            edges |> should equal afterEdges
            meta  |> should equal afterMeta
            faces |> should equal afterFaces
            stack |> should equal afterStack
            sp    |> should equal afterSp
        )


    module FlipTestMockup2 =

        module Before = 

            let V = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i( 3, 2, 5,-1)
                            | 1 -> yield V4i( 2,-1, 0, 5)
                            | 2 -> yield V4i( 5, 3, 2, 0)
                            | 3 -> yield V4i( 5, 2, 0,-1)
                            | 5 -> yield V4i( 3,-1, 2, 5)
                            | _ -> yield V4i(-1)
                    |]

            let E = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i( 5, 2,-1,-1)
                            | 1 -> yield V4i(-1,-1, 3, 2)
                            | 2 -> yield V4i( 0, 5, 1, 3)
                            | 3 -> yield V4i( 2, 1,-1,-1)
                            | 5 -> yield V4i(-1,-1, 2, 0)
                            | _ -> yield V4i(-1)
                    |]

            let M = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V2i(1, 0)
                            | 1 -> yield V2i(1, 0)
                            | 2 -> yield V2i(1, 1)
                            | 3 -> yield V2i(1, 0)
                            | 5 -> yield V2i(0, 0)
                            | _ -> yield V2i(-1)
                    |]

            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(5, 2, 0, (IDHash 5 2 0))
                            | 1 -> yield V4i(3, 2, 5, (IDHash 3 2 5))
                            | _ -> yield V4i(-1)
                    |]

            let FE =[| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i( 2, 1, 3)
                            | 1 -> yield V3i( 5, 2, 0)
                            | _ -> yield V3i(-1)
                    |]


                                            
            let STACK = Arr<N<MAX_EDGES>, int>()

            let SP = -1
            
        module After = 


            let V = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i( 3, 0, 5,-1)
                            | 1 -> yield V4i( 2,-1, 0, 3)
                            | 2 -> yield V4i( 3, 2, 0, 5)
                            | 3 -> yield V4i( 5, 3, 0,-1)
                            | 5 -> yield V4i( 3,-1, 2, 0)
                            | _ -> yield V4i(-1)
                    |]

            let E = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i( 2, 3,-1,-1)
                            | 1 -> yield V4i(-1,-1, 2, 5)
                            | 2 -> yield V4i( 5, 1, 3, 0)
                            | 3 -> yield V4i( 0, 2,-1,-1)
                            | 5 -> yield V4i(-1,-1, 1, 2)
                            | _ -> yield V4i(-1)
                    |]

            let M = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V2i(1, 1)
                            | 1 -> yield V2i(1, 1)
                            | 2 -> yield V2i(1, 0)
                            | 3 -> yield V2i(1, 1)
                            | 5 -> yield V2i(0, 0)
                            | _ -> yield V2i(-1)
                    |]

            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(0, 5, 3, (IDHash 0 5 3))
                            | 1 -> yield V4i(3, 2, 0, (IDHash 3 2 0))
                            | _ -> yield V4i(-1)
                    |]

            let FE = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i( 3, 0, 2)
                            | 1 -> yield V3i( 5, 1, 2)
                            | _ -> yield V3i(-1)
                    |]


                                            
            let STACK = Arr<N<MAX_EDGES>, int>([|
                                                    0
                                                    1
                                                    3
                                                |])

            let SP = 2
        
    [<Test>]
    let ``Flip Edge 2``() = 

        let (edges, meta) = EffectApDelaunayDataHandling.transformEdgesToCompactRepresentation (FlipTestMockup2.Before.V) (FlipTestMockup2.Before.E) (FlipTestMockup2.Before.M)
        let mutable meta = meta
        let faces = EffectApDelaunayDataHandling.transformFacesToCompactRepresentation (FlipTestMockup2.Before.FV) (FlipTestMockup2.Before.FE)
        let stack = FlipTestMockup2.Before.STACK
        let mutable sp = FlipTestMockup2.Before.SP

        let edges = Arr<N<MAX_EDGES_HALF>, V4i>(edges)
        let faces = Arr<N<MAX_FACES_HALF>, V4i>(faces)


        let (newMeta, newSp) = 2 |> flipEdge edges meta faces stack sp
        meta <- newMeta
        sp <- newSp


        let (afterEdges, afterMeta) = EffectApDelaunayDataHandling.transformEdgesToCompactRepresentation (FlipTestMockup2.After.V) (FlipTestMockup2.After.E) (FlipTestMockup2.After.M)
        let afterFaces = EffectApDelaunayDataHandling.transformFacesToCompactRepresentation (FlipTestMockup2.After.FV) (FlipTestMockup2.After.FE)
        let afterStack = FlipTestMockup2.After.STACK
        let afterSp = FlipTestMockup2.After.SP

        let afterEdges = Arr<N<MAX_EDGES_HALF>, V4i>(afterEdges)
        let afterFaces = Arr<N<MAX_FACES_HALF>, V4i>(afterFaces)

        Assert.Multiple( fun _ ->
            edges |> should equal afterEdges
            meta  |> should equal afterMeta
            faces |> should equal afterFaces
            stack |> should equal afterStack
            sp    |> should equal afterSp
        )

    module InsertVertexMockup =

        module Before = 

            let V = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i( 0,-1, 1, 2)
                            | 1 -> yield V4i( 1,-1, 2, 0)
                            | 2 -> yield V4i( 2,-1, 0, 1)
                            | _ -> yield V4i(-1)
                    |]

            let E = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i(-1,-1, 1, 2)
                            | 1 -> yield V4i(-1,-1, 2, 0)
                            | 2 -> yield V4i(-1,-1, 0, 1)
                            | _ -> yield V4i(-1)
                    |]

            let M = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V2i(1, 0)
                            | 1 -> yield V2i(1, 0)
                            | 2 -> yield V2i(0, 0)
                            | _ -> yield V2i(-1)
                    |]

            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2))
                            | _ -> yield V4i(-1)
                    |]

            let FE = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i( 0, 1, 2)
                            | _ -> yield V3i(-1)
                    |]
                    
            let nextFreeFaceAddr = 1
            let nextFreeEdgeAddr = 3
            
        module After = 

            let V = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i( 0,-1, 1, 3)
                            | 1 -> yield V4i( 1,-1, 2, 3)
                            | 2 -> yield V4i( 2,-1, 0, 3)
                            | 3 -> yield V4i( 0, 1, 3, 2)
                            | 4 -> yield V4i( 1, 2, 3, 0)
                            | 5 -> yield V4i( 2, 0, 3, 1)
                            | _ -> yield V4i(-1)
                    |]

            let E = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i(-1,-1, 4, 3)
                            | 1 -> yield V4i(-1,-1, 5, 4)
                            | 2 -> yield V4i(-1,-1, 3, 5)
                            | 3 -> yield V4i( 0, 4, 5, 2)
                            | 4 -> yield V4i( 1, 5, 3, 0)
                            | 5 -> yield V4i( 2, 3, 4, 1)
                            | _ -> yield V4i(-1)
                    |]

            let M = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V2i(1, 1)
                            | 1 -> yield V2i(1, 1)
                            | 2 -> yield V2i(0, 0)
                            | 3 -> yield V2i(1, 0)
                            | 4 -> yield V2i(1, 0)
                            | 5 -> yield V2i(1, 0)
                            | _ -> yield V2i(-1)
                    |]

            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(-1)
                            | 1 -> yield V4i(0, 1, 3, (IDHash 0 1 3))
                            | 2 -> yield V4i(1, 2, 3, (IDHash 1 2 3))
                            | 3 -> yield V4i(2, 0, 3, (IDHash 2 0 3))
                            | _ -> yield V4i(-1)
                    |]

            let FE = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i( 0, 1, 2)
                            | 1 -> yield V3i( 0, 4, 3)
                            | 2 -> yield V3i( 1, 5, 4)
                            | 3 -> yield V3i( 2, 3, 5)
                            | _ -> yield V3i(-1)
                    |]

            let nextFreeFaceAddr = 4
            let nextFreeEdgeAddr = 6
        
    [<Test>]
    let ``Insert Vertex``() = 

            
        let (edges, meta) = EffectApDelaunayDataHandling.transformEdgesToCompactRepresentation (InsertVertexMockup.Before.V) (InsertVertexMockup.Before.E) (InsertVertexMockup.Before.M)
        let faces = EffectApDelaunayDataHandling.transformFacesToCompactRepresentation (InsertVertexMockup.Before.FV) (InsertVertexMockup.Before.FE)

        let edges = Arr<N<MAX_EDGES_HALF>, V4i>(edges)
        let faces = Arr<N<MAX_FACES_HALF>, V4i>(faces)

        let (meta, nextFreeEdgeAddr, nextFreeFaceAddr) = 3 |> insertVertexIntoFace edges meta faces (InsertVertexMockup.Before.nextFreeEdgeAddr) (InsertVertexMockup.Before.nextFreeFaceAddr) 0 

        let (afterEdges, afterMeta) = EffectApDelaunayDataHandling.transformEdgesToCompactRepresentation (InsertVertexMockup.After.V) (InsertVertexMockup.After.E) (InsertVertexMockup.After.M)
        let afterFaces = EffectApDelaunayDataHandling.transformFacesToCompactRepresentation (InsertVertexMockup.After.FV) (InsertVertexMockup.After.FE)

        let afterEdges = Arr<N<MAX_EDGES_HALF>, V4i>(afterEdges)
        let afterFaces = Arr<N<MAX_FACES_HALF>, V4i>(afterFaces)

        Assert.Multiple( fun _ ->
            edges |> should equal afterEdges
            meta  |> should equal afterMeta
            faces |> should equal afterFaces
            nextFreeEdgeAddr |> should equal (InsertVertexMockup.After.nextFreeEdgeAddr)
            nextFreeFaceAddr |> should equal (InsertVertexMockup.After.nextFreeFaceAddr)
        )
        
    module SplitInsideEdgeMockup =

        module Before = 

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
                            | 2 -> yield V2i(0, 0)
                            | 3 -> yield V2i(0, 0)
                            | 4 -> yield V2i(0, 0)
                            | _ -> yield V2i(-1)
                    |]

            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2))
                            | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3))
                            | _ -> yield V4i(-1)
                    |]

            let FE = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i( 1, 2, 0)
                            | 1 -> yield V3i( 0, 3, 4)
                            | _ -> yield V3i(-1)
                    |]
                    
            let nextFreeFaceAddr = 2
            let nextFreeEdgeAddr = 5

        module After = 

            let V = [| 
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
                    |]

            let E = [| 
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
                    |]

            let M = [| 
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
                    |]

            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(-1)
                            | 1 -> yield V4i(-1)
                            | 2 -> yield V4i(0, 1, 4, (IDHash 0 1 4))
                            | 3 -> yield V4i(1, 2, 4, (IDHash 1 2 4))
                            | 4 -> yield V4i(2, 3, 4, (IDHash 2 3 4))
                            | 5 -> yield V4i(3, 0, 4, (IDHash 3 0 4))
                            | _ -> yield V4i(-1)
                    |]

            let FE = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i( 1, 2, 0)
                            | 1 -> yield V3i( 0, 3, 4)
                            | 2 -> yield V3i( 1, 5, 0)
                            | 3 -> yield V3i( 2, 6, 5)
                            | 4 -> yield V3i( 3, 7, 6)
                            | 5 -> yield V3i( 4, 0, 7)
                            | _ -> yield V3i(-1)
                    |]
                    
            let nextFreeFaceAddr = 6
            let nextFreeEdgeAddr = 8
             
    [<Test>]
    let ``Split Inside Edge``() = 

        let (edges, meta) = EffectApDelaunayDataHandling.transformEdgesToCompactRepresentation (SplitInsideEdgeMockup.Before.V) (SplitInsideEdgeMockup.Before.E) (SplitInsideEdgeMockup.Before.M)
        let faces = EffectApDelaunayDataHandling.transformFacesToCompactRepresentation (SplitInsideEdgeMockup.Before.FV) (SplitInsideEdgeMockup.Before.FE)

        let edges = Arr<N<MAX_EDGES_HALF>, V4i>(edges)
        let faces = Arr<N<MAX_FACES_HALF>, V4i>(faces)

        let (meta, nextFreeEdgeAddr, nextFreeFaceAddr) = 4 |> spliteEdge edges meta faces (SplitInsideEdgeMockup.Before.nextFreeEdgeAddr) (SplitInsideEdgeMockup.Before.nextFreeFaceAddr) 0 

        let (afterEdges, afterMeta) = EffectApDelaunayDataHandling.transformEdgesToCompactRepresentation (SplitInsideEdgeMockup.After.V) (SplitInsideEdgeMockup.After.E) (SplitInsideEdgeMockup.After.M)
        let afterFaces = EffectApDelaunayDataHandling.transformFacesToCompactRepresentation (SplitInsideEdgeMockup.After.FV) (SplitInsideEdgeMockup.After.FE)

        let afterEdges = Arr<N<MAX_EDGES_HALF>, V4i>(afterEdges)
        let afterFaces = Arr<N<MAX_FACES_HALF>, V4i>(afterFaces)
            

        Assert.Multiple( fun _ ->
            edges |> should equal afterEdges
            meta  |> should equal afterMeta
            faces |> should equal afterFaces
            nextFreeEdgeAddr |> should equal (InsertVertexMockup.After.nextFreeEdgeAddr)
            nextFreeFaceAddr |> should equal (InsertVertexMockup.After.nextFreeFaceAddr)
        )
          

    module SplitBorderEdgeMockup1 =

        module Before = 

            let V = [| 
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
                    |]

            let E = [| 
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
                    |]

            let M = [| 
                        for i in 0 ..MAX_EDGES - 1 do
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
                    |]

            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2)) 
                            | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3)) 
                            | 2 -> yield V4i(0, 3, 4, (IDHash 0 3 4)) 
                            | 3 -> yield V4i(0, 4, 1, (IDHash 0 4 1)) 
                            | _ -> yield V4i(-1)
                    |]

            let FE = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i(4, 0, 5) 
                            | 1 -> yield V3i(5, 1, 6) 
                            | 2 -> yield V3i(6, 2, 7) 
                            | 3 -> yield V3i(7, 3, 4) 
                            | _ -> yield V3i(-1)
                    |]
                    
            let nextFreeFaceAddr = 4
            let nextFreeEdgeAddr = 8

        module After = 

            let V = [| 
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
                    |]

            let E = [| 
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
                    |]

            let M = [| 
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
                    |]

            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2)) 
                            | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3)) 
                            | 2 -> yield V4i(-1) 
                            | 3 -> yield V4i(0, 4, 1, (IDHash 0 4 1)) 
                            | 4 -> yield V4i(4, 0, 5, (IDHash 4 0 5)) 
                            | 5 -> yield V4i(0, 3, 5, (IDHash 0 3 5)) 
                            | _ -> yield V4i(-1)
                    |]

            let FE = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i(4, 0, 5) 
                            | 1 -> yield V3i(5, 1, 6) 
                            | 2 -> yield V3i(6, 2, 7) 
                            | 3 -> yield V3i(7, 3, 4) 
                            | 4 -> yield V3i(7, 10, 9) 
                            | 5 -> yield V3i(6, 2, 10) 
                            | _ -> yield V3i(-1)
                    |]
                    
            let nextFreeFaceAddr = 6
            let nextFreeEdgeAddr = 11
             
    [<Test>]
    let ``Split Border Edge 1``() = 

        //let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = 5 |> spliteEdge (SplitBorderEdgeMockup1.Before.V) (SplitBorderEdgeMockup1.Before.E) (SplitBorderEdgeMockup1.Before.M) (SplitBorderEdgeMockup1.Before.FV) (SplitBorderEdgeMockup1.Before.FE) (SplitBorderEdgeMockup1.Before.nextFreeEdgeAddr) (SplitBorderEdgeMockup1.Before.nextFreeFaceAddr) 2


        //Assert.Multiple( fun _ ->
        //    vertices         |> should equal (SplitBorderEdgeMockup1.After.V)
        //    edges            |> should equal (SplitBorderEdgeMockup1.After.E)
        //    meta             |> should equal (SplitBorderEdgeMockup1.After.M)
        //    faceVertices     |> should equal (SplitBorderEdgeMockup1.After.FV)
        //    faceEdges        |> should equal (SplitBorderEdgeMockup1.After.FE)
        //    nextFreeEdgeAddr |> should equal (SplitBorderEdgeMockup1.After.nextFreeEdgeAddr)
        //    nextFreeFaceAddr |> should equal (SplitBorderEdgeMockup1.After.nextFreeFaceAddr)
        //)     
        true |> should equal false

    module SplitBorderEdgeMockup2 =

        module Before = 

            let V = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i(0, -1, 1, 2) 
                            | 1 -> yield V4i(1, -1, 2, 0) 
                            | 2 -> yield V4i(2, -1, 3, 0) 
                            | 3 -> yield V4i(3, -1, 4, 0) 
                            | 4 -> yield V4i(4, -1, 0, 3) 
                            | 5 -> yield V4i(0, 1, 2, 3) 
                            | 6 -> yield V4i(0, 2, 3, 4) 
                            | _ -> yield V4i(-1)
                    |]

            let E = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i(-1, -1, 1, 5)
                            | 1 -> yield V4i(-1, -1, 5, 0)
                            | 2 -> yield V4i(-1, -1, 6, 5)
                            | 3 -> yield V4i(-1, -1, 4, 6)
                            | 4 -> yield V4i(-1, -1, 6, 3)
                            | 5 -> yield V4i(0, 1, 2, 6)
                            | 6 -> yield V4i(5, 2, 3, 4)
                            | _ -> yield V4i(-1)
                    |]

            let M = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V2i(0, 0) 
                            | 1 -> yield V2i(0, 0) 
                            | 2 -> yield V2i(0, 0) 
                            | 3 -> yield V2i(0, 0) 
                            | 4 -> yield V2i(0, 0) 
                            | 5 -> yield V2i(1, 0)
                            | 6 -> yield V2i(1, 0)
                            | _ -> yield V2i(-1) 
                    |]

            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2)) 
                            | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3)) 
                            | 2 -> yield V4i(0, 3, 4, (IDHash 0 3 4)) 
                            | _ -> yield V4i(-1)
                    |]

            let FE = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i(0, 1, 5) 
                            | 1 -> yield V3i(5, 2, 6) 
                            | 2 -> yield V3i(6, 3, 4) 
                            | _ -> yield V3i(-1)
                    |]
                    
            let nextFreeFaceAddr = 3
            let nextFreeEdgeAddr = 7

        module After = 

            let V = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i(0, -1, 1, 2) 
                            | 1 -> yield V4i(1, -1, 2, 0) 
                            | 2 -> yield V4i(2, -1, 3, 0) 
                            | 3 -> yield V4i(3, -1, 4, 5) 
                            | 4 -> yield V4i(4, -1, 5, 3) 
                            | 5 -> yield V4i(0, 1, 2, 3) 
                            | 6 -> yield V4i(0, 2, 3, 5) 
                            | 8 -> yield V4i(0, 3, 5, -1)
                            | 9 -> yield V4i(3, 4, 5, 0)
                            | _ -> yield V4i(-1)
                    |]

            let E = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i(-1, -1, 1, 5)
                            | 1 -> yield V4i(-1, -1, 5, 0)
                            | 2 -> yield V4i(-1, -1, 6, 5)
                            | 3 -> yield V4i(-1, -1, 4, 9)
                            | 4 -> yield V4i(-1, -1, 9, 3)
                            | 5 -> yield V4i(0, 1, 2, 6)
                            | 6 -> yield V4i(5, 2, 9, 8)
                            | 8 -> yield V4i(6, 9, -1, -1)
                            | 9 -> yield V4i(3, 4, 8, 6)
                            | _ -> yield V4i(-1)
                    |]

            let M = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V2i(0, 0) 
                            | 1 -> yield V2i(0, 0) 
                            | 2 -> yield V2i(0, 0) 
                            | 3 -> yield V2i(0, 0) 
                            | 4 -> yield V2i(0, 0) 
                            | 5 -> yield V2i(1, 0)
                            | 6 -> yield V2i(1, 0)
                            | 8 -> yield V2i(0, 0)
                            | 9 -> yield V2i(1, 0)
                            | _ -> yield V2i(-1) 
                    |]

            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2)) 
                            | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3)) 
                            | 2 -> yield V4i(-1) 
                            | 3 -> yield V4i(0, 3, 5, (IDHash 0 3 5))
                            | 4 -> yield V4i(3, 4, 5, (IDHash 3 4 5))
                            | _ -> yield V4i(-1)
                    |]

            let FE = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i(0, 1, 5) 
                            | 1 -> yield V3i(5, 2, 6) 
                            | 2 -> yield V3i(6, 3, 4) 
                            | 3 -> yield V3i(6, 9, 8)
                            | 4 -> yield V3i(3, 4, 9)
                            | _ -> yield V3i(-1)
                    |]
                    
            let nextFreeFaceAddr = 5
            let nextFreeEdgeAddr = 10
             
    [<Test>]
    let ``Split Border Edge 2``() = 

        //let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = 5 |> spliteEdge (SplitBorderEdgeMockup2.Before.V) (SplitBorderEdgeMockup2.Before.E) (SplitBorderEdgeMockup2.Before.M) (SplitBorderEdgeMockup2.Before.FV) (SplitBorderEdgeMockup2.Before.FE) (SplitBorderEdgeMockup2.Before.nextFreeEdgeAddr) (SplitBorderEdgeMockup2.Before.nextFreeFaceAddr) 4


        //Assert.Multiple( fun _ ->
        //    vertices         |> should equal (SplitBorderEdgeMockup2.After.V)
        //    edges            |> should equal (SplitBorderEdgeMockup2.After.E)
        //    meta             |> should equal (SplitBorderEdgeMockup2.After.M)
        //    faceVertices     |> should equal (SplitBorderEdgeMockup2.After.FV)
        //    faceEdges        |> should equal (SplitBorderEdgeMockup2.After.FE)
        //    nextFreeEdgeAddr |> should equal (SplitBorderEdgeMockup2.After.nextFreeEdgeAddr)
        //    nextFreeFaceAddr |> should equal (SplitBorderEdgeMockup2.After.nextFreeFaceAddr)
        //)   
        true |> should equal false

    module SplitBorderEdgeMockup3 =

        module Before = 

            let V = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i(0, -1, 1, 2) 
                            | 1 -> yield V4i(1, -1, 2, 0) 
                            | 2 -> yield V4i(2, -1, 3, 0) 
                            | 3 -> yield V4i(3, -1, 4, 0) 
                            | 4 -> yield V4i(4, -1, 0, 3) 
                            | 5 -> yield V4i(0, 1, 2, 3) 
                            | 6 -> yield V4i(0, 2, 3, 4) 
                            | _ -> yield V4i(-1)
                    |]

            let E = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i(-1, -1, 1, 5)
                            | 1 -> yield V4i(-1, -1, 5, 0)
                            | 2 -> yield V4i(-1, -1, 6, 5)
                            | 3 -> yield V4i(-1, -1, 4, 6)
                            | 4 -> yield V4i(-1, -1, 6, 3)
                            | 5 -> yield V4i(0, 1, 2, 6)
                            | 6 -> yield V4i(5, 2, 3, 4)
                            | _ -> yield V4i(-1)
                    |]

            let M = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V2i(0, 0) 
                            | 1 -> yield V2i(0, 0) 
                            | 2 -> yield V2i(0, 0) 
                            | 3 -> yield V2i(0, 0) 
                            | 4 -> yield V2i(0, 0) 
                            | 5 -> yield V2i(1, 0)
                            | 6 -> yield V2i(1, 0)
                            | _ -> yield V2i(-1) 
                    |]

            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2)) 
                            | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3)) 
                            | 2 -> yield V4i(0, 3, 4, (IDHash 0 3 4)) 
                            | _ -> yield V4i(-1)
                    |]

            let FE = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i(0, 1, 5) 
                            | 1 -> yield V3i(5, 2, 6) 
                            | 2 -> yield V3i(6, 3, 4) 
                            | _ -> yield V3i(-1)
                    |]
                    
            let nextFreeFaceAddr = 3
            let nextFreeEdgeAddr = 7

        module After = 

            let V = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i(0, -1, 5, 2) 
                            | 1 -> yield V4i(1, -1, 2, 5) 
                            | 2 -> yield V4i(2, -1, 3, 0) 
                            | 3 -> yield V4i(3, -1, 4, 0) 
                            | 4 -> yield V4i(4, -1, 0, 3) 
                            | 5 -> yield V4i(0, 5, 2, 3) 
                            | 6 -> yield V4i(0, 2, 3, 4) 
                            | 8 -> yield V4i(1, 2, 5, -1)
                            | 9 -> yield V4i(2, 0, 5, 1)
                            | _ -> yield V4i(-1)
                    |]

            let E = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V4i(-1, -1, 9, 5)
                            | 1 -> yield V4i(-1, -1, 9, 8)
                            | 2 -> yield V4i(-1, -1, 6, 5)
                            | 3 -> yield V4i(-1, -1, 4, 6)
                            | 4 -> yield V4i(-1, -1, 6, 3)
                            | 5 -> yield V4i(0, 9, 2, 6)
                            | 6 -> yield V4i(5, 2, 3, 4)
                            | 8 -> yield V4i(1, 9, -1, -1)
                            | 9 -> yield V4i(5, 0, 8, 1)
                            | _ -> yield V4i(-1)
                    |]

            let M = [| 
                        for i in 0 .. MAX_EDGES - 1 do
                            match i with
                            | 0 -> yield V2i(0, 0) 
                            | 1 -> yield V2i(0, 0) 
                            | 2 -> yield V2i(0, 0) 
                            | 3 -> yield V2i(0, 0) 
                            | 4 -> yield V2i(0, 0) 
                            | 5 -> yield V2i(1, 0)
                            | 6 -> yield V2i(1, 0)
                            | 8 -> yield V2i(0, 0)
                            | 9 -> yield V2i(1, 0)
                            | _ -> yield V2i(-1) 
                    |]

            let FV = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V4i(-1) 
                            | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3)) 
                            | 2 -> yield V4i(0, 3, 4, (IDHash 0 3 4)) 
                            | 3 -> yield V4i(1, 2, 5, (IDHash 1 2 5))
                            | 4 -> yield V4i(2, 0, 5, (IDHash 2 0 5))
                            | _ -> yield V4i(-1)
                    |]

            let FE = [| 
                        for i in 0 .. MAX_FACES - 1 do
                            match i with
                            | 0 -> yield V3i(0, 1, 5) 
                            | 1 -> yield V3i(5, 2, 6) 
                            | 2 -> yield V3i(6, 3, 4) 
                            | 3 -> yield V3i(1, 9, 8)
                            | 4 -> yield V3i(5, 0, 9)
                            | _ -> yield V3i(-1)
                    |]
                    
            let nextFreeFaceAddr = 5
            let nextFreeEdgeAddr = 10
             
    [<Test>]
    let ``Split Border Edge 3``() = 

        //let (vertices, edges, meta, faceVertices, faceEdges, nextFreeEdgeAddr, nextFreeFaceAddr) = 5 |> spliteEdge (SplitBorderEdgeMockup3.Before.V) (SplitBorderEdgeMockup3.Before.E) (SplitBorderEdgeMockup3.Before.M) (SplitBorderEdgeMockup3.Before.FV) (SplitBorderEdgeMockup3.Before.FE) (SplitBorderEdgeMockup3.Before.nextFreeEdgeAddr) (SplitBorderEdgeMockup3.Before.nextFreeFaceAddr) 0


        //Assert.Multiple( fun _ ->
        //    vertices         |> should equal (SplitBorderEdgeMockup3.After.V)
        //    edges            |> should equal (SplitBorderEdgeMockup3.After.E)
        //    meta             |> should equal (SplitBorderEdgeMockup3.After.M)
        //    faceVertices     |> should equal (SplitBorderEdgeMockup3.After.FV)
        //    faceEdges        |> should equal (SplitBorderEdgeMockup3.After.FE)
        //    nextFreeEdgeAddr |> should equal (SplitBorderEdgeMockup3.After.nextFreeEdgeAddr)
        //    nextFreeFaceAddr |> should equal (SplitBorderEdgeMockup3.After.nextFreeFaceAddr)
        //)  
        true |> should equal false

        
    module TriangulationSetup = 
        let (EDGES, META, _) = Render.EffectApDelaunayGenInitTriangulation.genInitTriangulation 4

        let internal getInitEdgeData (caseOffset : int) = 
            let edgeArray = Arr<N<MAX_EDGES_HALF>, V4i>() 
            for i in 0 .. MAX_EDGES_HALF - 1 do
                edgeArray.[i] <- EDGES.[MAX_EDGES_HALF * caseOffset + i]
            edgeArray
                
            
        let internal getInitMetaData (caseOffset : int) = META.[caseOffset]

        let internal getData caseOffset =
            (getInitEdgeData caseOffset, getInitMetaData caseOffset, caseOffset)

        let getDataCaseCorner ()  = CASE_CORNER_OFFSET |> getData
        let getDataCaseEdge ()    = CASE_EDGE_OFFSET |> getData
        let getDataCaseInside ()  = CASE_INSIDE_OFFSET |> getData

        let genVertices (v : V4d list) = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_THREE>, V4d>(v)

    type Range = Within of float * float
    let (+/-) (a:float) b = Within(a, b)

    let equal x = 
        match box x with 
        | :? Range as r ->
            let (Within(x, within)) = r
            (new EqualConstraint(x)).Within(within)
        | _ ->
        new EqualConstraint(x)

    [<Test>]
    let ``Compute Illumination Case Corner``() =
        
        let (edges, meta, offset) = TriangulationSetup.getDataCaseCorner ()

        let vertices = 
            [
                V4d(V3d(3, -1, 1) |> Vec.normalize, 1.0)
                V4d(V3d(3, -1, 3) |> Vec.normalize, 1.0)
                V4d(V3d(3,  1, 3) |> Vec.normalize, 1.0)
                V4d(V3d(3,  1, 1) |> Vec.normalize, 1.0)
            ]
            |> TriangulationSetup.genVertices


        let expected =
            let area1 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[1].XYZ) (vertices.[2].XYZ)
            let area2 = computeSphericalExcess (vertices.[2].XYZ) (vertices.[3].XYZ) (vertices.[0].XYZ)
                
            area1 + area2

        let L = computeIlluminationCheap edges vertices offset

        L |> should equal (expected +/- 1e-5)

        
    [<Test>]
    let ``Compute Illumination Case Edge``() =
        
        let (edges, meta, offset) = TriangulationSetup.getDataCaseEdge ()

        let vertices = 
            [
                V4d(V3d(3,  0, 1) |> Vec.normalize, 1.0)
                V4d(V3d(3, -1, 1) |> Vec.normalize, 1.0)
                V4d(V3d(3, -1, 3) |> Vec.normalize, 1.0)
                V4d(V3d(3,  1, 3) |> Vec.normalize, 1.0)
                V4d(V3d(3,  1, 1) |> Vec.normalize, 1.0)

            ]
            |> TriangulationSetup.genVertices


        let expected =
            let area1 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[1].XYZ) (vertices.[2].XYZ)
            let area2 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[2].XYZ) (vertices.[3].XYZ)
            let area3 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[3].XYZ) (vertices.[4].XYZ)
                
            area1 + area2 + area3

        let L = computeIlluminationCheap edges vertices offset

        L |> should equal (expected +/- 1e-5)

    [<Test>]
    let ``Compute Illumination Case Inside``() =
        
        let (edges, meta, offset) = TriangulationSetup.getDataCaseInside ()

        let vertices = 
            [
                V4d(V3d(3,  0, 2) |> Vec.normalize, 1.0)
                V4d(V3d(3, -1, 1) |> Vec.normalize, 1.0)
                V4d(V3d(3, -1, 3) |> Vec.normalize, 1.0)
                V4d(V3d(3,  1, 3) |> Vec.normalize, 1.0)
                V4d(V3d(3,  1, 1) |> Vec.normalize, 1.0)
            ]
            |> TriangulationSetup.genVertices


        let expected =
            let area1 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[1].XYZ) (vertices.[2].XYZ)
            let area2 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[2].XYZ) (vertices.[3].XYZ)
            let area3 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[3].XYZ) (vertices.[4].XYZ)
            let area4 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[4].XYZ) (vertices.[1].XYZ)
                
            area1 + area2 + area3 + area4

        let L = computeIlluminationCheap edges vertices offset

        L |> should equal (expected +/- 1e-5)   
            
    module FillVertexArrayMock =

        let getTransforms n = 
            let t2w = n |> basisFrisvad 
            let w2t = t2w |> Mat.transpose
            (t2w, w2t)

        let getClippedData w2t p =

            let v = [
                        V3d(0, -1, 1)
                        V3d(0, -1, 3)
                        V3d(0,  1, 3)
                        V3d(0,  1, 1)
                    ]
                    |> List.map (fun v -> (v - p) )

            let v = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_ONE>, V3d>([
                            
                    ])

    let ``Fill Vertex Array`` () =

        let v = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_ONE>, V3d>

        let (vertices, caseOffset, offset) = fillVertexArray

