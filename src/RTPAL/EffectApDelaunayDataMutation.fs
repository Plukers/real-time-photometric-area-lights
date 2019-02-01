namespace Render

module EffectApDelaunayDataMutation = 

    open Aardvark.Base
    open FShade

    open EffectApDelaunayDataHandling
    open Config.Delaunay
    open EffectUtils


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

    [<ReflectedDefinition>][<Inline>]
    let private IDHash' a b c = (a + 1) * (b + 1) * (c + 1)

    [<ReflectedDefinition>][<Inline>]
    let getV4iValueByIdx (v : V4i) i =
        match i with
        | 0 -> v.X
        | 1 -> v.Y
        | 2 -> v.Z
        | _ -> v.W
        
    [<ReflectedDefinition>][<Inline>]
    let setV4iValueByIdx (v : V4i) i value =
        match i with
        | 0 -> V4i(value, v.Y, v.Z, v.W)
        | 1 -> V4i(v.X, value, v.Z, v.W)
        | 2 -> V4i(v.X, v.Y, value, v.W)
        | _ -> V4i(v.X, v.Y, v.Z, value)

    
    [<ReflectedDefinition>][<Inline>]
    let flipEdge (edges : Arr<N<MAX_EDGES_HALF>, V4i>) (meta : int) (faces : Arr<N<MAX_FACES_HALF>, V4i>) (stack : Arr<N<MAX_EDGES>, int>) (sp : int) eId =              
             
        let mutable sp = sp
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

        (meta, sp)


    [<ReflectedDefinition>][<Inline>]
    let cheapFlipEdge (edges : Arr<N<MAX_EDGES_HALF>, V4i>) (meta : int) (stack : Arr<N<MAX_EDGES>, int>) (sp : int) eId =              
             
        let mutable sp = sp
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
                    
        // flip edge and unmark edge
        eId |> leftShiftVerticesAndEdgesByOne edges
        meta <- eId |> unmarkEdge meta
        
        (meta, sp)
            

    // Inserts vertex with Id = vId into face with faceId. FaceId is position in Array, not faceHash
    [<ReflectedDefinition>][<Inline>]
    let insertVertexIntoFace (edges : Arr<N<MAX_EDGES_HALF>, V4i>) (meta : int) (faces : Arr<N<MAX_FACES_HALF>, V4i>) nextFreeEdgeAddr nextFreeFaceAddr faceId vId =
        
        let mutable meta = meta
        let mutable nextFreeFaceAddr = nextFreeFaceAddr

        for e in 0 .. 2 do
            let newEdgeId = nextFreeEdgeAddr + e

            // insert new edge
            writeAllVertexIds edges newEdgeId (readFaceVertexId faces faceId ((e + 0) % 3)) (readFaceVertexId faces faceId ((e + 1) % 3)) (vId) (readFaceVertexId faces faceId ((e + 2) % 3))
            writeAllEdgeIds edges newEdgeId (readFaceEdgeId faces faceId ((e + 0) % 3)) (nextFreeEdgeAddr + (e + 1) % 3) (nextFreeEdgeAddr + (e + 2) % 3) (readFaceEdgeId faces faceId ((e + 2) % 3))
            meta <- makeEdgeInside meta newEdgeId
            
            // update outside edge
            let currentOpposite = readFaceVertexId faces faceId ((e + 2) % 3)
            let updateEdgeId = readFaceEdgeId faces faceId e
            
            if readVertexId edges updateEdgeId OPPOSITE_0 = currentOpposite then
                writeVertexId edges updateEdgeId OPPOSITE_0 vId
                writeOppositeEdgeIds edges updateEdgeId OPPOSITE_0 (nextFreeEdgeAddr + (e + 1) % 3) newEdgeId
            else
                writeVertexId edges updateEdgeId OPPOSITE_1 vId
                writeOppositeEdgeIds edges updateEdgeId OPPOSITE_1 (nextFreeEdgeAddr + (e + 1) % 3) newEdgeId
                
            // flip new edge
            if edgeIsInside meta updateEdgeId then
                meta <- markEdge meta updateEdgeId
                
            // insert new face
            writeFaceVertexIds faces nextFreeFaceAddr (readVertexId edges newEdgeId 0) (readVertexId edges newEdgeId 1) (readVertexId edges newEdgeId 2) 
            writeFaceEdgeIds faces nextFreeFaceAddr (readEdgeId edges newEdgeId 0) (readEdgeId edges newEdgeId 1) (newEdgeId)
            nextFreeFaceAddr <- nextFreeFaceAddr + 1

       
        // remove face where vertex was inserted
        clearFaceVertices faces faceId

        (meta, nextFreeEdgeAddr + 3, nextFreeFaceAddr)


    [<ReflectedDefinition>]
    let private mapLocalToGlobalId freedAddr nextFreeEdgeAddr edgeLocalId =
        match edgeLocalId with
        | 0 -> freedAddr
        | _ -> nextFreeEdgeAddr + edgeLocalId - 1
       
    // Splits edgy edgeId by inseting vertex with Id = vId 
    [<ReflectedDefinition>]
    let spliteEdge (edges : Arr<N<MAX_EDGES_HALF>, V4i>) (meta : int) (faces : Arr<N<MAX_FACES_HALF>, V4i>) nextFreeEdgeAddr nextFreeFaceAddr edgeId vId =
        
        //let fhash1 = IDHash (vertices.[edgeId].X) (vertices.[edgeId].Y) (vertices.[edgeId].Z)
        //let fhash2 = IDHash (vertices.[edgeId].Z) (vertices.[edgeId].W) (vertices.[edgeId].X)
            
        //let splitEdgeV = Arr<N<4>, int>([| vertices.[edgeId].X; vertices.[edgeId].Y; vertices.[edgeId].Z; vertices.[edgeId].W |])
        //let splitEdgeE = Arr<N<4>, int>([| edges.[edgeId].X; edges.[edgeId].Y; edges.[edgeId].Z; edges.[edgeId].W |])


        //let mutable nextFreeFaceAddrDynamic = nextFreeFaceAddr

        //for eIdx in 0 .. 3 do
        //    if splitEdgeV.[eIdx] <> -1 then
                    
                    
                    
        //        let thisGlobalId = eIdx |> mapLocalToGlobalId edgeId nextFreeEdgeAddr


        //        ///////////////////////////////////////////////////
        //        // Insert new Edge

        //        vertices.[thisGlobalId] <- V4i(splitEdgeV.[eIdx], splitEdgeV.[(eIdx + 1) % 4], vId, splitEdgeV.[(eIdx + 3) % 4])

        //        let mutable eO1IsEmpty = false
        //        let mutable eO2IsEmpty = false

        //        let eO1 =
        //            if splitEdgeV.[(eIdx + 1) % 4] = -1 then
        //                eO1IsEmpty <- true
        //                V2i(-1, -1)
        //            else
        //                V2i(splitEdgeE.[eIdx], ((eIdx + 1) % 4) |> mapLocalToGlobalId edgeId nextFreeEdgeAddr)

        //        let eO2 =
        //            if splitEdgeV.[(eIdx + 3) % 4] = -1 then
        //                eO2IsEmpty <- true
        //                V2i(-1, -1)
        //            else
        //                V2i(((eIdx + 3) % 4) |> mapLocalToGlobalId edgeId nextFreeEdgeAddr, splitEdgeE.[(eIdx + 3) % 4])

        //        edges.[thisGlobalId] <- V4i(eO1.X, eO1.Y, eO2.X, eO2.Y)
                    
        //        meta.[thisGlobalId] <- if eO1IsEmpty || eO2IsEmpty then V2i(0, 0) else V2i(1, 0)
                    


        //        if not eO1IsEmpty then

        //            ///////////////////////////////////////////////////
        //            // Update Existing Edge

        //            let currentOppositeOffset = if eIdx = 1 || eIdx = 3 then 3 else 2

        //            if vertices.[splitEdgeE.[eIdx]].Y <> -1 && vertices.[splitEdgeE.[eIdx]].Y = splitEdgeV.[(eIdx + currentOppositeOffset) % 4] then
        //                vertices.[splitEdgeE.[eIdx]] <- V4i(vertices.[splitEdgeE.[eIdx]].X, vId, vertices.[splitEdgeE.[eIdx]].Z, vertices.[splitEdgeE.[eIdx]].W)
        //                edges.[splitEdgeE.[eIdx]] <- V4i(((eIdx + 1) % 4) |> mapLocalToGlobalId edgeId nextFreeEdgeAddr, thisGlobalId, edges.[splitEdgeE.[eIdx]].Z, edges.[splitEdgeE.[eIdx]].W)
        //            else
        //                vertices.[splitEdgeE.[eIdx]] <- V4i(vertices.[splitEdgeE.[eIdx]].X, vertices.[splitEdgeE.[eIdx]].Y, vertices.[splitEdgeE.[eIdx]].Z, vId)
        //                edges.[splitEdgeE.[eIdx]] <- V4i(edges.[splitEdgeE.[eIdx]].X, edges.[splitEdgeE.[eIdx]].Y, ((eIdx + 1) % 4) |> mapLocalToGlobalId edgeId nextFreeEdgeAddr, thisGlobalId)

        //            ///////////////////////////////////////////////////
        //            // Insert new Face

        //            faceVertices.[nextFreeFaceAddrDynamic] <- V4i(splitEdgeV.[eIdx], splitEdgeV.[(eIdx + 1) % 4], vId, IDHash splitEdgeV.[eIdx] splitEdgeV.[(eIdx + 1) % 4] vId)
        //            faceEdges.[nextFreeFaceAddrDynamic] <- V3i(splitEdgeE.[eIdx], ((eIdx + 1) % 4) |> mapLocalToGlobalId edgeId nextFreeEdgeAddr, thisGlobalId)
        //            nextFreeFaceAddrDynamic <- nextFreeFaceAddrDynamic + 1

        //for i in 0 .. nextFreeFaceAddr - 1 do
        //    if faceVertices.[i].W = fhash1 || faceVertices.[i].W = fhash2 then
        //        faceVertices.[i] <- V4i(-1)

        //(meta, nextFreeEdgeAddr + 3, nextFreeFaceAddrDynamic)
        (meta, nextFreeEdgeAddr, nextFreeFaceAddr)

    [<ReflectedDefinition>][<Inline>]
    let computeIllumination (edges : Arr<N<MAX_EDGES_HALF>, V4i>) (meta : int) (vertices : Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_THREE>, V4d>) caseOffset =
        let mutable patchIllumination = 0.0

        let mutable faces = V4i(-1)
        let mutable faceCounter = 0

        let maxFaceCounter = 
            if caseOffset = CASE_CORNER_OFFSET then
                1
            else if caseOffset = CASE_EDGE_OFFSET then
                2
            else
                3

        for i in 0 .. MAX_EDGES - 1 do
            if faceCounter < maxFaceCounter && edgeIsInside meta i then
                let v0 = readVertexId edges i 0
                let v1 = readVertexId edges i 1
                let v2 = readVertexId edges i 2
                let v3 = readVertexId edges i 3

                let face0 = (v0 + 1) * (v1 + 1) * (v2 + 1)
                let face1 = (v2 + 1) * (v3 + 1) * (v0 + 1)

                let mutable hasFace0 = false
                let mutable hasFace1 = false

                for i in 0 .. 3 do
                    let f = getV4iValueByIdx faces i
                    if f = face0 then hasFace0 <- true
                    if f = face1 then hasFace1 <- true

                if not hasFace0 then
                    faces <- setV4iValueByIdx faces faceCounter face0
                    faceCounter <- faceCounter + 1

                    let area = computeSphericalExcess (vertices.[v0].XYZ) (vertices.[v1].XYZ) (vertices.[v2].XYZ)
                    patchIllumination <- patchIllumination + area * (vertices.[v0].W + vertices.[v1].W + vertices.[v2].W)

                if not hasFace1 then
                    faces <- setV4iValueByIdx faces faceCounter face1
                    faceCounter <- faceCounter + 1

                    let area = computeSphericalExcess (vertices.[v2].XYZ) (vertices.[v3].XYZ) (vertices.[v0].XYZ)
                    patchIllumination <- patchIllumination + area * (vertices.[v2].W + vertices.[v3].W + vertices.[v0].W)

        patchIllumination / 3.0

    [<ReflectedDefinition>][<Inline>]
    let computeIlluminationCheap (edges : Arr<N<MAX_EDGES_HALF>, V4i>) (vertices : Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_THREE>, V4d>) caseOffset =
        let mutable patchIllumination = 0.0

        if caseOffset = CASE_CORNER_OFFSET then
            let v0 = readVertexId edges 1 0
            let v1 = readVertexId edges 1 1
            let v2 = readVertexId edges 1 2
            let v3 = readVertexId edges 1 3

            let area = computeSphericalExcess (vertices.[v0].XYZ) (vertices.[v1].XYZ) (vertices.[v2].XYZ)
            patchIllumination <- patchIllumination + area * (vertices.[v0].W + vertices.[v1].W + vertices.[v2].W)
                    
            let area = computeSphericalExcess (vertices.[v2].XYZ) (vertices.[v3].XYZ) (vertices.[v0].XYZ)
            patchIllumination <- patchIllumination + area * (vertices.[v2].W + vertices.[v3].W + vertices.[v0].W)
        else if caseOffset = CASE_EDGE_OFFSET then
                
            let v10 = readVertexId edges 1 0
            let v11 = readVertexId edges 1 1
            let v12 = readVertexId edges 1 2
            let v13 = readVertexId edges 1 3

            let v20 = readVertexId edges 2 0
            let v21 = readVertexId edges 2 1
            let v22 = readVertexId edges 2 2
            let v23 = readVertexId edges 2 3

            let face0 = IDHash' v10 v11 v12
            let face1 = IDHash' v12 v13 v10

            let face3 = IDHash' v20 v21 v22
            let face4 = IDHash' v22 v23 v20

            let area = computeSphericalExcess (vertices.[v10].XYZ) (vertices.[v11].XYZ) (vertices.[v12].XYZ)
            patchIllumination <- patchIllumination + area * (vertices.[v10].W + vertices.[v11].W + vertices.[v12].W)
                    
            let area = computeSphericalExcess (vertices.[v12].XYZ) (vertices.[v13].XYZ) (vertices.[v10].XYZ)
            patchIllumination <- patchIllumination + area * (vertices.[v12].W + vertices.[v13].W + vertices.[v10].W)

            if face0 <> face3 && face1 <> face3 then
                // face 3
                let area = computeSphericalExcess (vertices.[v20].XYZ) (vertices.[v21].XYZ) (vertices.[v22].XYZ)
                patchIllumination <- patchIllumination + area * (vertices.[v20].W + vertices.[v21].W + vertices.[v22].W)
            else
                // face 4
                let area = computeSphericalExcess (vertices.[v22].XYZ) (vertices.[v23].XYZ) (vertices.[v20].XYZ)
                patchIllumination <- patchIllumination + area * (vertices.[v22].W + vertices.[v23].W + vertices.[v20].W)

        else
            let v00 = readVertexId edges 0 0
            let v01 = readVertexId edges 0 1
            let v02 = readVertexId edges 0 2
            let v03 = readVertexId edges 0 3

            let v10 = readVertexId edges 1 0
            let v11 = readVertexId edges 1 1
            let v12 = readVertexId edges 1 2
            let v13 = readVertexId edges 1 3

            let v20 = readVertexId edges 2 0
            let v21 = readVertexId edges 2 1
            let v22 = readVertexId edges 2 2
            let v23 = readVertexId edges 2 3
                        
            let face0 = IDHash' v00 v01 v02
            let face1 = IDHash' v02 v03 v00

            let face2 = IDHash' v10 v11 v12
            let face3 = IDHash' v12 v13 v10

            let face4 = IDHash' v20 v21 v22
            let face5 = IDHash' v22 v23 v20

            let area = computeSphericalExcess (vertices.[v00].XYZ) (vertices.[v01].XYZ) (vertices.[v02].XYZ)
            patchIllumination <- patchIllumination + area * (vertices.[v00].W + vertices.[v01].W + vertices.[v02].W)
                    
            let area = computeSphericalExcess (vertices.[v02].XYZ) (vertices.[v03].XYZ) (vertices.[v00].XYZ)
            patchIllumination <- patchIllumination + area * (vertices.[v02].W + vertices.[v03].W + vertices.[v00].W)

            if face0 <> face2 && face1 <> face2 then
                // face 2
                let area = computeSphericalExcess (vertices.[v10].XYZ) (vertices.[v11].XYZ) (vertices.[v12].XYZ)
                patchIllumination <- patchIllumination + area * (vertices.[v10].W + vertices.[v11].W + vertices.[v12].W)
            else
                // face 3
                let area = computeSphericalExcess (vertices.[v12].XYZ) (vertices.[v13].XYZ) (vertices.[v10].XYZ)
                patchIllumination <- patchIllumination + area * (vertices.[v12].W + vertices.[v13].W + vertices.[v10].W)

            if face2 <> face4 && face3 <> face4 then
                // face 4
                let area = computeSphericalExcess (vertices.[v20].XYZ) (vertices.[v21].XYZ) (vertices.[v22].XYZ)
                patchIllumination <- patchIllumination + area * (vertices.[v20].W + vertices.[v21].W + vertices.[v22].W)
            else
                // face 5
                let area = computeSphericalExcess (vertices.[v22].XYZ) (vertices.[v23].XYZ) (vertices.[v20].XYZ)
                patchIllumination <- patchIllumination + area * (vertices.[v22].W + vertices.[v23].W + vertices.[v20].W)

        patchIllumination / 3.0

    module Test = 
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

        
        module ComputeIlluminationMockup = 
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
        
            let (edges, meta, offset) = ComputeIlluminationMockup.getDataCaseCorner ()

            let vertices = 
                [
                    V4d(V3d(3, -1, 1) |> Vec.normalize, 1.0)
                    V4d(V3d(3, -1, 3) |> Vec.normalize, 1.0)
                    V4d(V3d(3,  1, 3) |> Vec.normalize, 1.0)
                    V4d(V3d(3,  1, 1) |> Vec.normalize, 1.0)
                ]
                |> ComputeIlluminationMockup.genVertices


            let expected =
                let area1 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[1].XYZ) (vertices.[2].XYZ)
                let area2 = computeSphericalExcess (vertices.[2].XYZ) (vertices.[3].XYZ) (vertices.[0].XYZ)
                
                area1 + area2

            let L = computeIlluminationCheap edges vertices offset

            L |> should equal (expected +/- 1e-5)

        
        [<Test>]
        let ``Compute Illumination Case Edge``() =
        
            let (edges, meta, offset) = ComputeIlluminationMockup.getDataCaseEdge ()

            let vertices = 
                [
                    V4d(V3d(3,  0, 1) |> Vec.normalize, 1.0)
                    V4d(V3d(3, -1, 1) |> Vec.normalize, 1.0)
                    V4d(V3d(3, -1, 3) |> Vec.normalize, 1.0)
                    V4d(V3d(3,  1, 3) |> Vec.normalize, 1.0)
                    V4d(V3d(3,  1, 1) |> Vec.normalize, 1.0)

                ]
                |> ComputeIlluminationMockup.genVertices


            let expected =
                let area1 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[1].XYZ) (vertices.[2].XYZ)
                let area2 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[2].XYZ) (vertices.[3].XYZ)
                let area3 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[3].XYZ) (vertices.[4].XYZ)
                
                area1 + area2 + area3

            let L = computeIlluminationCheap edges vertices offset

            L |> should equal (expected +/- 1e-5)

        [<Test>]
        let ``Compute Illumination Case Inside``() =
        
            let (edges, meta, offset) = ComputeIlluminationMockup.getDataCaseInside ()

            let vertices = 
                [
                    V4d(V3d(3,  0, 2) |> Vec.normalize, 1.0)
                    V4d(V3d(3, -1, 1) |> Vec.normalize, 1.0)
                    V4d(V3d(3, -1, 3) |> Vec.normalize, 1.0)
                    V4d(V3d(3,  1, 3) |> Vec.normalize, 1.0)
                    V4d(V3d(3,  1, 1) |> Vec.normalize, 1.0)
                ]
                |> ComputeIlluminationMockup.genVertices


            let expected =
                let area1 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[1].XYZ) (vertices.[2].XYZ)
                let area2 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[2].XYZ) (vertices.[3].XYZ)
                let area3 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[3].XYZ) (vertices.[4].XYZ)
                let area4 = computeSphericalExcess (vertices.[0].XYZ) (vertices.[4].XYZ) (vertices.[1].XYZ)
                
                area1 + area2 + area3 + area4

            let L = computeIlluminationCheap edges vertices offset

            L |> should equal (expected +/- 1e-5)      