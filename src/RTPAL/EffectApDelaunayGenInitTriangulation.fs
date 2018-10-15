namespace Render

module EffectApDelaunayGenInitTriangulation =   

    open Aardvark.Base

    open Config.Delaunay

    open EffectApDelaunayDataHandling


    let private IDHash a b c =    
        let (a, b, c) =
            if a < b && a < c then
                (a, b, c)
            elif b < a && b < c then
                (b, c, a)
            else
                (c, a, b)                
        (a <<< 20) ||| (b <<< 10) ||| c


    // https://dev.to/maurobringolf/a-neat-trick-to-compute-modulo-of-negative-numbers-111e
    let private circle m x = (x % m + m) % m

    let private getInsideOutsideIds numCorners currentCorner = (currentCorner - 1, currentCorner - 1 + numCorners)

    let private addInsideEdge (EV : V4i []) (EE  : V4i []) (EM  : V2i []) (numCorners : int) (circle : int -> int) (getInsideOutsideIds : int -> (int * int)) (currentCorner : int) =
        let (idInside, idOutside) = getInsideOutsideIds currentCorner
        EV.[idInside] <- V4i(0, circle (currentCorner - 2) + 1, currentCorner, circle currentCorner + 1)
        EE.[idInside] <- V4i(circle (currentCorner + numCorners - 2), circle (idInside - 1) + numCorners, idOutside, circle currentCorner)
        EM.[idInside] <- V2i(1, 0)

    let private addOutsideEdge (EV : V4i []) (EE  : V4i []) (EM  : V2i []) (numCorners : int) (circle : int -> int) (getInsideOutsideIds : int -> (int * int)) (currentCorner : int) =
        let (_, idOutside) = getInsideOutsideIds currentCorner
        EV.[idOutside] <- V4i(currentCorner, -1, circle currentCorner + 1, 0)
        EE.[idOutside] <- V4i(-1, -1, circle currentCorner,  circle (currentCorner - 1))
        EM.[idOutside] <- V2i(0, 0)

    let private addFace (FV : V4i []) (FE  : V3i []) (circle : int -> int) (getInsideOutsideIds : int -> (int * int)) (currentCorner : int) =
        let (_, idOutside) = getInsideOutsideIds currentCorner
        FV.[currentCorner - 1] <- V4i(0, currentCorner, circle currentCorner + 1, IDHash 0 currentCorner (circle currentCorner + 1))
        FE.[currentCorner - 1] <- V3i(circle (currentCorner - 1), idOutside, circle currentCorner)

    
    let private initEdge (numCorners : int) = 

        let EV = Array.init MAX_EDGES (fun i -> V4i(-1))
        let EE = Array.init MAX_EDGES (fun i -> V4i(-1))
        let EM = Array.init MAX_EDGES (fun i -> V2i(-1))
        let FV = Array.init MAX_FACES (fun i -> V4i(-1))
        let FE = Array.init MAX_FACES (fun i -> V3i(-1))

        let circle = circle numCorners

        for v in 1 .. numCorners do
            let idInside = v - 1
            let idOutside = idInside + numCorners         
            
            if v = 1 then
                EV.[idInside] <- V4i(0, -1, v, circle v + 1)
                EE.[idInside] <- V4i(-1, -1, idOutside, circle v)
                EM.[idInside] <- V2i(0, 0)

                EV.[idOutside] <- V4i(v, -1, circle v + 1, 0)
                EE.[idOutside] <- V4i(-1, -1, circle v,  circle (v - 1))
                EM.[idOutside] <- V2i(0, 0)

                FV.[v - 1] <- V4i(0, v, circle v + 1, IDHash 0 v (circle v + 1))
                FE.[v - 1] <- V3i(circle (v - 1), idOutside, circle v)

            elif v = numCorners then
                EV.[idInside] <- V4i(0, circle (v - 2) + 1, v, -1)
                EE.[idInside] <- V4i(circle (v + numCorners - 2), circle (idInside - 1) + numCorners, -1, -1)
                EM.[idInside] <- V2i(0, 0)

            else
                EV.[idInside] <- V4i(0, circle (v - 2) + 1, v, circle v + 1)
                EE.[idInside] <- V4i(circle (v + numCorners - 2), circle (idInside - 1) + numCorners, idOutside, circle v)
                EM.[idInside] <- V2i(1, 0)

                EV.[idOutside] <- V4i(v, -1, circle v + 1, 0)
                EE.[idOutside] <- V4i(-1, -1, circle v,  circle (v - 1))
                EM.[idOutside] <- V2i(0, 0)

                FV.[v - 1] <- V4i(0, v, circle v + 1, IDHash 0 v (circle v + 1))
                FE.[v - 1] <- V3i(circle (v - 1), idOutside, circle v)

        (EV, EE, EM, FV, FE)

    let private initInside (numCorners : int) = 

        let EV = Array.init MAX_EDGES (fun i -> V4i(-1))
        let EE = Array.init MAX_EDGES (fun i -> V4i(-1))
        let EM = Array.init MAX_EDGES (fun i -> V2i(-1))
        let FV = Array.init MAX_FACES (fun i -> V4i(-1))
        let FE = Array.init MAX_FACES (fun i -> V3i(-1))
        
        let circle = circle numCorners
        let getInsideOutsideIds = getInsideOutsideIds numCorners
        let addInsideEdge = addInsideEdge EV EE EM numCorners circle getInsideOutsideIds
        let addOutsideEdge = addOutsideEdge EV EE EM numCorners circle getInsideOutsideIds
        let addFace = addFace FV FE circle getInsideOutsideIds


        for v in 1 .. numCorners do
            let idInside = v - 1
            let idOutside = idInside + numCorners           

            EV.[idInside] <- V4i(0, circle (v - 2) + 1, v, circle v + 1)
            EE.[idInside] <- V4i(circle (v + numCorners - 2), circle (idInside - 1) + numCorners, idOutside, circle v)
            EM.[idInside] <- V2i(1, 0)

            EV.[idOutside] <- V4i(v, -1, circle v + 1, 0)
            EE.[idOutside] <- V4i(-1, -1, circle v,  circle (v - 1))
            EM.[idOutside] <- V2i(0, 0)

            FV.[v - 1] <- V4i(0, v, circle v + 1, IDHash 0 v (circle v + 1))
            FE.[v - 1] <- V3i(circle (v - 1), idOutside, circle v)

        (EV, EE, EM, FV, FE)

    let genInitTriangulation (numCorners : int) (case : int) = 
        
        let EV, EE, EM, FV, FE = 
            match case with
            | CASE_CORNER -> ([||], [||], [||], [||], [||])
            | CASE_EDGE -> ([||], [||], [||], [||], [||])
            | _ (* CASE_INSIDE*) -> initInside numCorners

        let (EDGES, META) = transformEdgeCollectionToCompactCollection EV EE EM 3
        let FACES = transformFaceollectionToCompactCollection FV FE 3

        (EDGES, META, FACES)
        
    module Test =
        
        open NUnit.Framework
        open FsUnit

        [<Test>]
        let ``Init Quad Triangulation Inside``() = 

            let (EV, EE, EM, FV, FE) =  4 |> initInside

            Assert.Multiple( fun _ ->
                EV |> should equal ([|           
                                        for i in 0 .. MAX_EDGES - 1 do
                                            match i with
                                            // inner
                                            | 0 -> yield V4i(0, 4, 1, 2) 
                                            | 1 -> yield V4i(0, 1, 2, 3) 
                                            | 2 -> yield V4i(0, 2, 3, 4) 
                                            | 3 -> yield V4i(0, 3, 4, 1) 
                                            // outer
                                            | 4 -> yield V4i(1, -1, 2, 0)
                                            | 5 -> yield V4i(2, -1, 3, 0)
                                            | 6 -> yield V4i(3, -1, 4, 0)
                                            | 7 -> yield V4i(4, -1, 1, 0)  
                                            // fill up
                                            | _ -> yield V4i(-1)
                                    |])
                EE |> should equal ([|
                                        for i in 0 .. MAX_EDGES - 1 do
                                            match i with
                                            // inner
                                            | 0 -> yield V4i(3, 7, 4, 1)
                                            | 1 -> yield V4i(0, 4, 5, 2)
                                            | 2 -> yield V4i(1, 5, 6, 3)
                                            | 3 -> yield V4i(2, 6, 7, 0)
                                            // outer
                                            | 4 -> yield V4i(-1, -1, 1, 0) 
                                            | 5 -> yield V4i(-1, -1, 2, 1) 
                                            | 6 -> yield V4i(-1, -1, 3, 2) 
                                            | 7 -> yield V4i(-1, -1, 0, 3) 
                                            // fill up
                                            | _ -> yield V4i(-1)

                                    |])
                EM |> should equal ([|
                                        for i in 0 .. MAX_EDGES - 1 do
                                            match i with                           
                                            // inner
                                            | 0 -> yield V2i(1, 0) 
                                            | 1 -> yield V2i(1, 0) 
                                            | 2 -> yield V2i(1, 0) 
                                            | 3 -> yield V2i(1, 0)   
                                            // outer
                                            | 4 -> yield V2i(0, 0)
                                            | 5 -> yield V2i(0, 0)
                                            | 6 -> yield V2i(0, 0)
                                            | 7 -> yield V2i(0, 0)                          
                                            // fill up
                                            | _ -> yield V2i(-1)                                     
                                    |])
                FV |> should equal ([| 
                                        for i in 0 .. MAX_FACES - 1 do
                                            match i with
                                            | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2)) 
                                            | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3)) 
                                            | 2 -> yield V4i(0, 3, 4, (IDHash 0 3 4)) 
                                            | 3 -> yield V4i(0, 4, 1, (IDHash 0 4 1)) 
                            
                                            // fill up
                                            | _ -> yield V4i(-1)
                                    |])
                FE |> should equal ([| 
                                        for i in 0 .. MAX_FACES - 1 do
                                            match i with
                                            | 0 -> yield V3i(0, 4, 1) 
                                            | 1 -> yield V3i(1, 5, 2) 
                                            | 2 -> yield V3i(2, 6, 3) 
                                            | 3 -> yield V3i(3, 7, 0) 
                            
                                            // fill up
                                            | _ -> yield V3i(-1)
                                    |])
            )



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