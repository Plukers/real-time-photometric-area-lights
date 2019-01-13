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

    let private addOutsideEdge (EV : V4i []) (EE  : V4i []) (EM  : V2i []) (circle : int -> int) (getInsideOutsideIds : int -> (int * int)) (currentCorner : int) =
        let (_, idOutside) = getInsideOutsideIds currentCorner
        EV.[idOutside] <- V4i(currentCorner, -1, circle currentCorner + 1, 0)
        EE.[idOutside] <- V4i(-1, -1, circle currentCorner,  circle (currentCorner - 1))
        EM.[idOutside] <- V2i(0, 0)

    let private addFace (FV : V4i []) (FE  : V3i []) (circle : int -> int) (getInsideOutsideIds : int -> (int * int)) (currentCorner : int) =

        let (_, idOutside) = getInsideOutsideIds currentCorner
        FV.[currentCorner - 1] <- V4i(0, currentCorner, circle currentCorner + 1, IDHash 0 currentCorner (circle currentCorner + 1))
        FE.[currentCorner - 1] <- V3i(circle (currentCorner - 1), idOutside, circle currentCorner)

    let private fillVertexRangeWithTriangleFan numCorners (EV : V4i []) (EE  : V4i []) (EM  : V2i []) (FV : V4i []) (FE  : V3i []) (range : int list) =
            
        let circle = circle numCorners
        let getInsideOutsideIds = getInsideOutsideIds numCorners
        let addInsideEdge = addInsideEdge EV EE EM numCorners circle getInsideOutsideIds
        let addOutsideEdge = addOutsideEdge EV EE EM circle getInsideOutsideIds
        let addFace = addFace FV FE circle getInsideOutsideIds

        for v in range do
            v |> addInsideEdge
            v |> addOutsideEdge
            v |> addFace








    let private initInside (numCorners : int) = 

        let EV = Array.init MAX_EDGES (fun i -> V4i(-1))
        let EE = Array.init MAX_EDGES (fun i -> V4i(-1))
        let EM = Array.init MAX_EDGES (fun i -> V2i(-1))
        let FV = Array.init MAX_FACES (fun i -> V4i(-1))
        let FE = Array.init MAX_FACES (fun i -> V3i(-1))
        
        [1 .. numCorners] |> fillVertexRangeWithTriangleFan numCorners EV EE EM FV FE

        (EV, EE, EM, FV, FE)
    
    let private initEdge (numCorners : int) = 

        let EV = Array.init MAX_EDGES (fun i -> V4i(-1))
        let EE = Array.init MAX_EDGES (fun i -> V4i(-1))
        let EM = Array.init MAX_EDGES (fun i -> V2i(-1))
        let FV = Array.init MAX_FACES (fun i -> V4i(-1))
        let FE = Array.init MAX_FACES (fun i -> V3i(-1))

        let circle = circle numCorners
        let getInsideOutsideIds = getInsideOutsideIds numCorners

        // First Edge needs special treatment, as it is outside
        EV.[0] <- V4i(0, -1, 1, circle 1 + 1)
        EE.[0] <- V4i(-1, -1, numCorners, circle 1)
        EM.[0] <- V2i(0, 0)

        1 |> addOutsideEdge EV EE EM circle getInsideOutsideIds
        1 |> addFace FV FE circle getInsideOutsideIds

        // Fill inside with triangle Fan
        [2 .. numCorners - 1] |> fillVertexRangeWithTriangleFan numCorners EV EE EM FV FE

        // Last Edge needs special treatment, as it is outside
        let idInside = numCorners - 1
        EV.[idInside] <- V4i(0, circle (numCorners - 2) + 1, numCorners, -1)
        EE.[idInside] <- V4i(circle (numCorners + numCorners - 2), circle (idInside - 1) + numCorners, -1, -1)
        EM.[idInside] <- V2i(0, 0)

        (EV, EE, EM, FV, FE)

    let private initCorner (numCorners : int) = numCorners - 1 |> initEdge




    let appendTriangulationCaseData EV EE EM FV FE toAppend =
        let evToAppend, eeToAppend, emToAppend, fvToAppend, feToAppend = toAppend
        (Array.append EV evToAppend, Array.append EE eeToAppend, Array.append EM emToAppend, Array.append FV fvToAppend, Array.append FE feToAppend)

    let genInitTriangulation (numCorners : int) = 
        
        let EV = [||]
        let EE = [||]
        let EM = [||]
        let FV = [||]
        let FE = [||]

        let EV, EE, EM, FV, FE = initCorner numCorners |> appendTriangulationCaseData EV EE EM FV FE
        let EV, EE, EM, FV, FE = initEdge   numCorners |> appendTriangulationCaseData EV EE EM FV FE
        let EV, EE, EM, FV, FE = initInside numCorners |> appendTriangulationCaseData EV EE EM FV FE

        let (EDGES, META) = transformEdgeCollectionToCompactCollection EV EE EM 3
        let FACES = transformFaceollectionToCompactCollection FV FE 3

        (EDGES, META, FACES)

        
    (*
        Example for "genInitTriangulation 4"
        v0 is always the special point (e.g. closest)
        The other points follow in counter clockwise order

        CASE_CORNER

         v3               e4                   v2
            +--------------------------------+   
            |                              --|   
            |                            -/  |   
            |                          -/    |   
            |                        -/      |   
            |                      -/        |   
            |                   --/          |   
            |                 -/             |   
            |               -/               | e3
         e2 |             -/  e1             |   
            |          --/                   |   
            |        -/                      |   
            |      -/                        |   
            |    -/                          |   
            |  -/                            |   
            |-/                              |   
            +--------------------------------+   
          v0                e0                 v1
            

        CASE_EDGE

         v3               e5                   v2
            +--------------------------------+   
            |-                             - |   
            | \                           /  |   
            |  \                         /   |   
            |   \                       /    |   
            |    \                     /     |   
            |     \                   /      |   
            |      \                 /       |   
            |       \ e2           -/        | e4
         e6 |        \            / e1       |   
            |         \          /           |   
            |          \        /            |   
            |           \      /             |   
            |            \    /              |   
            |             \  /               |   
            |              \/                |   
            +--------------------------------+   
         v4         e3     v0        e0        v1


        CASE_INSIDE
                                                     
           v4             e6                 v3  
            +--------------------------------+   
            |--                            --|   
            |  \-                        -/  |   
            |    \-                    -/    |   
            |      \-e3           e2 -/      |   
            |        \-            -/        |   
            |          \--      --/          |   
            |             \-  -/             |   
            |               \- v0            | e5
         e7 |             -/  \-             |   
            |          --/      \--          |   
            |        -/            \-        |   
            |      -/ e0          e1 \-      |   
            |    -/                    \-    |   
            |  -/                        \-  |   
            |-/                            \-|   
            +--------------------------------+   
          v1             e4                   v2 

    *)
        
    module Test =
        
        open NUnit.Framework
        open FsUnit

        [<Test>]
        let ``Examplary test``() =

        
            let EV = [||]
            let EE = [||]
            let EM = [||]
            let FV = [||]
            let FE = [||]

            let EV, EE, EM, FV, FE = initInside 4 |> appendTriangulationCaseData EV EE EM FV FE

            let (EDGES, META) = transformEdgeCollectionToCompactCollection EV EE EM 1
            let FACES = transformFaceollectionToCompactCollection FV FE 1

            true |> should equal true

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
                                            | 4 -> yield V4i(1,-1, 2, 0)
                                            | 5 -> yield V4i(2,-1, 3, 0)
                                            | 6 -> yield V4i(3,-1, 4, 0)
                                            | 7 -> yield V4i(4,-1, 1, 0)  
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
                                            | 4 -> yield V4i(-1,-1, 1, 0) 
                                            | 5 -> yield V4i(-1,-1, 2, 1) 
                                            | 6 -> yield V4i(-1,-1, 3, 2) 
                                            | 7 -> yield V4i(-1,-1, 0, 3) 
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


        [<Test>]
        let ``Init Quad Triangulation Edge``() = 

            let (EV, EE, EM, FV, FE) =  4 |> initEdge

            Assert.Multiple( fun _ ->
                EV |> should equal ([|           
                                        for i in 0 .. MAX_EDGES - 1 do
                                            match i with
                                            // inner
                                            | 0 -> yield V4i(0,-1, 1, 2) 
                                            | 1 -> yield V4i(0, 1, 2, 3) 
                                            | 2 -> yield V4i(0, 2, 3, 4) 
                                            | 3 -> yield V4i(0, 3, 4,-1) 
                                            // outer
                                            | 4 -> yield V4i(1,-1, 2, 0)
                                            | 5 -> yield V4i(2,-1, 3, 0)
                                            | 6 -> yield V4i(3,-1, 4, 0)
                                            // fill up
                                            | _ -> yield V4i(-1)
                                    |])
                EE |> should equal ([|
                                        for i in 0 .. MAX_EDGES - 1 do
                                            match i with
                                            // inner
                                            | 0 -> yield V4i(-1,-1, 4, 1)
                                            | 1 -> yield V4i( 0, 4, 5, 2)
                                            | 2 -> yield V4i( 1, 5, 6, 3)
                                            | 3 -> yield V4i( 2, 6,-1,-1)
                                            // outer
                                            | 4 -> yield V4i(-1,-1, 1, 0) 
                                            | 5 -> yield V4i(-1,-1, 2, 1) 
                                            | 6 -> yield V4i(-1,-1, 3, 2) 
                                            // fill up
                                            | _ -> yield V4i(-1)
                                    |])
                EM |> should equal ([|
                                        for i in 0 .. MAX_EDGES - 1 do
                                            match i with                           
                                            // inner
                                            | 1 -> yield V2i(1, 0) 
                                            | 2 -> yield V2i(1, 0) 
                                            // outer                                            
                                            | 0 -> yield V2i(0, 0)                                            
                                            | 3 -> yield V2i(0, 0)   
                                            | 4 -> yield V2i(0, 0)
                                            | 5 -> yield V2i(0, 0)
                                            | 6 -> yield V2i(0, 0)                  
                                            // fill up
                                            | _ -> yield V2i(-1)                                     
                                    |])
                FV |> should equal ([| 
                                        for i in 0 .. MAX_FACES - 1 do
                                            match i with
                                            | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2)) 
                                            | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3)) 
                                            | 2 -> yield V4i(0, 3, 4, (IDHash 0 3 4))                             
                                            // fill up
                                            | _ -> yield V4i(-1)
                                    |])
                FE |> should equal ([| 
                                        for i in 0 .. MAX_FACES - 1 do
                                            match i with
                                            | 0 -> yield V3i(0, 4, 1) 
                                            | 1 -> yield V3i(1, 5, 2) 
                                            | 2 -> yield V3i(2, 6, 3)                             
                                            // fill up
                                            | _ -> yield V3i(-1)
                                    |])
            )
        
        
        [<Test>]
        let ``Init Quad Triangulation Corner``() = 

            let (EV, EE, EM, FV, FE) =  4 |> initCorner

            Assert.Multiple( fun _ ->
                EV |> should equal ([|           
                                        for i in 0 .. MAX_EDGES - 1 do
                                            match i with
                                            // inner
                                            | 1 -> yield V4i(0, 1, 2, 3) 
                                            // outer
                                            | 0 -> yield V4i(0,-1, 1, 2) 
                                            | 2 -> yield V4i(0, 2, 3,-1)
                                            | 3 -> yield V4i(1,-1, 2, 0)  
                                            | 4 -> yield V4i(2,-1, 3, 0)
                                            // fill up
                                            | _ -> yield V4i(-1)
                                    |])
                EE |> should equal ([|
                                        for i in 0 .. MAX_EDGES - 1 do
                                            match i with
                                            // inner
                                            | 1 -> yield V4i( 0, 3, 4, 2)
                                            // outer
                                            | 0 -> yield V4i(-1,-1, 3, 1)
                                            | 2 -> yield V4i( 1, 4,-1,-1)
                                            | 3 -> yield V4i(-1,-1, 1, 0)
                                            | 4 -> yield V4i(-1,-1, 2, 1)  
                                            // fill up
                                            | _ -> yield V4i(-1)
                                    |])
                EM |> should equal ([|
                                        for i in 0 .. MAX_EDGES - 1 do
                                            match i with                           
                                            // inner
                                            | 1 -> yield V2i(1, 0) 
                                            // outer                                            
                                            | 0 -> yield V2i(0, 0)  
                                            | 2 -> yield V2i(0, 0)                                           
                                            | 3 -> yield V2i(0, 0)   
                                            | 4 -> yield V2i(0, 0)                 
                                            // fill up
                                            | _ -> yield V2i(-1)                                     
                                    |])
                FV |> should equal ([| 
                                        for i in 0 .. MAX_FACES - 1 do
                                            match i with
                                            | 0 -> yield V4i(0, 1, 2, (IDHash 0 1 2)) 
                                            | 1 -> yield V4i(0, 2, 3, (IDHash 0 2 3))                     
                                            // fill up
                                            | _ -> yield V4i(-1)
                                    |])
                FE |> should equal ([| 
                                        for i in 0 .. MAX_FACES - 1 do
                                            match i with
                                            | 0 -> yield V3i(0, 3, 1) 
                                            | 1 -> yield V3i(1, 4, 2)                       
                                            // fill up
                                            | _ -> yield V3i(-1)
                                    |])
            )       
        