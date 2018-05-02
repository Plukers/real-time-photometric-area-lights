#r @"..\..\..\packages\DevILSharp\lib\net45\DevILSharp.dll"
#r @"..\..\..\packages\Aardvark.Base\lib\net45\Aardvark.Base.dll"
#r @"..\..\..\packages\Aardvark.Base.Essentials\lib\net45\Aardvark.Base.Essentials.dll"
#r @"..\..\..\packages\Aardvark.Base.FSharp\lib\net45\Aardvark.Base.TypeProviders.dll"
#r @"..\..\..\packages\Aardvark.Base.FSharp\lib\net45\Aardvark.Base.FSharp.dll"
#r @"..\..\..\custom_packages\Voronoi\Voronoi.dll"

open System
open System.IO
open Aardvark.Base

let printDebug = false

let computeVoronoiAreaWithPoint p =

    let voronoiInstance = Voronoi.Voronoi 1e-10

    let points = [ 
        // corner points
        (0.0, 0.0) // 0
        (1.0, 0.0) // 1
        (1.0, 1.0) // 2
        (0.0, 1.0) // 3

        // center
        (0.5, 0.5) // 4
        ] 

    let (points, pUsed) = 

        let (pX : float), (pY : float) = p
        let pV2d = V2d(pX, pY)

        let addCustomPoint = 
            let mutable minDist = 1e+100

            for point in points do
                let (pointX : float), (pointY : float) = point
                let pointV2d = V2d(pointX, pointY)
                let dist = Vec.length (pointV2d - pV2d)

                if dist < minDist then minDist <- dist            

            if minDist > 1e-9 then true else false

        if addCustomPoint then
            ((p :: (points |> List.rev)) |> List.rev, true)
        else 
            (points, false)

           
    let getCoordinateArrays p =
        let (pointsX, pointsY) = List.unzip p
        (pointsX |> List.toArray, pointsY |> List.toArray)

    let xVal, yVal = getCoordinateArrays points

    let voronoiEdges = voronoiInstance.generateVoronoi(xVal, yVal, 0.0, 1.0, 0.0, 1.0)

    // collect all edges per cell
    let mutable cellEdges = Map.empty

    let addEdgeToCell cellIdx edge = 
        if not (Map.containsKey cellIdx cellEdges) then
            cellEdges <- Map.add cellIdx [] cellEdges
        
        cellEdges <- Map.add  cellIdx (edge :: (Map.find cellIdx cellEdges)) cellEdges

    for ve in voronoiEdges do

        // Do not collect edges between corner points, because they are edges of the length 0
        if ve.site1 > 3 || ve.site2 > 3 then
            addEdgeToCell (ve.site1) ve
            addEdgeToCell (ve.site2) ve

  


    // take edges per cell and create a polygon defined by counter clockwise points
    let mutable cells = Map.empty

    let generatePolygon (edges : Voronoi.GraphEdge list) cornerPoint = 

        let mutable points : V2d list = 
            match cornerPoint with
            | Some cp -> cp :: []
            | None -> []

        // collect points
        let addPointToCollection point = 
            let mutable minDist = 1e+100

            for p in points do
                let dist = Vec.length (point - p)

                if dist < minDist then minDist <- dist

            if minDist > 1e-9 then true else false

        for e in edges do
            let p0 = V2d(e.x1, e.y1)
            let p1 = V2d(e.x2, e.y2)

            if addPointToCollection p0 then points <- p0 :: points
            if addPointToCollection p1 then points <- p1 :: points 
        
   
        // sort points counter clockwise
        let center = (points |> List.reduce (fun acc elem -> acc + elem)) / (points |> List.length |> float)
    
        let compAngle point =         
            let o = ((points |> List.item 0) - center) |> Vec.normalize
            let p : V2d = (point - center) |> Vec.normalize

            let angle = 
                let a = (atan2 p.Y p.X) - (atan2 o.Y o.X)
                if a < 0.0 then a + 2.0 * Math.PI else a

            angle

        let sortFun (v0 : V2d) (v1 : V2d) = 

            let v0angle = compAngle v0
            let v1angle = compAngle v1
        
            if v0angle < v1angle then -1 elif v0angle > v1angle then 1 else 0

        points <-  List.sortWith sortFun points

        points


    for c in cellEdges do

        if c.Key < 4 then // corner point
            let (cpx, cpy) = List.item c.Key points
            cells <- Map.add c.Key (generatePolygon c.Value (Some (V2d(cpx, cpy)))) cells
            ()
        else // no corner point
            cells <- Map.add c.Key (generatePolygon c.Value None) cells
            ()

    // compute cell areas
    let mutable cellAreas = Map.empty

    let computeArea cellPolygonPoints = 

        let cross (a : V2d) (b : V2d) =
            a.X * b.Y - a.Y * b.X

        let mutable area = 0.0

        for i in 0 .. (List.length cellPolygonPoints) - 1 do
            area <- area + cross (cellPolygonPoints |> List.item i) (cellPolygonPoints |> List.item ((i + 1) % (List.length cellPolygonPoints)))

        0.5 * area
        

    for c in cells do
        cellAreas <- cellAreas |> Map.add c.Key (computeArea c.Value)

    // Output
    if printDebug then
        printfn "P was used: %A" pUsed
        for c in cells do
            printfn "Cell %A [%A]: %A" c.Key (cellAreas |> Map.find c.Key) c.Value

    (cellAreas, pUsed)
   

/// Generate Textures

let texDimensions = V2i(256, 256)

let normalize x y = 
    let normalize v dim = (v |> float) / (dim |> float) |> clamp 0.0 1.0
    (normalize x (texDimensions.X), normalize y (texDimensions.Y))


let generateVoronoiTex pixelFunc texName =
    
    let voronoiTex = PixImage<float32>(PixImageInfo(PixFormat.FloatRGBA, texDimensions))
    let voronoiMatrix = voronoiTex.GetMatrix<C4f>();
    voronoiMatrix.SetByCoordParallelX(pixelFunc) |> ignore

    voronoiTex.SaveAsImage(Path.Combine("..", "misc", texName),PixFileFormat.Exr)

    printfn "Generated %A" (Path.Combine("..", "misc", texName))



printfn "Generate Voronoi Texture of size %A" texDimensions

let pixelFuncA = 
    let pFunc (x : int64) (y : int64) =        
        let p = normalize x y
        let areas, _ = computeVoronoiAreaWithPoint p        
        C4f(areas |> Map.find 0, areas |> Map.find 1, areas |> Map.find 2, areas |> Map.find 3) 

    Func<int64, int64, C4f>(pFunc)

generateVoronoiTex pixelFuncA "voronoiTexA"

let pixelFuncB = 
    let pFunc (x : int64) (y : int64) =        
        let p = normalize x y
        let areas, pUsed = computeVoronoiAreaWithPoint p   
        C4f(areas |> Map.find 4, (if pUsed then areas |> Map.find 5 else 0.0), 0.0, 0.0) 

    Func<int64, int64, C4f>(pFunc)

generateVoronoiTex pixelFuncB "voronoiTexB"