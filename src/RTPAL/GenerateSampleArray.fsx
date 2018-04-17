#r @"..\..\packages\Aardvark.Base\lib\net45\Aardvark.Base.dll"
#r @"..\..\packages\Aardvark.Base.Essentials\lib\net45\Aardvark.Base.Essentials.dll"
#r @"..\..\packages\Aardvark.Base.FSharp\lib\net45\Aardvark.Base.TypeProviders.dll"
#r @"..\..\packages\Aardvark.Base.FSharp\lib\net45\Aardvark.Base.FSharp.dll"

open System
open System.IO
open Aardvark.Base

let private debugPrint = true

module SampleGeneration = 

    let rec private insert v i l =
        match i, l with
        | 0, xs -> v::xs
        | i, x::xs -> x::insert v (i - 1) xs
        | i, [] -> failwith "index out of range"

    let rec private remove i l =
        match i, l with
        | 0, x::xs -> xs
        | i, x::xs -> x::remove (i - 1) xs
        | i, [] -> failwith "index out of range"

    let private replace v i l = remove i l |> insert v i

    let private replace2d v i j l = 
        let r = List.item i l |> replace v j
        l |> replace r i

    let private replace2dV2 v (ij : V2i) l =
        replace2d v (ij.X) (ij.Y) l

    let private item2d i j l =
        l |> List.item i |> List.item j
        
    let generate2DSamples height width r k initSample = 

        /////////////////////////////////////////
        // Setup

        let cellSize = r / sqrt(2.0) 

        let columns = width / cellSize |> floor |> int
        let rows = height / cellSize |> floor |> int

        if debugPrint then printfn "Columns %A; Rows %A; Cell Size %A" columns rows cellSize

        let grid =
            let mutable g = []
            for i in 0 .. rows - 1 do
                let mutable r = []
                for j in 0 .. columns - 1 do r <- -1 :: r
                g <- r :: g                
            g

        let activeList = []
        let samples = []

        // transforms x (vertical) and y (horizontal) to i (row) an j (column)
        let getGridCoord (x : float) (y : float) = V2i(y / cellSize |> floor |> int, x / cellSize |> floor |> int)
              
        let addSample samples grid (s : V2d) = 

            let updatetSamples = (s :: (samples |> List.rev)) |> List.rev
            let sampleIdx = List.length updatetSamples - 1
            let updatetGrid = replace2dV2 sampleIdx (getGridCoord s.X s.Y) grid

            if debugPrint then printfn "AddSample %A: %A" sampleIdx s
            if debugPrint then printfn "- GridChoords: %A" (getGridCoord s.X s.Y)

            (updatetSamples, updatetGrid, sampleIdx)

        let rnd = System.Random(0818152372)
        let rndIdx = System.Random(0818152372)

        /////////////////////////////////////////
        // Initialize First Sample

        let (samples, grid, sampleIdx) = initSample |> addSample samples grid
        let activeList = sampleIdx :: activeList

        /////////////////////////////////////////
        // Generate Samples

        let rec generateSamples activeList samples grid = 

            if debugPrint then printfn "%s" "###############################################################################"
            if debugPrint then printfn "%s" "Generate Samples Call"
         
            if debugPrint then printfn "- Active List: %A" activeList
            if debugPrint then printfn "- Samples: %A" samples
            if debugPrint then printfn "- Grid: %A" grid

            if activeList |> List.isEmpty then
                (activeList, samples, grid)
            else

                let activeSampleListIdx = 
                    int (rndIdx.NextDouble() * (float (activeList |> List.length)))
                let activeSampleIdx = activeList |> List.item activeSampleListIdx
                let activeSample = samples |> List.item activeSampleIdx

                if debugPrint then printfn "- Active Sample %A: %A" activeSampleIdx activeSample

                let candidates : List<V2d> = 

                    // https://ridlow.wordpress.com/2014/10/22/uniform-random-points-in-disk-annulus-ring-cylinder-and-sphere/
                    let randomAnnulus u v =
                        let rStart = r
                        let rEnd = 2.0 * r

                        let theta = 2.0 * Math.PI * u
                        let radius = sqrt((rEnd * rEnd - rStart * rStart) * v + rStart * rStart)

                        (radius * cos(theta), radius * sin(theta))

                    [
                        for i in 0 .. k - 1 do
                            let (x, y) = randomAnnulus (rnd.NextDouble()) (rnd.NextDouble())
                            yield activeSample + V2d(x, y)
                    ]


                let mutable nextSample = None

                for c in candidates do
                    match nextSample with
                    | Some s -> ()
                    | None ->
                        let (ci, cj) = 
                            let gc = getGridCoord (c.X) (c.Y)
                            (gc.X, gc.Y)

                        if debugPrint then printfn "- Candidate: %A" c
                        if debugPrint then printfn "- - GridChoords: i: %A; j: %A" ci cj

                        if 0 <= ci && ci < rows && 0 <= cj && cj < columns && grid |> item2d ci cj = -1 then
                            let mutable distAsserted = true

                            if debugPrint then printfn "- - Inside: %A" true

                            for i in ci - 1 .. ci + 1 do
                                for j in cj - 1 .. cj + 1 do
                                    if 0 <= i && i < rows && 0 <= j && j < columns && grid |> item2d i j <> -1 && distAsserted then
                                        let neighborSampleIdx =  grid |> item2d i j
                                        let neighborSample = samples |> List.item neighborSampleIdx

                                        if Vec.length (neighborSample - c) < r then
                                            if debugPrint then printfn" - - Dist Assertion failed with Neighbor %A: %A" neighborSampleIdx neighborSample
                                            distAsserted <- false                                           
                                    
                            if debugPrint then printfn "- - Dist Asserted: %A" distAsserted

                            if distAsserted then
                                nextSample <- Some c

                match nextSample with
                | Some s -> 
                    let (samples, grid, sampleIdx) = s |> addSample samples grid
                    let activeList = sampleIdx :: activeList
                    generateSamples activeList samples grid
                | None ->
                    if debugPrint then printfn "- Discard Idx %A from %A" activeSampleIdx activeList
                    generateSamples (activeList |> remove activeSampleListIdx) samples grid
          
        let (activeList, samples, grid) = generateSamples activeList samples grid

        if debugPrint then printfn "%s" "###############################################################################"
        printfn "Finished generating %A samples" (List.length samples)

        samples

module ShaderSampleGeneration = 

    let uniformRandomSamples = System.Random(0818152372)
    let uniformRandomSamplesIdx = System.Random(0818152372)

    [<Literal>]
    let R = 0.34

    [<Literal>]
    let K = 8
    
    [<Literal>]
    let CELL_SIZE = 0.2404163056 // R / sqrt(2)

    [<Literal>]
    let COLUMNS = 4

    [<Literal>]
    let ROWS = 4

    [<Literal>]
    let MAX_POSSIBLE_SAMPLES = 16 // 1.0 / ( R / sqrt(2.0) |> floor

    // transforms x (horizontal) and y (vertical) to i (row) an j (column)
    let getGridCoord (s : V2d) = V2i(s.Y / CELL_SIZE |> floor |> int, s.X / CELL_SIZE |> floor |> int)

    let getGridIdxV2i (ij : V2i) = ij.X * COLUMNS + ij.Y

    let getGridIdx i j = i * COLUMNS + j

    let gridIdxForSample (s : V2d) = s |> getGridCoord |> getGridIdxV2i

    let randomAnnulus u v =
        let rStart = R
        let rEnd = 2.0 * R

        let theta = 2.0 * Math.PI * u
        let radius = sqrt((rEnd * rEnd - rStart * rStart) * v + rStart * rStart)

        V2d(radius * cos(theta), radius * sin(theta))

    let createPoissonSamples startSample = 

        let mutable iterationCount = 0
    
        /////////////////////////////////////////
        // SETUP

        // access with i (row) * columns + j (column)
        let grid = Arr<N<MAX_POSSIBLE_SAMPLES>, int>([-1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1])

        let activeList = Arr<N<MAX_POSSIBLE_SAMPLES>, int>()
        let mutable activeListCnt = 0

        let samples = Arr<N<MAX_POSSIBLE_SAMPLES>, V2d>()
        let mutable sampleCnt = 0

        let mutable uniformRandomSamplesIdx = 0

        if debugPrint then printfn "Columns %A; Rows %A; Cell Size %A" COLUMNS ROWS CELL_SIZE

        /////////////////////////////////////////
        // Initialize First Sample

        samples.[sampleCnt] <- startSample
        grid.[startSample |> gridIdxForSample] <- sampleCnt
        activeList.[activeListCnt] <- sampleCnt

        if debugPrint then printfn "AddSample %A: %A" sampleCnt startSample
        if debugPrint then printfn "- GridChoords: %A" (getGridCoord startSample)

        sampleCnt <- sampleCnt + 1
        activeListCnt <- activeListCnt + 1
              
        /////////////////////////////////////////
        // Generate Samples

        while activeListCnt <> 0 do

            if debugPrint then printfn "%s" "###############################################################################"
            if debugPrint then printfn "%s" "Generate Samples Call"
         
            if debugPrint then printfn "- Active List (Len %A): %A" activeListCnt activeList
            if debugPrint then printfn "- Samples (Len %A): %A" sampleCnt samples 
            if debugPrint then printfn "- Grid: %A" grid

            (*
                let activeSampleListIdx = activeList |> List.length |> rnd.Next

            *)

            let activeSampleListIdx = 
                let rnd = uniformRandomSamplesIdx.NextDouble()
                uniformRandomSamplesIdx <- uniformRandomSamplesIdx + 1
                int (rnd * (float (activeListCnt)))
            let activeSampleIdx = activeList.[activeSampleListIdx]
            let activeSample = samples.[activeSampleIdx]

            if debugPrint then printfn "- Active Sample %A: %A" activeSampleIdx activeSample
                
            let mutable foundNextSample = false
            let mutable nextSample = V2d.Zero
            
            for c in 0 .. K - 1 do
                if not foundNextSample then

                    iterationCount <- iterationCount + 1

                    let u = uniformRandomSamples.NextDouble()
                    uniformRandomSamplesIdx <- uniformRandomSamplesIdx + 1
                    let v = uniformRandomSamples.NextDouble()
                    uniformRandomSamplesIdx <- uniformRandomSamplesIdx + 1

                    let candidate : V2d = activeSample + randomAnnulus u v

                    let (ci, cj) = 
                        let gc = getGridCoord candidate
                        (gc.X, gc.Y)

                    if debugPrint then printfn "- Candidate: %A" candidate
                    if debugPrint then printfn "- - GridChoords: i: %A; j: %A" ci cj

                    if 0 <= ci && ci < ROWS && 0 <= cj && cj < COLUMNS && grid.[getGridIdx ci cj] = -1 then
                        let mutable distAsserted = true

                        
                        if debugPrint then printfn "- - Inside: %A" true

                        for i in - 1 .. 1 do
                            for j in - 1 .. 1 do
                                if distAsserted then
                                    let ni = ci + i
                                    let nj = cj + j
                                    
                                    if 0 <= ni && ni < ROWS && 0 <= nj && nj < COLUMNS && grid.[getGridIdx ni nj] <> -1 then
                                        let neighborSampleIdx =  grid.[getGridIdx ni nj]
                                        let neighborSample = samples.[neighborSampleIdx]

                                        if Vec.length (neighborSample - candidate) < R then   
                                            if debugPrint then printfn" - - Dist Assertion failed with Neighbor %A: %A" neighborSampleIdx neighborSample
                                            distAsserted <- false                                           

                        if debugPrint then printfn "- - Dist Asserted: %A" distAsserted
                        
                        if distAsserted then
                            foundNextSample <- true
                            nextSample <- candidate
                        
            if foundNextSample then
                samples.[sampleCnt] <- nextSample
                grid.[nextSample |> gridIdxForSample] <- sampleCnt
                activeList.[activeListCnt] <- sampleCnt

                if debugPrint then printfn "AddSample %A: %A" sampleCnt nextSample
                if debugPrint then printfn "- GridChoords: %A" (getGridCoord nextSample)

                sampleCnt <- sampleCnt + 1
                activeListCnt <- activeListCnt + 1
            else
                if debugPrint then printfn "- Discard Idx %A from (Len %A) %A" activeSampleIdx activeListCnt activeList 
                if activeListCnt = 1 then
                    activeListCnt <- 0
                else
                    for i in 0 .. activeListCnt - 1 do
                        if i > activeSampleListIdx then
                            activeList.[i - 1] <- activeList.[i]

                    activeListCnt <- activeListCnt - 1
          
        if debugPrint then printfn "%s" "###############################################################################"
        printfn "Finished generating %A samples after %A iterations" (sampleCnt) (iterationCount)

        let mutable sampleList = []
        for i in  0 .. sampleCnt - 1 do
            sampleList <- samples.[i] :: sampleList

        sampleList |> List.rev


//let samples = SampleGeneration.generate2DSamples 1.0 1.0 0.34 8 (V2d(0.3265, 0.5998))
let samples = ShaderSampleGeneration.createPoissonSamples (V2d(0.3265, 0.5998))

let header = 
    """namespace Render
(* 
    This file is generated automatically with GenerateSampleArray.fsx 
    If you want to update it, please modify GenerateBlueNoise.fsx and execute 
        fsi --exec GenerateSampleArray.fsx
*)

module PoissonSampleCollection = 
    open Aardvark.Base
"""

let mutable sampleStr = sprintf "
        let samples = 
            ["

for s in samples do
    let nextSampleStr = 
        //sprintf "
        //        V2d(%.10f, %.10f)"  s.X s.Y
        sprintf "
                %.10f, %.10f"  s.X s.Y
    sampleStr <- String.concat "" [ sampleStr; nextSampleStr ]


let close = 
    sprintf "   
            ]"

File.WriteAllText("PoissonSampleCollection.fs", String.concat "" [ header; sampleStr; close ]);

//let list = ['A'; 'B'; 'C'; 'D']

//printfn "list: %A" list
//printfn "replace at 2: %A" (list |> SampleGeneration.replace 'E' 2)
//printfn "insert at 2:  %A" (list |> SampleGeneration.insert 'E' 2)

//let list2d = [[1; 2; 3];[4; 5; 6];[7; 8; 9];]

//printfn "list2d: %A" list2d
//printfn "replace at 0, 2: %A" (list2d |> SampleGeneration.replace2d 10 0 2)
