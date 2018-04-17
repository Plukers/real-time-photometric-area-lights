#r @"..\..\packages\Aardvark.Base\lib\net45\Aardvark.Base.dll"
#r @"..\..\packages\Aardvark.Base.Essentials\lib\net45\Aardvark.Base.Essentials.dll"
#r @"..\..\packages\Aardvark.Base.FSharp\lib\net45\Aardvark.Base.FSharp.dll"

open System
open System.IO

open Aardvark.Base


let private getOneRandomUV (rnd : System.Random) = 
    V2d(rnd.NextDouble (), rnd.NextDouble ())

let private genRandomUV (rnd : System.Random) count =
    List.init count (fun _ -> V2d(rnd.NextDouble (), rnd.NextDouble ()))
        
(*
    Returns sample points for a given triangle of size Config.Light.NUM_SS_LIGHT_SAMPLES
*)
(*
TODO add average distance
let private generateUVSequenceBestCandidate (discard : V2d -> bool) (distance : V2d -> V2d -> float) num (samplePoints : List<V2d>) =

    let rnd = System.Random(061815)

    let mutable samplePoints = samplePoints

    printfn "Computing %A new sample points. Current num of sample points: %A" num (samplePoints.Length)

    for i in 0 .. num - 1 do

        let mutable maxDistance = 0.0
        let mutable nextSample = V2d.Zero
            
        for sampleCandidate in genRandomUV rnd (500 * samplePoints.Length) do

            if not (discard sampleCandidate) then 

                let mutable sampleCandidateDist = infinity
                    
                for sample in samplePoints do
                        
                    let dist = distance sample sampleCandidate
                            
                    if dist < sampleCandidateDist then
                        sampleCandidateDist <- dist                   

                if sampleCandidateDist > maxDistance then
                    maxDistance <- sampleCandidateDist
                    nextSample  <- sampleCandidate


        let currentPercent = (((float) i)/((float) (num - 1))) * 100.0
        printfn "%4f%%" currentPercent

        samplePoints <- nextSample :: samplePoints

    printfn "Finished generating sample soints. New num of sample points: %A" (samplePoints.Length)

    samplePoints
*)

(*
    Returns sample points
*)
let private generateUVSequenceRelaxDartThrowing (discard : V2d -> bool) (distance : V2d -> V2d -> float) num (samplePoints : List<V2d>) =

    let rnd = System.Random(081815)

    let mutable samplePoints = samplePoints

    printfn "Computing %A new sample points. Current num of sample points: %A" num (samplePoints.Length)

    let mutable radius = 2.0
    let scaleFactor = 0.96

    let mutable failedCount = 0
    let maxFailedCount = 100000

    let mutable genCount = 0

    while genCount <> num - 1 do

        let sampleCandidate = getOneRandomUV rnd

        if not (discard sampleCandidate) then 

            if failedCount = maxFailedCount then
                radius <- radius * scaleFactor
                failedCount <- 0

            let mutable sampleCandidateDist = infinity

            for sample in samplePoints do

                let dist = distance sample sampleCandidate

                if dist < sampleCandidateDist then
                    sampleCandidateDist <- dist     
                        
            if sampleCandidateDist >= radius then
                genCount <- genCount + 1
                failedCount <- 0
                samplePoints <- sampleCandidate :: samplePoints

                let currentPercent = ((((float) genCount)/((float) (num))) * 100.0)
                printfn "%f%%" currentPercent
            else
                failedCount <- failedCount + 1
                    
    printfn "Finished generating sample points. New num of sample points: %A" (samplePoints.Length)

    (samplePoints, radius)


module Triangle = 
            
    (*
        Returns the sample points for the triangle and the abstract sample points which can later be used to continue the sequence
    *)
    let private computePointSequence (p0 : V3d) (p1 : V3d) (p2 : V3d) (samplePoints : Option<List<V2d>>) num =

        let u = p1 - p0
        let v = p2 - p0
            

        let uvtw (uv : V2d) = (uv.X * u + uv.Y * v) + p0

        let t2w = 
            let up = V3d.Cross(u, v ) |> Vec.normalize
            let t  = V3d.Cross(u, up) |> Vec.normalize
            M33d(u.X, t.X, up.X, u.Y, t.Y, up.Y, u.Z, t.Z, up.Z)

        let w2t = t2w |> Mat.transpose


        let to2d (p : V3d) = 
            let v = w2t * (p - p0)
            V2d(v.X, v.Y)
                
        let p02d = p0 |> to2d
        let p12d = p1 |> to2d
        let p22d = p2 |> to2d

        let computePointLineDistance (p2d : V2d) =         
            let n = (p22d.Y - p12d.Y) * p2d.X - (p22d.X - p12d.X) * p2d.Y + p22d.X * p12d.Y - p22d.Y * p12d.X
            let d = sqrt((p22d.Y - p12d.Y) * (p22d.Y - p12d.Y) + (p22d.X - p12d.X) * (p22d.X - p12d.X))
            n / d
            
        let p02dDist = computePointLineDistance p02d

        let discard  (p : V2d) = 

            let p = p |> uvtw

            let p2d = p  |> to2d

            let distance = computePointLineDistance p2d
            
            if (sign p02dDist) * (sign distance) > 0 then
                false
            else
                true

        let distance (a : V2d) (b : V2d) = 

            let a = uvtw a
            let b = uvtw b

            V3d.Distance(a, b)

        let samplePoints = 
            match samplePoints with 
            | Some sps -> sps
            | None ->
                let rnd = System.Random(061815)
                let mutable seed = (genRandomUV rnd 1).[0]

                while discard seed do
                    seed <- (genRandomUV rnd 1).[0]

                seed :: List.empty<V2d>
                
        let (uvSamplePoints, avgDist) = samplePoints |> generateUVSequenceRelaxDartThrowing discard distance num 

        (uvSamplePoints |> List.map (fun uv -> uvtw uv), avgDist)

    (*
        Returns the sample points for the triangle and the abstract sample points which can later be used to continue the sequence
    *)
    let generateNewPointSequence (p0 : V3d) (p1 : V3d) (p2 : V3d) num =                
        computePointSequence p0 p1 p2 None num
             


module Rectangle = 
        
    (*
        Generates a UV sequence for a rectancle with both ranges [0,1]
    *)
    let computeUVSequence (samplePoints : Option<List<V2d>>) num = 
                
        let discard  (p : V2d) = 
            false

        let distance (p0 : V2d) (p1 : V2d) = 
            V2d.Distance(p0, p1)
                
        let samplePoints = 
            match samplePoints with 
            | Some sps -> sps
            | None ->
                let rnd = System.Random(061815)
                let mutable seed = (genRandomUV rnd 1).[0]

                while discard seed do
                    seed <- (genRandomUV rnd 1).[0]

                seed :: List.empty<V2d>
                
        let (uvSamplePoints, avgDist) = samplePoints |> generateUVSequenceRelaxDartThrowing discard distance num 

        (uvSamplePoints, avgDist)

    (*
        Rectangle is given by its sides a and b, which have a corner point in common and are orthonormal
    *)
    let private computePointSequence (a : V3d) (b : V3d) (o : V3d) (samplePoints : Option<List<V2d>>) num = 
                
        let uvtw (uv : V2d) = (uv.X * a + uv.Y * b) + o

        let discard  (p : V2d) = 
            false

        let distance (p0 : V2d) (p1 : V2d) = 

            let p0 = uvtw p0
            let p1 = uvtw p1

            V3d.Distance(p0, p1)
                
        let samplePoints = 
            match samplePoints with 
            | Some sps -> sps
            | None ->
                let rnd = System.Random(061815)
                let mutable seed = (genRandomUV rnd 1).[0]

                while discard seed do
                    seed <- (genRandomUV rnd 1).[0]

                seed :: List.empty<V2d>
                
        let (uvSamplePoints, avgDist) = samplePoints |> generateUVSequenceRelaxDartThrowing discard distance num 

        (uvSamplePoints |> List.map (fun uv -> uvtw uv), avgDist)

    (*
        Rectangle is given by its sides a and b, which have a corner point in common and are orthonormal
        Returns the sample points for the triangle and the abstract sample points which can later be used to continue the sequence
    *)
    let generateNewPointSequence (a : V3d) (b : V3d) (o : V3d) num =                
        computePointSequence a b o None num


let numOfSamples = 400

let mutable sampleStr = 
    """namespace Render
(* 
    This file is generated automatically with GenerateBlueNoise.fsx 
    If you want to update it, please modify GenerateBlueNoise.fsx and execute 
        fsi --exec GenerateBlueNoise.fsx
*)

module OfflineStructuredSamplePoints = 
    open Aardvark.Base
"""

//////////////////////////////////////////////////////////////////////////////////////////

module TriangleSamples =

// Triangle Samples 
    let addSamples sampleStr =

        let mutable newSampleStr = sampleStr

        let vertices = [|
                V3d(0.0, -0.5, -0.5)
                V3d(0.0,  0.5, -0.5)
                V3d(0.0,  0.0,  1.0)
            |] 

        let title = 
            sprintf "
    module Triangle = 
        let samples = 
            let l = ["

        newSampleStr <- String.concat "" [ newSampleStr; title ] 

        let (samples, avg_dist) = Triangle.generateNewPointSequence vertices.[0] vertices.[1] vertices.[2] numOfSamples 

        for s in samples |> List.rev do
            let sampleString = 
                sprintf "
                V3d(%.10f, %.10f, %.10f)"  s.X s.Y s.Z
            newSampleStr <- String.concat "" [ newSampleStr; sampleString ]
    
        let close = 
            sprintf "   
                ]
            l
        "
        newSampleStr <- String.concat "" [ newSampleStr; close ]

        newSampleStr

//////////////////////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////////////////////


module SquareSamples = 

// Square Samples 
    let addSamples sampleStr = 

        let mutable newSampleStr = sampleStr

        let vertices = [|
                    V3d(0.0, -0.5, -0.5)
                    V3d(0.0,  0.5, -0.5)
                    V3d(0.0,  0.5,  0.5)
                    V3d(0.0, -0.5,  0.5)
                |] 

        let title = 
            sprintf "
    module Square = 
        let samples = 
            let l = ["

        newSampleStr <- String.concat "" [ newSampleStr; title ] 

        let (samples, avg_dist) = Rectangle.generateNewPointSequence (vertices.[1] - vertices.[0]) (vertices.[3] - vertices.[0]) (vertices.[0]) numOfSamples 

        for s in samples |> List.rev do
            let sampleString = 
                sprintf "
                V3d(%.10f, %.10f, %.10f)"  s.X s.Y s.Z
            newSampleStr <- String.concat "" [ newSampleStr; sampleString ]
    
        let close = 
            sprintf "   
                ]
            l
        "
   
        newSampleStr <- String.concat "" [ newSampleStr; close ]

        newSampleStr


// Square UV Samples 
    let addUVSamples sampleStr = 

        let mutable newSampleStr = sampleStr

        let title = 
            sprintf "
        let uvSamples = 
            let l = ["

        newSampleStr <- String.concat "" [ newSampleStr; title ] 

        let (samples, avg_dist) = Rectangle.computeUVSequence None numOfSamples 

        for s in samples |> List.rev do
            let sampleString = 
                sprintf "
                V3d(%.10f, %.10f,  %.10f)"  s.X s.Y 0.0
            newSampleStr <- String.concat "" [ newSampleStr; sampleString ]
    
        let close = 
            sprintf "   
                ]
            l
        "
   
        newSampleStr <- String.concat "" [ newSampleStr; close ]

        newSampleStr


//////////////////////////////////////////////////////////////////////////////////////////

sampleStr <- TriangleSamples.addSamples sampleStr
sampleStr <- SquareSamples.addSamples sampleStr
sampleStr <- SquareSamples.addUVSamples sampleStr

File.WriteAllText("BlueNoiseSamples.fs", sampleStr);



        