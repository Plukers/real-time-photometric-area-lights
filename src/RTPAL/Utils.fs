module Utils

module Sg =
        
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Base.Incremental.Operators        
    open Aardvark.Base.Rendering
                
    open Aardvark.SceneGraph
            

    let fullscreenQuad size =
        Sg.draw IndexedGeometryMode.TriangleStrip
            |> Sg.vertexAttribute DefaultSemantic.Positions (Mod.constant [|V3f(-1.0,-1.0,0.0); V3f(1.0,-1.0,0.0); V3f(-1.0,1.0,0.0);V3f(1.0,1.0,0.0) |])
            |> Sg.vertexAttribute DefaultSemantic.DiffuseColorCoordinates (Mod.constant [|V2f.OO; V2f.IO; V2f.OI; V2f.II|])
            |> Sg.depthTest ~~DepthTestMode.None
            |> Sg.uniform "ViewportSize" size


    let setupCamera (view : IMod<CameraView>) (projTrafo : IMod<Trafo3d>) (viewportSize : IMod<V2i>) sg =

        let a = view |> Mod.map (fun v -> CameraView.viewTrafo v)

        sg
            // |> Sg.viewTrafo (view |> Mod.map CameraView.viewTrafo)
            |> Sg.viewTrafo (view |> Mod.map (fun v ->
                                                CameraView.viewTrafo v
                                                ))
            |> Sg.projTrafo projTrafo
            |> Sg.uniform "ViewportSize" viewportSize
                
    let fbToSg (viewportSize : IMod<V2i>) fb = 
        fullscreenQuad viewportSize
            |> Sg.texture DefaultSemantic.DiffuseColorTexture fb
            |> Sg.effect [DefaultSurfaces.diffuseTexture |> toEffect]

    let setupFbEffects effects sg =   
        sg
        |> Sg.effect ( List.append [
                            DefaultSurfaces.trafo |> toEffect
                            DefaultSurfaces.diffuseTexture |> toEffect
                        ] effects)

module Assimp =
    open Aardvark.Base
    open Aardvark.UI

    let loadFromFile zUp f =  
        f 
            |> Aardvark.SceneGraph.IO.Loader.Assimp.load 
            |> Sg.adapter 
            |> Sg.noEvents
            |> Sg.transform (if zUp then Trafo3d.FromOrthoNormalBasis(V3d.IOO, V3d.OOI, -V3d.OIO) else Trafo3d.Identity)

module PoissonSequence = 

    open Aardvark.Base

    let private getOneRandomUV (rnd : System.Random) = 
        V2d(rnd.NextDouble (), rnd.NextDouble ())

    let private genRandomUV (rnd : System.Random) count =
        List.init count (fun _ -> V2d(rnd.NextDouble (), rnd.NextDouble ()))
        
    (*
        Returns sample points
    *)
    let private generateUVSequenceBestCandidate (discard : V2d -> bool) (distance : V2d -> V2d -> float) num (samplePoints : List<V2d>) =

        let rnd = System.Random(061815)

        let mutable samplePoints = samplePoints
            
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


            let currentPercent = (int)((((float) i)/((float) (num - 1))) * 100.0)

            samplePoints <- nextSample :: samplePoints
                
        samplePoints

    (*
        Returns sample points
    *)
    let private generateUVSequenceRelaxDartThrowing (discard : V2d -> bool) (distance : V2d -> V2d -> float) num (samplePoints : List<V2d>) =

        let rnd = System.Random(081815)

        let mutable samplePoints = samplePoints
            
        let mutable radius = 2.0
        let scaleFactor = 0.96

        let mutable failedCount = 0
        let maxFailedCount = 100000

        let mutable genCount = 0

        while genCount <> num - 1 do

            if failedCount = maxFailedCount then
                radius <- radius * scaleFactor
                failedCount <- 0

            let sampleCandiate = getOneRandomUV rnd
            let mutable sampleCandidateDist = infinity

            for sample in samplePoints do

                let dist = distance sample sampleCandiate

                if dist < sampleCandidateDist then
                    sampleCandidateDist <- dist     
                        
            if sampleCandidateDist >= radius then
                genCount <- genCount + 1
                failedCount <- 0
                samplePoints <- sampleCandiate :: samplePoints

                let currentPercent = ((((float) genCount)/((float) (num - 1))) * 100.0)
                printfn "%f%%" currentPercent
            else
                failedCount <- failedCount + 1
                    
        samplePoints

    module Triangle = 
            
        open EffectUtils

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
            

            let discard  (p : V2d) = 

                let p = p |> uvtw

                let p2d = p  |> to2d

                let (u, v, w) = barycentricCoordinates p2d p02d p12d p22d

                if u >= 0.0 && v >= 0.0 && w >= 0.0 then
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
                
            let uvSamplePoints = samplePoints |> generateUVSequenceRelaxDartThrowing discard distance num 

            (uvSamplePoints |> List.map (fun uv -> uvtw uv), uvSamplePoints)

        (*
            Returns the sample points for the triangle and the abstract sample points which can later be used to continue the sequence
        *)
        let generateNewPointSequence (p0 : V3d) (p1 : V3d) (p2 : V3d) num =                
            computePointSequence p0 p1 p2 None num

        (*
            Returns the sample points for the triangle and the abstract sample points which can later be used to continue the sequence
        *)
        let continuePointSequence (p0 : V3d) (p1 : V3d) (p2 : V3d) abstractSamples num = 
            computePointSequence p0 p1 p2 (Some abstractSamples) num


    module Rectangle = 
        
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
                
            let uvSamplePoints = samplePoints |> generateUVSequenceRelaxDartThrowing discard distance num 

            (uvSamplePoints |> List.map (fun uv -> uvtw uv), uvSamplePoints)

        (*
            Rectangle is given by its sides a and b, which have a corner point in common and are orthonormal
            Returns the sample points for the triangle and the abstract sample points which can later be used to continue the sequence
        *)
        let generateNewPointSequence (a : V3d) (b : V3d) (o : V3d) num =                
            computePointSequence a b o None num

        (*
            Rectangle is given by its sides a and b, which have a corner point in common and are orthonormal
            Returns the sample points for the triangle and the abstract sample points which can later be used to continue the sequence
        *)
        let continuePointSequence (a : V3d) (b : V3d) (o : V3d) abstractSamples num = 
            computePointSequence a b o (Some abstractSamples) num

            

    
module HaltonSequence = 
    open FShade
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.SceneGraph
        
    type UniformScope with
        member uniform.HaltonSamples : Arr<N<Config.Light.NUM_SAMPLES>, V2d> = uniform?HaltonSamples
        
    // Generates a 2d halton sequence of length Config.Light.NUM_SAMPlES with the bases 2 and 3
    let init = 

        let h = V2d.Zero |> Array.create Config.Light.NUM_SAMPLES

        h.[0] <- V2d(0.5, 1.0/3.0)

        for i in 1 .. Config.Light.NUM_SAMPLES - 1 do
               
            h.[i] <- V2d(
                Quasi.QuasiHaltonWithIndex(0, h.[i - 1].X),
                Quasi.QuasiHaltonWithIndex(1, h.[i - 1].Y))
              
        h
        // h |> Mod.constant

    // Generates a 2d halton sequence of length Config.Light.NUM_SAMPlES with the bases 2 and 3
    // The seed is used as a startpoint for the seqauence
    let next (seed : V2d) = 

        let h = V2d.Zero |> Array.create (Config.Light.NUM_SAMPLES + 1)

        h.[0] <- seed

        for i in 1 .. Config.Light.NUM_SAMPLES do

            h.[i] <- V2d(
                Quasi.QuasiHaltonWithIndex(0, h.[i - 1].X),
                Quasi.QuasiHaltonWithIndex(1, h.[i - 1].Y))
              
        h.[1..]
            
    let addSequenceToSg (sequence : IMod<V2d[]>) sg = sg |> Sg.uniform "HaltonSamples" sequence