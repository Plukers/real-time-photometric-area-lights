namespace Render

module EffectApPoissonSampling = 
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open FShade

    open Light.Effect
    open EffectUtils
    open PhotometricLight

    (*
        2N - 1 times executed (N number of created Poisson samples)
        Each iteration takes O(k) iterations

        N = 16
        k = 8

        31 executed -> 31 Random Numbers

        16*2 Random Numbers Per Step 2
        32 * 31 = 992

        992 + 31 = 1023 random numbers <= Upper Limit
        
        We still choose 1024 because its a power of 2 and for fun
    *)
    [<Literal>]
    let MAX_REQUIRED_RANDOM_SAMPLES = 1024

    type UniformScope with
        member uniform.uniformRandomSamples : Arr<N<MAX_REQUIRED_RANDOM_SAMPLES>, float>  = uniform?uniformRandomSamples 

    type Vertex = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
        [<FragCoord>]       fc      : V4d
    }  

    [<Literal>]
    let R = 0.34

    [<Literal>]
    let K = 32
    
    [<Literal>]
    let CELL_SIZE = 0.2404163056 // R / sqrt(2)

    [<Literal>]
    let COLUMNS = 4

    [<Literal>]
    let ROWS = 4

    [<Literal>]
    let MAX_POSSIBLE_SAMPLES = 16 // 1.0 / ( R / sqrt(2.0) |> floor

    // transforms x (horizontal) and y (vertical) to i (row) an j (column)
    [<ReflectedDefinition>]
    let getGridCoord (s : V2d) = V2i(s.Y / CELL_SIZE |> floor |> int, s.X / CELL_SIZE |> floor |> int)

    [<ReflectedDefinition>]
    let getGridIdxV2i (ij : V2i) = ij.X * COLUMNS + ij.Y

    [<ReflectedDefinition>]
    let getGridIdx i j = i * COLUMNS + j

    [<ReflectedDefinition>]
    let gridIdxForSample (s : V2d) = s |> getGridCoord |> getGridIdxV2i

    [<ReflectedDefinition>]
    let randomAnnulus u v =
        let rStart = R
        let rEnd = 2.0 * R

        let theta = 2.0 * PI * u
        let radius = sqrt((rEnd * rEnd - rStart * rStart) * v + rStart * rStart)

        V2d(radius * cos(theta), radius * sin(theta))

    [<ReflectedDefinition>]
    let private createPoissonSamples startSample randomOffset = 
    
        /////////////////////////////////////////
        // SETUP

        // access with i (row) * columns + j (column)
        let grid = Arr<N<MAX_POSSIBLE_SAMPLES>, int>([-1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1; -1])

        let activeList = Arr<N<MAX_POSSIBLE_SAMPLES>, int>()
        let mutable activeListCnt = 0

        let samples = Arr<N<MAX_POSSIBLE_SAMPLES>, V2d>()
        let mutable sampleCnt = 0

        let mutable uniformRandomSamplesIdx = randomOffset % MAX_REQUIRED_RANDOM_SAMPLES

        /////////////////////////////////////////
        // Initialize First Sample

        samples.[sampleCnt] <- startSample
        grid.[startSample |> gridIdxForSample] <- sampleCnt
        activeList.[activeListCnt] <- sampleCnt
        sampleCnt <- sampleCnt + 1
        activeListCnt <- activeListCnt + 1
              
        /////////////////////////////////////////
        // Generate Samples

        while activeListCnt <> 0 do

            let activeSampleListIdx = 
                let rnd = uniform.uniformRandomSamples.[uniformRandomSamplesIdx]
                uniformRandomSamplesIdx <- (uniformRandomSamplesIdx + 1)  % MAX_REQUIRED_RANDOM_SAMPLES
                int (rnd * (float activeListCnt))
            let activeSampleIdx = activeList.[activeSampleListIdx]
            let activeSample = samples.[activeSampleIdx]
                
            let mutable foundNextSample = false
            let mutable nextSample = V2d.Zero
            
            for c in 0 .. K - 1 do
                if not foundNextSample then

                    let u = uniform.uniformRandomSamples.[uniformRandomSamplesIdx]
                    uniformRandomSamplesIdx <- (uniformRandomSamplesIdx + 1)  % MAX_REQUIRED_RANDOM_SAMPLES
                    let v = uniform.uniformRandomSamples.[uniformRandomSamplesIdx]
                    uniformRandomSamplesIdx <- (uniformRandomSamplesIdx + 1)  % MAX_REQUIRED_RANDOM_SAMPLES

                    let candidate : V2d = activeSample + randomAnnulus u v

                    let (ci, cj) = 
                        let gc = getGridCoord candidate
                        (gc.X, gc.Y)

                    if 0 <= ci && ci < ROWS && 0 <= cj && cj < COLUMNS && grid.[getGridIdx ci cj] = -1 then
                        let mutable distAsserted = true

                        for i in - 1 .. 1 do
                            for j in - 1 .. 1 do
                                if distAsserted then
                                    let ni = ci + i
                                    let nj = cj + j

                                    if 0 <= ni && ni < ROWS && 0 <= nj && nj < COLUMNS && grid.[getGridIdx ni nj] <> -1 then
                                        let neighborSampleIdx =  grid.[getGridIdx ni nj]
                                        let neighborSample = samples.[neighborSampleIdx]

                                        if Vec.length (neighborSample - candidate) < R then                                        
                                            distAsserted <- false                                           

                        if distAsserted then
                            foundNextSample <- true
                            nextSample <- candidate
                        
            if foundNextSample then
                samples.[sampleCnt] <- nextSample
                grid.[nextSample |> gridIdxForSample] <- sampleCnt
                activeList.[activeListCnt] <- sampleCnt
                sampleCnt <- sampleCnt + 1
                activeListCnt <- activeListCnt + 1
            else
                if activeListCnt = 1 then
                    activeListCnt <- 0
                else
                    for i in 0 .. activeListCnt - 1 do
                        if i > activeSampleListIdx then
                            activeList.[i - 1] <- activeList.[i]

                    activeListCnt <- activeListCnt - 1
                                        
        (samples, sampleCnt)

    [<ReflectedDefinition>]
    let private sampleIrr (t2w : M33d) (addr : int) (p : V3d) = 
    
        let i = p |> Vec.normalize  
        let iw = t2w * -i
 
        let dotOut = max 1e-9 (abs (Vec.dot iw uniform.LForwards.[addr]))
        let invDistSquared = 1.0 / (Vec.lengthSquared p + 1e-9)

        //if uniform.sampleLight then

        let irr = getPhotometricIntensity iw uniform.LForwards.[addr]  uniform.LUps.[addr] / (uniform.LAreas.[addr] * dotOut)

        (irr * i.Z, i.Z)
        //else
        //    let irr = getPhotometricIntensity iw uniform.LForwards.[addr]  uniform.LUps.[addr] 


        //    (irr * i.Z * invDistSquared, i.Z * (* uniform.LAreas.[addr] *) dotOut * invDistSquared)

    let poissonIrradianceSampling (v : Vertex) = 
        fragment {

            ////////////////////////////////////////////////////////

            let P = v.wp.XYZ

            let t2w = v.n |> Vec.normalize |> basisFrisvad 
            let w2t = t2w |> Mat.transpose

            let brdf = v.c / PI 

            let mutable illumination = V4d.Zero * (uniform.dT * 1e-256 * 0.0)
            
            ////////////////////////////////////////////////////////

            for addr in 0 .. (Config.Light.NUM_LIGHTS - 1) do 
                match uniform.Lights.[addr] with
                    | -1 -> ()
                    |  _ ->    
                        
                        let vAddr = addr * Config.Light.VERT_PER_LIGHT
                        let iAddr = addr * Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT

                        ////////////////////////////////////////////////////////

                        let l2w = M33dFromCols  (V3d.Cross((uniform.LUps.[addr]), (uniform.LForwards.[addr]))) uniform.LUps.[addr] uniform.LForwards.[addr]
                            
                        let w2l = l2w |> Mat.transpose

                        let t2l = w2l * t2w

                        let l2t = l2w * w2t

                        ////////////////////////////////////////////////////////

                        for iIdx in iAddr .. Config.Light.MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT .. (iAddr + uniform.LNumPatchIndices.[addr] - 1) do
                            
                            let mutable vt = Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>() 
                            
                            for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                                let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                                vt.[vtc] <- uniform.LVertices.[vtcAddr]

                            //let squad = 
                            //    let ex = vt.[1] - vt.[0]
                            //    let ey = vt.[3] - vt.[0]
                            //    let squad = SphericalQuad.sphQuadInit vt.[0] ex ey P
                            //    squad

                            for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                                let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                                vt.[vtc] <- w2t * (vt.[vtc] - P)

                            ////////////////////////////////////////////////////////

                            let (clippedVa, clippedVc) = clipPatch(V3d.Zero, V3d.OOI, vt, uniform.LBaseComponents.[addr])

                            if clippedVc <> 0 then

                                let eps = 1e-9
                                               
                                let lightPlaneN = w2t * uniform.LForwards.[addr] |> Vec.normalize                                

                                // find closest point limited to upper hemisphere
                                let t = (- clippedVa.[0]) |> Vec.dot lightPlaneN
                                let mutable closestPoint = t * (-lightPlaneN)
                                                    
                                if (Vec.dot closestPoint V3d.OOI) < 0.0 then
                                    let newDir = V3d(closestPoint.X, closestPoint.Y, 0.0) |> Vec.normalize
                                    closestPoint <- linePlaneIntersection V3d.Zero newDir (clippedVa.[0]) lightPlaneN
                                    
                                let insideLightPlane = (Vec.length closestPoint) < eps
                                
                                if not insideLightPlane then
                                    
                                    let (closestPoint, _, _, _) = clampPointToPolygon clippedVa clippedVc closestPoint t2l

                                    let (irr, weight) = sampleIrr t2w addr closestPoint

                                    let mutable patchIllumination = irr
                                    let mutable weightSum = weight

                                    let closestProjected = (t2l * (closestPoint - clippedVa.[0]))
                                    let closestPoint2d = V2d(closestProjected.X, closestProjected.Y)
                                       

                                    let (samples, sampleCnt) = createPoissonSamples closestPoint2d (int (0.5 * (v.fc.X + v.fc.Y) * (v.fc.X + v.fc.Y + 1.0) + v.fc.Y)) // https://math.stackexchange.com/questions/23503/create-unique-number-from-2-numbers

                                    if sampleCnt > 0 then
                                        for i in 0 .. sampleCnt - 1 do
                                        
                                            let samplePoint = l2t * V3d(samples.[i], 0.0) + clippedVa.[0]
                                            let (irr, weight) = sampleIrr t2w addr samplePoint

                                            patchIllumination <- patchIllumination + irr
                                            weightSum <- weightSum + weight


                                    let L = patchIllumination / weightSum

                                    // Project polygon light onto sphere
                                    for l in 0 .. Config.Light.MAX_PATCH_SIZE_PLUS_ONE - 1 do
                                        if l < clippedVc then
                                            clippedVa.[l] <- Vec.normalize clippedVa.[l]

                                    let I = abs (baumFormFactor(clippedVa, clippedVc)) / (2.0) // should be divided by 2 PI, but PI is already in the brdf
                                        
                                    illumination <- illumination + L * brdf * I //* scale // * i.Z  
                                    
                            ////////////////////////////////////////////////////////
                        ()

            return V4d(illumination.XYZ, v.c.W)
        }

    module Rendering =

        open Aardvark.SceneGraph

        open RenderInterop
        open Utils
        open Utils.Sg
        open Aardvark.Base.Incremental

        let psIrrApproxRenderTask (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 

            let rnd = System.Random(0818152372)
            let uniformRandomSamples = 0.0 |> Array.create MAX_REQUIRED_RANDOM_SAMPLES 

            for i in 0 .. MAX_REQUIRED_RANDOM_SAMPLES - 1 do
                uniformRandomSamples.[i] <- rnd.NextDouble()

            let uniformRandomSamples = uniformRandomSamples |> Mod.init


            sceneSg
                |> setupFbEffects [ 
                        poissonIrradianceSampling |> toEffect
                    ]
                |> Light.Sg.setLightCollectionUniforms data.lights
                |> setupPhotometricData data.photometricData
                |> setupCamera data.view data.projTrafo data.viewportSize 
                |> setUniformDT data.dt
                |> setUniformUsePhotometry data.usePhotometry
                |> Sg.uniform "uniformRandomSamples" uniformRandomSamples
                |> Sg.compile data.runtime signature

        let psIrrApproxFb (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            psIrrApproxRenderTask data signature sceneSg
            |> RenderTask.renderToColor data.viewportSize