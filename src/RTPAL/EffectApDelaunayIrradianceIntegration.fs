namespace Render

module EffectApDelaunayIrradianceIntegration = 
    open Aardvark.Base
    open Aardvark.Base.Rendering
    open FShade

    open Light.Effect
    open EffectUtils
    open PhotometricLight

    [<Literal>]
    let MAX_EDGES = 10

    [<Literal>]
    let MAX_FACES = 5


    (*
            // vertices
            // vertex0 (v0), opposite0 (o0), vertex1 (v1), opposite1 (o1)
            let V = Arr<N<MAX_EDGES>, V4i>([
                                            ])

            // edges
            // v0 -> o0, o0 -> v1, v1 -> o1, o1 -> v0
            let E = Arr<N<MAX_EDGES>, V4i>([
                                            ])

            // meta    
            // inside, marked
            // 1 = true, 0 = false
            let M = Arr<N<MAX_EDGES>, V2i>([
                                            ])
            
            // faces
            // (v0 + v1 + v2), v0, v1, v2
            let F = Arr<N<MAX_FACES>, V4i>([
                                            ])

    *)

    module QUAD_DATA =

        module INSIDE =

            // vertices
            // vertex0 (v0), opposite0 (o0), vertex1 (v1), opposite1 (o1)
            let V = Arr<N<MAX_EDGES>, V4i>([
                                                V4i()
                                            ])

            // edges
            // v0 -> o0, o0 -> v1, v1 -> o1, o1 -> v0
            let E = Arr<N<MAX_EDGES>, V4i>([
                                                V4i()
                                            ])

            // meta    
            // inside, marked
            // 1 = true, 0 = false
            let M = Arr<N<MAX_EDGES>, V2i>([
                                                V2i()
                                            ])
            
            // faces
            // (v0 + v1 + v2), v0, v1, v2
            let F = Arr<N<MAX_FACES>, V4i>([
                                                V4i()
                                            ])

            // stack of marked edges
            let S = Arr<N<MAX_EDGES>, int>([
                                                
                                            ])

            // stack pointer
            let SP = 0

        


    type Vertex = {
        [<WorldPosition>]   wp      : V4d
        [<Normal>]          n       : V3d
        [<Color>]           c       : V4d
        [<FragCoord>]       fc      : V4d
    }  

    let delaunyIrrIntegration (v : Vertex) = 
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
                            
                            let vt = Arr<N<Config.Light.MAX_PATCH_SIZE>, V3d>() 
                            
                            for vtc in 0 .. uniform.LBaseComponents.[addr] - 1 do
                                let vtcAddr = uniform.LPatchIndices.[iIdx + vtc] + vAddr
                                vt.[vtc] <- w2t * (uniform.LVertices.[vtcAddr] - P)

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

                                    let vt = Arr<N<Config.Light.MAX_PATCH_SIZE_PLUS_TWO>, V3d>() 
                                                                        
                                    let (closestPoint, CLAMP_POLYGON_RESULT, clampP0Id, clampP1ID) = clampPointToPolygon clippedVa clippedVc closestPoint t2l

                                    // create triangulation

                                    match CLAMP_POLYGON_RESULT with
                                    | CLAMP_POLYGON_RESULT_POINT -> ()
                                    | CLAMP_POLYGON_RESULT_LINE -> ()
                                    | _ (* CLAMP_POLYGON_RESULT_NONE *) -> ()



                                    // transform to a Delaunay triangulation


                                    // integrate


                                    ()
                           
                            ////////////////////////////////////////////////////////
                        

            return V4d(illumination.XYZ, v.c.W)

        }


    module Rendering =

        open Aardvark.SceneGraph

        open RenderInterop
        open Utils
        open Utils.Sg

        let delIrrIntApproxRenderTask (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            sceneSg
                |> setupFbEffects [ 
                        delaunyIrrIntegration |> toEffect
                    ]
                |> Light.Sg.setLightCollectionUniforms data.lights
                |> setupPhotometricData data.photometricData
                |> setupCamera data.view data.projTrafo data.viewportSize 
                |> setUniformDT data.dt
                |> setUniformUsePhotometry data.usePhotometry
                |> Sg.compile data.runtime signature

        let delIrrIntApproxFb (data : RenderData) (signature : IFramebufferSignature) (sceneSg : ISg) = 
            delIrrIntApproxRenderTask data signature sceneSg
            |> RenderTask.renderToColor data.viewportSize
