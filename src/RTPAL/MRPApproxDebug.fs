namespace Render

module MRPApproxDebug = 
    open Aardvark.Base
    open Aardvark.Base.Incremental
    open Aardvark.Base.Rendering

    open Aardvark.SceneGraph

    open Light

    open EffectUtils

    let private pointSg color trafo = 
         IndexedGeometryPrimitives.solidSubdivisionSphere (Sphere3d(V3d.Zero, 0.04)) 6 color
        |> Sg.ofIndexedGeometry
        |> Sg.trafo trafo
        |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.vertexColor |> toEffect
            ]

    let private lineSg (p1t : IMod<Trafo3d>) (p2t : IMod<Trafo3d>) color = 
        let trafo = 
            Mod.map2 (fun (p1t : Trafo3d) (p2t : Trafo3d) -> 
                let p1 = V3d(p1t.Forward * V4d.Zero)
                let p2 = V3d(p2t.Forward * V4d.Zero)
                
                let scale = Trafo3d.Scale (V3d(0.0, 0.0, Vec.length (p2 - p1)))

                let rotate = Trafo3d.RotateInto(p1, p2)

                let translate = Trafo3d.Translation p1

                scale * rotate * translate           
            ) p1t p2t

        IndexedGeometryPrimitives.line (Line3d(V3d.OOO, V3d.OOI)) color
        |> Sg.ofIndexedGeometry
        |> Sg.trafo trafo
        |> Sg.effect [
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.vertexColor |> toEffect
            ]

    // closestPointTrafo, clampedClosestPointTrafo, normPlanePointTrafo, clampedNormPlanePointTrafo, MRPTrafo
    let private createTrafos (lc : LightCollection) (point : V3d * V3d) = 

        let M33dFromCols (c1 : V3d) (c2 : V3d) (c3 : V3d) =
            M33d(c1.X, c2.X, c3.X, c1.Y, c2.Y, c3.Y, c1.Z, c2.Z, c3.Z)

        let basisFrisvad (n : V3d) = 
            let c1 = V3d(
                        1.0 - (n.X  * n.X) / (1.0 + n.Z),
                        (-n.X * n.Y) / (1.0 + n.Z),
                        -n.X
                        )

            let c2 = V3d(
                        (-n.X * n.Y) / (1.0 + n.Z),
                        1.0 - (n.Y  * n.Y) / (1.0 + n.Z),
                        -n.Y
                        )

            let c3 = n
            
            M33dFromCols c1 c2 c3
            
        let (P, n) = point

        let t2w = n |> Vec.normalize |> basisFrisvad 
        let w2t = t2w |> Mat.transpose

        let trafos = // closestPoint, clampedClosestPoint, normPlanePoint, clampedNormPlanePoint, MRP
            adaptive {

                let! lights = lc.Lights
                
                ////////////////////////////////////////////////////////

                let addr = 0

                let vAddr = addr * Config.VERT_PER_LIGHT
                let iAddr = addr * Config.MAX_IDX_BUFFER_SIZE_PER_LIGHT

                ////////////////////////////////////////////////////////

                let! lUps = lc.Ups
                let! lForwards = lc.Forwards

                let l2w = M33dFromCols (V3d.Cross((lUps.[addr]), (lForwards.[addr]))) lUps.[addr] lForwards.[addr]
                            
                let w2l = l2w |> Mat.transpose

                let t2l = w2l * t2w
                        
                ////////////////////////////////////////////////////////
                
                let! lIndices = lc.Indices
                let! lVertices = lc.Vertices
                
                let computeLightData iIdx = 
                            
                    let v0Addr = lIndices.[iIdx + 0] + vAddr
                    let v0 = w2t * (lVertices.[v0Addr] - P)
                           
                    let v1Addr = lIndices.[iIdx + 1] + vAddr
                    let v1 = w2t * (lVertices.[v1Addr] - P)
                           
                    let v2Addr = lIndices.[iIdx + 2] + vAddr
                    let v2 = w2t * (lVertices.[v2Addr] - P) 

                    ////////////////////////////////////////////////////////

                    let (clippedVa, clippedVc) = clipTriangle(V3d.Zero, V3d.OOI, Arr<N<3>, V3d>([| v0; v1; v2|]))

                    if clippedVc <> 0 then

                        let eps = 1e-9
                        let epb = 1e-3

                        let lightPlaneN = w2t * lForwards.[addr] |> Vec.normalize 

                        // find closest point limited to upper hemisphere
                        let t = (- clippedVa.[0]) |> Vec.dot lightPlaneN
                        let mutable closestPoint = t * (-lightPlaneN)
                                                    
                        if (Vec.dot closestPoint V3d.OOI) < 0.0 then
                            let newDir = V3d(closestPoint.X, closestPoint.Y, 0.0) |> Vec.normalize
                            closestPoint <- linePlaneIntersection V3d.Zero newDir (clippedVa.[0]) lightPlaneN

                        let insideLightPlane = (Vec.length closestPoint) < eps
                                
                        if not insideLightPlane then

                            let closestPointDir = closestPoint |> Vec.normalize

                            // intersect normal with plane
                            let mutable up = V3d.OOI
                                
                            if abs(Vec.dot up lightPlaneN) < eps then
                                up <- up + (epb * closestPointDir) |> Vec.normalize     
                            else 
                                let abovePlane = if (Vec.dot V3d.OOI closestPoint) < 0.0 && (Vec.dot closestPoint lightPlaneN) < 0.0 then false else true
                                if abovePlane then
                                    if (Vec.dot up lightPlaneN) > 0.0 then
                                        up <- up + (abs(Vec.dot up lightPlaneN) + epb) * (-lightPlaneN) |> Vec.normalize

                            let normPlanePoint = linePlaneIntersection V3d.Zero up (clippedVa.[0]) lightPlaneN // tangent space

                            let (closestPointDir, normPlanePointDir, sa) = 

                                if clippedVc = 3 then
                                    let sa = computeSolidAngle clippedVa.[0] clippedVa.[1] clippedVa.[2]

                                    let closestPoint = clampPointToTriangle clippedVa.[0] clippedVa.[1] clippedVa.[2] closestPoint t2l   
                                    let normPlaneP =   clampPointToTriangle clippedVa.[0] clippedVa.[1] clippedVa.[2] normPlanePoint t2l 

                                    (closestPoint |> Vec.normalize, normPlaneP |> Vec.normalize, sa)

                                else

                                    let sa1 = computeSolidAngle clippedVa.[0] clippedVa.[1] clippedVa.[2]
                                    let sa2 = computeSolidAngle clippedVa.[0] clippedVa.[2] clippedVa.[3]
                                    let sa = sa1 + sa2

                                    let closestPoint = clampPointToPolygon clippedVa.[0] clippedVa.[1] clippedVa.[2] clippedVa.[3] closestPoint t2l   
                                    let normPlaneP =   clampPointToPolygon clippedVa.[0] clippedVa.[1] clippedVa.[2] clippedVa.[3] normPlanePoint t2l  
     
                                    (closestPoint |> Vec.normalize, normPlaneP |> Vec.normalize, sa)

                            let mrpDir  = closestPointDir + normPlanePointDir |> Vec.normalize



                            let mrp = linePlaneIntersection V3d.Zero mrpDir (clippedVa.[0]) lightPlaneN
                            let clampedClosestPoint = linePlaneIntersection V3d.Zero closestPointDir (clippedVa.[0]) lightPlaneN
                            let clampedNormPlanePoint = linePlaneIntersection V3d.Zero normPlanePointDir (clippedVa.[0]) lightPlaneN

                            (t2w * closestPoint, t2w * clampedClosestPoint, t2w * normPlanePoint, t2w * clampedNormPlanePoint, t2w * mrp)

                        else
                            (t2w * closestPoint, t2w * closestPoint, t2w * closestPoint, t2w * closestPoint, t2w * closestPoint)

                    else

                        (V3d.Zero, V3d.Zero, V3d.Zero, V3d.Zero, V3d.Zero)

                return computeLightData 0
                
            }

        let closestPointTrafo = 
            trafos 
            |> Mod.map (fun trafos ->
                    let ( closestPoint, _, _, _, _) =  trafos

                    Trafo3d.Translation closestPoint
                    )

        let clampedClosestPointTrafo = 
            trafos 
            |> Mod.map (fun trafos ->
                    let ( _, clampedClosestPoint, _, _, _) =  trafos

                    Trafo3d.Translation clampedClosestPoint
                    )

        let normPlanePointTrafo = 
            trafos 
            |> Mod.map (fun trafos ->
                    let ( _, _, normPlanePoint, _, _) =  trafos

                    Trafo3d.Translation normPlanePoint
                    )

        let clampedNormPlanePointTrafo = 
            trafos 
            |> Mod.map (fun trafos ->
                    let ( _, _, _, clampedNormPlanePoint, _) =  trafos

                    Trafo3d.Translation clampedNormPlanePoint
                    )

        let mrpTrafo = 
            trafos 
            |> Mod.map (fun trafos ->
                    let ( _, _, _, _, MRP) =  trafos

                    Trafo3d.Translation MRP
                    )


        (closestPointTrafo, clampedClosestPointTrafo, normPlanePointTrafo, clampedNormPlanePointTrafo, mrpTrafo)

    let sceneSg (lc : LightCollection) =
        
        let planeSg = 
            let path = Path.combine [__SOURCE_DIRECTORY__;"meshes";"plane.obj"] |> Mod.constant

            Mod.map( fun path -> path |> Utils.Assimp.loadFromFile true |> Sg.normalize) path
            |> Sg.dynamic
            |> Sg.scale 18.0

        let (closestPointTrafo, clampedClosestPointTrafo, normPlanePointTrafo, clampedNormPlanePointTrafo, MRPTrafo) = createTrafos lc (V3d.Zero, V3d.OOI)

        let measurePointTrafo = Trafo3d.Identity |> Mod.constant
        let measurePoint = pointSg C4b.VRVisGreen measurePointTrafo

        let closestPoint            = pointSg C4b.Red closestPointTrafo
        let closestLine             = lineSg measurePointTrafo closestPointTrafo C4b.Red
            
        let clampedClosestPoint     = pointSg C4b.DarkRed clampedClosestPointTrafo
        let clampedClosestLine      = lineSg measurePointTrafo clampedClosestPointTrafo C4b.DarkRed

        let normPlanePoint          = pointSg C4b.Blue normPlanePointTrafo
        let normPlaneLine           = lineSg measurePointTrafo normPlanePointTrafo C4b.Blue

        let clampedNormPlanePoint   = pointSg C4b.DarkBlue clampedNormPlanePointTrafo
        let clampedNormPlaneLine    = lineSg measurePointTrafo clampedNormPlanePointTrafo C4b.DarkBlue

        let MRP                     = pointSg C4b.Green MRPTrafo
        let MRPLine                 = lineSg measurePointTrafo MRPTrafo C4b.Green

        [
            
            planeSg
            measurePoint
            closestPoint
            closestLine
            clampedClosestPoint
            clampedClosestLine
            normPlanePoint
            normPlaneLine
            clampedNormPlanePoint
            clampedNormPlaneLine
            MRP
            MRPLine
        ]
        |> Sg.group'


