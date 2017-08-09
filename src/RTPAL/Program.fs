open System

open Suave
open System.Windows.Forms

open Aardvark.Base
open Aardvark.Application.WinForms
open Aardvark.UI
open Aardvark.SceneGraph.IO

[<EntryPoint>]
let main argv = 
    (*
    let rayTriangleIntersaction (orig : V3d) (dir : V3d) (v0 : V3d) (v1 : V3d) (v2 : V3d) = 
        let e1 = v1 - v0
        let e2 = v2 - v0
        
        let pVec = Vec.cross dir e2
        let det  = Vec.dot e1 pVec
        
        if (det < 1e-8 && det > -1e-8) then
            0.0
        else
            let invDet = 1.0 / det
            let tVec   = orig - v0
            let u      = (Vec.dot tVec pVec) * invDet

            if (u < 0.0 || 1.0 < u) then
                0.0
            else
                let qVec = Vec.cross tVec e1
                let v    = (Vec.dot dir qVec) * invDet

                if (v < 0.0 || 1.0 < u + v) then
                    0.0
                else 
                    ((Vec.dot e2 qVec) * invDet)

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

        M33d.FromCols(c1, c2, n)

    let o = V3d.OOO
    let n = V3d.OIO

    let a = V3d.IOO

    let p0 = V3d( 0.0, -0.5, 0.0)
    let p1 = V3d( 0.0,  0.5, 0.0)
    let p2 = V3d( 0.0,  0.5, 1.0)
    let p3 = V3d( 0.0, -0.5, 1.0)

    let b = n |> Vec.normalize |> basisFrisvad 

    let w2t = b |> Mat.transpose
    let t2w = w2t |> Mat.inverse

    let p0t = w2t * (p0 - a)
    let p1t = w2t * (p1 - a)
    let p2t = w2t * (p2 - a)

    printfn "Basis       p0: %A" p0
    printfn "Transformed p0: %A" p0t
    printfn "Basis       p1: %A" p1
    printfn "Transformed p1: %A" p1t
    printfn "Basis       p2: %A" p2
    printfn "Transformed p2: %A" p2t

    //printfn "Intersaction with Normal t: %A" (rayTriangleIntersaction o V3d.OOI p0t p1t p2t )
    //printfn "Intersaction with OIO t: %A" (rayTriangleIntersaction o V3d.OIO p0t p1t p2t )

    Console.ReadKey() |> ignore
    
    *)
    
    Xilium.CefGlue.ChromiumUtilities.unpackCef()
    Aardvark.UI.Chromium.init argv

    Loader.Assimp.initialize()

    Ag.initialize()
    Aardvark.Init()
    use app = new OpenGlApplication()
    let runtime = app.Runtime

    let a = RenderApp.app |> App.start

    WebPart.startServer 4321 [ 
        MutableApp.toWebPart runtime a
    ]  

    use form = new Form(Width = 1024, Height = 768)
    use ctrl = new AardvarkCefBrowser()
    ctrl.Dock <- DockStyle.Fill
    form.Controls.Add ctrl
    ctrl.StartUrl <- "http://localhost:4321/"

    ctrl.ShowDevTools()

    Application.Run form
    System.Environment.Exit 0
    
    0
