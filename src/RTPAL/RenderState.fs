namespace Render

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.Application

open Utils
open Light
open Aardvark.Data.Photometry

type CameraControllerAction = 
        | Down of button : MouseButtons * pos : V2i
        | Up of button : MouseButtons
        | Move of V2i
        | StepTime
        | KeyDown of key : Keys
        | KeyUp of key : Keys
        | Blur

[<DomainType>]
type CameraControllerState =
    {
        view : CameraView

        dragStart : V2i
        look      : bool
        zoom      : bool
        pan       : bool

        moving    : bool

        forward     : bool
        backward    : bool
        left        : bool
        right       : bool
        moveVec     : V3i
        orbitCenter : Option<V3d>
        lastTime    : Option<float>

        sensitivity     : float
        zoomFactor      : float
        panFactor       : float
        rotationFactor  : float        

        [<TreatAsValue>]
        stash : Option<CameraControllerState> 
    }

type RenderMode =
    | GroundTruth = 0
    | BaumFormFactor = 1
    | Compare = 2

type Action =
    | IMPORT_PHOTOMETRY of string
    | CHANGE_RENDER_MODE of RenderMode
    | GROUND_TRUTH_UPDATE 
    | GROUND_TRUTH_CLEAR
    | CHANGE_COMPARE of RenderMode
    | COMPUTED_ERROR of double
    | CAMERA of CameraControllerAction
    | OPEN_GROUND_TRUTH_WINDOW


[<DomainType>]
type RenderState =
    {    
        lights          : LightCollection

        renderMode      : RenderMode
        
        clear           : bool
        frameCount      : int
        haltonSequence  : seq<V2d>

        compare        : RenderMode
        
        error           : double

        geometryFiles   : list<string>
        scenePath       : string
        sceneSg         : Aardvark.SceneGraph.ISg

        photometryName  : Option<string>
        photometryData  : Option<IntensityProfileSampler>

        cameraState     : CameraControllerState
    }
    