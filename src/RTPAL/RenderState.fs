namespace Render

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.Application

open Utils
open Light

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
    | GroundTruth
    | BaumFormFactor
    | Compare

type Action =
    | IMPORT
    | CHANGE_RENDER_MODE of RenderMode
    | GROUND_TRUTH_UPDATE 
    | GROUND_TRUTH_CLEAR
    | CAMERA of CameraControllerAction


[<DomainType>]
type RenderState =
    {    


        lights          : LightCollection

        renderMode      : RenderMode
        
        clear           : bool
        frameCount      : int
        haltonSequence  : seq<V2d>

        files           : list<string>
        scenes          : hset<ISg<Action>>
        bounds          : Box3d

        cameraState     : CameraControllerState
    }

