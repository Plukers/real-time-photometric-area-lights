namespace Render

open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives

open Utils
open Light

type Action =
    | IMPORT
    | HALTON_UPDATE 
    | CAMERA of CameraController.Message

type RenderMode =
    | GroundTruth

[<DomainType>]
type RenderState =
    {    
        files          : list<string>
        scenes         : hset<ISg<Action>>
        bounds         : Box3d
        lights         : LightCollection
        renderMode     : RenderMode
        haltonSequence : ModRef< V2d[]>
        cameraState    : CameraControllerState
    }

