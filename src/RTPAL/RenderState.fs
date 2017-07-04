namespace Render

open Aardvark.Base.Incremental
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Base.ShaderReflection.ShaderPath

type Action =
    | IMPORT
    | CAMERA of CameraController.Message

[<DomainType>]
type RenderState =
    {    
        files : list<string>
        scenes : hset<ISg<Action>>
        bounds : Box3d
        cameraState : CameraControllerState
    }

