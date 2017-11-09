namespace Render

open Aardvark.Base
open Aardvark.Base.Incremental
open Aardvark.UI
open Aardvark.Application

open Utils
open Light
open Aardvark.Data.Photometry

type RenderMode =
    | GroundTruth = 0
    | CenterPointApprox = 1
    | BaumFFApprox = 2
    | SolidAngleApprox = 3
    | Compare = 4

type LightTransformMode =
    | Translate
    | Rotate

type Action =
    | IMPORT_PHOTOMETRY of string
    | IMPORT_SCENE of string
    | CHANGE_RENDER_MODE of RenderMode
    | CHANGE_COMPARE of RenderMode
    | COMPUTED_ERROR of double
    | OPENED_WINDOW 
    | CHANGE_LIGHT_TRANSFORM_MODE of LightTransformMode
    | TRANSLATE_LIGHT of int * V3d // lightID, direction
    | ROTATE_LIGHT of int * V3d // lightID, euler angles


[<DomainType>]
type RenderState =
    {    
        lights             : LightCollection

        renderMode         : RenderMode
        
        compare            : RenderMode        
        error              : double
        
        scenePath          : string

        photometryName     : Option<string>
        photometryData     : Option<IntensityProfileSampler>

        lightTransformMode : LightTransformMode
    }
    