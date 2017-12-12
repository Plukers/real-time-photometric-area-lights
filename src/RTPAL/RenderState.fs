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
    | MRPApprox = 2
    | BaumFFApprox = 3
    | StructuredSampling = 4
    | Compare = 5

type LightTransformMode =
    | Translate
    | Rotate

type Action =
    | IMPORT_PHOTOMETRY of string
    | IMPORT_SCENE of string
    | CHANGE_RENDER_MODE of RenderMode
    | CHANGE_COMPARE of RenderMode
    | COMPUTED_ERROR of double * double * double // error, bright error, dark error
    | UPDATE_GROUND_TRUTH of bool
    | OPENED_WINDOW 
    | CHANGE_LIGHT_TRANSFORM_MODE of LightTransformMode
    | TRANSLATE_LIGHT of int * V3d // lightID, direction
    | ROTATE_LIGHT of int * V3d // lightID, euler angles

    | SET_MRP_CLOSEST_WEIGHT of string
    | SET_MRP_NORMAL_WEIGHT of string
    | SET_MRP_BARYCENTER_WEIGHT of string

    | TOGGLE_SAMPLE_CORNERS 
    | TOGGLE_SAMPLE_BARYCENTER 
    | TOGGLE_SAMPLE_CLOSEST
    | TOGGLE_SAMPLE_NORM

[<DomainType>]
type RenderState =
    {    
        lights             : LightCollection

        renderMode         : RenderMode
        updateGroundTruth  : bool

        mrpWeights         : V3d // closest, normal, barycenter

        sampleCorners      : bool
        sampleBarycenter   : bool
        sampleClosest      : bool
        sampleNorm         : bool
        
        compare            : RenderMode        
        error              : double
        brightError        : double
        darkError          : double
        
        scenePath          : string

        photometryName     : Option<string>
        photometryData     : Option<IntensityProfileSampler>

        lightTransformMode : LightTransformMode
    }
    