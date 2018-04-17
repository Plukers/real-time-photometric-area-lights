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
    | StructuredIrrSampling = 4
    | StructuredSampling = 5
    | SolidAngle = 6
    | FormFactor = 7
    | Compare = 8

type OfflineRenderMode =
    | AbstractData = 0
    | GroundTruth = 1
    | Approximations = 2

type LightTransformMode =
    | Translate
    | Rotate

type GTSamplingMode =
    | BRDF = 0
    | Light = 1

type SolidAngleCompMethod = 
    | Triangle = 0
    | Square = 1

type Action =
    | IMPORT_PHOTOMETRY of string
    | IMPORT_SCENE of string
    | CHANGE_RENDER_MODE of RenderMode
    | CHANGE_COMPARE of RenderMode
    | COMPUTED_ERROR of double * double * double // error, bright error, dark error
    | UPDATE_GROUND_TRUTH of bool

    | SET_GT_SAMPLING_MODE of GTSamplingMode
    | SET_SOLID_ANGLE_COMP_METHOD of SolidAngleCompMethod

    | TOGGLE_USE_PHOTOMETRY

    | OPENED_WINDOW 
    | CHANGE_LIGHT_TRANSFORM_MODE of LightTransformMode
    | TRANSLATE_LIGHT of int * V3d // lightID, direction
    | ROTATE_LIGHT of int * V3d // lightID, euler angles

    | CHANGE_OFFLINE_RENDER_MODE of OfflineRenderMode

    | SET_MRP_CLOSEST_WEIGHT of string
    | SET_MRP_NORMAL_WEIGHT of string
    | SET_MRP_BARYCENTER_WEIGHT of string

    | TOGGLE_SAMPLE_CORNERS 
    | TOGGLE_SAMPLE_BARYCENTER 
    | TOGGLE_SAMPLE_CLOSEST
    | TOGGLE_SAMPLE_NORM
    | TOGGLE_SAMPLE_MRP
    | TOGGLE_SAMPLE_RND
    | TOGGLE_BLEND_SAMPLES
    | TOGGLE_SAMPLE_LIGHT
    | TOGGLE_BLEND_EASING
    | CHANGE_BLEND_DIST of Numeric.Action
    | CHANGE_SRS_SAMPLE_NUM of Numeric.Action
    | CHANGE_SRS_WEIGHT_SCALE of Numeric.Action
    | CHANGE_TANGENT_APPROX_DIST of Numeric.Action
    | CHANGE_SRS_WEIGHT_SCALE_IRR of Numeric.Action
    | CHANGE_TANGENT_APPROX_DIST_IRR of Numeric.Action
    | CHANGE_COMBINED_WEIGHT       of Numeric.Action

    | TOGGLE_TONEMAPPING
    | CHANGE_TONEMAP_SCALE of Numeric.Action
    | RENDER_IMAGES of IMod<Async<unit>>

[<DomainType>]
type RenderState =
    {    
        lights             : LightCollection

        usePhotometry      : bool

        renderMode         : RenderMode
        updateGroundTruth  : bool

        gtSamplingMode       : GTSamplingMode
        solidAngleCompMethod : SolidAngleCompMethod 

        offlineRenderMode  : OfflineRenderMode

        mrpWeights         : V3d // closest, normal, barycenter

        sampleCorners        : bool
        sampleBarycenter     : bool
        sampleClosest        : bool
        sampleNorm           : bool
        sampleMRP            : bool
        sampleRandom         : bool
        sampleLight          : bool
        blendSamples         : bool
        blendEasing          : bool
        blendDistance        : NumericInput
        numOfSRSamples       : NumericInput
        SRSWeightScale       : NumericInput
        TangentApproxDist    : NumericInput
        SRSWeightScaleIrr    : NumericInput
        TangentApproxDistIrr : NumericInput
        CombinedSSWeight     : NumericInput

        toneMap            : bool
        toneMapScale       : NumericInput
        
        compare            : RenderMode        
        error              : double
        brightError        : double
        darkError          : double
        
        scenePath          : string

        photometryName     : Option<string>
        photometryData     : Option<IntensityProfileSampler>

        lightTransformMode : LightTransformMode
    }
    