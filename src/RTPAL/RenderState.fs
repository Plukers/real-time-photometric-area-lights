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
    | BaumFormFactor = 1
    | Compare = 2

type Action =
    | IMPORT_PHOTOMETRY of string
    | CHANGE_RENDER_MODE of RenderMode
    | CHANGE_COMPARE of RenderMode
    | COMPUTED_ERROR of double
    | OPENED_WINDOW 


[<DomainType>]
type RenderState =
    {    
        lights          : LightCollection

        renderMode      : RenderMode
        
        compare        : RenderMode        
        error           : double

        geometryFiles   : list<string>
        scenePath       : string

        photometryName  : Option<string>
        photometryData  : Option<IntensityProfileSampler>
    }
    