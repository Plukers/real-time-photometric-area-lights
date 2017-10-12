﻿namespace Render

(*
    Compares renderings of different effects with a transfer function
*)
module EffectCompare =

    open Aardvark.Base
    open Aardvark.Base.Rendering
    open Aardvark.Base.Rendering.Effects
    open Aardvark.Base.Vec
    open FShade

    open EffectUtils

    type UniformScope with
        member uniform.TexA : ShaderTextureHandle = uniform?TexA
        member uniform.TexB : ShaderTextureHandle = uniform?TexB

    let private texA =
        sampler2d {
            texture uniform.TexA
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private texB =
        sampler2d {
            texture uniform.TexB
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private texDepth = 
        sampler2d {
            texture uniform?TexDepth
            filter Filter.MinMagLinear
            addressU WrapMode.Border
            addressV WrapMode.Border
        }

    type CompareVertex = {
        [<TexCoord>] tc : V2d
        [<Semantic("EdgeSample")>] es : Arr<N<4>, V2d>
    } 

    let compareV (v : CompareVertex) =
        vertex {

            let sampleDistance = 1.0
            let pixelSize = V2d(0.000732, 0.0013) // TODO get from uniform

            let edgeSamples = Arr<N<4>, V2d>([|
                                                v.tc + V2d( 1.0,  1.0) * sampleDistance * pixelSize
                                                v.tc + V2d( 1.0, -1.0) * sampleDistance * pixelSize
                                                v.tc + V2d(-1.0,  1.0) * sampleDistance * pixelSize
                                                v.tc + V2d(-1.0, -1.0) * sampleDistance * pixelSize
                                            |])
            
            return {
                tc = v.tc
                es = edgeSamples
            }
        }
    
    let compareF (v : CompareVertex) = 
        fragment {

            // Outlines
            
            let s0 = texDepth.Sample(v.es.[0]).X
            let s1 = texDepth.Sample(v.es.[1]).X
            let s2 = texDepth.Sample(v.es.[2]).X
            let s3 = texDepth.Sample(v.es.[3]).X

            let threshold = 0.007

            let diffA = abs(s0 - s3) > (threshold * (s0 + s3))
            let diffB = abs(s1 - s2) > (threshold * (s1 + s2))

            let oultine = diffA || diffB

                        
            // Error
            let upp = 0.02

            let ta = texA.Sample(v.tc) 
            let tb = texB.Sample(v.tc) 
            
            let err = 
                let diff = V3d(ta - tb)  
                clamp 0.0 upp (sqrt (dot diff diff))

            let cGood = V3d(1.0)
            let cBad = V3d(1.0, 0.35686, 0.14902)

            // Result
            
            if oultine then
                return V4d(V3d(0.0), 1.0)
            else
                return V4d((Lerp cGood cBad (err / upp)), 1.0)
            

            // return V4d(depth, depth, depth, 1.0)
            // return V4d((Lerp cGood cBad (err / upp)), 1.0)
        }

    ()
