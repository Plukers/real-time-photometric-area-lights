namespace Render

(*
    Compares renderings of different effects with a transfer function
*)
module EffectOutline =

    open Aardvark.Base
    open Aardvark.Base.Rendering
    open Aardvark.Base.Rendering.Effects
    open Aardvark.Base.Vec
    open FShade

    open EffectUtils

    let private texDepth = 
        sampler2d {
            texture uniform?TexDepth
            filter Filter.MinMagLinear
            addressU WrapMode.Border
            addressV WrapMode.Border
        }

    let private tex =
        sampler2d {
            texture uniform?Tex
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    type OutlineVertex = {
        [<TexCoord>] tc : V2d
        [<Semantic("EdgeSample")>] es : Arr<N<4>, V2d>
    } 

    let outlineV (v : OutlineVertex) =
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
    
    let outlineF (v : OutlineVertex) = 
        fragment {

            let s0 = texDepth.Sample(v.es.[0]).X
            let s1 = texDepth.Sample(v.es.[1]).X
            let s2 = texDepth.Sample(v.es.[2]).X
            let s3 = texDepth.Sample(v.es.[3]).X

            let threshold = 0.007

            let diffA = abs(s0 - s3) > (threshold * (s0 + s3))
            let diffB = abs(s1 - s2) > (threshold * (s1 + s2))

            let oultine = diffA || diffB
            
            if oultine then
                return V4d(V3d(0.0), 1.0)
            else
                return tex.Sample(v.tc) 
        }

    ()
