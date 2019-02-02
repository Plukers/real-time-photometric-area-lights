(*
Compares renderings of different effects with a transfer function
*)
module EffectCompare

open Aardvark.Base
open Aardvark.Base.Rendering
open Aardvark.Base.Rendering.Effects
open Aardvark.Base.Vec
open FShade

open EffectUtils
open RenderState

type CompareVertex = {
        [<TexCoord>] tc : V2d
    } 

module Compute = 

    type UniformScope with
        member uniform.GTTex : ShaderTextureHandle = uniform?GTTex
        member uniform.CompTex : ShaderTextureHandle = uniform?CompTex

    let private gtTex =
        sampler2d {
            texture uniform.GTTex
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let private compTex =
        sampler2d {
            texture uniform.CompTex
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }
            
    let computeError (v : CompareVertex) = 
        fragment {
            let luminance = V3d(0.2126, 0.7152, 0.0722)

            let gt = dot luminance (V3d(gtTex.Sample(v.tc))) 
            let comp = dot luminance (V3d(compTex.Sample(v.tc))) 
            
            let diff = comp - gt   
            let err = diff * diff
                
            return V4d(err, float (sign diff), 0.0, 1.0)
        }


module Visualize = 
    
    let private texError =
        sampler2d {
            texture uniform?TexError
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }
            
    let visualize (v : CompareVertex) = 
        fragment {  
            let upp1 = 10.0
            let b1 = V3d(1.0, 0.5089, 0.34902)      // [255, 130,  89] 
            let d1 = V3d(0.21177, 0.29412, 0.69804) // [ 54,  75, 178] 

            let upp2 = 20.0
            let b2 = V3d(1.0, 0.35686, 0.14902)     // [255,  91,  38] 
            let d2 = V3d(0.07059, 0.18039, 0.69804) // [ 18,  46, 178]

            let s = texError.Sample(v.tc)

            let error = sqrt (s.X)
            let sign =  s.Y
                                                     

            let (cTrue, cFalse, low, upp) =
                if sign > 0.0 then
                    if error <= upp1 then // Too bright
                        (V3d(1.0), b1, 0.0, upp1) 
                    else 
                        (b1, b2, upp1, upp2) 
                else 
                    if error <= upp1 then // Too dark
                        (V3d(1.0), d1, 0.0, upp1) 
                    else 
                        (d1, d2, upp1, upp2) 

               
                
            let error = (clamp low upp error) - low

                
            return V4d((Lerp cTrue cFalse (error / (upp - low))), 1.0)

        }


module Rendering = 
        
        
    open Aardvark.SceneGraph
    open Aardvark.Base.RenderTask
    open Aardvark.Base.Incremental

    open EffectGT.Rendering

    open RenderInterop
    open Utils
    open Utils.Sg


    let private renderToFbo (fbo : IOutputMod<IFramebuffer>) (task : IRenderTask) =
        let sem = (Set.singleton DefaultSemantic.Colors)
        let res = 
            new SequentialRenderTask([|task|]) |> renderTo fbo
        
        sem |> Seq.map (fun k -> k, getResult k res) |> Map.ofSeq |> Map.find DefaultSemantic.Colors

    let private depthSignature (runtime : Aardvark.Rendering.GL.Runtime) = 
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Depth, { format = RenderbufferFormat.DepthComponent32; samples = 1 }
        ]
                
    let private  diffSignature (runtime : Aardvark.Rendering.GL.Runtime) =
        runtime.CreateFramebufferSignature [
            DefaultSemantic.Colors, { format = RenderbufferFormat.Rgba32f; samples = 1 }
        ]
                                        
    let diffFb (data : RenderData) (effectFbs : Map<RenderMode, IOutputMod<ITexture>>) =
                        
        Sg.fullscreenQuad data.viewportSize
            |> Sg.effect [ 
                Compute.computeError |> toEffect 
            ]
            |> Sg.texture (Sym.ofString "GTTex") (Map.find RenderMode.GroundTruth effectFbs)
            |> Sg.texture (Sym.ofString "CompTex") 
                (
                    data.compare |> Mod.bind (fun rm ->
                        match rm with
                            | RenderMode.Compare -> Map.find RenderMode.GroundTruth effectFbs
                            | _ -> Map.find rm effectFbs
                        )
                )
            |> Sg.compile data.runtime (diffSignature data.runtime)
            |> RenderTask.renderToColor data.viewportSize
                                
    let private depthFb (data : RenderData) (sceneSg : ISg)=
        sceneSg
            |> Sg.effect [ 
                DefaultSurfaces.trafo |> toEffect
                DefaultSurfaces.vertexColor |> toEffect 
                ]
            |> Sg.compile data.runtime (depthSignature data.runtime)
            |> RenderTask.renderToDepth data.viewportSize

    let compareRenderTask (data : RenderData) (gtData : GroundTruthData) (signature : IFramebufferSignature) (sceneSg : ISg) (diffFb : IOutputMod<ITexture>) = 
        Sg.fullscreenQuad data.viewportSize
            |> Sg.effect [ 
                Visualize.visualize |> toEffect
            ]
            |> Sg.uniform "PixelSize" (data.viewportSize |> Mod.map (fun vs -> V2d(1.0 / (float) vs.X, 1.0 / (float) vs.Y)))
            |> Sg.texture (Sym.ofString "TexError") diffFb
            |> Sg.texture (Sym.ofString "TexDepth") (depthFb data sceneSg)
            |> Sg.compile data.runtime signature

    let compareFb (data : RenderData) (gtData : GroundTruthData) (signature : IFramebufferSignature) (sceneSg : ISg) (diffFb : IOutputMod<ITexture>) = 
        compareRenderTask data gtData signature sceneSg diffFb
        |> RenderTask.renderToColor data.viewportSize
     
    let compareSg (data : RenderData) (gtData : GroundTruthData) (signature : IFramebufferSignature) (sceneSg : ISg) (diffFb : IOutputMod<ITexture>) = 
        compareFb data gtData signature sceneSg diffFb
        |> fbToSg data.viewportSize
