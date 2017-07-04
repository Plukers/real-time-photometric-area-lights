open System
open System.IO

(* 
Define Configuration
*)
let NUM_LIGHTS = 1

let VERT_PER_LIGHT = 7

let VERT_ALL_LIGHT = NUM_LIGHTS * VERT_PER_LIGHT

(* 
Build config
*)
let config = String.Format("""namespace Render
(* This file is generated automatically with ConfigGenerator.fsx 
    If you want to update the configuration, please modify ConfigGenerator.fsx and execute 
    Call fsi --exec ConfigGenerator.fsx*)

module Config = 
    [<Literal>]
    let NUM_LIGHTS = {0}

    [<Literal>]
    let VERT_PER_LIGHT = {1}

    [<Literal>]
    let VERT_ALL_LIGHT = {2}
                           """, 
                           NUM_LIGHTS, 
                           VERT_PER_LIGHT, 
                           VERT_ALL_LIGHT)

                           
File.WriteAllText("Config.fs", config);
