namespace Render
(* 
    This file is generated automatically with ConfigGenerator.fsx 
    If you want to update the configuration, please modify ConfigGenerator.fsx and execute 
        fsi --exec ConfigGenerator.fsx
*)

module Config = 
    [<Literal>]
    let NUM_LIGHTS = 1

    [<Literal>]
    let VERT_PER_LIGHT = 4

    [<Literal>]
    let VERT_PER_LIGHT_PLUS_ONE = 5

    [<Literal>]
    let VERT_ALL_LIGHT = 4

    [<Literal>]
    let MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT = 12

    [<Literal>]
    let MAX_RENDER_IDX_BUFFER_SIZE_ALL_LIGHT = 12

    [<Literal>]
    let MAX_EVAL_IDX_BUFFER_SIZE_PER_LIGHT = 16

    [<Literal>]
    let MAX_EVAL_IDX_BUFFER_SIZE_ALL_LIGHT = 16

    [<Literal>]
    let NUM_SAMPLES = 8
                           