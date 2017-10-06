﻿open System
open System.IO

(* 
Define Configuration
*)
let NUM_LIGHTS = 1

let VERT_PER_LIGHT = 4

let VERT_PER_LIGHT_PLUS_ONE = VERT_PER_LIGHT + 1

let VERT_ALL_LIGHT = NUM_LIGHTS * VERT_PER_LIGHT

(*
    A) V - E + F = 2    Euler's formular for planar graphs  https://en.wikipedia.org/wiki/Planar_graph
    B) E <= 3V - 6      Euler's formular for planar graphs  https://en.wikipedia.org/wiki/Planar_graph
    C) 3F <= 2E         Average Degree                      https://en.wikipedia.org/wiki/Planar_graph
    => F <= 2V - 4
    => Max number of faces is 2V - 4
    => Max size of index buffer is 3 * max number of faces
*)
let MAX_IDX_BUFFER_SIZE_PER_LIGHT = 3 * (2 * VERT_PER_LIGHT - 4)

let MAX_IDX_BUFFER_SIZE_ALL_LIGHT = NUM_LIGHTS * MAX_IDX_BUFFER_SIZE_PER_LIGHT


let NUM_SAMPLES = 8

(* 
Build config
*)
let config = String.Format("""namespace Render
(* 
    This file is generated automatically with ConfigGenerator.fsx 
    If you want to update the configuration, please modify ConfigGenerator.fsx and execute 
        fsi --exec ConfigGenerator.fsx
*)

module Config = 
    [<Literal>]
    let NUM_LIGHTS = {0}

    [<Literal>]
    let VERT_PER_LIGHT = {1}

    [<Literal>]
    let VERT_PER_LIGHT_PLUS_ONE = {2}

    [<Literal>]
    let VERT_ALL_LIGHT = {3}

    [<Literal>]
    let MAX_IDX_BUFFER_SIZE_PER_LIGHT = {4}

    [<Literal>]
    let MAX_IDX_BUFFER_SIZE_ALL_LIGHT = {5}

    [<Literal>]
    let NUM_SAMPLES = {6}
                           """, 
                           NUM_LIGHTS, 
                           VERT_PER_LIGHT, 
                           VERT_PER_LIGHT_PLUS_ONE,
                           VERT_ALL_LIGHT,
                           MAX_IDX_BUFFER_SIZE_PER_LIGHT,
                           MAX_IDX_BUFFER_SIZE_ALL_LIGHT,
                           NUM_SAMPLES)

                           
File.WriteAllText("Config.fs", config);