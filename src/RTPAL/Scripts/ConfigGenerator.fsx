﻿open System
open System.IO

let lightConfig =
    
    let newEntry key value config = (value, config |> Map.add key value)
        
    let c = Map.empty

    // DEFINE CONFIG HERE    
    let (NUM_LIGHTS, c) = c |> newEntry "NUM_LIGHTS" 1
    
    let (VERT_PER_LIGHT, c) = c |> newEntry "VERT_PER_LIGHT" 4
    
    let (VERT_PER_LIGHT_PLUS_ONE, c) = c |> newEntry "VERT_PER_LIGHT_PLUS_ONE" (VERT_PER_LIGHT + 1)
    
    let (VERT_ALL_LIGHT, c) = c |> newEntry "VERT_ALL_LIGHT" (NUM_LIGHTS * VERT_PER_LIGHT)


    (*
    A) V - E + F = 2    Euler's formular for planar graphs  https://en.wikipedia.org/wiki/Planar_graph
    B) E <= 3V - 6      Euler's formular for planar graphs  https://en.wikipedia.org/wiki/Planar_graph
    C) 3F <= 2E         Average Degree                      https://en.wikipedia.org/wiki/Planar_graph
    => F <= 2V - 4
    => Max number of faces is 2V - 4
    => Max size of index buffer is 3 * max number of faces
    *)
    let (MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT, c) = c |> newEntry "MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT" (3 * (2 * VERT_PER_LIGHT - 4))
    
    let (MAX_RENDER_IDX_BUFFER_SIZE_ALL_LIGHT, c) = c |> newEntry "MAX_RENDER_IDX_BUFFER_SIZE_ALL_LIGHT" (NUM_LIGHTS * MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT)


    let (MAX_PATCH_SIZE, c) = c |> newEntry "MAX_PATCH_SIZE" 4

    let (MAX_PATCH_SIZE_PLUS_ONE, c) = c |> newEntry "MAX_PATCH_SIZE_PLUS_ONE" (MAX_PATCH_SIZE + 1)

    let (MAX_PATCH_SIZE_PLUS_TWO, c) = c |> newEntry "MAX_PATCH_SIZE_PLUS_TWO" (MAX_PATCH_SIZE + 2)

    let (MAX_PATCH_SIZE_PLUS_THREE, c) = c |> newEntry "MAX_PATCH_SIZE_PLUS_THREE" (MAX_PATCH_SIZE + 3)

    let (MAX_PATCH_SIZE_MINUS_ONE, c) = c |> newEntry "MAX_PATCH_SIZE_MINUS_ONE" (MAX_PATCH_SIZE - 1)
    
    let (MAX_PATCH_SIZE_TIMES_TWO, c) = c |> newEntry "MAX_PATCH_SIZE_TIMES_TWO" (MAX_PATCH_SIZE * 2)

    (*
    Same as MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT but with MAX_PATCH_SIZE instead of 3, since patches can hold more vertices than 3
    *)
    let (MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT, c) = c |> newEntry "MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT" (MAX_PATCH_SIZE * (2 * VERT_PER_LIGHT - 4))

    let (MAX_PATCH_IDX_BUFFER_SIZE_ALL_LIGHT, c) = c |> newEntry "MAX_PATCH_IDX_BUFFER_SIZE_ALL_LIGHT" (NUM_LIGHTS * MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT)
    
    let (NUM_SAMPLES, c) = c |> newEntry "NUM_SAMPLES" 8


    let (SS_LIGHT_SAMPLES_PER_LIGHT, c) = c |> newEntry "SS_LIGHT_SAMPLES_PER_LIGHT" 400

    let (SS_LIGHT_SAMPLES_ALL_LIGHT, c) = c |> newEntry "SS_LIGHT_SAMPLES_ALL_LIGHT" (NUM_LIGHTS * SS_LIGHT_SAMPLES_PER_LIGHT)
    
    c

let delaunayConfig =
    
    let newEntry key value config = (value, config |> Map.add key value)
        
    let c = Map.empty

    // DEFINE CONFIG HERE    
    let (MAX_EDGES, c) = c |> newEntry "MAX_EDGES" 8
    
    let (MAX_EDGES_HALF, c) = c |> newEntry "MAX_EDGES_HALF" (MAX_EDGES / 2)

    let (MAX_FACES, c) = c |> newEntry "MAX_FACES" 4
    
    let (MAX_FACES_HALF, c) = c |> newEntry "MAX_FACES_HALF" (MAX_FACES / 2)


    let (CASE_CORNER, c) = c |> newEntry "CASE_CORNER" 1

    let (CASE_CORNER_OFFSET, c) = c |> newEntry "CASE_CORNER_OFFSET" 0


    let (CASE_EDGE, c) = c |> newEntry "CASE_EDGE" 2

    let (CASE_EDGE_OFFSET, c) = c |> newEntry "CASE_EDGE_OFFSET" 1


    let (CASE_INSIDE, c) = c |> newEntry "CASE_INSIDE" 4

    let (CASE_INSIDE_OFFSET, c) = c |> newEntry "CASE_INSIDE_OFFSET" 2
    
    
    let (NUM_CASE, c) = c |> newEntry "NUM_CASE" 3

    let (MAX_EDGES_ALL, c) = c |> newEntry "MAX_EDGES_ALL" (MAX_EDGES * NUM_CASE)

    let (MAX_FACES_ALL, c) = c |> newEntry "MAX_FACES_ALL" (MAX_FACES * NUM_CASE)

    let (MAX_EDGES_ALL_HALF, c) = c |> newEntry "MAX_EDGES_ALL_HALF" (MAX_EDGES_HALF * NUM_CASE)

    let (MAX_FACES_ALL_HALF, c) = c |> newEntry "MAX_FACES_ALL_HALF" (MAX_FACES_HALF * NUM_CASE)

    c

let mutable configStr = 
    """namespace Render
(* 
    This file is generated automatically with ConfigGenerator.fsx 
    If you want to update the configuration, please modify ConfigGenerator.fsx and execute 
        fsi --exec ConfigGenerator.fsx
*)

module Config = 
"""


let lightConfigHeader = """
    module Light = 
"""
configStr <- String.concat "" [ configStr; lightConfigHeader ] 

lightConfig |> Map.iter (fun entry value -> 
    
    let entrystring = 
        sprintf "
        [<Literal>]
        let %s = %A
                      " entry value

    configStr <- String.concat "" [ configStr; entrystring ] 
    ()) 


let delaunayHeader = """
    module Delaunay = 
"""
configStr <- String.concat "" [ configStr; delaunayHeader ] 

delaunayConfig |> Map.iter (fun entry value -> 

    let entrystring = 
        sprintf "
        [<Literal>]
        let %s = %A
                        " entry value

    configStr <- String.concat "" [ configStr; entrystring ] 
    ()) 
                     
File.WriteAllText(Path.Combine("..", "Config.fs"), configStr);
