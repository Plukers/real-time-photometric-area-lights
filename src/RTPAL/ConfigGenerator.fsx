open System
open System.IO

let config =
    
    let newEntry entry value config = (value, config |> Map.add entry value)
        
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

    let (MAX_PATCH_SIZE_MINUS_ONE, c) = c |> newEntry "MAX_PATCH_SIZE_MINUS_ONE" (MAX_PATCH_SIZE - 1)
    
    let (MAX_PATCH_SIZE_TIMES_TWO, c) = c |> newEntry "MAX_PATCH_SIZE_TIMES_TWO" (MAX_PATCH_SIZE * 2)

    (*
    Same as MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT but with MAX_PATCH_SIZE instead of 3, since patches can hold more vertices than 3
    *)
    let (MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT, c) = c |> newEntry "MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT" (MAX_PATCH_SIZE * (2 * VERT_PER_LIGHT - 4))

    let (MAX_PATCH_IDX_BUFFER_SIZE_ALL_LIGHT, c) = c |> newEntry "MAX_PATCH_IDX_BUFFER_SIZE_ALL_LIGHT" (NUM_LIGHTS * MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT)
    
    let (NUM_SAMPLES, c) = c |> newEntry "NUM_SAMPLES" 8
    
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

config |> Map.iter (fun entry value -> 
    
    let entrystring = 
        String.Format("""
    [<Literal>]
    let {0} = {1}
                      """, entry, value)

    configStr <- String.concat "" [ configStr; entrystring ] 
    ()) 
                     
File.WriteAllText("Config.fs", configStr);
