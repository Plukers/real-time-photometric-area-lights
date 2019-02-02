
(* 
    This file is generated automatically with ConfigGenerator.fsx 
    If you want to update the configuration, please modify ConfigGenerator.fsx and execute 
        fsi --exec ConfigGenerator.fsx
*)

module Config

    module Light = 

        [<Literal>]
        let MAX_PATCH_IDX_BUFFER_SIZE_ALL_LIGHT = 16
                      
        [<Literal>]
        let MAX_PATCH_IDX_BUFFER_SIZE_PER_LIGHT = 16
                      
        [<Literal>]
        let MAX_PATCH_SIZE = 4
                      
        [<Literal>]
        let MAX_PATCH_SIZE_MINUS_ONE = 3
                      
        [<Literal>]
        let MAX_PATCH_SIZE_PLUS_ONE = 5
                      
        [<Literal>]
        let MAX_PATCH_SIZE_PLUS_THREE = 7
                      
        [<Literal>]
        let MAX_PATCH_SIZE_PLUS_TWO = 6
                      
        [<Literal>]
        let MAX_PATCH_SIZE_TIMES_TWO = 8
                      
        [<Literal>]
        let MAX_RENDER_IDX_BUFFER_SIZE_ALL_LIGHT = 12
                      
        [<Literal>]
        let MAX_RENDER_IDX_BUFFER_SIZE_PER_LIGHT = 12
                      
        [<Literal>]
        let NUM_LIGHTS = 1
                      
        [<Literal>]
        let NUM_SAMPLES = 8
                      
        [<Literal>]
        let SS_LIGHT_SAMPLES_ALL_LIGHT = 400
                      
        [<Literal>]
        let SS_LIGHT_SAMPLES_PER_LIGHT = 400
                      
        [<Literal>]
        let VERT_ALL_LIGHT = 4
                      
        [<Literal>]
        let VERT_PER_LIGHT = 4
                      
        [<Literal>]
        let VERT_PER_LIGHT_PLUS_ONE = 5
                      
    module Delaunay = 

        [<Literal>]
        let CASE_CORNER = 1
                        
        [<Literal>]
        let CASE_CORNER_OFFSET = 0
                        
        [<Literal>]
        let CASE_EDGE = 2
                        
        [<Literal>]
        let CASE_EDGE_OFFSET = 1
                        
        [<Literal>]
        let CASE_INSIDE = 4
                        
        [<Literal>]
        let CASE_INSIDE_OFFSET = 2
                        
        [<Literal>]
        let MAX_EDGES = 8
                        
        [<Literal>]
        let MAX_EDGES_ALL = 24
                        
        [<Literal>]
        let MAX_EDGES_ALL_HALF = 12
                        
        [<Literal>]
        let MAX_EDGES_HALF = 4
                        
        [<Literal>]
        let MAX_FACES = 4
                        
        [<Literal>]
        let MAX_FACES_ALL = 12
                        
        [<Literal>]
        let MAX_FACES_ALL_HALF = 6
                        
        [<Literal>]
        let MAX_FACES_HALF = 2
                        
        [<Literal>]
        let NUM_CASE = 3
                        