#load @"SL\GraphvizOutput.fs"
open SL.GraphvizOutput

let test01 () = 
    let outPath = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..","data/graph1.dot")
    let procM = 
        graph "graph1" <| 
            graphvizOutput { 
                do! tellLine "node1"
                return () 
                }
    ignore <| runGraphvizOutput procM outPath

