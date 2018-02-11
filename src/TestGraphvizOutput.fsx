#load @"SL\GraphvizOutput.fs"
open SL.GraphvizOutput

let test01 () = 
    let procM = 
        graphvizOutput { 
            do! comment "dot -Tjpeg mydot.dot -o mydot.jpg"
            do! digraph "G1" <| 
                graphvizOutput { 
                
                    do! rankdirLR
                    do! ranksep 1.5
                    do! nodeAttributes [fontname "Arial"; fontsize 10]
                    do! node "node1" []
                    return () 
                }
            }
    ignore <| runGraphvizOutputConsole procM

