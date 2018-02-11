#load @"SL\GraphvizOutput.fs"
open SL.GraphvizOutput

let test01 () : unit = 
    let procM = 
        graphvizOutput { 
            do! lineComment "dot -Tjpeg mydot.dot -o mydot.jpg"
            do! digraph "G1" <| 
                graphvizOutput { 
                    do! attrib <| rankdir LR
                    do! attrib <| ranksep 1.5
                    do! nodeAttributes [fontname "Arial"; fontsize 10]
                    do! anonSubgraph 
                        <| graphvizOutput { 
                            do! node "node1" []
                            do! node "node2" []
                            do! node "node3" []
                            do! edge "node1" "node2" []
                            do! edges "node3" ["node2";"node1"] []
                            return ()
                            } 
                }
            }
    ignore <| runGraphvizOutputConsole procM

