#load "RoseTree.fs"

open FilePaths

let tree1 : RoseTree.Tree<string> = 
    RoseTree.Tree("root", [ RoseTree.Tree("branch1", [])
                          ; RoseTree.Tree("branch2", [])
                          ])



let test01 () = printfn "%s" <| RoseTree.drawFlat id "\\" tree1


