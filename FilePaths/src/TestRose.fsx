#load "RoseTree.fs"

open System.Text
open FilePaths

let tree1 : RoseTree.Tree<string> = 
    RoseTree.Tree("root", [ RoseTree.Tree("branch1", [])
                          ; RoseTree.Tree("branch2", [])
                          ])



let test01 () = printfn "%s" <| RoseTree.drawFlat id "\\" tree1




let pathSplit1 (sep:char) (ss:string) : string*string = 
    let parts = ss.Split(sep)
    let len = parts.Length
    let node = parts.[len-1]
    let parent = let sb = new StringBuilder ()
                 ignore <| sb.Append(parts.[0])
                 for i = 1 to len-2 do
                    Printf.bprintf sb "%c%s" sep parts.[i]
                 sb.ToString ()
    (parent,node)


let dummy () = let ss = @"root\branch\sub\subsub" in pathSplit1 '\\' ss
