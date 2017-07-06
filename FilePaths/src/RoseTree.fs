[<RequireQualifiedAccess>]
module FilePaths.RoseTree

open System.Text


type 'T Tree = 
    | Tree of 'T * Tree<'T> list


let drawFlat (fn:'T -> string) (sep:string) (t1:Tree<'T>) : string = 
    let sb = new StringBuilder ()
    let rec child prefix t1 = 
        match t1 with
        | Tree(x,xs) -> let prefix1 = sprintf "%s%s%s" prefix sep (fn x)
                        ignore <| sb.AppendLine(prefix1)
                        descend prefix1 xs
    and descend prefix kids = 
        match kids with 
        | [] -> ()
        | z::zs -> child prefix z
                   descend prefix zs
    match t1 with
    | Tree(a,xs) -> let prefix = fn a
                    ignore <| sb.AppendLine(prefix)
                    descend prefix xs
                    sb.ToString ()


                     




