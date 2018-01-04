
open System

let text1 : string = @"UNNAMED/XYZ/PROCESS CATEGORY/PROCESS NAME/EQUIPMENT: ULTRASONIC LEVEL INSTRUMENT"

let prefix (ss : string) : string = 
    let temp  = ss.Split ('/') |> Array.toList
    match temp with
    | a::b::_  -> a + "/" + b
    | _ -> ""


let suffix (ss : string) : string list = 
    let temp  = ss.Split ('/') |> Array.toList
    match temp with
    | _::_::rest  -> rest
    | _ -> []

let test01 () = prefix text1
let test02 () = suffix text1