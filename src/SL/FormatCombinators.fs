// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module SL.FormatCombinators


open System.Text
open System

/// This is not a "pretty printer" as it makes no effort to "fit" the output.
type Doc = 
    | Empty
    | Doc of string
    | HDoc of Doc * Doc
    | VDoc of Doc * Doc
    | Indent of int * Doc
    
    


let render (source:Doc) : string = 
    let rec work (doc:Doc) (indent:int) (sb:StringBuilder) : StringBuilder = 
        match doc with
        | Empty -> sb
        | Doc str -> sb.Append(str)
        | HDoc(d1,d2) -> 
            let sb1 = work d1 indent sb in work d2 indent sb1
        | VDoc(d1,d2) -> 
            let sb1 = work d1 indent sb
            let sb2 = sb1 // sb1.Append(String.replicate indent " ")
            let sb3 = sb2.Append("\n" + String.replicate indent " ")
            work d2 indent sb3
        | Indent(i,d1) -> 
            let sb1 = sb.Append(String.replicate i " ")
            work d1 (indent + i) sb1
    work source 0 (new StringBuilder()) |> fun sb -> sb.ToString()

/// Print the soc to the console.
let testRender (source:Doc) : unit = 
    render source |> printfn  "----------\n%s\n----------\n"

// *************************************
// Primitive values

let empty : Doc = 
    Empty


let bool (value:bool) : Doc = 
    Doc <| if value then "true" else "false"


let int (i:int) : Doc = 
    Doc(i.ToString())

let float (d:float) : Doc = 
    Doc(d.ToString())

let double (d:double) : Doc = 
    Doc(d.ToString())
    
let decimal (d:Decimal) : Doc = 
    Doc(d.ToString())



let char (ch:char) : Doc = 
    Doc (ch.ToString())


let string (value:string) : Doc = 
    Doc <| value

let singleQuoted (value:string) : Doc = 
    Doc << sprintf "'%s'" <| value.Replace("'","''")

let doubleQuoted (value:string) : Doc = 
    Doc <| sprintf "\"%s\"" value


// *************************************
// Character documents

/// A single space
let space = char ' '

let dot = char '.'
let semi = char ';'
let colon = char ':'
let comma = char ','
let backslash = char '\\'
let forwardslash = char '/'

let underscore : Doc = char '_'




// *************************************
// Combinators

let indent (i:int) (d:Doc) = 
    Indent(i,d)

/// Horizontal concat
let (+++) (d1:Doc) (d2:Doc) : Doc = 
    HDoc(d1,d2)

/// Horizontal concat with a separating space 
let (+^+) (d1:Doc) (d2:Doc) : Doc = 
   d1 +++ space +++ d2


/// Vertical concat
let (@@@) (d1:Doc) (d2:Doc) : Doc = 
    VDoc(d1,d2)

/// Vertical concat with a separating blank line 
let (@^@) (d1:Doc) (d2:Doc) : Doc = 
   (d1 @@@ empty) @@@ d2


let concat (operator:Doc -> Doc -> Doc) (source:Doc list) : Doc = 
    let rec work (ac:Doc) (xs:Doc list) : Doc = 
        match xs with 
        | [] -> ac
        | [y] -> operator ac y
        | y :: ys -> work (operator ac y) ys
    match source with 
    | [] -> Empty
    | x :: xs -> work x xs


let hcat (source:Doc list) : Doc = concat (+++) source
let hsep (source:Doc list) : Doc = concat (+^+) source
    
let vcat (source:Doc list) : Doc = concat (@@@) source


let punctuate (sep:Doc) (source:Doc list) : Doc = 
    let rec work (ac:Doc) (xs:Doc list) : Doc = 
        match xs with 
        | [] -> ac
        | [y] -> (ac +++ sep) +++ y
        | y :: ys -> work ((ac +++ sep) +++ y) ys
    match source with 
    | [] -> Empty
    | x :: xs -> work x xs

let punctuateVertically (sep:Doc) (source:Doc list) : Doc = 
    let rec work (ac:Doc) (xs:Doc list) : Doc = 
        match xs with 
        | [] -> ac
        | [y] -> (ac +++ sep) @@@ y
        | y :: ys -> work ((ac +++ sep) @@@ y) ys
    match source with 
    | [] -> Empty
    | x :: xs -> work x xs

let enclose (left:Doc) (right:Doc) (d1:Doc) : Doc = 
    (left +++ d1) +++ right


let parens (doc:Doc) : Doc = 
    enclose (char '(') (char ')') doc

let angles (doc:Doc) : Doc = 
    enclose (char '<') (char '>') doc

let squares (doc:Doc) : Doc = 
    enclose (char '[') (char ']') doc

let braces (doc:Doc) : Doc = 
    enclose (char '{') (char '}') doc



let tupled (source:Doc list) : Doc = 
    parens (punctuate (string ", ") source)

let commaSepList (source: Doc list) : Doc = 
    squares (punctuate (string ", ") source)

let semiSepList (source: Doc list) : Doc = 
    squares (punctuate (string "; ") source)


let commaSepListVertically (source:Doc list) : Doc = 
    squares (punctuateVertically (char ',') source)


