module SL.GraphvizOutput


open System.IO

// Graphviz Monad
// Output is to a handle so this is not really a writer monad
// (all output must be sequential)


// Maybe have indent as an int...
// StreamWriter or StringWriter?

type Indent = int

type GraphvizOutput<'a> = 
    GraphvizOutput of (StringWriter -> Indent-> 'a)

let inline private apply1 (ma : GraphvizOutput<'a>) (handle:StringWriter) (indent:Indent) : 'a = 
    let (GraphvizOutput f) = ma in f handle indent

let inline private unitM (x:'a) : GraphvizOutput<'a> = GraphvizOutput (fun _ _ -> x)


let inline private bindM (ma:GraphvizOutput<'a>) (f : 'a -> GraphvizOutput<'b>) : GraphvizOutput<'b> =
    GraphvizOutput (fun r i -> let a = apply1 ma r i in apply1 (f a) r i)

let fail : GraphvizOutput<'a> = GraphvizOutput (fun _ _ -> failwith "GraphvizOutput fail")


type GraphvizOutputBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let (graphvizOutput:GraphvizOutputBuilder) = new GraphvizOutputBuilder()


// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:GraphvizOutput<'a>) : GraphvizOutput<'b> = 
    GraphvizOutput <| fun (handle:StringWriter) (indent:Indent) ->
        let ans = apply1 ma handle indent in fn ans

let mapM (fn:'a -> GraphvizOutput<'b>) (xs:'a list) : GraphvizOutput<'b list> = 
    let rec work ac ys = 
        match ys with
        | [] -> unitM <| List.rev ac
        | z :: zs -> bindM (fn z) (fun a -> work (a::ac) zs)
    work [] xs

let forM (xs:'a list) (fn:'a -> GraphvizOutput<'b>) : GraphvizOutput<'b list> = mapM fn xs

let mapMz (fn:'a -> GraphvizOutput<'b>) (xs:'a list) : GraphvizOutput<unit> = 
    let rec work ys = 
        match ys with
        | [] -> unitM ()
        | z :: zs -> bindM (fn z) (fun _ -> work zs)
    work xs

let forMz (xs:'a list) (fn:'a -> GraphvizOutput<'b>) : GraphvizOutput<unit> = mapMz fn xs

let traverseM (fn: 'a -> GraphvizOutput<'b>) (source:seq<'a>) : GraphvizOutput<seq<'b>> = 
    GraphvizOutput <| fun handle indent ->
        Seq.map (fun x -> let mf = fn x in apply1 mf handle indent) source

// Need to be strict - hence use a fold
let traverseMz (fn: 'a -> GraphvizOutput<'b>) (source:seq<'a>) : GraphvizOutput<unit> = 
    GraphvizOutput <| fun handle indent ->
        Seq.fold (fun ac x -> 
                    let ans  = apply1 (fn x) handle indent in ac) 
                 () 
                 source 

// GraphvizOutput-specific operations

let runGraphvizOutput (ma:GraphvizOutput<'a>) (outputPath:string) : 'a = 
    use handle : System.IO.StringWriter = new System.IO.StringWriter()
    match ma with 
    | GraphvizOutput(f) -> 
        let ans = f handle 0
        System.IO.File.WriteAllText(outputPath, handle.ToString())
        ans

let runGraphvizOutputConsole (ma:GraphvizOutput<'a>) : 'a = 
    use handle : System.IO.StringWriter = new System.IO.StringWriter()
    match ma with 
    | GraphvizOutput(f) -> 
        let ans = f handle 0
        printfn "%s" <| handle.ToString()
        ans


/// TODO this is too low level to expose...
let tellLine (line:string) : GraphvizOutput<unit> = 
    GraphvizOutput <| fun handle indent -> 
        let prefix = String.replicate indent " "
        handle.WriteLine (sprintf "%s%s" prefix line )

/// Same as tellLine but the string is suffixed with ";".
let tellStatement (line:string) : GraphvizOutput<unit> = tellLine <| sprintf "%s;" line

let indent (body:GraphvizOutput<'a>) : GraphvizOutput<'a> = 
    GraphvizOutput <| fun handle indent -> apply1 body handle (indent+4)


let nested (initial:string) (body:GraphvizOutput<'a>) : GraphvizOutput<'a> =
    graphvizOutput {
        do! tellLine (sprintf "%s {" initial)
        let! ans = indent body
        do! tellLine "}"
        return ans }

type Attribute =
    | Unquoted of string * string
    | Quoted of string * string

let showAttribute (attrib:Attribute) : string = 
    match attrib with
    | Unquoted(n,v) -> sprintf "%s=%s" n v
    | Quoted(n,v) -> sprintf "%s=\"%s\"" n v

let showAttributeList (xs:Attribute list) = 
    sprintf "[%s]" << String.concat "," <| List.map showAttribute xs

let tellStmt (name:string) (value:string) (quoted:bool) : GraphvizOutput<unit> =
    let attrib = if quoted then Quoted(name,value) else Unquoted(name,value)
    tellStatement <| showAttribute attrib

let comment (text:string) : GraphvizOutput<unit> =
    let lines = text.Split [|'\n'|] |> Array.toList
    forMz lines <| (tellLine << sprintf "// %s")

let graph (name:string) (body:GraphvizOutput<'a>) : GraphvizOutput<'a> =
    nested (sprintf "graph %s" name) body

let digraph (name:string) (body:GraphvizOutput<'a>) : GraphvizOutput<'a> =
    nested (sprintf "digraph %s" name) body


/// cat should be one of "graph", "node","edge"
let attrStmt (cat:string) (attribs:Attribute list) : GraphvizOutput<unit> =
    tellStatement <| sprintf "%s %s" cat (showAttributeList attribs)


let rankdirLR : GraphvizOutput<unit> = tellStmt "rankdir" "LR" false
let rankdirTB : GraphvizOutput<unit> = tellStmt "rankdir" "TB" false

let ranksep (sep:float) = tellStmt "ranksep" (sprintf "%.2f" sep) false

let node (id:string) (attribs:Attribute list) : GraphvizOutput<unit> =
    match attribs with
    | [] -> tellStatement id
    |_ -> tellStatement <| sprintf "%s %s" id (showAttributeList attribs)

let edge (id1:string) (id2:string) (attribs:Attribute list) : GraphvizOutput<unit> =
    match attribs with
    | [] -> tellStatement <| sprintf "%s -> %s" id1 id2
    |_ -> tellStatement <| sprintf "%s -> %s %s" id1 id2 (showAttributeList attribs)

let nodeAttributes (attribs:Attribute list) : GraphvizOutput<unit> = attrStmt "node" attribs

let fontname (name:string) : Attribute = Quoted ("fontname", name)
let fontsize (size:int) : Attribute = Unquoted ("fontsize", size.ToString())
let label (value:string) : Attribute = Quoted ("label", value)
let shape (value:string) : Attribute = Quoted ("shape", value)