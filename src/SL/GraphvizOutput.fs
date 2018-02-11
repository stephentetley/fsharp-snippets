module SL.GraphvizOutput


open System.IO

// Graphviz Monad
// Output is to a handle so this is not really a writer monad
// (all output must be sequential)


// Maybe have indent as an int...
// StreamWriter or StringWriter?

// type Indent = int
type Config = 
    { Indent : int
      EdgeOp: string }

type GraphvizOutput<'a> = 
    GraphvizOutput of (Config -> StringWriter -> 'a)

let inline private apply1 (ma : GraphvizOutput<'a>) (config:Config)  (handle:StringWriter) : 'a = 
    let (GraphvizOutput f) = ma in f config handle

let inline private unitM (x:'a) : GraphvizOutput<'a> = GraphvizOutput (fun _ _ -> x)


let inline private bindM (ma:GraphvizOutput<'a>) (f : 'a -> GraphvizOutput<'b>) : GraphvizOutput<'b> =
    GraphvizOutput (fun r sw -> let a = apply1 ma r sw in apply1 (f a) r sw)

let fail : GraphvizOutput<'a> = GraphvizOutput (fun _ _ -> failwith "GraphvizOutput fail")


type GraphvizOutputBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let (graphvizOutput:GraphvizOutputBuilder) = new GraphvizOutputBuilder()


// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:GraphvizOutput<'a>) : GraphvizOutput<'b> = 
    GraphvizOutput <| fun (config:Config) (sw:StringWriter) ->
        let ans = apply1 ma config sw in fn ans

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
        let ans = f { Indent=0; EdgeOp="->" } handle
        System.IO.File.WriteAllText(outputPath, handle.ToString())
        ans

let runGraphvizOutputConsole (ma:GraphvizOutput<'a>) : 'a = 
    use handle : System.IO.StringWriter = new System.IO.StringWriter()
    match ma with 
    | GraphvizOutput(f) -> 
        let ans = f { Indent=0; EdgeOp="->" } handle
        printfn "%s" <| handle.ToString()
        ans

/// This is too low level to expose.
let private askConfig : GraphvizOutput<Config> = 
    GraphvizOutput <| fun config _ -> config

/// This is too low level to expose.
let private asksConfig (extract:Config -> 'a) : GraphvizOutput<'a> = 
    GraphvizOutput <| fun config _ -> extract config

/// This is too low level to expose.
let private local (project:Config -> Config) (ma:GraphvizOutput<'a>) : GraphvizOutput<'a> = 
    GraphvizOutput <| fun config sw -> apply1 ma (project config) sw
        
/// This is too low level to expose.
let private tellLine (line:string) : GraphvizOutput<unit> = 
    GraphvizOutput <| fun config sw -> 
        let prefix = String.replicate config.Indent " "
        sw.WriteLine (sprintf "%s%s" prefix line )

/// Same as tellLine but the string is suffixed with ";".
let tellStatement (line:string) : GraphvizOutput<unit> = tellLine <| sprintf "%s;" line

let indent (body:GraphvizOutput<'a>) : GraphvizOutput<'a> = 
    GraphvizOutput <| fun config sw -> 
        let indent = config.Indent
        apply1 body { config with Indent=indent+4 } sw


let nested (initial:string) (body:GraphvizOutput<'a>) : GraphvizOutput<'a> =
    let line1 = 
        match initial with 
        | "" -> "{"
        | _ -> sprintf "%s {" initial
    graphvizOutput {
        do! tellLine line1
        let! ans = indent body
        do! tellLine "}"
        return ans }

type ValueRep = QUOTED | UNQUOTED

type Attribute =
    | Unquoted of string * string
    | Quoted of string * string

let showAttribute (attrib:Attribute) : string = 
    match attrib with
    | Unquoted(n,v) -> sprintf "%s=%s" n v
    | Quoted(n,v) -> sprintf "%s=\"%s\"" n v

let showAttributeList (xs:Attribute list) = 
    sprintf "[%s]" << String.concat "," <| List.map showAttribute xs

let tellStmt (name:string) (value:string) (rep:ValueRep) : GraphvizOutput<unit> =
    let attrib = 
        match rep with
        | QUOTED -> Quoted(name,value) 
        | UNQUOTED -> Unquoted(name,value)
    tellStatement <| showAttribute attrib

let comment (text:string) : GraphvizOutput<unit> =
    let lines = text.Split [|'\n'|] |> Array.toList
    forMz lines <| (tellLine << sprintf "// %s")

let graph (name:string) (body:GraphvizOutput<'a>) : GraphvizOutput<'a> =
    local (fun x -> {x with EdgeOp="--"}) <| nested (sprintf "graph %s" name) body

let digraph (name:string) (body:GraphvizOutput<'a>) : GraphvizOutput<'a> =
    local (fun x -> {x with EdgeOp="->"}) <| nested (sprintf "digraph %s" name) body


let subgraph (name:string) (body:GraphvizOutput<'a>) : GraphvizOutput<'a> =
    nested (sprintf "subgraph %s" name) body

let anonSubgraph (body:GraphvizOutput<'a>) : GraphvizOutput<'a> = nested "" body

/// cat should be one of "graph", "node","edge"
let attrStmt (cat:string) (attribs:Attribute list) : GraphvizOutput<unit> =
    tellStatement <| sprintf "%s %s" cat (showAttributeList attribs)

type Rankdir = TB | LR

let rankdir (dir:Rankdir) : GraphvizOutput<unit> = 
    match dir with
    | TB -> tellStmt "rankdir" "TB" UNQUOTED
    | LR -> tellStmt "rankdir" "LR" UNQUOTED

let ranksep (sep:float) : GraphvizOutput<unit> = tellStmt "ranksep" (sprintf "%.2f" sep) UNQUOTED

let rank (mode:string) : GraphvizOutput<unit> = tellStmt "ranksep" mode UNQUOTED

let node (id:string) (attribs:Attribute list) : GraphvizOutput<unit> =
    match attribs with
    | [] -> tellStatement id
    |_ -> tellStatement <| sprintf "%s %s" id (showAttributeList attribs)

let edge (id1:string) (id2:string) (attribs:Attribute list) : GraphvizOutput<unit> =
    graphvizOutput { 
        let! edgeOp = asksConfig (fun x -> x.EdgeOp)
        match attribs with
        | [] -> do! tellStatement <| sprintf "%s %s %s" id1 edgeOp id2
        |_ -> do! tellStatement <| sprintf "%s %s %s %s" id1 edgeOp id2 (showAttributeList attribs)
        }

let edges(id1:string) (ids:string list) (attribs:Attribute list) : GraphvizOutput<unit> =
    graphvizOutput { 
        let! edgeOp = fmapM (sprintf " %s ") <| asksConfig (fun x -> x.EdgeOp)
        let path = String.concat edgeOp (id1::ids)
        match attribs with
        | [] -> do! tellStatement path
        |_ -> do! tellStatement <| sprintf "%s %s" path (showAttributeList attribs)
        }


let nodeAttributes (attribs:Attribute list) : GraphvizOutput<unit> = attrStmt "node" attribs

let fontname (name:string) : Attribute = Quoted ("fontname", name)
let fontsize (size:int) : Attribute = Unquoted ("fontsize", size.ToString())
let label (value:string) : Attribute = Quoted ("label", value)
let shape (value:string) : Attribute = Unquoted ("shape", value)