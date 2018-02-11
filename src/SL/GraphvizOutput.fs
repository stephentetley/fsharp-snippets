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

let showAttribute (attr:Attribute) : string = 
    match attr with
    | Unquoted(n,v) -> sprintf "%s=%s" n v
    | Quoted(n,v) -> sprintf "%s=\"%s\"" n v

let showAttributeList (attrs:Attribute list) = 
    sprintf "[%s]" << String.concat "," <| List.map showAttribute attrs


let lineComment (text:string) : GraphvizOutput<unit> =
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
let attrStmt (cat:string) (attrs:Attribute list) : GraphvizOutput<unit> =
    tellStatement <| sprintf "%s %s" cat (showAttributeList attrs)

let attrib (attr:Attribute) : GraphvizOutput<unit> = 
    tellStatement <| showAttribute attr



let node (id:string) (attrs:Attribute list) : GraphvizOutput<unit> =
    match attrs with
    | [] -> tellStatement id
    |_ -> tellStatement <| sprintf "%s %s" id (showAttributeList attrs)

let edge (id1:string) (id2:string) (attrs:Attribute list) : GraphvizOutput<unit> =
    graphvizOutput { 
        let! edgeOp = asksConfig (fun x -> x.EdgeOp)
        match attrs with
        | [] -> do! tellStatement <| sprintf "%s %s %s" id1 edgeOp id2
        |_ -> do! tellStatement <| sprintf "%s %s %s %s" id1 edgeOp id2 (showAttributeList attrs)
        }

let edges(id1:string) (ids:string list) (attrs:Attribute list) : GraphvizOutput<unit> =
    graphvizOutput { 
        let! edgeOp = fmapM (sprintf " %s ") <| asksConfig (fun x -> x.EdgeOp)
        let path = String.concat edgeOp (id1::ids)
        match attrs with
        | [] -> do! tellStatement path
        |_ -> do! tellStatement <| sprintf "%s %s" path (showAttributeList attrs)
        }


let nodeAttributes (attrs:Attribute list) : GraphvizOutput<unit> = attrStmt "node" attrs

let makeBoolAttribute (name:string) (value:bool)  : Attribute = 
    let value1 = if value then "true" else "false" in Unquoted(name,value1)

let color (value:string) : Attribute            = Unquoted ("color", value)

let fillcolor (value:string) : Attribute        = Unquoted ("fillcolor", value)
let fixedsize (value:bool) : Attribute          = makeBoolAttribute "fixedsize" value
let fontname (name:string) : Attribute          = Quoted ("fontname", name)
let fontsize (size:int) : Attribute             = Unquoted ("fontsize", size.ToString())
let height (h:float) : Attribute                = Unquoted ("height", sprintf "%.2f" h)
let label (value:string) : Attribute            = Quoted ("label", value)
let regular (value:bool) : Attribute            = makeBoolAttribute "regular" value
let shape (value:string) : Attribute            = Unquoted ("shape", value)
let sides (count:int) : Attribute               = Unquoted ("sides", count.ToString())
let style (values:string list) : Attribute      = Quoted ("style", String.concat "," values)
let width (w:float) : Attribute                 = Unquoted ("width", sprintf "%.2f" w)

let arrowhead (value:string) : Attribute        = Unquoted ("arrowhead", value)
let arrowsize (size:float) : Attribute          = Unquoted ("arrowsize", sprintf "%.2f" size)
let arrowtail (value:string) : Attribute        = Unquoted ("arrowtail", value)

type EdgeDirection = FORWARD | BACK | BOTH | NONE

let dir (value:EdgeDirection) : Attribute = 
    let value1 = 
        match value with
        | FORWARD -> "forward"
        | BACK -> "back"
        | BOTH -> "both"
        | NONE -> "none"
    Unquoted ("dir", value1)

let labelfloat (value:bool) : Attribute         = makeBoolAttribute "labelfloat" value
let labelfontcolor (value:string) : Attribute   = Unquoted ("labelfontcolor", value)
let labelfontname (name:string) : Attribute     = Quoted ("labelfontname", name)
let labelfontsize (size:int) : Attribute        = Unquoted ("labelfontsize", size.ToString())

let center (value:bool) : Attribute             = makeBoolAttribute "center" value
let fontcolor (value:string) : Attribute        = Unquoted ("fontcolor", value)

type Rankdir = TB | LR

let rankdir (dir:Rankdir) : Attribute = 
    let dir1 = match dir with | TB -> "TB" | LR -> "LR"
    Unquoted("rankdir",dir1)

let ranksep (sep:float) : Attribute             = Unquoted("ranksep", sprintf "%.2f" sep)
let rank (mode:string) : Attribute              = Unquoted("ranksep", mode)

