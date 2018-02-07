module SL.GraphvizOutput


open System.IO

// Graphviz Monad
// Output is to a handle so this is not really a writer monad
// (all output must be sequential)


// Maybe have indent as an int...
// StreamWriter or StringWriter
type GraphvizOutput<'a> = 
    GraphvizOutput of (StreamWriter -> 'a)

let inline private apply1 (ma : GraphvizOutput<'a>) (handle:StreamWriter) : 'a = 
    let (GraphvizOutput f) = ma in f handle

let inline private unitM (x:'a) : GraphvizOutput<'a> = GraphvizOutput (fun r -> x)


let inline private bindM (ma:GraphvizOutput<'a>) (f : 'a -> GraphvizOutput<'b>) : GraphvizOutput<'b> =
    GraphvizOutput (fun r -> let a = apply1 ma r in apply1 (f a) r)

let fail : GraphvizOutput<'a> = GraphvizOutput (fun r -> failwith "GraphvizOutput fail")


type GraphvizOutputBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let (graphvizOutput:GraphvizOutputBuilder) = new GraphvizOutputBuilder()


// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:GraphvizOutput<'a>) : GraphvizOutput<'b> = 
    GraphvizOutput <| fun (handle:StreamWriter) ->
        let ans = apply1 ma handle in fn ans

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
    GraphvizOutput <| fun handle ->
        Seq.map (fun x -> let mf = fn x in apply1 mf handle) source

// Need to be strict - hence use a fold
let traverseMz (fn: 'a -> GraphvizOutput<'b>) (source:seq<'a>) : GraphvizOutput<unit> = 
    GraphvizOutput <| fun handle ->
        Seq.fold (fun ac x -> 
                    let ans  = apply1 (fn x) handle in ac) 
                 () 
                 source 

// GraphvizOutput-specific operations

let runGraphvizOutput (ma:GraphvizOutput<'a>) (outputPath:string) : 'a = 
    use handle : System.IO.StreamWriter = new System.IO.StreamWriter(outputPath)
    match ma with | GraphvizOutput(f) -> f handle



