// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module SL.SwOutput

open System.IO

type SwOutput<'a> = SwOutput of (StringWriter -> 'a)


let inline private apply1 (ma : SwOutput<'a>) (handle:StringWriter) : 'a = 
    match ma with | SwOutput fn -> fn handle

let private unitM (x:'a) : SwOutput<'a> = 
    SwOutput <| fun handle -> x

let private bindM (ma:SwOutput<'a>) (f : 'a -> SwOutput<'b>) : SwOutput<'b> =
    SwOutput <| fun handle -> 
        let a = apply1 ma handle in apply1 (f a) handle


type SwOutputBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let swOutput:SwOutputBuilder = new SwOutputBuilder()

// Common operations
let fmapM (fn:'a -> 'b) (ma:SwOutput<'a>) : SwOutput<'b> = 
    SwOutput <| fun handle ->
        let a = apply1 ma handle in fn a


let mapM (fn: 'a -> SwOutput<'b>) (xs: 'a list) : SwOutput<'b list> = 
    let rec work ac list = 
        match list with
        | y :: ys -> bindM (fn y) (fun b -> work (b::ac) ys)
        | [] -> unitM <| List.rev ac
    work [] xs

let forM (xs:'a list) (fn:'a -> SwOutput<'b>) : SwOutput<'b list> = mapM fn xs


let mapMz (fn: 'a -> SwOutput<'b>) (xs: 'a list) : SwOutput<unit> = 
    let rec work list = 
        match list with
        | y :: ys -> bindM (fn y) (fun _ -> work ys)
        | [] -> unitM ()
    work xs

let forMz (xs:'a list) (fn:'a -> SwOutput<'b>) : SwOutput<unit> = mapMz fn xs

let traverseM (fn: 'a -> SwOutput<'b>) (source:seq<'a>) : SwOutput<seq<'b>> = 
    SwOutput <| fun handle ->
        Seq.map (fun x -> let mf = fn x in apply1 mf handle) source


let traverseiM (fn: int ->  'a -> SwOutput<'b>) (source:seq<'a>) : SwOutput<seq<'b>> = 
    SwOutput <| fun handle ->
        Seq.mapi (fun ix x -> let mf = fn ix x in apply1 mf handle) source


// Need to be strict - hence use a fold
let traverseMz (fn: 'a -> SwOutput<'b>) (source:seq<'a>) : SwOutput<unit> = 
    SwOutput <| fun handle ->
        Seq.fold (fun ac x -> 
                    let ans  = apply1 (fn x) handle in ac) 
                 () 
                 source 

let traverseiMz (fn: int -> 'a -> SwOutput<'b>) (source:seq<'a>) : SwOutput<unit> = 
    SwOutput <| fun handle ->
        ignore <| Seq.fold (fun ix x -> 
                            let ans  = apply1 (fn ix x) handle in (ix+1))
                            0
                            source 

let mapiM (fn: 'a -> int -> SwOutput<'b>) (xs: 'a list) : SwOutput<'b list> = 
    let rec work ac ix list = 
        match list with
        | y :: ys -> bindM (fn y ix) (fun b -> work (b::ac) (ix+1) ys)
        | [] -> unitM <| List.rev ac
    work [] 0 xs

let mapiMz (fn: 'a -> int -> SwOutput<'b>) (xs: 'a list) : SwOutput<unit> = 
    let rec work ix list = 
        match list with
        | y :: ys -> bindM (fn y ix) (fun _ -> work (ix+1) ys)
        | [] -> unitM ()
    work 0 xs

// SwOutput-specific operations

let runSwOutput (ma:SwOutput<'a>) : string * 'a =
    use sw = new System.IO.StringWriter()
    let ans = apply1 ma sw
    let output = sw.ToString()
    (output,ans)

let runSwOutputFile (outputPath:string) (ma:SwOutput<'a>) : 'a = 
    let (text,ans) = runSwOutput ma
    System.IO.File.WriteAllText(outputPath, text)
    ans

let runSwOutputConsole (ma:SwOutput<'a>) : 'a = 
    let (text,ans) = runSwOutput ma
    printfn "%s" text
    ans


let tellLine (line:string) : SwOutput<unit> =
    SwOutput <| fun handle ->
        handle.WriteLine line
