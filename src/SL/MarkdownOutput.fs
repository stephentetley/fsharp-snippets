// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module SL.MarkdownOutput

open System.IO

open SL.FormatCombinators

// MarkdownOutput Monad
// Output is to a handle so this is not really a writer monad
// (all output must be sequential)

type MarkdownOutput<'a> = MarkdownOutput of (StringWriter -> 'a)

let inline private apply1 (ma : MarkdownOutput<'a>) (handle:StringWriter) : 'a = 
    let (MarkdownOutput f) = ma in f handle

let inline private mdreturn (x:'a) : MarkdownOutput<'a> = MarkdownOutput (fun r -> x)


let inline private bindM (ma:MarkdownOutput<'a>) (f : 'a -> MarkdownOutput<'b>) : MarkdownOutput<'b> =
    MarkdownOutput (fun r -> let a = apply1 ma r in apply1 (f a) r)

// Hard failure (exception) , the monad has real notion of failure
let fail (msg:string) : MarkdownOutput<'a> = MarkdownOutput (fun r -> failwith msg)


type MarkdownOutputBuilder() = 
    member self.Return x    = mdreturn x
    member self.Bind (p,f)  = bindM p f
    member self.Zero ()     = fail "Zero"

let (markdownOutput:MarkdownOutputBuilder) = new MarkdownOutputBuilder()


// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:MarkdownOutput<'a>) : MarkdownOutput<'b> = 
    MarkdownOutput <| fun (handle:StringWriter) ->
        let ans = apply1 ma handle in fn ans

let mapM (fn:'a -> MarkdownOutput<'b>) (xs:'a list) : MarkdownOutput<'b list> = 
    let rec work ac ys = 
        match ys with
        | [] -> mdreturn <| List.rev ac
        | z :: zs -> bindM (fn z) (fun a -> work (a::ac) zs)
    work [] xs

let forM (xs:'a list) (fn:'a -> MarkdownOutput<'b>) : MarkdownOutput<'b list> = mapM fn xs

let mapMz (fn:'a -> MarkdownOutput<'b>) (xs:'a list) : MarkdownOutput<unit> = 
    let rec work ys = 
        match ys with
        | [] -> mdreturn ()
        | z :: zs -> bindM (fn z) (fun _ -> work zs)
    work xs

let forMz (xs:'a list) (fn:'a -> MarkdownOutput<'b>) : MarkdownOutput<unit> = mapMz fn xs

let traverseM (fn: 'a -> MarkdownOutput<'b>) (source:seq<'a>) : MarkdownOutput<seq<'b>> = 
    MarkdownOutput <| fun handle ->
        Seq.map (fun x -> let mf = fn x in apply1 mf handle) source

// Need to be strict - hence use a fold
let traverseMz (fn: 'a -> MarkdownOutput<'b>) (source:seq<'a>) : MarkdownOutput<unit> = 
    MarkdownOutput <| fun handle ->
        Seq.fold (fun ac x -> 
                    let ans  = apply1 (fn x) handle in ac) 
                 () 
                 source 



let runMarkdownOutput (outputFile:string) (ma:MarkdownOutput<'a>) : 'a = 
    use handle : StringWriter = new StringWriter()
    match ma with 
    | MarkdownOutput(f) -> 
        let ans = f handle
        File.WriteAllText(path = outputFile, contents= handle.ToString()) |> ignore
        ans 

let runMarkdownOutputConsole (ma:MarkdownOutput<'a>) : 'a = 
    use handle : StringWriter = new StringWriter()
    match ma with 
    | MarkdownOutput(f) ->
        let ans = f handle
        printfn "----------"
        printfn "%s" (handle.ToString ())
        printfn "----------"
        ans 


type Markdown = Doc


let tellMarkdown (md:Markdown) : MarkdownOutput<unit> = 
    MarkdownOutput <| fun (handle:StringWriter) ->
        handle.WriteLine (render md)

let (@@@) (d1:Markdown) (d2:Markdown) : Markdown = d1 +^+ d2
let (@@) (d1:Markdown) (d2:Markdown) : Markdown = d1 +++ d2


let h1 (text:string ) : Markdown = 
    char '#' +^+ string text

let h2 (text:string ) : Markdown = 
    string "##" +^+ string text

let h3 (text:string ) : Markdown = 
    string "##" +^+ string text

let plaintext (text:string) : Markdown = 
    string text

let asterisks (doc:Markdown) : Markdown = 
    enclose (char '*') (char '*') doc


let underscores (doc:Markdown) : Markdown = 
    enclose (char '_') (char '_') doc