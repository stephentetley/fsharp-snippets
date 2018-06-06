// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module SL.ScriptMonad

open System.IO

open SL.AnswerMonad
open SL.JsonExtractor

// Design Note:
// Ideally we would like an iterative way of outputting Csv but as we don't
// have monad transformers at our disposal I'm not sure how to do this.
// At the moment the solution seems to be generate a seq<RowWriter> and
// output it by lifting outputToNew and tellRows or writeRowsWithHeaders.



type LogAction = StringWriter -> unit


/// This is the default logger, writes the output to the console with printfn.
let consoleLogger : LogAction = fun sw -> printfn "--- Log: ----------\n%s" (sw.ToString())

type ScriptMonad<'r,'a> = private ScriptMonad of (StringWriter -> 'r -> Answer<'a>)


let inline private apply1 (ma : ScriptMonad<'r,'a>) (sw:StringWriter) (env:'r) : Answer<'a> = 
    let (ScriptMonad fn) = ma  in  fn sw env

let inline private unitM (x:'a) : ScriptMonad<'r,'a> = 
    ScriptMonad <| fun sw r -> answerMonad.Return x



let inline private bindM (ma:ScriptMonad<'r,'a>) (f : 'a -> ScriptMonad<'r,'b>) : ScriptMonad<'r,'b> =
    ScriptMonad <| fun sw env -> 
        answerMonad.Bind (apply1 ma sw env, fun a -> apply1 (f a) sw env)


let failM (msg:string)  : ScriptMonad<'r,'a> = 
    ScriptMonad <| fun sw env ->  AnswerMonad.Err msg


type ScriptBuilder() = 
    member self.Return x        = unitM x
    member self.Bind (p,f)      = bindM p f
    member self.Zero ()         = failM "Zero"



let (scriptMonad:ScriptBuilder) = new ScriptBuilder()


// Common monadic operations
let fmapM (fn:'a -> 'b) (ma:ScriptMonad<'r,'a>) : ScriptMonad<'r,'b> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.fmapM fn (apply1 ma sw env)

let liftM (fn:'a -> 'x) (ma:ScriptMonad<'r,'a>) : ScriptMonad<'r,'x> = fmapM fn ma

let liftM2 (fn:'a -> 'b -> 'x) (ma:ScriptMonad<'r,'a>) (mb:ScriptMonad<'r,'b>) : ScriptMonad<'r,'x> = 
    scriptMonad { 
        let! a = ma
        let! b = mb
        return (fn a b)
    }

let liftM3 (fn:'a -> 'b -> 'c -> 'x) (ma:ScriptMonad<'r,'a>) (mb:ScriptMonad<'r,'b>) (mc:ScriptMonad<'r,'c>) : ScriptMonad<'r,'x> = 
    scriptMonad { 
        let! a = ma
        let! b = mb
        let! c = mc
        return (fn a b c)
    }

let liftM4 (fn:'a -> 'b -> 'c -> 'd -> 'x) (ma:ScriptMonad<'r,'a>) (mb:ScriptMonad<'r,'b>) (mc:ScriptMonad<'r,'c>) (md:ScriptMonad<'r,'d>) : ScriptMonad<'r,'x> = 
    scriptMonad { 
        let! a = ma
        let! b = mb
        let! c = mc
        let! d = md
        return (fn a b c d)
    }

let liftM5 (fn:'a -> 'b -> 'c -> 'd -> 'e -> 'x) (ma:ScriptMonad<'r,'a>) (mb:ScriptMonad<'r,'b>) (mc:ScriptMonad<'r,'c>) (md:ScriptMonad<'r,'d>) (me:ScriptMonad<'r,'e>) : ScriptMonad<'r,'x>= 
    scriptMonad { 
        let! a = ma
        let! b = mb
        let! c = mc
        let! d = md
        let! e = me
        return (fn a b c d e)
    }
let tupleM2 (ma:ScriptMonad<'r,'a>) (mb:ScriptMonad<'r,'b>) : ScriptMonad<'r,'a * 'b> = 
    liftM2 (fun a b -> (a,b)) ma mb

let tupleM3 (ma:ScriptMonad<'r,'a>) (mb:ScriptMonad<'r,'b>) (mc:ScriptMonad<'r,'c>) : ScriptMonad<'r,'a * 'b * 'c> = 
    liftM3 (fun a b c -> (a,b,c)) ma mb mc

let tupleM4 (ma:ScriptMonad<'r,'a>) (mb:ScriptMonad<'r,'b>) (mc:ScriptMonad<'r,'c>) (md:ScriptMonad<'r,'d>) : ScriptMonad<'r,'a * 'b * 'c * 'd> = 
    liftM4 (fun a b c d -> (a,b,c,d)) ma mb mc md

let tupleM5 (ma:ScriptMonad<'r,'a>) (mb:ScriptMonad<'r,'b>) (mc:ScriptMonad<'r,'c>) (md:ScriptMonad<'r,'d>) (me:ScriptMonad<'r,'e>)  : ScriptMonad<'r,'a * 'b * 'c * 'd * 'e> = 
    liftM5 (fun a b c d e -> (a,b,c,d,e)) ma mb mc md me

// NOTE - FParsec defines flipped versions of liftM* (e.g. pipe2, pipe3, ...)

let mapM (fn:'a -> ScriptMonad<'r,'b>) (xs:'a list) : ScriptMonad<'r,'b list> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.mapM (fun x -> apply1 (fn x) sw env) xs


let forM (xs:'a list) (fn:'a -> ScriptMonad<'r,'b>) : ScriptMonad<'r,'b list> = 
    mapM fn xs

let mapMz (fn:'a -> ScriptMonad<'r,'b>) (xs:'a list) : ScriptMonad<'r,unit> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.mapMz (fun x -> apply1 (fn x) sw env) xs

let forMz (xs:'a list) (fn:'a -> ScriptMonad<'r,'b>) : ScriptMonad<'r,unit> = 
    mapMz fn xs


let mapiM (fn:int -> 'a -> ScriptMonad<'r,'b>) (xs:'a list) : ScriptMonad<'r,'b list> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.mapiM (fun ix x -> apply1 (fn ix x) sw env) xs

let mapiMz (fn:int -> 'a -> ScriptMonad<'r,'b>) (xs:'a list) : ScriptMonad<'r,unit> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.mapiMz (fun ix x -> apply1 (fn ix x) sw env) xs

let foriM (xs:'a list) (fn:int -> 'a -> ScriptMonad<'r,'b>) : ScriptMonad<'r,'b list> =
    mapiM fn xs

let foriMz (xs:'a list) (fn:int -> 'a -> ScriptMonad<'r,'b>) : ScriptMonad<'r,unit> =
    mapiMz fn xs


let traverseM (fn: 'a -> ScriptMonad<'r,'b>) (source:seq<'a>) :  ScriptMonad<'r,seq<'b>> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.traverseM (fun x -> apply1 (fn x) sw env) source

let traverseMz (fn: 'a -> ScriptMonad<'r,'b>) (source:seq<'a>) :  ScriptMonad<'r,unit> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.traverseMz (fun x -> apply1 (fn x) sw env) source

let traverseiM (fn:int -> 'a -> ScriptMonad<'r,'b>) (source:seq<'a>) :  ScriptMonad<'r,seq<'b>> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.traverseiM (fun ix x -> apply1 (fn ix x) sw env) source

let traverseiMz (fn:int -> 'a -> ScriptMonad<'r,'b>) (source:seq<'a>) :  ScriptMonad<'r,unit> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.traverseiMz (fun ix x -> apply1 (fn ix x) sw env) source


let sequenceM (source:ScriptMonad<'r,'a> list) : ScriptMonad<'r,'a list> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.sequenceM <| List.map (fun ma -> apply1 ma sw env) source

let sequenceMz (source:ScriptMonad<'r,'a> list) : ScriptMonad<'r,unit> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.sequenceMz <| List.map (fun ma -> apply1 ma sw env) source

// Summing variants

let sumMapM (fn:'a -> ScriptMonad<'r,int>) (xs:'a list) : ScriptMonad<'r,int> = 
    fmapM List.sum <| mapM fn xs

let sumMapiM (fn:int -> 'a -> ScriptMonad<'r,int>) (xs:'a list) : ScriptMonad<'r,int> = 
    fmapM List.sum <| mapiM fn xs

let sumForM (xs:'a list) (fn:'a -> ScriptMonad<'r,int>) : ScriptMonad<'r,int> = 
    fmapM List.sum <| forM xs fn

let sumForiM (xs:'a list) (fn:int -> 'a -> ScriptMonad<'r,int>) : ScriptMonad<'r,int> = 
    fmapM List.sum <| foriM xs fn

let sumTraverseM (fn: 'a -> ScriptMonad<'r,int>) (source:seq<'a>) : ScriptMonad<'r,int> =
    fmapM Seq.sum <| traverseM fn source

let sumTraverseiM (fn:int -> 'a -> ScriptMonad<'r,int>) (source:seq<'a>) : ScriptMonad<'r,int> =
    fmapM Seq.sum <| traverseiM fn source

let sumSequenceM (source:ScriptMonad<'r,int> list) : ScriptMonad<'r,int> = 
    fmapM List.sum <| sequenceM source


// Applicatives (<*>)
let apM (mf:ScriptMonad<'r,'a ->'b>) (ma:ScriptMonad<'r,'a>) : ScriptMonad<'r,'b> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.apM (apply1 mf sw env) (apply1 ma sw env)

// Perform two actions in sequence. Ignore the results of the second action if both succeed.
let seqL (ma:ScriptMonad<'r,'a>) (mb:ScriptMonad<'r,'b>) : ScriptMonad<'r,'a> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.seqL (apply1 ma sw env) (apply1 mb sw env)

// Perform two actions in sequence. Ignore the results of the first action if both succeed.
let seqR (ma:ScriptMonad<'r,'a>) (mb:ScriptMonad<'r,'b>) : ScriptMonad<'r,'b> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.seqR (apply1 ma sw env) (apply1 mb sw env)

// Answer sepcific operations
let runScript (failure: string -> 'b) (success: 'a -> 'b) (logger:LogAction) (env:'r) (ma:ScriptMonad<'r,'a>) : 'b = 
    use sw = new System.IO.StringWriter()
    let ans = AnswerMonad.runAnswer failure success (apply1 ma sw env)
    let () = logger sw
    ans



let runConsoleScript (success:'a -> unit) (env:'r) (ma:ScriptMonad<'r,'a>) : unit = 
    runScript failwith id (consoleLogger) env ma |> success
    

let throwError (msg:string) : ScriptMonad<'r,'a> =     
    ScriptMonad <| fun _ _ -> AnswerMonad.throwError msg

let swapError (msg:string) (ma:ScriptMonad<'r,'a>) : ScriptMonad<'r,'a> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.swapError msg (apply1 ma sw env)

let augmentError (fn:string -> string) (ma:ScriptMonad<'r,'a>) : ScriptMonad<'r,'a> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.augmentError fn (apply1 ma sw env)

let replaceFailure (defaultValue:'a) (ma:ScriptMonad<'r,'a>) : ScriptMonad<'r,'a> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.replaceError defaultValue (apply1 ma sw env)

let logWriteLine (text:string) : ScriptMonad<'r,unit> = 
    ScriptMonad <| fun sw env -> 
        let () = sw.WriteLine text
        answerMonad.Return ()

let logScript (makeLine:'a -> string) (proc:ScriptMonad<'r,'a>) : ScriptMonad<'r,'a> = 
    scriptMonad { 
        let! a = proc
        do! logWriteLine (makeLine a)
        return a
        }

// Note - pass in unit to avoid value restriction.
let ask () : ScriptMonad<'r,'r> = 
    ScriptMonad <| fun _ env -> answerMonad.Return env

let asks (proj:'r -> 's) : ScriptMonad<'r,'s> = 
    ScriptMonad <| fun _ env -> answerMonad.Return (proj env)

let local (extend:'r -> 'r) (ma: ScriptMonad<'r,'a>) : ScriptMonad<'r,'a> = 
    ScriptMonad <| fun sw env -> apply1 ma sw (extend env)

let liftAction (action:'a) : ScriptMonad<'r,'a> = 
    ScriptMonad <| fun _ _ -> AnswerMonad.liftAction action

let liftAnswer (result:Answer<'a>) : ScriptMonad<'r,'a> = 
    ScriptMonad <| fun _ _ -> result

let liftOption (source:'a option) : ScriptMonad<'r,'a> = 
    ScriptMonad <| fun _ _ -> AnswerMonad.liftOption source


// Left biased choice, if ``ma`` succeeds return its result, otherwise try ``mb``.
let alt (ma:ScriptMonad<'r,'a>) (mb:ScriptMonad<'r,'a>) : ScriptMonad<'r,'a> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.alt (apply1 ma sw env) (apply1 mb sw env)


// Catch failing computations, return None. 
// Successful operations are returned as Some(_).
let optional (ma:ScriptMonad<'r,'a>) : ScriptMonad<'r,'a option> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.optional (apply1 ma sw env)

// Perform an operation for its effect, ignore whether it succeeds or fails.
// (Comptations always return ``Ok ()``)
let optionalz (ma:ScriptMonad<'r,'a>) : ScriptMonad<'r,unit> = 
    ScriptMonad <| fun sw env -> 
        AnswerMonad.optionalz (apply1 ma sw env)

let runOptional (failMsg:string) (ma:ScriptMonad<'r,'a option>) : ScriptMonad<'r,'a> = 
    scriptMonad { 
        let! opt1 = ma
        match opt1 with
        | None -> 
            let! err = throwError failMsg
            return err
        | Some a -> return a        
        }    



/// Are specific lifters worth the dependency?
let liftJsonExtract (ma:JsonExtractor<'a>) (fileName:string) : ScriptMonad<'r,'a> = 
    match extractFromFile ma fileName with
    | Ok a -> unitM a
    | Err msg -> throwError msg
