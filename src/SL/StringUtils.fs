module SL.StringUtils


let doubleQuote (s:string) : string = sprintf "\"%s\"" s
let singleQuote (s:string) : string = sprintf "'%s'" s


let private ArraySwap (a:int[]) (b:int[]) : unit = 
    for i = 0 to (a.Length-1) do a.[i] <- b.[i]


let levenshtein (s:string) (t:string) : int =
    let m = s.Length
    let n = t.Length
    let v0 = Array.zeroCreate (n+1)
    let v1 = Array.zeroCreate (n+1)
    for i = 0 to n do v0.[i] <- i
    for i = 0 to (m-1) do
        v1.[0] <- i + 1
        for j = 0 to (n-1) do
            let substitutionCost = if s.[i] = t.[j] then 0 else 1
            let a = v1.[j] + 1
            let b = v0.[j + 1] + 1
            let c = v0.[j] + substitutionCost
            v1.[j+1] <- min a (min b c)
        ArraySwap v0 v1
    v0.[n]

let makeGlobPattern (questions:string list) (stars:string list) (source:string) : string =
    let s1 = List.fold (fun (s:string) rep -> s.Replace(rep, "?")) source questions
    List.fold (fun (s:string) rep -> s.Replace(rep, "*")) s1 stars