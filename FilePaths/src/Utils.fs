[<AutoOpen>]
module FilePath.Utils

let slowLevenshtein (s:string) (t:string) : int =
    let rec lev (slen:int) (tlen:int) = 
        if slen = 0 then tlen
        else if tlen = 0 then slen
        else let cost = if s.[slen-1] = t.[tlen-1] then 0 else 1
             let a = 1 + lev (slen-1) tlen
             let b = 1 + lev slen (tlen-1)
             let c = cost + lev (slen-1) (tlen-1)
             min a (min b c)
    lev (s.Length) (t.Length)


