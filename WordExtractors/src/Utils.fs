[<AutoOpen>]
module Utils

let rbox (v : 'a) : obj ref = ref (box v)


let sRestOfLine (s:string) : string = 
    let lines = s.Split('\n')
    lines.[0]



