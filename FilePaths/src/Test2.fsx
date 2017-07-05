#load "Utils.fs"

open FilePath.Utils

let levboth (s:string) (t:string) : (int * int) =
     (slowLevenshtein s t, levenshtein s t)

let test01 () = levboth "history" "mystery"

// Should be 3 (this is Wikipedia's example...)
let test02 () = levboth "kitten" "sitting"

let test03 () = levboth "very equal" "very equal"
