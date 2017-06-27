#load "Utils.fs"

open FilePath.Utils

let test01 () = slowLevenshtein "history" "mystery"

// Should be 3 (this is Wikipedia's example...)
let test02 () = slowLevenshtein "kitten" "sitting"

let test03 () = slowLevenshtein "very equal" "very equal"
