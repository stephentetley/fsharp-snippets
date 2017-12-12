// NOTE - normal regexs can still capture (don't need TypeProvider).
// Captures are typed to string and stored in the Groups collection.
open System.Text.RegularExpressions

// `hasMatch` should probably be [string -> string -> bool] as creating regexs in user
// code has the faff of using new, but using two strings as arguments gives
// opportunity to flip them in error.

let hasMatch (re:Regex) (s:string) : bool = re.Match(s).Success

let regex (s:string) = new Regex(s)

let test01 () = let re = regex "[\p{L}_\d]+" in hasMatch re "VICTORIA_CSO"

let test02 () = let re = new Regex("[\p{L}_\d]+") in hasMatch re "VICTORIA_CSO"


// Testing matching for OSGB36 grid refs, should Trim() any input first...
let test03 () = let re = new Regex("^[A-Za-z][A-Za-z][0-9]+$") in hasMatch re "TE403605"

let test04 () = let re = new Regex("^[A-Za-z][A-Za-z] [0-9]+ [0-9]+$") in hasMatch re "TE 403 605"

// OSGB36 - can use capture groups to save reparsing once format has been identified:
// Two letters, one number. Arbitrary spacing.
// NOTE - for a real implementation we need to convert to Int wrt. length of the number
// 3 chars => * 100
// 4 chars => * 10
// 5 chars => id
let test05 () =
    let re = "^([A-Za-z])([A-Za-z])\s*([0-9]+)$"
    let input = "TE403605  ".Trim() 
    let match1 = Regex.Match(input,re)
    if match1.Success then
        printfn "%c%c %i" (match1.Groups.[1].Value.[0]) 
                          (match1.Groups.[2].Value.[0])
                          (let s =  match1.Groups.[3].Value in System.Convert.ToInt32 s)
    else printfn "No match"

// Two letters, two numbers. Arbitrary spacing.
let test06 () =
    let re = "^([A-Za-z])([A-Za-z])\s*([0-9]+)\s+([0-9]+)$"
    let input = "TE 403  605  ".Trim() 
    let match1 = Regex.Match(input,re)
    if match1.Success then
        printfn "%c%c %i %i" 
                (match1.Groups.[1].Value.[0]) 
                (match1.Groups.[2].Value.[0])
                (let s =  match1.Groups.[3].Value in System.Convert.ToInt32 s)
                (let s =  match1.Groups.[4].Value in System.Convert.ToInt32 s)
    else printfn "No match"