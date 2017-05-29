
open System.Text.RegularExpressions

// `hasMatch` should probably be [string -> string -> bool] as creating regexs in user
// code has the faff of using new, but using two strings as arguments allows them to be 
// flipped in error.

let hasMatch (re:Regex) (s:string) : bool = re.Match(s).Success

let regex (s:string) = new Regex(s)

let test01 = let re = regex "[\p{L}_\d]+" in hasMatch re "VICTORIA_CSO"

let test02 = let re = new Regex("[\p{L}_\d]+") in hasMatch re "VICTORIA_CSO"
