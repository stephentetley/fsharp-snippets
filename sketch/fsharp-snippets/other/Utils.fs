
[<AutoOpen>]
module Hazards.Utils

open System
open System.Globalization


// sqlite format for insert is yyyy-MM-dd hh:mm:ss, we cannot rely on the default formatter.


let decodeTime (s:string) : DateTime = 
    DateTime.ParseExact(s, "dd/MM/yyyy hh:mm:ss", Globalization.CultureInfo.InvariantCulture)

let encodeTime (t:DateTime) : string = t.ToString("yyyy-MM-dd hh:mm:ss")
