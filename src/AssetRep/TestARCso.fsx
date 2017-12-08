
#I @"..\..\packages\System.Data.SQLite.Core.1.0.105.2\lib\net46"
#r "System.Data.SQLite"

#I @"..\..\packages\FSharpx.Extras.2.2.1\lib\net45"
#I @"..\..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections.dll"
#r "FSharpx.Extras.dll"

#I @"..\..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"


#load @"CsoOutputs.fs"


open System
open System.Data.SQLite
open FSharpx
open FSharp.ExcelProvider

open AssetRep.CsoOutputs



let test01 () =
    let loc = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", @"data/cso_outputs.db")
    printfn "%s" loc
    let dbconn = makeConn(loc) 
    dbconn.Do (fun conn -> ignore <| withOpenConn conn deleteAllUltrasonics
                           withOpenConn conn insertAllUltrasonics)

