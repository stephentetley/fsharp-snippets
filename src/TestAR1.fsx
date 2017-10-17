
#I @"..\packages\System.Data.SQLite.Core.1.0.105.2\lib\net46"
#r "System.Data.SQLite"

#I @"..\packages\FSharpx.Extras.2.2.1\lib\net45"
#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections.dll"
#r "FSharpx.Extras.dll"

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"


#load @"AssetRep\SiteImport.fs"


open System
open System.Data.SQLite
open FSharpx
open FSharp.ExcelProvider

open AssetRep.SiteImport


let test01 () =
    let loc = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..", @"data\assetrep.db")
    printfn "%s" loc
    let dbconn = makeConn(loc) 
    dbconn.Do (fun conn -> withOpenConn conn (fun oc -> let _ = deleteAllSites oc
                                                        let _ = insertDummy oc
                                                        testConn oc))


let test02 () = 
    let file = new SiteRow()
    for rowi in file.Data do
        printfn "%s %s %s" rowi.SiteReference rowi.``Postal Address 1`` rowi.``Post Code``


let test03 () =
    let loc = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..", @"data\assetrep.db")
    let dbconn = makeConn(loc) 
    dbconn.Do (fun conn -> withOpenConn conn (fun oc -> let _ = deleteAllSites oc
                                                        let _ = insertAllSites oc
                                                        testConn oc))


let test04 () =
    let loc = System.IO.Path.Combine(__SOURCE_DIRECTORY__,"..", @"data\assetrep.db")
    let dbconn = makeConn(loc) 
    dbconn.Do (fun conn -> withOpenConn conn (fun oc -> let _ = deleteAllLocations oc
                                                        let _ = insertAllLocations oc
                                                        testConn oc))



let temp01 () = escapeValueText "ALBION STREET/ST JOHN'S PLACE"

