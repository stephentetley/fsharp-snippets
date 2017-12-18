#I @"..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"
#r "System.Data.SQLite"
open System.Data.SQLite

#I @"..\packages\SQLProvider.1.0.54\lib"
#r "FSharp.Data.SqlProvider.dll"
open FSharp.Data.Sql

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#load @"SQLiteUtils.fs"
open SQLiteUtils

#I @"..\packages\DocumentFormat.OpenXml.2.7.2\lib\net46\"
#I @"..\packages\FastMember.Signed.1.1.0\lib\net40\"
#I @"..\packages\ClosedXML.0.90.0\lib\net452\"
#r "ClosedXML"
#load @"ClosedXMLWriter.fs"
open ClosedXMLWriter


// Note [<Literal>]'s only appear to support concat (+) and not general
// function calls as they are evaluated "before" compilation.

// Location of "System.Data.SQLite.dll"
let [<Literal>]  DllResolutionPath = 
    __SOURCE_DIRECTORY__ + @"\..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"

// Location of the sqlite database
let [<Literal>] SQLiteConnString = 
    "Data Source=" + __SOURCE_DIRECTORY__ + @"\..\data\sai_refs.sqlite;Version=3;"

type SqlDB = 
    SqlDataProvider<
        ConnectionString = SQLiteConnString,
        DatabaseVendor = Common.DatabaseProviderTypes.SQLITE,
        ResolutionPath = DllResolutionPath,
        IndividualsAmount = 1000,
        UseOptionTypes = true >

let dbCtx = SqlDB.GetDataContext()

let query01 = 
    query { for site in dbCtx.Main.AllSites do
            where (site.Sainum = "SAI00262693")
            select (site)
        }
    

let test01 () = 
    query01 |> Seq.toList |> List.iter (fun site -> printfn "%s,%s" site.Sainum site.InstallationName)

// Problem - I've no idea how to get a type abbreviation for Intellisense, etc.
// The code below is invalid:
// type SitesRow = System.Linq.IQueryable<SqlDataProvider<...>.dataContext.main.all_sitesEntity>

// Untyped...
let findRow (uid:string) : Map<string,obj> [] = 
    query { for site in dbCtx.Main.AllSites do
            where (site.Sainum = uid)
            select (site)
        } |> Seq.toArray |> Array.map (fun c -> c.ColumnValues |> Map.ofSeq)

let tryFindRow1 (uid:string) : Map<string,obj> option = 
    match Array.toList <| findRow uid with
    | (x::_) -> Some x
    | _ -> None

type WorkListTable = 
    ExcelFile< @"G:\work\Projects\rtu\Final_Docs\year3-batch2-manuals-todo.xlsx",
                SheetName = "TO_MAKE",
                ForceString = true >

type WorkListRow = WorkListTable.Row

let headers = 
    [ "SAI"
    ; "Installation name"
    ; "Asset type"
    ; "Grid ref"
    ; "Address"
    ; "Postcode"
    ; "Operational Contact" ]

let makeOutputCells (row:Map<string,obj>) : string list = 
    [ row.["sainum"] :?> string
    ; row.["installation_name"] :?> string
    ; row.["asset_type"] :?> string
    ; row.["grid_ref"] :?> string
    ; row.["full_address"] :?> string
    ; row.["postcode"] :?> string
    ; row.["ops_contact"] :?> string 
    ]

let test03 () = 
    match tryFindRow1 "SAI00262693" with
    | Some(dict) -> 
        Map.iter (fun k v -> printfn "%s: %O" k v) dict
        let strings = makeOutputCells dict
        printfn "%s" (String.concat "," strings)
    | None -> printfn "not found"

let writeRow (sai:string) : ClosedXMLWriter<unit> = 
    match tryFindRow1 sai with
    | Some(dict) -> 
        tellRow <| makeOutputCells dict
    | None -> tellRow [sai; "#N/A"]


let xlsOutputPath = @"G:\work\Projects\rtu\SiteList1.xlsx"

let main () : unit = 
    let workData = new WorkListTable()
    let nullPred (row:WorkListRow) = match row.sitename with null -> false | _ -> true
    let rows : WorkListRow list = workData.Data |> Seq.filter nullPred |> Seq.toList
    let writerProc = closedXMLWriter {
        do! tellHeaders headers
        do! mapMz (fun (row:WorkListRow) -> writeRow row.uid) rows }

    ignore <| outputToNew writerProc xlsOutputPath "Site_List"