#I @"..\packages\System.Data.SQLite.Core.1.0.108.0\lib\net45"
#r "System.Data.SQLite"
open System.Data.SQLite

#I @"..\packages\SQLProvider.1.1.41\lib\net451"
#r "FSharp.Data.SqlProvider.dll"
open FSharp.Data.Sql

#I @"C:\WINDOWS\assembly\GAC_MSIL\Microsoft.Office.Interop.Excel\15.0.0.0__71e9bce111e9429c"
#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"

#I @"..\packages\DocumentFormat.OpenXml.2.8.1\lib\net46"
#I @"..\packages\FastMember.Signed.1.3.0\lib\net45"
#I @"..\packages\ClosedXML.0.92.1\lib\net46"
#r "ClosedXML"

#load @"SL\AnswerMonad.fs"
#load @"SL\SqlUtils.fs"
#load @"SL\SQLiteConn.fs"
#load @"SL\ClosedXMLOutput.fs"
open SL.SQLiteConn
open SL.ClosedXMLOutput


// Note [<Literal>]'s only appear to support concat (+) and not general
// function calls as they are evaluated "before" compilation.

// Location of "System.Data.SQLite.dll"
let [<Literal>]  DllResolutionPath = 
    __SOURCE_DIRECTORY__ + @"\..\packages\System.Data.SQLite.Core.1.0.105.0\lib\net451"

// Location of the sqlite database
let [<Literal>] SQLiteConnString = 
    "Data Source=" + __SOURCE_DIRECTORY__ + @"\..\data\sites.sqlite;Version=3;"

type SqlDB = 
    SqlDataProvider<
        ConnectionString = SQLiteConnString,
        DatabaseVendor = Common.DatabaseProviderTypes.SQLITE,
        ResolutionPath = DllResolutionPath,
        IndividualsAmount = 1000,
        UseOptionTypes = true >

let dbCtx = SqlDB.GetDataContext()

// Problem - I've no idea how to get a type abbreviation for Intellisense, etc.
// The code below is invalid:
// type SitesRow = System.Linq.IQueryable<SqlDataProvider<...>.dataContext.main.all_sitesEntity>

// Untyped...
let findRow (uid:string) : Map<string,obj> [] = 
    query { for site in dbCtx.Main.AllSites do
            where (site.Uid = uid)
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

let makeOutputCells (row:Map<string,obj>) : RowWriter = 
    [ tellString        <| (row.["uid"] :?> string)
    ; tellString        <| (row.["installation_name"] :?> string)
    ; tellString        <| (row.["asset_type"] :?> string)
    ; tellString        <| (row.["grid_ref"] :?> string)
    ; tellString        <| (row.["full_address"] :?> string)
    ; tellString        <| (row.["postcode"] :?> string)
    ; tellString        <| (row.["ops_contact"] :?> string)
    ]


let writeRow (uid:string) : ClosedXMLOutput<unit> = 
    match tryFindRow1 uid with
    | Some(dict) -> 
        tellRow <| makeOutputCells dict
    | None -> tellRow [ tellString uid; tellString "#N/A"]


let xlsOutputPath = @"G:\work\Projects\rtu\SiteList1.xlsx"

let main () : unit = 
    let workData = new WorkListTable()
    let nullPred (row:WorkListRow) = match row.sitename with null -> false | _ -> true
    let rows : WorkListRow list = workData.Data |> Seq.filter nullPred |> Seq.toList
    let writerProc = closedXMLOutput {
        do! tellHeaders headers
        do! mapMz (fun (row:WorkListRow) -> writeRow row.uid) rows }

    ignore <| outputToNew { SheetName = "Site_List"} xlsOutputPath writerProc  