#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider


#r "Microsoft.Office.Interop.Excel"
open Microsoft.Office.Interop

#load "Coord.fs"
open Coord


type HospitalsTable = 
    ExcelFile< @"G:\work\Accident-and-Emergency-Hospitals-Yorkshire.xlsx",
               SheetName = "Site_List",
               ForceString = true >

type HospitalsRow = HospitalsTable.Row

let test01 () = 
    let hosiptalData = new HospitalsTable()
    for (rowi:HospitalsRow) in hosiptalData.Data do
        match rowi.Name with
        | null -> printfn "<finished>"
        | _ -> let pt = Option.map Coord.enToLatLon  <| Coord.fromOSGridRef10 rowi.``Grid Reference``
               printfn "%s, %s, %A" rowi.Name rowi.``Grid Reference`` pt

[<StructuredFormatDisplay("{Name} ({LatLon})")>]
type Hospital1 = 
    { Name : string
      AddressString : string
      LatLon : Coord.WGS84Point }

type HospitalList = Hospital1 list


let buildHospitalList () = 
    let hospitalData = new HospitalsTable()
    let make1 (rowi : HospitalsRow) : Hospital1 option = 
        match rowi.Name with 
        | null -> None
        | _ -> let optPt = Option.map Coord.enToLatLon  <| Coord.fromOSGridRef10 rowi.``Grid Reference``
               match optPt with
               | Some pt -> Some <| { Name = rowi.Name
                                    ; AddressString = sprintf "%s\n%s\n%s\n%s" rowi.Name rowi.Telephone rowi.Address rowi.Postcode
                                    ; LatLon = pt }
               | None -> None
    hospitalData.Data |> Seq.map make1
                      |> Seq.toList
                      |> List.choose id

type BestMatch = float<Coord.kilometer> * Hospital1

let findClosest (pt : Coord.WGS84Point) (hospitals:HospitalList) : BestMatch option =
    let find1 (dist,best) (hospital:Hospital1) = 
        let dist1 = Coord.haversineDistance pt hospital.LatLon
        if dist1 <= dist then
            (dist1, Some hospital)
        else (dist,best)
    List.fold find1 (50000.0<Coord.kilometer>, None) hospitals 
        |> fun (d,o) -> match o with 
                        | Some a -> Some (d,a)
                        | None -> None
        

type DerivedAssets = 
    ExcelFile< @"G:\work\Derived-Asset-List3.xlsx",
               SheetName = "Site_List",
               ForceString = true >

type AssetRow = DerivedAssets.Row

let cellIndex (col:string) (row:int) : string = 
    sprintf "%s%d" col row

let saveAndCloseWorkbook (workbook:Excel.Workbook) (filename:string)  : unit =
    let app:Excel.Application = workbook.Application
    // To disable overwrite alert
    app.DisplayAlerts <- false
    workbook.SaveAs(Filename = filename)
    app.DisplayAlerts <- true
    workbook.Close(SaveChanges = false)

let writeRow (sheet:Excel.Worksheet) (rowindex:int) (uid:string) (oname:string) (address:string) (dist:float<Coord.kilometer>) : unit = 
    sheet.Cells.Range(cellIndex "A" rowindex).Value2 <- uid
    sheet.Cells.Range(cellIndex "B" rowindex).Value2 <- oname
    sheet.Cells.Range(cellIndex "C" rowindex).Value2 <- address
    sheet.Cells.Range(cellIndex "D" rowindex).Value2 <- dist


let main () = 
    // Run Excel as a visible application
    let app = new Excel.ApplicationClass(Visible = true) 
    let outputPath = @"G:\work\hospitals-output2.xlsx"
    let hosiptalData = buildHospitalList ()
    let assetData = new DerivedAssets()
    let outputWorkbook : Excel.Workbook = app.Workbooks.Add()
    let outputWorksheet = outputWorkbook.Sheets.[1] :?> Excel.Worksheet
    writeRow outputWorksheet 1 "UID" "Name" "Address" 0.0<Coord.kilometer>
    let mutable ix = 1
    let bestAddress (opt: BestMatch option) = 
        match opt with
        | None -> "Not found (ERROR)"
        | Some (dist,hospital) -> hospital.AddressString
    let bestName (opt: BestMatch option) = 
        match opt with
        | None -> "Not found (ERROR)"
        | Some (dist,hospital) -> hospital.Name
    let bestDist (opt: BestMatch option) = 
        match opt with
        | None -> 0.0<Coord.kilometer>
        | Some (dist,hospital) -> dist
    for (rowi:AssetRow) in assetData.Data do
        match rowi.Name with
        | null -> printfn "<finished>"
        | _ -> let opt = Option.map Coord.enToLatLon  <| Coord.fromOSGridRef10 rowi.``Grid Ref``
               let best = match opt with
                          | None -> None
                          | Some pt -> findClosest pt hosiptalData
               ix <- ix + 1
               writeRow outputWorksheet ix rowi.UID rowi.Name (bestAddress best) (bestDist best)
               printfn "%s,%s,%O,%A" rowi.UID rowi.Name (bestName best) (bestDist best)
    saveAndCloseWorkbook outputWorkbook outputPath
    app.Quit()