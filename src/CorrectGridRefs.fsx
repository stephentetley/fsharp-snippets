// Use ExcelProvider...
#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames

#load @"SL\Coord.fs"
open SL.Geo

#I @"..\packages\FSharpx.Collections.1.17.0\lib\net40"
#r "FSharpx.Collections"
#I @"..\packages\DocumentFormat.OpenXml.2.7.2\lib\net46\"
#I @"..\packages\FastMember.Signed.1.1.0\lib\net40\"
#I @"..\packages\ClosedXML.0.90.0\lib\net452\"
#r "ClosedXML"
#load @"SL\ClosedXMLOutput.fs"
open SL.ClosedXMLOutput


type InputTable = 
    ExcelFile< @"G:\work\Projects\events2\Discharge-outfalls.xlsx",
               SheetName = "OUTFALLS",
               ForceString = true >

type InputRow = InputTable.Row

let readInputRows () : InputRow list = 
    let workData = new InputTable()
    // In this case filter on column "B" (aka 1)
    // There are bad values in column "A"
    let nullPred (row:InputRow) = match row.GetValue(1) with null -> false | _ -> true
    workData.Data |> Seq.filter nullPred |> Seq.toList

let headers : string list = 
    [ "SAI Num"; "Site Name"; "Outfall NGR"; "Receiving Watercourse" ]

let defaultIfNull (defaultValue:string) (check:string) = 
    match check with 
    | null -> defaultValue
    | _ -> check

let correctGridRef (input:string) : string = 
    match input with 
    | null -> sprintf "Invalid: %s" input
    | _ -> match Coord.tryReadOSGB36Grid input with
            | Some(pt) -> Coord.showOSGB36Grid pt
            | None -> sprintf "Invalid: %s" input

        
let tellRow1 (row:InputRow) : ClosedXMLOutput<unit> = 
    // printfn "%s" (row.``Related AI Asset Name``)
    tellRow [ tellString    << defaultIfNull "" <| row.``SAI of related asset``
            ; tellString    << defaultIfNull "" <| row.``Related AI Asset Name``
            ; tellString    << correctGridRef   <| row.``Outlet NGR``
            ; tellString    << defaultIfNull "" <| row.``Receiving water/ environment``
            ]

let outFile : string = 
    @"G:\work\Projects\events2\Discharge-outfalls-CORRECTED.xlsx"

let main () : unit = 
    let input = readInputRows ()
    let proc = 
        closedXMLOutput { do! tellHeaders headers
                          do! mapMz tellRow1 input }
    outputToNew { SheetName = "Outfalls" } proc outFile        
        

