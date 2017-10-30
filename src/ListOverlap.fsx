#I @"..\packages\ExcelProvider.0.8.2\lib"
#r "ExcelProvider.dll"
open FSharp.ExcelProvider

// TODO - ExcelProvider is the wrong solution, need something less type specific


open Microsoft.FSharp.Collections

[<StructuredFormatDisplay("{Name} {UID} '{SetLabel}'")>]
type Element = 
    { UID : string
      Name : string }

// "Sites_Barrier_Rep" ; "Sites_Battery_Rep" ; "Sites_Year4" ; "Sites_Year5"

     
type BarriersTable = 
    ExcelFile< @"G:\work\Sites-overlapping.xlsx",
               SheetName = "Sites_Barrier_Rep",
               ForceString = true >

type BarriersRow = BarriersTable.Row
     
type BatteriesTable = 
    ExcelFile< @"G:\work\Sites-overlapping.xlsx",
               SheetName = "Sites_Battery_Rep",
               ForceString = true >

type BatteriesRow = BatteriesTable.Row

type Year4Table = 
    ExcelFile< @"G:\work\Sites-overlapping.xlsx",
               SheetName = "Sites_Year4",
               ForceString = true >

type Year4Row = Year4Table.Row

type Year5Table = 
    ExcelFile< @"G:\work\Sites-overlapping.xlsx",
               SheetName = "Sites_Year5",
               ForceString = true >

type Year5Row = Year5Table.Row

let test01 () = 
    let barriersData = new BarriersTable()
    for (rowi:BarriersRow) in barriersData.Data do
        match rowi.Name with
        | null -> printfn "<finished>"
        | _ -> printfn "%s, %s, %A" rowi.UID rowi.Name rowi.``Grid Ref``


let buildBarriersSet () : Set<Element> = 
    let barriersData = new BarriersTable()
    let make1 (rowi : BarriersRow) : Element option = 
        match rowi.Name with 
        | null -> None
        | _ -> Some <| { UID = rowi.UID
                       ; Name = rowi.Name }
    barriersData.Data 
        |> Seq.map make1
        |> Seq.toList
        |> List.choose id
        |> Set.ofList

let buildBatteriesSet () : Set<Element> = 
    let batteriesData = new BatteriesTable()
    let make1 (rowi : BatteriesRow) : Element option = 
        match rowi.Name with 
        | null -> None
        | _ -> Some <| { UID = rowi.UID
                       ; Name = rowi.Name }
    batteriesData.Data 
        |> Seq.map make1
        |> Seq.toList
        |> List.choose id
        |> Set.ofList

let buildYear4Set () : Set<Element> = 
    let year4Data = new Year4Table()
    let make1 (rowi : Year4Row) : Element option = 
        match rowi.Name with 
        | null -> None
        | _ -> Some <| { UID = rowi.UID
                       ; Name = rowi.Name }
    year4Data.Data 
        |> Seq.map make1
        |> Seq.toList
        |> List.choose id
        |> Set.ofList

let buildYear5Set () : Set<Element> = 
    let year5Data = new Year5Table()
    let make1 (rowi : Year5Row) : Element option = 
        match rowi.Name with 
        | null -> None
        | _ -> Some <| { UID = rowi.UID
                       ; Name = rowi.Name }
    year5Data.Data 
        |> Seq.map make1
        |> Seq.toList
        |> List.choose id
        |> Set.ofList

let showSet (headline:string) (elems:Set<Element>) : unit =
    printfn "** %s **" headline
    Set.iter (fun (e:Element) -> printfn "%s %s" e.UID e.Name) elems

let compareSets () : unit = 
    let barriersA = buildBarriersSet ()
    let batteriesB = buildBatteriesSet ()
    let year4 = buildYear4Set ()
    let year5 = buildYear5Set ()
    let ab = Set.intersect barriersA batteriesB 
    showSet "Intersection Barriers and Batteries" ab
    let ay4 = Set.intersect barriersA year4 
    showSet "Intersection Barriers and Year 4" ay4
    let ay5 = Set.intersect barriersA year5 
    showSet "Intersection Barriers and Year 5" ay5
    let by4 = Set.intersect batteriesB year4 
    showSet "Intersection Batteries and Year 4" by4
    let by5 = Set.intersect batteriesB year5 
    showSet "Intersection Batteries and Year 5" by5

let main () = compareSets ()

