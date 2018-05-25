module Scripts.RtuData

open FParsec

/// R D C
type PointTag = Real | Derived | Calculated

/// S A I
type PointType = Analogue | Status | Integrated


type QualifiedType = 
    { Qualifier: PointTag
      PointType: PointType }

type Scaling = int * decimal

type AlarmLocation = AlarmOS | AlarmDG

type AlarmParam = 
    | LimitAlarm of string * string
    | StatusAlarm of string * string

type AlarmParameters = 
    { Location: AlarmLocation
      Parameters: AlarmParam list } 

type RecordingCycleUnit = CycleM | CycleH

type RecordingCycleTime = 
    { CycleTime: int
      CycleUnit: RecordingCycleUnit }

/// Tag is "(8d:A)" or similar, for the moment it is uninterpreted
type RecordingInfo = 
    { VarName: string
      Cycle: RecordingCycleTime
      Tag: string }

// *************************************** 
// Parsers


// Helpers
let private (|||) (f:char -> bool) (g:char -> bool) : char -> bool = 
    fun ch -> f ch || g ch


let pdecimal : Parser<decimal,unit> = pfloat |>> decimal


let parens (p:Parser<'a, unit>) : Parser<'a, unit> = 
    between (pchar '(' .>> spaces) (pchar ')') p

let identifier : Parser<string,unit> = many1Satisfy System.Char.IsLetter



let parsePointDerived : Parser<PointTag,unit> = 
    (pchar 'D' |>> fun _ -> Derived)        <|> 
    (pchar 'R' |>> fun _ -> Real)           <|>
    (pchar 'C' |>> fun _ -> Calculated)

let parsePointType : Parser<PointType,unit> = 
    (pchar 'S' |>> fun _ -> Status)   <|> 
    (pchar 'A' |>> fun _ -> Analogue)   <|>
    (pchar 'I' |>> fun _ -> Integrated)

let parseQualifiedType : Parser<QualifiedType,unit> = 
    pipe2 (parsePointDerived .>> spaces) 
            parsePointType
            (fun tag ty -> { Qualifier = tag; PointType = ty })

let parseDerTypNo : Parser<QualifiedType * int, unit> = 
    tuple2 (parseQualifiedType .>> spaces) pint32


let parseMnemonic1 : Parser<string,unit> = 
    many1Satisfy2 (System.Char.IsUpper) 
                    (System.Char.IsLetterOrDigit ||| (fun c -> c='_')) 

let parseMnemonics : Parser<string list, unit> = 
    sepBy1 parseMnemonic1 spaces1



let parseScaling1 : Parser<Scaling,unit> = 
    tuple2 (pint32 .>> pchar '=') pdecimal

let parseScalings : Parser<Scaling list,unit> = 
    sepBy1 parseScaling1 (pchar ',' .>> spaces1)



let parseAlarmLocation : Parser<AlarmLocation, unit> = 
    (pstring "OS" |>> fun _ -> AlarmOS)  <|>
    (pstring "DG" |>> fun _ -> AlarmDG) 

let parseLimitAlarm : Parser<AlarmParam, unit> = 
    let generalLimit : Parser<string,unit> = 
        many1Satisfy (not << System.Char.IsWhiteSpace) 

    pipe2 (identifier .>> pchar '=')
            generalLimit
            (fun name limit -> LimitAlarm(name,limit))

let parseStatusAlarm : Parser<AlarmParam, unit> = 
    pipe2 (identifier .>> pchar '/')
            identifier
            (fun a b -> StatusAlarm(a,b))


/// Needs backtracking            
let parseAlarmParam : Parser<AlarmParam, unit> = 
    (attempt parseStatusAlarm) <|> parseLimitAlarm

let parseAlarmParameters : Parser<AlarmParameters, unit> = 
    pipe2 (parseAlarmLocation .>> spaces1) 
            (sepBy parseAlarmParam spaces1)
            (fun loc ps -> {Location = loc; Parameters = ps})


let parseRecordingCycleUnit : Parser<RecordingCycleUnit, unit> = 
    (pchar 'H' |>> fun _ -> CycleH)  <|>
    (pchar 'M' |>> fun _ -> CycleM) 

let parseRecordingCycleTime : Parser<RecordingCycleTime, unit> = 
    pipe2 pint32 
            parseRecordingCycleUnit
            (fun i u -> {CycleTime = i; CycleUnit = u})

let parserRecordingInfo : Parser<RecordingInfo, unit> = 
    let pTag : Parser<string,unit> =
        parens (many1Satisfy (System.Char.IsLetterOrDigit ||| (fun c -> c=':')) )

    pipe3 (identifier .>> pchar '@')
            parseRecordingCycleTime
            pTag
            (fun name rct tag -> {VarName = name; Cycle = rct; Tag = tag})

let parserRecordingInfos : Parser<RecordingInfo list, unit> = 
    sepBy1 parserRecordingInfo spaces1

// *************************************** 
// Printers
let longhandPointTag (tag:PointTag) : string = tag.ToString()

/// S A I
let longhandPointType (typ:PointType) : string = typ.ToString ()
    

let longhandQualifiedType (qt:QualifiedType) : string = 
    sprintf "%s %s" (longhandPointTag qt.Qualifier) (longhandPointType qt.PointType) 