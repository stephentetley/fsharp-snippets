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


// E.g "UNAVAIL" "E"
type MnemonicAlarmParam = 
    { MnemonicName: string
      AlarmLimit: string } 

type SourceAlarmParam = 
    { AlarmSource: string
      AlarmLimit: string
      PrAc: string option }



type AlarmParam = 
    | MnemonicParam of MnemonicAlarmParam
    | SourceParam of SourceAlarmParam

type AlarmParameters = 
    { Location: AlarmLocation
      Parameters: AlarmParam list } 

type RecordingCycleUnit = CycleM | CycleH

type RecordingCycleTime = 
    { TimeValue: int
      CycleUnit: RecordingCycleUnit }

/// Tag is "(8d:A)" or similar, for the moment it is uninterpreted
type RecordingCycle = 
    { CycleTime: RecordingCycleTime
      CycleTag: string }


type RecordingInfo = 
    { VarName: string
      RecordingCycles: RecordingCycle list }

// *************************************** 
// Parsers


// Helpers
let private (|||) (f:char -> bool) (g:char -> bool) : char -> bool = 
    fun ch -> f ch || g ch

let private (&&&) (f:char -> bool) (g:char -> bool) : char -> bool = 
    fun ch -> f ch && g ch

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

let private sourceLimit : Parser<string * (string option), unit> = 
    let make2 (s:string) (t:string) : string = 
        sprintf "%s/%s" s t

    let segment : Parser<string,unit> = 
        many1Satisfy <|
            ((not << System.Char.IsWhiteSpace) &&& (fun ch -> ch<>'/'))

    let post (ss:string list) = 
        match ss with
        | [a;b;c;d] -> (make2 a b, Some <| make2 c d)
        | [a;b] -> (make2 a b, None)
        | _ -> (String.concat "/" ss, None)

    sepBy1 segment (pchar '/') |>> post

let parseSourceAlarmParam : Parser<SourceAlarmParam, unit> = 
    pipe2 (identifier .>> pchar '=')
            sourceLimit
            (fun name (limit,opt) -> 
                { AlarmSource = name; AlarmLimit = limit; PrAc = opt})

let parseMnemonicAlarmParam: Parser<MnemonicAlarmParam, unit> = 
    pipe2 (identifier .>> pchar '/')
            identifier
            (fun name limit -> {MnemonicName = name; AlarmLimit = limit})


/// Needs backtracking            
let parseAlarmParam : Parser<AlarmParam, unit> = 
    (attempt parseMnemonicAlarmParam |>> MnemonicParam) <|> 
    (parseSourceAlarmParam |>> SourceParam)

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
            (fun i u -> {TimeValue = i; CycleUnit = u})

let parseRecordingCycle : Parser<RecordingCycle, unit> = 
    let pTag : Parser<string,unit> =
        parens (many1Satisfy (System.Char.IsLetterOrDigit ||| (fun c -> c=':')) )

    pipe2 parseRecordingCycleTime
            pTag
            ( fun cytime tag -> { CycleTime = cytime; CycleTag = tag})


let parserRecordingInfo : Parser<RecordingInfo, unit> = 
    let pTag : Parser<string,unit> =
        parens (many1Satisfy (System.Char.IsLetterOrDigit ||| (fun c -> c=':')) )

    pipe2 (identifier .>> pchar '@')
            (sepBy parseRecordingCycle (pchar ','))
            (fun name cys -> {VarName = name; RecordingCycles = cys})

let parserRecordingInfos : Parser<RecordingInfo list, unit> = 
    sepBy1 parserRecordingInfo spaces1

// *************************************** 
// Printers
let longhandPointTag (tag:PointTag) : string = tag.ToString()

/// S A I
let longhandPointType (typ:PointType) : string = typ.ToString ()
    

let longhandQualifiedType (qt:QualifiedType) : string = 
    sprintf "%s %s" (longhandPointTag qt.Qualifier) (longhandPointType qt.PointType) 