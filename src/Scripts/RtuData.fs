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

// *************************************** 
// Parsers


// Helpers
let private (|||) (f:char -> bool) (g:char -> bool) : char -> bool = 
    fun ch -> f ch || g ch


let pdecimal : Parser<decimal,unit> = pfloat |>> decimal


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

// TO COMPLETE!
let parseAlarmParameters : Parser<AlarmLocation * string, unit> = 
    tuple2 (parseAlarmLocation .>> spaces1) (restOfLine false)



// *************************************** 
// Printers
let longhandPointTag (tag:PointTag) : string = tag.ToString()

/// S A I
let longhandPointType (typ:PointType) : string = typ.ToString ()
    

let longhandQualifiedType (qt:QualifiedType) : string = 
    sprintf "%s %s" (longhandPointTag qt.Qualifier) (longhandPointType qt.PointType) 