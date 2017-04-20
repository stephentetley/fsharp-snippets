// This parses a configuration file used by a proprietry telemetry system
// [It does not parser OpenOffice odf]

#I @"..\packages\FParsec.1.0.2\lib\net40-client"
#r "FParsec"
#r "FParsecCS"
open FParsec
open System

// Newlines are significant - so be careful with symbol parser
// AI and AID points have ranges
// DI and DID points have mnemonics
// PID points have nothing


type Address = int * int
type PointID = string
type Limit = { ival : int; rval : decimal}
type Mnemonic = string

type PointType = ANALOGUE | DIGITAL | PIDPOINT | UNKNOWN

type SysPoint =
    | PAnalogue of string * PointID * Limit * Limit
    | PDigital of string * PointID * Mnemonic * Mnemonic
    | PID of string * PointID



let ws = spaces

let symbol p = p .>> ws

// TODO attend to newline 
let pAddr : Parser<Address,unit> = pipe2 (symbol pint32) (symbol pint32) (fun a b -> (a,b))

let pBody = symbol (pstring "DCL") >>. symbol (pstring "DCLEND")

let parseODF = pipe2 pAddr pBody (fun a b -> (a, b))

let pointName : Parser<string,unit> = many1Chars (upper <|> digit <|> pchar '_') 

let pointID : Parser<PointID,unit> = pipe2 (many1Chars upper) (many1Chars digit) (+)

let mnemonic : Parser<Mnemonic,unit> = many1Chars letter

let pdecimal : Parser<decimal,unit> = pfloat |>> decimal

let limit : Parser<Limit,unit> = 
    let eqsign = pchar '='
    pipe2 (pint32 .>> eqsign) pdecimal (fun l r -> {ival=l; rval=r})



let pointType : string -> PointType = 
    fun s -> if s.StartsWith("AI") then
                ANALOGUE
             elif s.StartsWith("DI") then
                DIGITAL 
             elif s.StartsWith("PI") then
                PIDPOINT
             else
                UNKNOWN
            


let sysPoint : Parser<SysPoint,unit> = 
    let parseK (name,id) = 
        match pointType id with
        | ANALOGUE -> pipe2 (symbol limit) (symbol limit)
                            (fun l1 l2 -> PAnalogue(name, id, l1, l2))
        | DIGITAL -> pipe2 (symbol mnemonic) (symbol mnemonic)
                            (fun m1 m2 -> PDigital(name, id, m1, m2))
        | PIDPOINT -> preturn (PID(name,id))
        | UNKNOWN -> fail ("Unknown point-type:" + id)
    ((symbol pointName) .>>. (symbol pointID)) >>= parseK




let test01 : ParserResult<int,unit> = run pint32 "-14"
let test02 = run parseODF "14\n189\nDCL\nDCLEND"
let test03 = run pointName "W_WELL_HI_AND_2_PMPS_RUNNING "
let test04 = run pointID "AI12 "
let test05 = run pdecimal "1440.37500 "
let test06 = run limit "32766=100.37500 "
let test07 = pointType "PID1"
let test08 = run sysPoint "P2_CURRENT_DAY_MINS_RUN AID3 0=0.000000 1440=1440.000000 "
