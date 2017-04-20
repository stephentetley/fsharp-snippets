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

type Config = { address : Address; points : SysPoint list}

let ws = manyChars (pchar ' ' <|> pchar '\t')
let ws1 : Parser<string,unit> = many1Chars (pchar ' ' <|> pchar '\t')

let symbol p = p .>> ws
let symbol1 p = p .>> ws1

let pline (p : Parser<'a,'u>) : Parser<'a,'u> = p .>> newline

let pstringline (ss : string) : Parser<string,unit> = pline (pstring ss)

let address : Parser<Address,unit> = pipe2 (pline pint32) (pline pint32) (fun a b -> (a,b))



let pointName : Parser<string,unit> = many1Chars (upper <|> digit <|> pchar '_') 

let pointID : Parser<PointID,unit> = pipe2 (many1Chars upper) (many1Chars digit) (+)

let mnemonic : Parser<Mnemonic,unit> = many1Chars (letter <|> digit <|> pchar '_') 

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
        | ANALOGUE -> pipe2 (symbol1 limit) (symbol limit)
                            (fun l1 l2 -> PAnalogue(name, id, l1, l2))
        | DIGITAL -> pipe2 (symbol1 mnemonic) (symbol mnemonic)
                            (fun m1 m2 -> PDigital(name, id, m1, m2))
        | PIDPOINT -> preturn (PID(name,id))
        | UNKNOWN -> fail ("Unknown point-type:" + id)
    ((symbol pointName) .>>. (symbol pointID)) >>= parseK



let sysPointList : Parser<SysPoint list,unit> = pstringline "DCL" >>. many1Till (pline sysPoint) (pstringline "DCLEND") 

let pConfig = pipe2 address sysPointList (fun a b -> {address=a; points=b})

let test01 : ParserResult<int,unit> = run pint32 "-14"
let test02 = run address "14\n189\nDCL"
let test03 = run pointName "W_WELL_HI_AND_2_PMPS_RUNNING "
let test04 = run pointID "AI12 "
let test05 = run pdecimal "1440.37500 "
let test06 = run limit "32766=100.37500 "
let test07 = pointType "PID1"
let test08 = run sysPoint "P2_CURRENT_DAY_MINS_RUN AID3 0=0.000000 1440=1440.000000 "
let test09 = runParserOnFile pConfig () @"G:\work\working\SAMPLE.odf" Text.ASCIIEncoding.ASCII