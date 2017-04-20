
#I @"..\packages\FParsec.1.0.2\lib\net40-client"
#r "FParsec"
#r "FParsecCS"
open FParsec


let test01 : ParserResult<int,unit> = run pint32 "-14"

let ws = spaces

let symbol p = p .>> ws

let pAddr : Parser<int * int,unit> = pipe2 (symbol pint32) (symbol pint32) (fun a b -> (a,b))

let pBody = symbol (pstring "DCL") >>. symbol (pstring "DCLEND")

let parseODF = pipe2 pAddr pBody (fun a b -> (a, b))

let pointName : Parser<string,unit> = many1Chars (upper <|> digit <|> pchar '_') 

let rtsPoint : Parser<string,unit> = pipe2 (many1Chars upper) (many1Chars digit) 
                                           (fun s t -> s + t)

// Newlines are significant - so be careful with symbol parser
// AI and AID points have ranges
// DI and DID points have mnemonics
// PID points have nothing

let test02 = run parseODF "14\n189\nDCL\nDCLEND"
let test03 = run pointName "W_WELL_HI_AND_2_PMPS_RUNNING "
let test04 = run rtsPoint "AI12 "
