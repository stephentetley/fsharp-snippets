
// reference the type provider dll
#I @"..\packages\FSharp.Text.RegexProvider.1.0.0\lib\net40"
#r "FSharp.Text.RegexProvider.dll"
open FSharp.Text.RegexProvider

// Pump names should end in the suffix PUMP_R (or *_A *_F or *_P)
type PumpR = Regex< @"(?<Name>[\p{L}_\d]+)_(?<Suffix>PUMP_[AFPR])\z" >


let test01 = PumpR().TypedMatch("DIGESTER_2_FEED_PUMP_R").Suffix.Value
let test02 = PumpR().TypedMatch("DIGESTER_2_FEED_PUMP_F").Suffix.Value
let test03 = PumpR().TypedMatch("DIGESTER_2_FEED_PUMP_R").Name.Value

