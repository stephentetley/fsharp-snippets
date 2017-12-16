[<AutoOpen>]
module DocSoup.PathExpressions

open Microsoft.Office.Interop


// Cannot use Microsoft.FSharp.Collections.Set
// The type 'Word.Range' does not support the 'comparison' constraint. 
// For example, it does not support the 'System.IComparable' interface


// cf NodeSet (xpath)
type RangeSet = RangeSet of List<Word.Range>

type Value = 
    | ValString of string
    | ValInt of int
    | ValRangeSet of RangeSet


type Path =
    | Or of Path * Path
    | Seq of Path * Path

            


