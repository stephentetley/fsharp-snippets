module SL.Tolerance

open System

type Tolerance = 
    {TOL:decimal}
    member v.tolerance = v.TOL

/// Tolerant equality
let tEqual (tx:Tolerance) (a:decimal) (b:decimal) : bool = 
    Math.Abs (a-b) < tx.tolerance

/// Tolerant less than
let tLessThan (tx:Tolerance) (a:decimal) (b:decimal) : bool = 
    a < b && (b-a) > tx.tolerance

/// Tolerant greater than
let tGreaterThan (tx:Tolerance) (a:decimal) (b:decimal) : bool = 
    a > b && (a-b) > tx.tolerance

/// Tolerant less than or Equal
let tLessThanEqual (tx:Tolerance) (a:decimal) (b:decimal) : bool = 
    tEqual tx a b || tLessThan tx a b

/// Tolerant greater than
let tGreaterThanEqual (tx:Tolerance) (a:decimal) (b:decimal) : bool = 
    tEqual tx a b || tGreaterThan tx a b

/// Tolerant compare
let tCompare (tx:Tolerance) (a:decimal) (b:decimal) : int = 
    if tEqual tx a b then 0 else compare a b 
