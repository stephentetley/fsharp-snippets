// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module SL.SqlUtils

open System
open System.Globalization

// Helpers for values

let escapeValueText (s:string) : string = 
    let escape (s1 :string) = s1.Replace("'", "''")
    match s with null -> "" | _ -> escape s

let cleanseValue (s:string) : string = (escapeValueText s).Trim() 

// SQLite format for date is yyyy-MM-dd hh:mm:ss, we cannot rely on the default formatter.

let sqliteDecodeTime (s:string) : DateTime = 
    DateTime.ParseExact(s, "dd/MM/yyyy hh:mm:ss", Globalization.CultureInfo.InvariantCulture)

let trySQLiteDecodeTime (s:string) : DateTime option = 
    try
        Some <|  DateTime.ParseExact(s, "dd/MM/yyyy hh:mm:ss", Globalization.CultureInfo.InvariantCulture)
    with
    | ex -> None
   

let sqliteEncodeTime (t:DateTime) : string = t.ToString("yyyy-MM-dd hh:mm:ss")


type ColumnNames = string list

// Pack a value as a dynamic type together with its rendering function
// and field name
type AttributeValue = 
    { columnName : string
    ; renderFun : obj -> string
    ; dynValue : obj
    }

type AttributeValues = AttributeValue list

let renderValue (value:AttributeValue) : string = 
    value.renderFun <| value.dynValue

let renderExpression (value:AttributeValue) : string = 
    sprintf "%s = %s" value.columnName (renderValue value)

let private parens (input:string) : string = 
    sprintf "(%s)" input

let columnsList (columns:ColumnNames) : string =
    String.concat ", " columns

let valuesList (values:AttributeValues) : string = 
    String.concat ", " <| List.map renderValue values

let sqlINSERT (tableName:string) (values:AttributeValues) : string = 
    sprintf "INSERT INTO %s (%s) VALUES (%s);"
            tableName
            (columnsList <| List.map (fun v -> v.columnName) values)
            (valuesList values)


let sqlUPDATE (tableName:string) (updates:AttributeValues) (restrictions:AttributeValues) : string = 
    sprintf "UPDATE %s\nSET %s\nWHERE %s;"
            tableName
            (String.concat ",\n"  <| List.map renderExpression updates)
            (String.concat "\nAND "  <| List.map renderExpression restrictions)


/// PostgreSQL only.
let pgsqlEXECUTE (procName:string) (values:AttributeValues) : string = 
    sprintf "EXECUTE %s(%s);" procName (valuesList values)



let nullValue (column:string) () : AttributeValue = 
    let render (o:obj) = "NULL"
    { columnName = column
    ; renderFun = render
    ; dynValue = () :> obj
    }

/// The value is transposed to SQL exactly as written (no quoting, no escaping).
/// Use this to generate e.g function calls in insert statements.
let literalValue (column:string) (value:string) : AttributeValue = 
    let render (o:obj) = 
        let s = o :?> string in s
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }



// Writes TRUE or FALSE
let boolValue (column:string) (value:bool) : AttributeValue = 
    let render (o:obj) = 
        let v = o :?> bool in match v with | true -> "TRUE" | false -> "FALSE"
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }

    
let boolNumericValue (column:string) (value:bool) : AttributeValue = 
    let render (o:obj) = 
        let v = o :?> bool in match v with | true -> "1" | false -> "0"
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }

let stringValue (column:string) (value:string) : AttributeValue = 
    // TODO escaping special chars to check...
    let render (o:obj) = 
        let s = o :?> string in sprintf "'%s'" (cleanseValue s)
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }

let stringNullableValue (column:string) (value:string) : AttributeValue = 
    // TODO escaping special chars to check...
    let render (o:obj) = 
        let s = o :?> string 
        match s with | null -> "NULL" | ss -> sprintf "'%s'" (cleanseValue ss)
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }
    
let intValue (column:string) (value:int) : AttributeValue = 
    let render (o:obj) = 
        let i = o :?> int in i.ToString()
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }
    
let intNullableValue (column:string) (value:Nullable<int>) : AttributeValue = 
    let render (o:obj) = 
        let i = o :?> Nullable<int>
        if i.HasValue then i.Value.ToString() else "NULL"
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }

let floatValue (column:string) (value:float) : AttributeValue = 
    let render (o:obj) = 
        let d = o :?> float in d.ToString()
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }

let floatNullableValue (column:string) (value:Nullable<float>) : AttributeValue = 
    let render (o:obj) = 
        let d = o :?> Nullable<float>
        if d.HasValue then d.Value.ToString() else "NULL"
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }


let int64Value (column:string) (value:int64) : AttributeValue = 
    let render (o:obj) = 
        let i = o :?> int64 in i.ToString()
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }

let int64NullableValue (column:string) (value:Nullable<int64>) : AttributeValue = 
    let render (o:obj) = 
        let i = o :?> Nullable<int64>
        if i.HasValue then i.Value.ToString() else "NULL"
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }


let decimalValue (column:string) (value:decimal) : AttributeValue = 
    let render (o:obj) = 
        let d = o :?> decimal in d.ToString()
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }

let decimalNullableValue (column:string) (value:Nullable<decimal>) : AttributeValue = 
    let render (o:obj) = 
        let d = o :?> Nullable<decimal>
        if d.HasValue then d.Value.ToString() else "NULL"
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }


let sqliteDateTimeValue (column:string) (value:DateTime) : AttributeValue = 
    let render (o:obj) = 
        let dt = o :?> DateTime in sprintf "'%s'" (sqliteEncodeTime dt)
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }


let pgsqlDateTimeValue (column:string) (value:DateTime) : AttributeValue = 
    let render (o:obj) = 
        let dt = o :?> DateTime 
        sprintf "to_date('%s', '%s')" (sqliteEncodeTime dt) "YYYY-MM-DD HH24:MI:SS"
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }
