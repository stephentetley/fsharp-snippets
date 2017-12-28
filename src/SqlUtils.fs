module SqlUtils



// Helpers for values

let escapeValueText (s:string) : string = 
    let escape (s1 :string) = s1.Replace("'", "''")
    match s with null -> "" | _ -> escape s

let cleanseValue (s:string) : string = (escapeValueText s).Trim() 




type ColumnNames = string list

// Pack a value as a dynamic type together with its rendering function
// and field name
type InsertValue = 
    { columnName : string
    ; renderFun : obj -> string
    ; dynValue : obj
    }

type InsertValues = InsertValue list

let private parens (input:string) : string = 
    sprintf "(%s)" input

let columnsList (columns:ColumnNames) : string =
    String.concat ", " columns

let renderValue (value:InsertValue) : string = 
    value.renderFun <| value.dynValue

let valuesList (values:InsertValues) : string = 
    String.concat ", " <| List.map renderValue values

let sqlINSERT (tableName:string) (values:InsertValues) : string = 
    sprintf "INSERT INTO %s (%s) VALUES (%s);"
            tableName
            (columnsList <| List.map (fun v -> v.columnName) values)
            (valuesList values)

let stringValue (column:string) (value:string) : InsertValue = 
    // TODO escaping special chars to add...
    let render (o:obj) = 
        let s = o :?> string in sprintf "'%s'" (cleanseValue s)
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }

let intValue (column:string) (value:int) : InsertValue = 
    let render (o:obj) = 
        let i = o :?> int in sprintf "%d" i
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }


let floatValue (column:string) (value:float) : InsertValue = 
    let render (o:obj) = 
        let d = o :?> float in sprintf "%f" d
    { columnName = column
    ; renderFun = render
    ; dynValue = value :> obj
    }