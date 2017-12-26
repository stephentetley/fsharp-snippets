module SqlUtils

type FieldNames = string list

// Pack a value as a dynamic type together with its rendering function
// and field name
type InsertValue = 
    { fieldName : string
    ; renderFun : obj -> string
    ; fieldValue : obj
    }

type InsertValues = InsertValue list

let private parens (input:string) : string = 
    sprintf "(%s)" input

let fieldsList (fields:FieldNames) : string =
    String.concat ", " fields

let renderValue (value:InsertValue) : string = 
    value.renderFun <| value.fieldValue

let valuesList (values:InsertValues) : string = 
    String.concat ", " <| List.map renderValue values

let sqlINSERT (tableName:string) (values:InsertValues) : string = 
    sprintf "INSERT INTO %s (%s) VALUES (%s);"
            tableName
            (fieldsList <| List.map (fun v -> v.fieldName) values)
            (valuesList values)

let stringValue (field:string) (value:string) : InsertValue = 
    // TODO escaping special chars to add...
    let render (o:obj) = 
        let s = o :?> string in sprintf "'%s'" s
    { fieldName = field
    ; renderFun = render
    ; fieldValue = value :> obj
    }

let intValue (field:string) (value:int) : InsertValue = 
    let render (o:obj) = 
        let i = o :?> int in sprintf "%d" i
    { fieldName = field
    ; renderFun = render
    ; fieldValue = value :> obj
    }


let floatValue (field:string) (value:float) : InsertValue = 
    let render (o:obj) = 
        let d = o :?> float in sprintf "%f" d
    { fieldName = field
    ; renderFun = render
    ; fieldValue = value :> obj
    }