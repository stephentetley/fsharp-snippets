#I @"..\packages\System.Data.SQLite.Core.1.0.105.2\lib\net46"
#r "System.Data.SQLite"

#load @"AssetRep\FreshDB.fs"

open AssetRep.FreshDB


// Actually - creating tables with "abstract syntax" (i.e. datatypes) is more verbose 
// than using SQL CREATE TABLE statement.
let dummy01 () = 
    makeTable "ultrasonics" 
              {Name="reference"; DataType="TEXT"}
              <| makeColumns [ ("common_name", "TEXT")
                             ; ("site_name", "TEXT") 
                             ; ("installed_from", "DATE")
                             ; ("manufacturer", "TEXT")
                             ; ("model", "TEXT")
                             ; ("asset_status", "TEXT") ]