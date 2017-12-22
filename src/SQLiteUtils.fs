module SQLiteUtils

open System
open System.IO
open System.Data
open System.Data.SQLite



// Helpers for values

let escapeValueText (s:string) : string = 
    let escape (s1 :string) = s1.Replace("'", "''")
    match s with null -> "" | _ -> escape s

let cleanseValue (s:string) : string = (escapeValueText s).Trim() 


// SQLiteConn Monad

type SQLiteConn<'a> = SQLiteConn of (SQLite.SQLiteConnection -> 'a)

let inline private apply1 (ma : SQLiteConn<'a>) (conn:SQLite.SQLiteConnection) : 'a = 
    let (SQLiteConn f) = ma in f conn

let inline private unitM (x:'a) : SQLiteConn<'a> = SQLiteConn (fun r -> x)


let inline private bindM (ma:SQLiteConn<'a>) (f : 'a -> SQLiteConn<'b>) : SQLiteConn<'b> =
    SQLiteConn (fun r -> let a = apply1 ma r in apply1 (f a) r)

let fail : SQLiteConn<'a> = SQLiteConn (fun r -> failwith "SQLiteConn fail")


type SQLiteConnBuilder() = 
        member self.Return x = unitM x
        member self.Bind (p,f) = bindM p f
        member self.Zero () = unitM ()

let (sqliteConn:SQLiteConnBuilder) = new SQLiteConnBuilder()



// Common operations
let fmapM (fn:'a -> 'b) (ma:SQLiteConn<'a>) : SQLiteConn<'b> = 
    SQLiteConn <| fun conn ->
        let ans = apply1 ma conn in fn ans
        
let mapM (fn:'a -> SQLiteConn<'b>) (xs:'a list) : SQLiteConn<'b list> = 
    let rec work ac ys = 
        match ys with
        | [] -> unitM <| List.rev ac
        | z :: zs -> bindM (fn z) (fun a -> work (a::ac) zs)
    work [] xs

let forM (xs:'a list) (fn:'a -> SQLiteConn<'b>) : SQLiteConn<'b list> = mapM fn xs

let mapMz (fn:'a -> SQLiteConn<'b>) (xs:'a list) : SQLiteConn<unit> = 
    let rec work ys = 
        match ys with
        | [] -> unitM ()
        | z :: zs -> bindM (fn z) (fun _ -> work zs)
    work xs

let forMz (xs:'a list) (fn:'a -> SQLiteConn<'b>) : SQLiteConn<unit> = mapMz fn xs


// SQLiteConn specific operations
let runSQLiteConn (ma:SQLiteConn<'a>) (connString:string) : 'a = 
    let dbconn = new SQLiteConnection(connString)
    dbconn.Open()
    let a = match ma with | SQLiteConn(f) -> f dbconn
    dbconn.Close()
    a
    
let liftConn (proc:SQLite.SQLiteConnection -> 'a) : SQLiteConn<'a> = SQLiteConn proc
    
let execNonQuery (statement:string) : SQLiteConn<int> = 
    SQLiteConn <| fun conn -> 
        let cmd : SQLiteCommand = new SQLiteCommand(statement, conn)
        cmd.ExecuteNonQuery ()

let execReader (statement:string) (proc:SQLite.SQLiteDataReader -> 'a) : SQLiteConn<'a> =
    SQLiteConn <| fun conn -> 
        let cmd : SQLiteCommand = new SQLiteCommand(statement, conn)
        let reader : SQLiteDataReader = cmd.ExecuteReader()
        let ans = proc reader
        reader.Close()
        ans


let withTransaction (ma:SQLiteConn<'a>) : SQLiteConn<'a> = 
    SQLiteConn <| fun conn -> 
        let trans = conn.BeginTransaction(System.Data.IsolationLevel.ReadCommitted)
        let ans = apply1 ma conn
        trans.Commit ()
        ans
