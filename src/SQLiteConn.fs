﻿module SQLiteConn

open System
open System.IO
open System.Data
open System.Data.SQLite

open ResultMonad
open SqlUtils


// SQLiteConn Monad - a Reader-Error monad
type SQLiteConn<'a> = SQLiteConn of (SQLite.SQLiteConnection -> Result<'a>)

let inline private apply1 (ma : SQLiteConn<'a>) (conn:SQLite.SQLiteConnection) : Result<'a> = 
    let (SQLiteConn f) = ma in f conn

let inline private unitM (x:'a) : SQLiteConn<'a> = SQLiteConn (fun _ -> Ok x)


let inline private bindM (ma:SQLiteConn<'a>) (f : 'a -> SQLiteConn<'b>) : SQLiteConn<'b> =
    SQLiteConn <| fun conn -> 
        match apply1 ma conn with
        | Ok(a) -> apply1 (f a) conn
        | Err(msg) -> Err(msg)

let fail : SQLiteConn<'a> = SQLiteConn (fun r -> Err "SQLiteConn fail")


type SQLiteConnBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let (sqliteConn:SQLiteConnBuilder) = new SQLiteConnBuilder()



// Common operations
let fmapM (fn:'a -> 'b) (ma:SQLiteConn<'a>) : SQLiteConn<'b> = 
    SQLiteConn <| fun conn ->
       match apply1 ma conn with
       | Ok(ans) -> Ok <| fn ans
       | Err(msg) -> Err(msg)

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
let runSQLiteConn (ma:SQLiteConn<'a>) (connString:string) : Result<'a> = 
    let dbconn = new SQLiteConnection(connString)
    dbconn.Open()
    let a = match ma with | SQLiteConn(f) -> f dbconn
    dbconn.Close()
    a

let throwError (msg:string) : SQLiteConn<'a> = 
    SQLiteConn <| fun _ -> Err(msg)

let annotateError (msg:string) (ma:SQLiteConn<'a>) : SQLiteConn<'a> = 
    SQLiteConn <| fun conn -> 
        match apply1 ma conn with
        | Err(_) -> Err msg
        | Ok(a) -> Ok a
        
let liftConn (proc:SQLite.SQLiteConnection -> 'a) : SQLiteConn<'a> = 
    SQLiteConn <| fun conn -> 
        try 
            let ans = proc conn in Ok (ans)
        with
        | ex -> Err(ex.ToString())

    
let execNonQuery (statement:string) : SQLiteConn<int> = 
    liftConn <| fun conn -> 
        let cmd : SQLiteCommand = new SQLiteCommand(statement, conn)
        cmd.ExecuteNonQuery ()

let execReader (statement:string) (proc:SQLite.SQLiteDataReader -> 'a) : SQLiteConn<'a> =
    liftConn <| fun conn -> 
        let cmd : SQLiteCommand = new SQLiteCommand(statement, conn)
        let reader : SQLiteDataReader = cmd.ExecuteReader()
        let ans = proc reader
        reader.Close()
        ans

// The read procedure (proc) is expected to read from a single row.
let execReaderList (statement:string) (proc:SQLite.SQLiteDataReader -> 'a) : SQLiteConn<'a list> =
    liftConn <| fun conn -> 
        let cmd : SQLiteCommand = new SQLiteCommand(statement, conn)
        let reader = cmd.ExecuteReader()
        let resultset = 
            seq { while reader.Read() do
                    let ans = proc reader
                    yield ans }  |> Seq.toList
        reader.Close()
        resultset

// The read procedure (proc) is expected to read from a single row.
let execReaderArray (statement:string) (proc:SQLite.SQLiteDataReader -> 'a) : SQLiteConn<'a []> =
    liftConn <| fun conn -> 
        let cmd : SQLiteCommand = new SQLiteCommand(statement, conn)
        let reader = cmd.ExecuteReader()
        let resultset = 
            seq { while reader.Read() do
                    let ans = proc reader
                    yield ans }  |> Seq.toArray
        reader.Close()
        resultset


// The read procedure (proc) is expected to read from a single row.
// The query should return exactly one row.
let execReaderSingleton (statement:string) (proc:SQLite.SQLiteDataReader -> 'a) : SQLiteConn<'a> =
    SQLiteConn <| fun conn -> 
        try 
            let cmd : SQLiteCommand = new SQLiteCommand(statement, conn)
            let reader = cmd.ExecuteReader()
            if reader.Read() then
                let ans = proc reader
                let hasMore =  reader.Read()
                reader.Close()
                if not hasMore then
                    Ok <| ans
                else 
                    Err <| "execReaderSingleton - too many results."
            else
                reader.Close ()
                Err <| "execReaderSingleton - no results."
        with
        | ex -> Err(ex.ToString())

let withTransaction (ma:SQLiteConn<'a>) : SQLiteConn<'a> = 
    SQLiteConn <| fun conn -> 
        let trans = conn.BeginTransaction(System.Data.IsolationLevel.ReadCommitted)
        try 
            let ans = apply1 ma conn
            match ans with
            | Ok(a) -> trans.Commit () ; ans
            | Err(msg) -> trans.Rollback () ; ans
        with 
        | ex -> trans.Rollback() ; Err( ex.ToString() )