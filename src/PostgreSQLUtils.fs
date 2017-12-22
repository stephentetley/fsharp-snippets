﻿module PostgreSQLUtils

open Npgsql

// SQLiteConn Monad

type PGSQLConn<'a> = PGSQLConn of (NpgsqlConnection -> 'a)

let inline private apply1 (ma : PGSQLConn<'a>) (conn:NpgsqlConnection) : 'a = 
    let (PGSQLConn f) = ma in f conn

let inline private unitM (x:'a) : PGSQLConn<'a> = PGSQLConn (fun r -> x)


let inline private bindM (ma:PGSQLConn<'a>) (f : 'a -> PGSQLConn<'b>) : PGSQLConn<'b> =
    PGSQLConn (fun r -> let a = apply1 ma r in apply1 (f a) r)

let fail : PGSQLConn<'a> = PGSQLConn (fun r -> failwith "PGSQLConn fail")


type PGSQLConnBuilder() = 
        member self.Return x = unitM x
        member self.Bind (p,f) = bindM p f
        member self.Zero () = unitM ()

let (pgsqlConn:PGSQLConnBuilder) = new PGSQLConnBuilder()


// Common operations
let fmapM (fn:'a -> 'b) (ma:PGSQLConn<'a>) : PGSQLConn<'b> = 
    PGSQLConn <| fun conn ->
        let ans = apply1 ma conn in fn ans

let mapM (fn:'a -> PGSQLConn<'b>) (xs:'a list) : PGSQLConn<'b list> = 
    let rec work ac ys = 
        match ys with
        | [] -> unitM <| List.rev ac
        | z :: zs -> bindM (fn z) (fun a -> work (a::ac) zs)
    work [] xs

let forM (xs:'a list) (fn:'a -> PGSQLConn<'b>) : PGSQLConn<'b list> = mapM fn xs

let mapMz (fn:'a -> PGSQLConn<'b>) (xs:'a list) : PGSQLConn<unit> = 
    let rec work ys = 
        match ys with
        | [] -> unitM ()
        | z :: zs -> bindM (fn z) (fun _ -> work zs)
    work xs

let forMz (xs:'a list) (fn:'a -> PGSQLConn<'b>) : PGSQLConn<unit> = mapMz fn xs


// PGSQLConn-specific operations
let runPGSQLConn (ma:PGSQLConn<'a>) (connString:string) : 'a = 
    let dbconn = new NpgsqlConnection(connString)
    dbconn.Open()
    let a = match ma with | PGSQLConn(f) -> f dbconn
    dbconn.Close()
    a

let liftConn (proc:NpgsqlConnection -> 'a) : PGSQLConn<'a> = PGSQLConn proc

let execNonQuery (statement:string) : PGSQLConn<int> = 
    PGSQLConn <| fun conn -> 
        let cmd : NpgsqlCommand = new NpgsqlCommand(statement, conn)
        cmd.ExecuteNonQuery ()

let execReader (statement:string) (proc:NpgsqlDataReader -> 'a) : PGSQLConn<'a> =
    PGSQLConn <| fun conn -> 
        let cmd : NpgsqlCommand = new NpgsqlCommand(statement, conn)
        let reader = cmd.ExecuteReader()
        let ans = proc reader
        reader.Close()
        ans

// With error handling added to the monad we should be able to rollback instead...
let withTransaction (ma:PGSQLConn<'a>) : PGSQLConn<'a> = 
    PGSQLConn <| fun conn -> 
        let trans = conn.BeginTransaction(System.Data.IsolationLevel.ReadCommitted)
        let ans = apply1 ma conn
        trans.Commit ()
        ans
        

 