module PGSQLConn

open Npgsql

open ResultMonad
open SqlUtils

// SQLiteConn Monad
type PGSQLConn<'a> = PGSQLConn of (NpgsqlConnection -> Result<'a>)

let inline private apply1 (ma : PGSQLConn<'a>) (conn:NpgsqlConnection) : Result<'a> = 
    let (PGSQLConn f) = ma in f conn

let inline private unitM (x:'a) : PGSQLConn<'a> = PGSQLConn (fun _ -> Ok x)


let inline private bindM (ma:PGSQLConn<'a>) (f : 'a -> PGSQLConn<'b>) : PGSQLConn<'b> =
    PGSQLConn <| fun conn -> 
        match apply1 ma conn with
        | Ok(a) -> apply1 (f a) conn
        | Err(msg) -> Err(msg)

let fail : PGSQLConn<'a> = PGSQLConn (fun _ -> Err "PGSQLConn fail")


type PGSQLConnBuilder() = 
    member self.Return x = unitM x
    member self.Bind (p,f) = bindM p f
    member self.Zero () = unitM ()

let (pgsqlConn:PGSQLConnBuilder) = new PGSQLConnBuilder()


// Common operations
let fmapM (fn:'a -> 'b) (ma:PGSQLConn<'a>) : PGSQLConn<'b> = 
    PGSQLConn <| fun conn ->
       match apply1 ma conn with
       | Ok(ans) -> Ok <| fn ans
       | Err(msg) -> Err(msg)

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

let mapiM (fn:int -> 'a -> PGSQLConn<'b>) (xs:'a list) : PGSQLConn<'b list> = 
    let rec work ac ix ys = 
        match ys with
        | [] -> unitM <| List.rev ac
        | z :: zs -> bindM (fn ix z) (fun a -> work (a::ac) (ix+1) zs)
    work [] 0 xs

let mapiMz (fn:int -> 'a -> PGSQLConn<'b>) (xs:'a list) : PGSQLConn<unit> = 
    let rec work ix ys = 
        match ys with
        | [] -> unitM ()
        | z :: zs -> bindM (fn ix z) (fun _ -> work (ix+1) zs)
    work 0 xs
let forMz (xs:'a list) (fn:'a -> PGSQLConn<'b>) : PGSQLConn<unit> = mapMz fn xs


let traverseM (fn: 'a -> PGSQLConn<'b>) (source:seq<'a>) : PGSQLConn<seq<'b>> = 
    PGSQLConn <| fun conn ->
        ResultMonad.traverseM (fun x -> let mf = fn x in apply1 mf conn) source

let traverseMz (fn: 'a -> PGSQLConn<'b>) (source:seq<'a>) : PGSQLConn<unit> = 
    PGSQLConn <| fun conn ->
        ResultMonad.traverseMz (fun x -> let mf = fn x in apply1 mf conn) source

// PGSQLConn-specific operations
let runPGSQLConn (ma:PGSQLConn<'a>) (connString:string) : Result<'a> = 
    let dbconn = new NpgsqlConnection(connString)
    dbconn.Open()
    let a = match ma with | PGSQLConn(f) -> f dbconn
    dbconn.Close()
    a

let throwError (msg:string) : PGSQLConn<'a> = 
    PGSQLConn <| fun _ -> Err(msg)

let annotateError (msg:string) (ma:PGSQLConn<'a>) : PGSQLConn<'a> = 
    PGSQLConn <| fun conn -> 
        match apply1 ma conn with
        | Err(_) -> Err msg
        | Ok(a) -> Ok a


let liftConn (proc:NpgsqlConnection -> 'a) : PGSQLConn<'a> = 
    PGSQLConn <| fun conn -> 
        try 
            let ans = proc conn in Ok (ans)
        with
        | ex -> Err(ex.ToString())

let execNonQuery (statement:string) : PGSQLConn<int> = 
    liftConn <| fun conn -> 
        let cmd : NpgsqlCommand = new NpgsqlCommand(statement, conn)
        cmd.ExecuteNonQuery ()

let execReader (statement:string) (proc:NpgsqlDataReader -> 'a) : PGSQLConn<'a> =
    liftConn <| fun conn -> 
        let cmd : NpgsqlCommand = new NpgsqlCommand(statement, conn)
        let reader = cmd.ExecuteReader()
        let ans = proc reader
        reader.Close()
        ans

// The read procedure (proc) is expected to read from a single row.
let execReaderList (statement:string) (proc:NpgsqlDataReader -> 'a) : PGSQLConn<'a list> =
    liftConn <| fun conn -> 
        let cmd : NpgsqlCommand = new NpgsqlCommand(statement, conn)
        let reader = cmd.ExecuteReader()
        let resultset = 
            seq { while reader.Read() do
                    let ans = proc reader
                    yield ans }  |> Seq.toList
        reader.Close()
        resultset

// The read procedure (proc) is expected to read from a single row.
let execReaderArray (statement:string) (proc:NpgsqlDataReader -> 'a) : PGSQLConn<'a []> =
    liftConn <| fun conn -> 
        let cmd : NpgsqlCommand = new NpgsqlCommand(statement, conn)
        let reader = cmd.ExecuteReader()
        let resultset = 
            seq { while reader.Read() do
                    let ans = proc reader
                    yield ans }  |> Seq.toArray
        reader.Close()
        resultset


// The read procedure (proc) is expected to read from a single row.
// The query should return exactly one row.
let execReaderSingleton (statement:string) (proc:NpgsqlDataReader -> 'a) : PGSQLConn<'a> =
    PGSQLConn <| fun conn -> 
        try 
            let cmd : NpgsqlCommand = new NpgsqlCommand(statement, conn)
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

// With error handling added to the monad we should be able to rollback instead...
let withTransaction (ma:PGSQLConn<'a>) : PGSQLConn<'a> = 
    PGSQLConn <| fun conn -> 
        let trans = conn.BeginTransaction(System.Data.IsolationLevel.ReadCommitted)
        try 
            let ans = apply1 ma conn
            match ans with
            | Ok(a) -> trans.Commit () ; ans
            | Err(msg) -> trans.Rollback () ; ans
        with 
        | ex -> trans.Rollback() ; Err( ex.ToString() )
        


let withTransactionList (values:'a list) (proc1:'a -> PGSQLConn<'b>) : PGSQLConn<'b list> = 
    withTransaction (forM values proc1)


let withTransactionListSum (values:'a list) (proc1:'a -> PGSQLConn<int>) : PGSQLConn<int> = 
    fmapM (List.sum) <| withTransactionList values proc1



let withTransactionSeq (values:seq<'a>) (proc1:'a -> PGSQLConn<'b>) : PGSQLConn<seq<'b>> = 
    withTransaction (traverseM proc1 values)
    
let withTransactionSeqSum (values:seq<'a>) (proc1:'a -> PGSQLConn<int>) : PGSQLConn<int> = 
    fmapM (Seq.sum) <| withTransactionSeq values proc1

// Run a ``TRUNCATE TABLE`` query
let deleteAllRows (tableName:string) : PGSQLConn<int> = 
    let query = sprintf "TRUNCATE TABLE %s;" tableName
    execNonQuery query