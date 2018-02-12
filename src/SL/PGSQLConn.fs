module SL.PGSQLConn

open Npgsql

open SL.AnswerMonad
open SL.SqlUtils


type PGSQLConnParams = 
    { Host : string 
      Username : string 
      Password : string 
      Database : string }


let paramsConnString (config:PGSQLConnParams) : string = 
    sprintf "Host=%s;Username=%s;Password=%s;Database=%s" config.Host config.Username config.Password config.Database

/// Host="localhost"; Username="postgres"
let pgsqlConnParamsTesting (dbName:string) (password:string) : PGSQLConnParams = 
    { Host = "localhost"; Username = "postgres"; Database = dbName; Password = password }



// SQLiteConn Monad
type PGSQLConn<'a> = PGSQLConn of (NpgsqlConnection -> Answer<'a>)

let inline private apply1 (ma : PGSQLConn<'a>) (conn:NpgsqlConnection) : Answer<'a> = 
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
    PGSQLConn <| fun conn ->
        AnswerMonad.mapM (fun a -> apply1 (fn a) conn) xs

let forM (xs:'a list) (fn:'a -> PGSQLConn<'b>) : PGSQLConn<'b list> = mapM fn xs

let mapMz (fn:'a -> PGSQLConn<'b>) (xs:'a list) : PGSQLConn<unit> = 
    PGSQLConn <| fun conn ->
        AnswerMonad.mapMz (fun a -> apply1 (fn a) conn) xs

let forMz (xs:'a list) (fn:'a -> PGSQLConn<'b>) : PGSQLConn<unit> = mapMz fn xs

let mapiM (fn:int -> 'a -> PGSQLConn<'b>) (xs:'a list) : PGSQLConn<'b list> = 
    PGSQLConn <| fun conn ->
        AnswerMonad.mapiM (fun ix a -> apply1 (fn ix a) conn) xs

let mapiMz (fn:int -> 'a -> PGSQLConn<'b>) (xs:'a list) : PGSQLConn<unit> = 
    PGSQLConn <| fun conn ->
        AnswerMonad.mapiMz (fun ix a -> apply1 (fn ix a) conn) xs

let foriM (xs:'a list) (fn:int -> 'a -> PGSQLConn<'b>) : PGSQLConn<'b list> = mapiM fn xs

let traverseM (fn: 'a -> PGSQLConn<'b>) (source:seq<'a>) : PGSQLConn<seq<'b>> = 
    PGSQLConn <| fun conn ->
        AnswerMonad.traverseM (fun x -> let mf = fn x in apply1 mf conn) source

let traverseMz (fn: 'a -> PGSQLConn<'b>) (source:seq<'a>) : PGSQLConn<unit> = 
    PGSQLConn <| fun conn ->
        AnswerMonad.traverseMz (fun x -> let mf = fn x in apply1 mf conn) source

let traverseiM (fn:int -> 'a -> PGSQLConn<'b>) (source:seq<'a>) : PGSQLConn<seq<'b>> = 
    PGSQLConn <| fun conn ->
        AnswerMonad.traverseiM (fun ix x -> let mf = fn ix x in apply1 mf conn) source

let traverseiMz (fn:int -> 'a -> PGSQLConn<'b>) (source:seq<'a>) : PGSQLConn<unit> = 
    PGSQLConn <| fun conn ->
        AnswerMonad.traverseiMz (fun ix x -> let mf = fn ix x in apply1 mf conn) source

let sequenceM (source:PGSQLConn<'a> list) : PGSQLConn<'a list> = 
    PGSQLConn <| fun conn ->
        AnswerMonad.sequenceM <| List.map (fun ma -> apply1 ma conn) source

let sequenceMz (source:PGSQLConn<'a> list) : PGSQLConn<unit> = 
    PGSQLConn <| fun conn ->
        AnswerMonad.sequenceMz <| List.map (fun ma -> apply1 ma conn) source


// Summing variants

let sumMapM (fn:'a -> PGSQLConn<int>) (xs:'a list) : PGSQLConn<int> = 
    fmapM List.sum <| mapM fn xs

let sumMapiM (fn:int -> 'a -> PGSQLConn<int>) (xs:'a list) : PGSQLConn<int> = 
    fmapM List.sum <| mapiM fn xs

let sumForM (xs:'a list) (fn:'a -> PGSQLConn<int>) : PGSQLConn<int> = 
    fmapM List.sum <| forM xs fn

let sumForiM (xs:'a list) (fn:int -> 'a -> PGSQLConn<int>) : PGSQLConn<int> = 
    fmapM List.sum <| foriM xs fn

let sumTraverseM (fn: 'a -> PGSQLConn<int>) (source:seq<'a>) : PGSQLConn<int> =
    fmapM Seq.sum <| traverseM fn source

let sumTraverseiM (fn:int -> 'a -> PGSQLConn<int>) (source:seq<'a>) : PGSQLConn<int> =
    fmapM Seq.sum <| traverseiM fn source

let sumSequenceM (source:PGSQLConn<int> list) : PGSQLConn<int> = 
    PGSQLConn <| fun conn ->
        AnswerMonad.sumSequenceM (List.map (fun mf -> apply1 mf conn) source)


// PGSQLConn-specific operations
let runPGSQLConn (ma:PGSQLConn<'a>) (connParams:PGSQLConnParams) : Answer<'a> = 
    let conn = paramsConnString connParams
    let dbconn = new NpgsqlConnection(conn)
    dbconn.Open()
    let a = match ma with | PGSQLConn(f) -> f dbconn
    dbconn.Close()
    a

let throwError (msg:string) : PGSQLConn<'a> = 
    PGSQLConn <| fun _ -> Err(msg)

let swapError (msg:string) (ma:PGSQLConn<'a>) : PGSQLConn<'a> = 
    PGSQLConn <| fun conn -> 
        AnswerMonad.swapError msg (apply1 ma conn)

let augmentError (fn:string -> string) (ma:PGSQLConn<'a>) : PGSQLConn<'a> = 
    PGSQLConn <| fun conn -> 
        AnswerMonad.augmentError fn (apply1 ma conn)

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

//let execPreparedStatement (statement:string) : PGSQLConn<int> = 
//    liftConn <| fun conn -> 
//        let cmd : NpgsqlCommand = new NpgsqlCommand(statement, conn)
//        cmd.Prepare()
//        printfn "IsPrepared: %A"       cmd.IsPrepared
//        cmd.ExecuteNonQuery ()

let execReader (statement:string) (proc:NpgsqlDataReader -> 'a) : PGSQLConn<'a> =
    liftConn <| fun conn -> 
        let cmd : NpgsqlCommand = new NpgsqlCommand(statement, conn)
        let reader = cmd.ExecuteReader()
        let ans = proc reader
        reader.Close()
        ans

/// The read procedure (proc) is expected to read from a single row.
/// WARNING - this does not seem to work, possibly releasing the reader
/// before all records are read. Use execReaderList until we have invetigated...
let execReaderSeq (statement:string) (proc:NpgsqlDataReader -> 'a) : PGSQLConn<seq<'a>> =
    liftConn <| fun conn -> 
        let cmd : NpgsqlCommand = new NpgsqlCommand(statement, conn)
        let reader = cmd.ExecuteReader()
        let resultset = 
            seq { while reader.Read() do
                    let ans = proc reader
                    yield ans } 
        reader.Close()
        resultset

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

/// Err if no answers
let execReaderFirst (statement:string) (proc:NpgsqlDataReader -> 'a) : PGSQLConn<'a> =
    PGSQLConn <| fun conn -> 
        try 
            let cmd : NpgsqlCommand = new NpgsqlCommand(statement, conn)
            let reader = cmd.ExecuteReader()
            if reader.Read() then
                let ans = proc reader
                reader.Close()
                Ok <| ans
            else
                reader.Close ()
                Err <| "execReaderFirst - no results."
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

/// Run a ``TRUNCATE TABLE`` query
let deleteAllRows (tableName:string) : PGSQLConn<int> = 
    let query = sprintf "TRUNCATE TABLE %s;" tableName
    execNonQuery query

/// Run a ``TRUNCATE TABLE name RESTART IDENTITY;`` query.
/// Use this for tables with a SERIAL id.
let deleteAllRowsRestartIdentity (tableName:string) : PGSQLConn<int> = 
    let query = sprintf "TRUNCATE TABLE %s RESTART IDENTITY;" tableName
    execNonQuery query
