module SL.SQLiteConn

open System
open System.IO
open System.Data
open System.Data.SQLite

open SL.AnswerMonad
open SL.SqlUtils


type SQLiteConnParams = 
    { PathToDB : string 
      SQLiteVersion : string }


let paramsConnString (config:SQLiteConnParams) : string = 
    sprintf "Data Source=%s;Version=%s;" config.PathToDB config.SQLiteVersion

let sqliteConnParamsVersion3 (pathToDB:string) : SQLiteConnParams = 
    { PathToDB = pathToDB; SQLiteVersion = "3" }




// SQLiteConn Monad - a Reader-Error monad
type SQLiteConn<'a> = SQLiteConn of (SQLite.SQLiteConnection -> Answer<'a>)

let inline private apply1 (ma : SQLiteConn<'a>) (conn:SQLite.SQLiteConnection) : Answer<'a> = 
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
       AnswerMonad.fmapM fn <| apply1 ma conn
       

let mapM (fn:'a -> SQLiteConn<'b>) (xs:'a list) : SQLiteConn<'b list> = 
    SQLiteConn <| fun conn ->
        AnswerMonad.mapM (fun a -> apply1 (fn a) conn) xs

let forM (xs:'a list) (fn:'a -> SQLiteConn<'b>) : SQLiteConn<'b list> = mapM fn xs

let mapMz (fn:'a -> SQLiteConn<'b>) (xs:'a list) : SQLiteConn<unit> = 
    SQLiteConn <| fun conn ->
        AnswerMonad.mapMz (fun a -> apply1 (fn a) conn) xs

let forMz (xs:'a list) (fn:'a -> SQLiteConn<'b>) : SQLiteConn<unit> = mapMz fn xs


let mapiM (fn:int -> 'a -> SQLiteConn<'b>) (xs:'a list) : SQLiteConn<'b list> = 
    SQLiteConn <| fun conn ->
        AnswerMonad.mapiM (fun ix a -> apply1 (fn ix a) conn) xs

let mapiMz (fn:int -> 'a -> SQLiteConn<'b>) (xs:'a list) : SQLiteConn<unit> = 
    SQLiteConn <| fun conn ->
        AnswerMonad.mapiMz (fun ix a -> apply1 (fn ix a) conn) xs

let foriM (xs:'a list) (fn:int -> 'a -> SQLiteConn<'b>) : SQLiteConn<'b list> = 
    mapiM fn xs

let foriMz (xs:'a list) (fn:int -> 'a -> SQLiteConn<'b>) : SQLiteConn<unit> = 
    mapiMz fn xs


// Note - goes through intermediate list, see AnswerMonad.traverseM
let traverseM (fn: 'a -> SQLiteConn<'b>) (source:seq<'a>) : SQLiteConn<seq<'b>> = 
    SQLiteConn <| fun conn ->
        AnswerMonad.traverseM (fun x -> let mf = fn x in apply1 mf conn) source

let traverseMz (fn: 'a -> SQLiteConn<'b>) (source:seq<'a>) : SQLiteConn<unit> = 
    SQLiteConn <| fun conn ->
        AnswerMonad.traverseMz (fun x -> let mf = fn x in apply1 mf conn) source

let traverseiM (fn:int -> 'a -> SQLiteConn<'b>) (source:seq<'a>) : SQLiteConn<seq<'b>> = 
    SQLiteConn <| fun conn ->
        AnswerMonad.traverseiM (fun ix x -> let mf = fn ix x in apply1 mf conn) source

let traverseiMz (fn:int -> 'a -> SQLiteConn<'b>) (source:seq<'a>) : SQLiteConn<unit> = 
    SQLiteConn <| fun conn ->
        AnswerMonad.traverseiMz (fun ix x -> let mf = fn ix x in apply1 mf conn) source

let sequenceM (source:SQLiteConn<'a> list) : SQLiteConn<'a list> = 
    SQLiteConn <| fun conn ->
        AnswerMonad.sequenceM <| List.map (fun ma -> apply1 ma conn) source

let sequenceMz (source:SQLiteConn<'a> list) : SQLiteConn<unit> = 
    SQLiteConn <| fun conn ->
        AnswerMonad.sequenceMz <| List.map (fun ma -> apply1 ma conn) source

// Summing variants

let sumMapM (fn:'a -> SQLiteConn<int>) (xs:'a list) : SQLiteConn<int> = 
    fmapM List.sum <| mapM fn xs

let sumMapiM (fn:int -> 'a -> SQLiteConn<int>) (xs:'a list) : SQLiteConn<int> = 
    fmapM List.sum <| mapiM fn xs

let sumForM (xs:'a list) (fn:'a -> SQLiteConn<int>) : SQLiteConn<int> = 
    fmapM List.sum <| forM xs fn

let sumForiM (xs:'a list) (fn:int -> 'a -> SQLiteConn<int>) : SQLiteConn<int> = 
    fmapM List.sum <| foriM xs fn

let sumTraverseM (fn: 'a -> SQLiteConn<int>) (source:seq<'a>) : SQLiteConn<int> =
    fmapM Seq.sum <| traverseM fn source

let sumTraverseiM (fn:int -> 'a -> SQLiteConn<int>) (source:seq<'a>) : SQLiteConn<int> =
    fmapM Seq.sum <| traverseiM fn source

let sumSequenceM (source:SQLiteConn<int> list) : SQLiteConn<int> = 
    fmapM List.sum <| sequenceM source


// SQLiteConn specific operations
let runSQLiteConn (connParams:SQLiteConnParams) (ma:SQLiteConn<'a>): Answer<'a> = 
    let conn = paramsConnString connParams
    try 
        let dbconn = new SQLiteConnection(conn)
        dbconn.Open()
        let a = match ma with | SQLiteConn(f) -> f dbconn
        dbconn.Close()
        a
    with
    | err -> Err err.Message

let throwError (msg:string) : SQLiteConn<'a> = 
    SQLiteConn <| fun _ -> Err(msg)

let swapError (msg:string) (ma:SQLiteConn<'a>) : SQLiteConn<'a> = 
    SQLiteConn <| fun conn -> 
        AnswerMonad.swapError msg (apply1 ma conn)

let augmentError (fn:string -> string) (ma:SQLiteConn<'a>) : SQLiteConn<'a> = 
    SQLiteConn <| fun conn ->
        AnswerMonad.augmentError fn (apply1 ma conn)


let liftConn (proc:SQLite.SQLiteConnection -> 'a) : SQLiteConn<'a> = 
    SQLiteConn <| fun conn -> 
        try 
            let ans = proc conn in Ok (ans)
        with
        | err -> Err err.Message

    
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

/// The read procedure (proc) is expected to read from a single row.
/// WARNING - the equivalent function on PGSQLConn does not work.
/// Both need to be investigated.
let execReaderSeq (statement:string) (proc:SQLite.SQLiteDataReader -> 'a) : SQLiteConn<seq<'a>> =
    liftConn <| fun conn -> 
        let cmd : SQLiteCommand = new SQLiteCommand(statement, conn)
        let reader = cmd.ExecuteReader()
        let resultset = 
            seq { while reader.Read() do
                    let ans = proc reader
                    yield ans }
        reader.Close()
        resultset

// The read procedure (proc) is expected to read from a single row.
let execReaderList (statement:string) (proc:SQLite.SQLiteDataReader -> 'a) : SQLiteConn<'a list> =
    liftConn <| fun conn -> 
        let cmd : SQLiteCommand = new SQLiteCommand(statement, conn)
        let reader = cmd.ExecuteReader()
        let resultset = 
            seq { while reader.Read() do
                    let ans = proc reader
                    yield ans } |> Seq.toList
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
                    yield ans } |> Seq.toArray
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

let withTransactionList (values:'a list) (proc1:'a -> SQLiteConn<'b>) : SQLiteConn<'b list> = 
    withTransaction (forM values proc1)


let withTransactionListSum (values:'a list) (proc1:'a -> SQLiteConn<int>) : SQLiteConn<int> = 
    fmapM (List.sum) <| withTransactionList values proc1


let withTransactionSeq (values:seq<'a>) (proc1:'a -> SQLiteConn<'b>) : SQLiteConn<seq<'b>> = 
    withTransaction (traverseM proc1 values)
    
let withTransactionSeqSum (values:seq<'a>) (proc1:'a -> SQLiteConn<int>) : SQLiteConn<int> = 
    fmapM (Seq.sum) <| withTransactionSeq values proc1

// Run a ``DELETE FROM`` query
let deleteAllRows (tableName:string) : SQLiteConn<int> = 
    let query = sprintf "DELETE FROM %s;" tableName
    execNonQuery query