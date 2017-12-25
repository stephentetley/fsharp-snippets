module PostgreSQLUtils

open Npgsql

// SQLiteConn Monad
type Result<'a> = 
    | Ok of 'a
    | Err of string

let private resultToChoice (result:Result<'a>) : Choice<string,'a> =
    match result with
    | Err(msg) -> Choice1Of2(msg)
    | Ok(a) -> Choice2Of2(a)


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

let forMz (xs:'a list) (fn:'a -> PGSQLConn<'b>) : PGSQLConn<unit> = mapMz fn xs


// PGSQLConn-specific operations
let runPGSQLConn (ma:PGSQLConn<'a>) (connString:string) : Choice<string,'a> = 
    let dbconn = new NpgsqlConnection(connString)
    dbconn.Open()
    let a = match ma with | PGSQLConn(f) -> f dbconn
    dbconn.Close()
    resultToChoice a

let throwError (msg:string) : PGSQLConn<'a> = 
    PGSQLConn <| fun _ -> Err(msg)

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
        

 