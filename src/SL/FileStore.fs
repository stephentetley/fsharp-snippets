module SL.FileStore

open System
open System.IO

open FParsec
open FSharpx.Collections

open SL.SwOutput

type Name = string
type FilePath = string
type Size = int64


// Mode is not currently interpreted
type Properties = 
    { Mode : string option
      ModificationTime : DateTime option
    }
 

type FileObj = 
    | FsFolder of Name * Properties * FileObj list
    | FsFile of Name * Properties * Size

type FileStore = FileStore of FilePath * FileObj list


let makeDateTime (year:int) (month:int) (day:int) (hour:int) (minute:int) (second:int) : DateTime = 
    new DateTime(year, month, day, hour, minute, second)


/// ***** Operations on Files and Folders

let isFile (source:FileObj) : bool = 
    match source with
    | FsFolder _ -> false
    | FsFile _ -> true

let isFolder (source:FileObj) : bool = 
    match source with
    | FsFolder _ -> true
    | FsFile _ -> false

let tryGetExtension (source:FileObj) : string option = 
    match source with
    | FsFolder _ -> None
    | FsFile (name,_,_) -> Some <| Path.GetExtension name

let getFiles1 (source:FileStore) : FileObj list = 
    match source with
    | FileStore (_,xs) -> List.filter isFile xs
    

/// ***** PARSER
// Parsing output of "dir -Recurse"

// Utility combinators
let private ws : Parser<string,unit> = manyChars (pchar ' ' <|> pchar '\t')
let private ws1 : Parser<string,unit> = many1Chars (pchar ' ' <|> pchar '\t')

let private symbol (p:Parser<'a,unit>)      : Parser<'a,unit> = p .>> ws
let private symbol1 (p:Parser<'a,unit>)     : Parser<'a,unit> = p .>> ws1

let private keyword (s:string) : Parser<string,unit> = pstring s .>> ws
let private keyword1 (s:string) : Parser<string,unit> = pstring s .>> ws1

let private lineOf (p:Parser<'a,unit>) : Parser<'a,unit> = 
    p .>> newline

let private twice (p:Parser<'a,unit>) : Parser<('a * 'a),unit> = pipe2 p p (fun a b -> (a,b))

let private blankline : Parser<unit,unit> = lineOf ws >>. preturn ()

let private pName : Parser<Name,unit> = restOfLine false |>> (fun s -> s.TrimEnd ())


// Note this is UK centric    
let private pDateTime : Parser<DateTime,unit> = 
    pipe5   pint32 
            (pchar '/' >>. pint32) 
            (pchar '/' >>. symbol pint32) 
            pint32 
            (pchar ':' >>. pint32)
            (fun dd dm dy th tm -> makeDateTime dy dm dd th tm 0)
    
let private pMode : Parser<string,unit> = many1Chars (lower <|> pchar '-') 

let private isDir (mode:string) : bool = mode.StartsWith("d")

// Note - if directory name longer than 100(?) chars it is listed on a new line
let private pQualifiedDirectoryName : Parser<Name,unit> = 
    let indent = manyChars (pchar ' ')
    indent >>. pstring "Directory:" >>. spaces >>. pName

let private pHeadings : Parser<string list,unit> = 
    let columns = pipe4 (keyword "Mode")
                        (keyword "LastWriteTime")
                        (keyword "Length")
                        (keyword "Name")
                        (fun a b c d -> [a;b;c;d])
    let underline = restOfLine false
    columns .>> newline .>> underline

// Note - file store is flat at parse time (needs postprocessing to build)
let private pFileObj : Parser<FileObj,unit> = 
    let pFolder mode = 
         pipe2 (symbol pDateTime) 
               pName 
               (fun timestamp name -> FsFolder (name, { Mode = Some mode; ModificationTime = Some timestamp}, []))
    let pFile mode = 
        pipe3 (symbol pDateTime) 
              (symbol pint64) 
              pName 
              (fun timestamp size name-> FsFile (name, { Mode = Some mode; ModificationTime = Some timestamp}, size))
    let parseK mode = 
        if isDir mode then pFolder mode else  pFile mode
    (symbol pMode) >>= parseK


type Block = string * FileObj list

// TODO - many1 looks wrong (empty directories?)
let private pBlock : Parser<Block, unit> = 
    pipe3 (spaces >>. lineOf pQualifiedDirectoryName) 
          (twice blankline >>. lineOf pHeadings >>. many1 (lineOf pFileObj))
          spaces
          (fun dirName objs zzz -> (dirName, objs))


let private pListing : Parser<Block list,unit> = many (pBlock .>> spaces)


// Build from flat.


type private Level1Kids = Map<Name, FileObj list>

// Root is always first
let private getRoot (blocks:Block list) : Block option = 
    match blocks with
    | x :: _ -> Some x
    | _ -> None


let private makeLevel1Kids (blocks:Block list) : Level1Kids = 
    let step acc (dirName, objs) = Map.add dirName objs acc
    List.fold step Map.empty blocks

let rec private makeRecur (store:Level1Kids) (parent:Name) (fileObj:FileObj) : FileObj = 
    match fileObj with
    | FsFile _ -> fileObj
    | FsFolder(name,props,_) -> 
        let fullName = parent + "\\" + name
        let kids1 = Map.findOrDefault fullName [] store
        let kids2 = List.map (makeRecur store fullName) kids1
        FsFolder (name,props,kids2) 



let private buildTopDown (blocks:Block list) : FileStore option = 
    match blocks with 
    | (dirName,objs) :: _ -> 
        let level1Kids  = makeLevel1Kids blocks
        let realKids    = List.map (makeRecur level1Kids dirName) objs
        Some <| FileStore (dirName, realKids)
    | [] -> None



let readDirRecurseOutput (inputPath:string) : Choice<string,FileStore> = 
    let ans1 = runParserOnFile pListing () inputPath Text.ASCIIEncoding.ASCII
    match ans1 with
    | Success(a,_,_) ->  
        match buildTopDown a with
        | None -> Choice1Of2 "Parsed, but could not build FileStore"
        | Some ans -> Choice2Of2 ans
    | Failure(s,_,_) -> Choice1Of2 s



// ***** Display

let private catPath (str1:string) (str2:string) : string = 
    if String.IsNullOrEmpty str1 then str2 else str1 + "\\" + str2

let private display1 (source:FileObj) : SwOutput<unit> = 
    let rec work (path:string) (obj1:FileObj)  = 
        match obj1 with
        | FsFile (name,_,_) -> tellLine <| catPath path name
        | FsFolder (name,_,xs) -> 
            let path1 = catPath path name
            ignore <| tellLine path1
            let sortedKids = xs // TODO - need case-insensitive sort...
            forMz sortedKids (work path1)
    work "" source

let display (source:FileStore) : string = 
    let procM = 
        match source with
        | FileStore (path,kids) -> 
            swOutput { do! tellLine path
                       do! forMz kids display1 }
    fst <| runSwOutput procM
    