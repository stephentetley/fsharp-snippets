module WadlerXPath

open Microsoft.FSharp.Collections  // for Set



type Axis =
    | Child
    | Parent
    | Descendant
    | Ancestor
    | Self
    | Attribute
    | Namespace

type Nodetype = 
    | TyAttribute
    | TyElement
    | TyNamespace

type Direction =
    | Forward
    | Reverse

type Pattern =
    | Pipe of Pattern * Pattern
    | Root of Pattern
    // more to do

type Expr = 
    | Plus of Expr * Expr
    | Mult of Expr * Expr
    | Position of unit
    | Last of unit
    | Int of int

type Qualifier = 
    | And of Qualifier * Qualifier
    | Or of Qualifier * Qualifier
    | Not of Qualifier
    | Equal of Qualifier * Qualifier
    | Pattern of unit   // placeholder



type Node = Node of int     // placeholder

type Context = Node * Set<Node>

// selected i.e. S
let rec selected1 (a:Axis) (p:Pattern) ((x,s1) : Context) : Set<Node> = 
    match p with
    | Pipe (p1,p2) -> 
        let s1 = selected1 a p1 (x,s1)
        let s2 = selected1 a p2 (x,s1)
        Set.union s1 s2

    | Root p1 -> failwith "err"

// qualifier i.e. Q
let rec qualifier1 (a:Axis) (q:Qualifier) ((x,s1) : Context) : bool = 
    match q with
    | And (q1,q2) -> 
        let b1 = qualifier1 a q1 (x,s1)
        let b2 = qualifier1 a q2 (x,s1)
        b1 && b2

    | Or (q1,q2) -> 
        let b1 = qualifier1 a q1 (x,s1)
        let b2 = qualifier1 a q2 (x,s1)
        b1 || b2

    | Not q1 -> not <| qualifier1 a q1 (x,s1)

    | Equal (q1,q2) -> 
        let b1 = qualifier1 a q1 (x,s1)
        let b2 = qualifier1 a q2 (x,s1)
        b1 = b2

    | Pattern () -> failwith "err"

// evaluate i.e. E

let rec evaluate1 (a:Axis) (e:Expr) ((x,s1) : Context) : int = 
    match e with
    | Plus (e1,e2) -> 
        let i1 = evaluate1 a e1 (x,s1)
        let i2 = evaluate1 a e2 (x,s1)                  
        i1 + i2

    | Mult (e1,e2) -> 
        let i1 = evaluate1 a e1 (x,s1)
        let i2 = evaluate1 a e2 (x,s1)                  
        i1 * i2
    | Position () -> failwith "err"
    | Last () -> s1.Count
    | Int i -> i


let principal (p:Axis) : Nodetype = 
    match p with
    | Child -> TyElement
    | Parent -> TyElement
    | Descendant -> TyElement
    | Ancestor -> TyElement
    | Self -> TyElement
    | Attribute -> TyAttribute
    | Namespace -> TyNamespace


let direction (d:Axis) : Direction = 
    match d with
    | Child -> Forward
    | Parent -> Reverse
    | Descendant -> Forward
    | Ancestor -> Reverse
    | Self -> Forward
    | Attribute -> Forward
    | Namespace -> Forward


    

