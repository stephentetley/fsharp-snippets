﻿module WadlerXPath1

// Unfinished
// This is "Figure 1: First semantics of XPath"

open Microsoft.FSharp.Collections  // for Set

type Name = String

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
    | TyText        // not generated by P, matched on in S

type Direction =
    | Forward
    | Reverse

type Node = 
    | Root of Set<Node>
    | Element of int * Name

type Context = Node * Set<Node>


type Pattern =
    | PatPipe of Pattern * Pattern         // Disjunction "|"
    | PatRoot of Pattern
    | PatSlash of Pattern * Pattern
    | PatStep of Axis * Pattern
    | PatNamed of string                   // aka [n]
    | PatWild
    | PatText of unit
    | PatTest of Pattern * Qualifier


and Qualifier = 
    | And of Qualifier * Qualifier
    | Or of Qualifier * Qualifier
    | Not of Qualifier
    | Equal of Qualifier * Qualifier
    | Pattern of Pattern   // placeholder

type Expr = 
    | Plus of Expr * Expr
    | Mult of Expr * Expr
    | Position of unit
    | Last of unit
    | Int of int



let nodetype (x:Node) : Nodetype = 
    match x with 
    | Root _ -> TyElement
    | Element _ -> TyElement

let nodename (x:Node) : string = "<bad node name>"

// this is <=doc used by E
// There appears to be an explanation of "<doc" in Gottlob, Koch and Pichler
// "Efficient Algorithms for Processing XPath Queries" page 12

let lteDoc (x1:Node) (x2:Node) : bool = 
    failwith "err"
    


// Auxillary functions

// axis i.e. A
// filling out this will need a full definition of Node
let axis (a:Axis) (x:Node) : Set<Node> = 
    match a with
    | Self -> Set.singleton x
    | _ -> failwith "err"


// principal i.e. P
let principal (p:Axis) : Nodetype = 
    match p with
    | Child -> TyElement
    | Parent -> TyElement
    | Descendant -> TyElement
    | Ancestor -> TyElement
    | Self -> TyElement
    | Attribute -> TyAttribute
    | Namespace -> TyNamespace

// principal i.e. D
let direction (d:Axis) : Direction = 
    match d with
    | Child -> Forward
    | Parent -> Reverse
    | Descendant -> Forward
    | Ancestor -> Reverse
    | Self -> Forward
    | Attribute -> Forward
    | Namespace -> Forward


// selected i.e. S
let rec selected (a:Axis) (p:Pattern) ((x,eS) : Context) : Set<Node> = 
    match p with
    | PatPipe (p1,p2) -> 
        let s1 = selected a p1 (x,eS)
        let s2 = selected a p2 (x,eS)
        Set.union s1 s2

    | PatRoot p1 -> failwith "err"

    | PatSlash (p1,p2) -> 
        let s1 = selected a p1 (x,eS)
        failwith "err"
    
    | PatStep (a1,p1) ->
        let s1 = selected a p1 (x,eS)
        Set.filter (fun x2 -> failwith "err") s1

    | PatNamed ss -> 
        let anodes = axis a x
        let typ = principal a
        Set.filter (fun x1 -> nodetype x1 = typ && nodename x1 = ss) anodes

    | PatWild ->
        let anodes = axis a x
        let typ = principal a
        Set.filter (fun x1 -> nodetype x1 = typ) anodes

    | PatText () ->
        let anodes = axis a x
        Set.filter (fun x1 -> nodetype x1 = TyText) anodes 

    | PatTest (p,q) ->
        let s1 = selected a p (x,eS)
        Set.filter (fun x1 -> qualifier a q (x1,s1)) s1

// qualifier i.e. Q
and qualifier (a:Axis) (q:Qualifier) ((x,eS) : Context) : bool = 
    match q with
    | And (q1,q2) -> 
        let b1 = qualifier a q1 (x,eS)
        let b2 = qualifier a q2 (x,eS)
        b1 && b2

    | Or (q1,q2) -> 
        let b1 = qualifier a q1 (x,eS)
        let b2 = qualifier a q2 (x,eS)
        b1 || b2

    | Not q1 -> not <| qualifier a q1 (x,eS)

    | Equal (q1,q2) -> 
        let b1 = qualifier a q1 (x,eS)
        let b2 = qualifier a q2 (x,eS)
        b1 = b2

    | Pattern p1 -> let sel1 = selected a p1 (x,eS) in not (Set.isEmpty sel1)


// evaluate i.e. E

and evaluate (a:Axis) (e:Expr) ((x,eS) : Context) : int = 
    match e with
    | Plus (e1,e2) -> 
        let i1 = evaluate a e1 (x,eS)
        let i2 = evaluate a e2 (x,eS)                  
        i1 + i2

    | Mult (e1,e2) -> 
        let i1 = evaluate a e1 (x,eS)
        let i2 = evaluate a e2 (x,eS)                  
        i1 * i2

    | Position () -> 
        let j = let subset = Set.filter (fun x1 -> lteDoc x1 x) eS in subset.Count
        if direction a = Forward then j
        else eS.Count + 1 - j

    | Last () -> eS.Count
    | Int i -> i





    
