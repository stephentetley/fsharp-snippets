module XPath2


open Microsoft.FSharp.Collections  // for Set

type Name = string

type Node =
    | Root of Name * Node list
    | Element of Name * Node list
    | Text of string
     

let doc4 : Node = 
    Root("a", 
         [ Element("b", 
                   [Text "c1"; Element("b", [Text "c2"])]) ])





// firstchild has an obvious tree interpretation, but Gottlob et al talk of nodesets
let firstchild0 (x:Node) : Node option = 
    match x with
    | Root (_,[x1:_]) -> Some x1
    | Element (_,[x1:_]) -> Some x1
    | _ -> None

// Navigation through axes is not supported by the Node type, we would need
// a tree zipper for firstchild-1 and nextsibling-1.

type Path = 
    | PathRoot 
    | Path of Name * Node list * Path * Node list      // Todo - should store Name of element in Path for rebuilding

type Location = Loc of Node * Path

let goLeft (x:Location) : Location option = 
    match x with
    | Loc (t, Path(name, l::ls, up, rs)) -> Some <| Loc(l, Path(name, ls, up, rs))
    | Loc (_, Path(_,[],_,_)) -> None
    | Loc (_, PathRoot) -> None


let goRight (x:Location) : Location option = 
    match x with 
    | Loc (t, Path (name, ls, up, r::rs)) -> Some <| Loc(r, Path(name, (t::ls), up, rs))
    | _ -> None


let goUp (x:Location) : Location option =
    match x with
    | Loc (t, PathRoot)  -> None
    | Loc (t, Path(name, ls, up, rs)) -> Some <| Loc (Element(name, List.append (List.rev ls) (t::rs)), up)

let goDown (x:Location) : Location option = 
    match x with
    | Loc (Root (name, x::xs), p) -> Some <| Loc (x, Path(name, [], p, xs))
    | Loc (Element (name, x::xs), p) -> Some <| Loc (x, Path(name, [], p, xs))
    | _ -> None

let firstchild = goDown
let nextsibling = goRight
let invfirstchild = goUp
let invnextsibling = goLeft
let self (x:Location) : Location option = Some x

let ocombine (mf : Location -> Location option) (mg : Location -> Location option) = 
    function a -> Option.bind mg (mf a)

let top (x:Node) : Location = Loc (x, PathRoot)

// This is not correct implementation of Gottlob et als child, it does not account for
// transistive closure (the '*' operator).
let (child : Location -> Location option) = ocombine firstchild nextsibling 

