// Facts are variable free Prolog terms

namespace Fact

module Fact = 

    type Constant = 
        | Atom of string        // must start with lowercase letter
        | QuotedAtom of string
        | NumberI of int
        | NumberF of float

    type Fact = { Name : string; Args : Constant list }

    let pp (c:Constant) = 
        match c with
        | Atom s -> s
        | QuotedAtom s -> sprintf "'%s'" s
        | NumberI i -> sprintf "%d" i
        | NumberF d  -> sprintf "%f" d
        
    
    let pp2 (x:Fact) = 
        let body = match x.Args with
                   | [] -> ""
                   | x :: xs -> List.fold (fun ac a -> sprintf "%s, %s" ac (pp a)) (pp x) xs
        sprintf "%s(%s)" x.Name body
        

    let bool (b:bool) = if b then Atom "true" else Atom "false"


