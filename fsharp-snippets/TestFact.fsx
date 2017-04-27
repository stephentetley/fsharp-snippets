#load "Fact.fs"

open Fact

let test01 = Fact.pp (Fact.bool false)

let test02 = Fact.pp2 {Name="isA"; Args=[Fact.Atom "datalog"; Fact.Atom "prolog"] }
