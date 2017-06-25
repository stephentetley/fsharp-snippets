[<AutoOpen>]
module FilePath.Syntax

open System

type Name = string

type Date = { year : int; month : int; day : int }

type Time = { hour :int; minute : int}

type TimeStamp = TimeStamp of Date * Time

type Mode = string

type FileLength = System.Int64



