[<AutoOpen>]
module FilePath.Syntax

open System

type Name = string

type Date = { year : int; month : int; day : int }

type Time = { hour :int; minute : int}

type TimeStamp = TimeStamp of Date * Time

type Mode = string

type FileLength = System.Int64


type File = { name: Name; mode: Mode; timestamp : TimeStamp; length: FileLength }

type Directory = { name: Name; mode: Mode; timestamp : TimeStamp; subdirs: Directory list; files: File list }

type Root = { name: Name; subdirs: Directory list; files: File list }

