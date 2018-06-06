// Copyright (c) Stephen Tetley 2018
// License: BSD 3 Clause

module SL.ExcelProviderHelper

// The Excel Type Provider seems to read a trailing null row.
// This dictionary and procedure provide a skeleton to get round this.

type GetRowsDict<'table, 'row> = 
    { GetRows : 'table -> seq<'row>
      NotNullProc : 'row -> bool }

let excelTableGetRowsSeq (dict:GetRowsDict<'table,'row>) (table:'table) : seq<'row> = 
    let allrows = dict.GetRows table
    allrows |> Seq.filter dict.NotNullProc

let excelTableGetRows (dict:GetRowsDict<'table,'row>) (table:'table) : 'row list = 
    let allrows = dict.GetRows table
    allrows |> Seq.filter dict.NotNullProc |> Seq.toList