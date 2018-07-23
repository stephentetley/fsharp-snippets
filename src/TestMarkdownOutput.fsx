#load @"SL\FormatCombinators.fs"
#load @"SL\MarkdownOutput.fs"
open SL.MarkdownOutput


let outFile = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", @"data\markdown-temp.md")

let test01 () : unit = 
    let procM = 
        let bodytext = 
            plaintext "Emphasis, aka italics, with" 
                @@@ (asterisks <| plaintext "asterisks")
                @@@ plaintext "or"
                @@@ (underscores <| plaintext "asterisks")
                @@ plaintext "."
        markdownOutput { 
            do! tellMarkdown (h1 "title")
            do! tellMarkdown bodytext
            return ()
            }
    runMarkdownOutput outFile procM |> ignore

