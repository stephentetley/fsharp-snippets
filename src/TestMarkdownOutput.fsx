#load @"SL\FormatCombinators.fs"
#load @"SL\MarkdownOutput.fs"
open SL.MarkdownOutput

let test01 () : unit = 
    let procM = 
        let bodytext = 
            plaintext "Empasis, aka italics, with" 
                @@@ (asterisks <| plaintext "asterisks")
                @@@ plaintext "or"
                @@@ (underscores <| plaintext "asterisks")
                @@ plaintext "."
        markdownOutput { 
            do! tellMarkdown (h1 "title")
            do! tellMarkdown bodytext
            return ()
            }
    runMarkdownOutputConsole procM |> ignore

