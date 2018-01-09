// Make sure fsi is set to 64 bit if using x64 versions of Magick.NET
// [Tools >> Options >> F# Tools >> F# Interactive]

open System


#I @"..\packages\Magick.NET-Q8-x64.7.3.0\lib\net40"
#r @"Magick.NET-Q8-x64.dll"
open ImageMagick

let imgPath = @"G:\work\photos1\TestFolder\DSCF0001.jpg"
let test01 () = 
    let info = new MagickImageInfo(imgPath)
    printfn "W:%i, H:%i" info.Width info.Height