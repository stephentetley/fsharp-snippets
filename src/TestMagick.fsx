// Make sure fsi is set to 64 bit if using x64 versions of Magick.NET
// [Tools >> Options >> F# Tools >> F# Interactive]

open System


#I @"..\packages\Magick.NET-Q8-AnyCPU.7.3.0\lib\net40"
#r @"Magick.NET-Q8-AnyCPU.dll"
open ImageMagick

type Orientation = Portrait | Landscape

let getOrientation (info:MagickImageInfo) : Orientation = 
    if info.Width > info.Height then Landscape else Portrait

let imgPath = @"G:\work\photos1\TestFolder\DSCF0001.jpg"
let test01 () = 
    let info = new MagickImageInfo(imgPath)
    printfn "W:%i, H:%i (%A)" info.Width info.Height (getOrientation info)
    printfn "Density: %A"  info.Density


let makeRevisedFileName (annotation:string)  (filePath:string) : string = 
    let root = System.IO.Path.GetDirectoryName filePath
    let justfile = System.IO.Path.GetFileNameWithoutExtension filePath
    let ext  = System.IO.Path.GetExtension filePath
    let newfile = sprintf "%s.%s%s" justfile annotation ext
    IO.Path.Combine(root, newfile)


let resample72Dpi (filePath:string) : unit = 
    use (img:MagickImage) = new MagickImage(filePath)
    let out = makeRevisedFileName "72dpi" filePath
    img.Density <- new Density(72.0, 72.0, DensityUnit.PixelsPerInch)
    printfn "Writing: '%s' (%A)" out img.Density
    img.Write out

let test02 () = 
    resample72Dpi imgPath

let calculateNewPixelSize (info:MagickImageInfo) (maxima:int) : (int * int) = 
    match getOrientation info with
    | Landscape -> 
        let scaling = maxima / info.Width in (maxima, info.Height * scaling)
    | Portrait -> 
        let scaling = maxima / info.Height in (info.Width * scaling, maxima)


let resample72DpiAndRescale (filePath:string) : unit = 
    use (img:MagickImage) = new MagickImage(filePath)
    let info = new MagickImageInfo(imgPath)
    let out = makeRevisedFileName "600px" filePath
    let (newWidth,newHeight) = calculateNewPixelSize info 600
    img.Density <- new Density(72.0, 72.0, DensityUnit.PixelsPerInch)
    img.Resize(new MagickGeometry(newWidth, newHeight))
    img.Write out

let imgPath2 = @"G:\work\photos1\TestFolder\DSCF0002.jpg"

let test03 () = 
    resample72DpiAndRescale imgPath2