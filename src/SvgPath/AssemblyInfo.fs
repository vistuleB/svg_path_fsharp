namespace SvgPath

open System.Runtime.CompilerServices

[<assembly: InternalsVisibleTo("SvgPath.Tests")>]
#if GALLERY_DIAGNOSTICS
[<assembly: InternalsVisibleTo("GalleryFigures")>]
#endif
do ()
