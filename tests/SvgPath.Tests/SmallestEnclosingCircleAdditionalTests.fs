module SvgPath.Tests.SmallestEnclosingCircleAdditionalTests

open SvgPath
open Xunit

[<Fact>]
let ``empty input has no enclosing circle`` () =
    Assert.Equal(Error(), SmallestEnclosingCircle.points [])
