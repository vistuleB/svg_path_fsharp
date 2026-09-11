module SvgPath.Tests.PublicTypeLayoutTests

open SvgPath
open Xunit

[<Fact>]
let ``operation types belong to their operation modules`` () =
    let expected =
        [ typeof<Stroke.Error>, "SvgPath.Stroke"
          typeof<Stroke.Options>, "SvgPath.Stroke"
          typeof<Stroke.DashOptions>, "SvgPath.Stroke"
          typeof<Offset.Error>, "SvgPath.Offset"
          typeof<Offset.Options>, "SvgPath.Offset"
          typeof<Arrangement.Error>, "SvgPath.Arrangement"
          typeof<Intersections.Error>, "SvgPath.Intersections"
          // F# adds Module to the CLR name of a same-name type's companion module.
          typeof<Affine.Error>, "SvgPath.AffineModule"
          typeof<Bezier.Error>, "SvgPath.Bezier"
          typeof<Ellipse.Error>, "SvgPath.Ellipse"
          typeof<Curvature.Options>, "SvgPath.Curvature"
          typeof<Parse.Error>, "SvgPath.Parse"
          typeof<Serialize.Options>, "SvgPath.Serialize"
          typeof<NumberFormat.LeftPaddingStyle>, "SvgPath.NumberFormat"
          typeof<NumberFormat.LeftDecimalOptions>, "SvgPath.NumberFormat"
          typeof<NumberFormat.RightDecimalOptions>, "SvgPath.NumberFormat" ]
    for actual, owner in expected do
        Assert.True(actual.IsNestedPublic, actual.FullName)
        Assert.Equal(owner, actual.DeclaringType.FullName)

[<Fact>]
let ``root namespace retains geometry but not old operation aliases`` () =
    let assembly = typeof<Segment>.Assembly
    for name in [ "Error"; "Options"; "StrokeError"; "StrokeOptions";
                  "AffineError"; "ArrangementError"; "CurvatureOptions";
                  "PathParseError"; "NumberFormatOptions" ] do
        Assert.Null(assembly.GetType("SvgPath." + name))
    for geometry in [ typeof<Point<length>>; typeof<Segment>; typeof<Subpath>; typeof<Path>; typeof<Affine> ] do
        Assert.False(geometry.IsNested, geometry.FullName)

[<Fact>]
let ``qualified style and option types are usable together`` () =
    let options: Stroke.Options = Stroke.defaultOptions
    let technical: Offset.Options = options.Offset
    let join: Stroke.Join = Offset.Round
    let cap: Stroke.Cap = Offset.Butt
    Assert.Equal(Offset.Round, join)
    Assert.Equal(Offset.Butt, cap)
    Assert.Equal(Offset.defaultOptions, technical)
