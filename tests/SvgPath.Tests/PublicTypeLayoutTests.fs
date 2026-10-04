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
    let fitting: Offset.FittingOptions = options.Fitting
    let join: Stroke.Join = Offset.Round
    let cap: Stroke.Cap = Offset.Butt
    Assert.Equal(Offset.Round, join)
    Assert.Equal(Offset.Butt, cap)
    Assert.Equal(Offset.defaultOptions.Fitting, fitting)

[<Fact>]
let ``v3 operation modules replace the former public entry points`` () =
    let assembly = typeof<Segment>.Assembly
    let publicMembers owner =
        assembly.GetType("SvgPath." + owner).GetMembers()
        |> Array.map (fun memberInfo -> memberInfo.Name)
        |> Set.ofArray
    for owner, removed in
        [ "SegmentModule", "length"; "SubpathModule", "fromParametric"
          "PathModule", "boundingBox"; "WindingField", "pathContainment"
          "Intersections", "pathPathClosestPair" ] do
        Assert.DoesNotContain(removed, publicMembers owner)
    for owner, added in
        [ "Measure", "segmentLength"; "Fit", "subpathFromParametric"
          "Bounds", "pathBoundingBox"; "Containment", "pathContainment"
          "Distance", "pathPathClosestPair" ] do
        let members = publicMembers owner
        Assert.True(Set.contains added members || Set.contains ("get_" + added) members, owner + "." + added)
    let transforms = publicMembers "Transform"
    for operation in [ "translate"; "scale"; "scaleXY"; "rotate"; "skewX"; "skewY" ] do
        for geometry in [ "Point"; "Segment"; "Subpath"; "Path" ] do
            Assert.DoesNotContain(operation + geometry, transforms)
    let fields = Microsoft.FSharp.Reflection.FSharpType.GetRecordFields typeof<Stroke.Options>
    Assert.Equal<string list>(["Fitting"; "StalledOffsetDiameter"; "TangentHealAngleDegrees"; "InnerJoin"], fields |> Array.map _.Name |> Array.toList)

[<Fact>]
let ``path-only boolean output composes with measurement and transforms`` () =
    let parse text = Parse.path text |> Result.defaultWith (failwithf "%A")
    let left = parse "M 0 0 H 10 V 10 H 0 Z"
    let right = parse "M 5 0 H 15 V 10 H 5 Z"
    let result = Csg.unionPath left right Nonzero |> Result.defaultWith (failwithf "%A")
    let moved = Transform.path result (Transform.translate 3.0<length> 4.0<length>) |> Result.defaultWith (failwithf "%A")
    Assert.Equal(Ok 50.0<length>, Measure.pathLength moved)
    Assert.Equal(Csg.union left right Nonzero |> Result.map _.Path, Ok result)
