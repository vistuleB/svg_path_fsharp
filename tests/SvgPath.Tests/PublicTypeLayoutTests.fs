module SvgPath.Tests.PublicTypeLayoutTests

open SvgPath
open Xunit

[<Fact>]
let ``operation types belong to their operation modules`` () =
    let expected =
        [ typeof<Distance.ClosestPairOptions>, "SvgPath.Distance"
          typeof<Stroke.Error>, "SvgPath.Stroke"
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
    let options: Offset.Options = Offset.defaultOptions
    let fitting: Offset.FittingOptions = options.Fitting
    let join: Offset.Join = Offset.Round
    let cap: Offset.Cap = Offset.Butt
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
    let fields = Microsoft.FSharp.Reflection.FSharpType.GetRecordFields typeof<Offset.Options>
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

[<Fact>]
let ``closest pair options validate even for empty geometry`` () =
    let line = Line(Point.create 0.0<length> 0.0<length>, Point.create 10.0<length> 0.0<length>)
    let subpath = Subpath.ofSegment line
    for options, expected in
        [ { Distance.defaultClosestPairOptions with Tolerance = 0.0<length> }, InvalidIntersectionTolerance 0.0<length>
          { Distance.defaultClosestPairOptions with MaxDepth = 0 }, InvalidIntersectionMaxDepth 0 ] do
        Assert.Equal(Error expected, Distance.segmentSegmentClosestPairWith line line options)
        Assert.Equal(Error expected, Distance.segmentSubpathClosestPairWith line subpath options)
        Assert.Equal(Error expected, Distance.segmentPathClosestPairWith line Path.empty options)
        Assert.Equal(Error expected, Distance.subpathSubpathClosestPairWith subpath subpath options)
        Assert.Equal(Error expected, Distance.subpathPathClosestPairWith subpath Path.empty options)
        Assert.Equal(Error expected, Distance.pathPathClosestPairWith Path.empty Path.empty options)
    let fields = Microsoft.FSharp.Reflection.FSharpType.GetRecordFields typeof<Distance.ClosestPairOptions>
    Assert.Equal<string list>(["Tolerance"; "MaxDepth"], fields |> Array.map _.Name |> Array.toList)

[<Fact>]
let ``projection helpers compose across address types`` () =
    let projectionDistance (value: Projection<'address>) = value.Distance
    let pairDistance (value: ClosestPair<'left, 'right>) = value.Distance
    let line = Line(Point.create 0.0<length> 0.0<length>, Point.create 10.0<length> 0.0<length>)
    let sample = Point.create 4.0<length> 3.0<length>
    let segment = Distance.segmentProjection line sample |> Result.defaultWith (failwithf "%A")
    let path = Path.singleton (Subpath.ofSegment line)
    let pathProjection = Distance.pathProjection path sample |> Result.defaultWith (failwithf "%A")
    Assert.Equal(3.0<length>, projectionDistance segment)
    Assert.Equal(3.0<length>, projectionDistance pathProjection)
    Assert.Equal(0.4<parameter>, segment.At)
    let pair = Distance.segmentPathClosestPair line path |> Result.defaultWith (failwithf "%A")
    Assert.Equal(0.0<length>, pairDistance pair)

[<Fact>]
let ``explicit offset policies preserve default entry points`` () =
    let source = Subpath.assertCreate [Line(Point.create 0.0<length> 0.0<length>, Point.create 10.0<length> 0.0<length>)]
    let path = Path.singleton source
    let options = Offset.defaultOptions
    let single = Offset.defaultSingleOffsetTrimming
    let band = Offset.defaultBandTrimming
    let check expected actual =
        Assert.True(Result.isOk actual)
        Assert.Equal(expected, actual)
    check (Offset.subpath source 1.0<length> Offset.Round Offset.Butt)
          (Offset.subpathWith source 1.0<length> Offset.Round Offset.Butt options single)
    check (Offset.path path 1.0<length> Offset.Round Offset.Butt)
          (Offset.pathWith path 1.0<length> Offset.Round Offset.Butt options single)
    check (Offset.subpathBand source -1.0<length> 1.0<length> Offset.Round Offset.Butt)
          (Offset.subpathBandWith source -1.0<length> 1.0<length> Offset.Round Offset.Butt options band)
    check (Offset.pathBand path -1.0<length> 1.0<length> Offset.Round Offset.Butt)
          (Offset.pathBandWith path -1.0<length> 1.0<length> Offset.Round Offset.Butt options band)
    check (Stroke.path path 2.0<length> Offset.Round Offset.Butt)
          (Stroke.pathWith path 2.0<length> Offset.Round Offset.Butt options)

[<Fact>]
let ``path bands pass custom trimming to every source`` () =
    let path = Parse.path "M0 0H10V10 M20 0H30V10" |> Result.defaultWith (failwithf "%A")
    let options = Offset.defaultOptions
    let trimming : Offset.BandTrimming = {InnerCusps=false; OuterCusps=false; InBand=false}
    let expected = path.Subpaths |> List.collect (fun source ->
        Offset.subpathBandWith source -1.0<length> 1.0<length> Offset.Round Offset.Square options trimming
        |> Result.defaultWith (failwithf "%A") |> Path.subpaths) |> Path.ofSubpaths
    Assert.Equal(Ok expected, Offset.pathBandWith path -1.0<length> 1.0<length> Offset.Round Offset.Square options trimming)
