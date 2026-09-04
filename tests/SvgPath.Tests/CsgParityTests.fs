module SvgPath.Tests.CsgParityTests

open SvgPath
open Xunit

let private point x y = Point.create (Length.fromFloat x) (Length.fromFloat y)
let private rectangleSubpath left top right bottom =
    Subpath.polygon [ point left top; point right top; point right bottom; point left bottom ]
    |> Result.defaultWith (failwithf "%A")
let private rectangle left top right bottom = Path.singleton (rectangleSubpath left top right bottom)
let private output result = result |> Result.defaultWith (failwithf "%A") |> _.Path
let private area path = Area.path path Nonzero |> Result.defaultWith (failwithf "%A")
let private containment path sample = Path.containment sample path Nonzero |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``csg result retains its arrangement build`` () =
    let result = Csg.union (rectangle 0.0 0.0 2.0 2.0) (rectangle 1.0 0.0 3.0 2.0) Nonzero |> Result.defaultWith (failwithf "%A")
    Assert.Equal(8, result.Build.Segments.Length)
    Assert.Single(Path.subpaths result.Path) |> ignore
    Assert.Equal(Ok(), Arrangement.validate result.Build.Graph 1.0e-6<length> 1.0e-5<length>)

[<Fact>]
let ``overlapping rectangles match expected union geometry`` () =
    let union = Csg.union (rectangle 0.0 0.0 2.0 2.0) (rectangle 1.0 0.0 3.0 2.0) Nonzero |> output
    Assert.Equal(6.0, float (area union), 6)
    Assert.Single(Path.subpaths union) |> ignore

[<Fact>]
let ``adjacent rectangles remove shared edge slit`` () =
    let union = Csg.union (rectangle 0.0 0.0 1.0 1.0) (rectangle 1.0 0.0 2.0 1.0) Nonzero |> output
    Assert.Equal(2.0, float (area union), 6)
    Assert.Single(Path.subpaths union) |> ignore
    Assert.Equal(6, union |> Path.subpaths |> List.collect Subpath.segments |> List.length)

[<Fact>]
let ``collapsed cubic endpoint direction reconstructs boundary`` () =
    let start = point 0.0 0.0
    let contour =
        Subpath.create [ CubicBezier(start, start, point 0.0 10.0, point 10.0 10.0); Line(point 10.0 10.0, point 10.0 0.0); Line(point 10.0 0.0, start) ]
        |> Result.bind (Subpath.setClosed true)
        |> Result.defaultWith (failwithf "%A")
    let union = Csg.union (Path.singleton contour) (Path.ofSubpaths []) Nonzero |> output
    Assert.Single(Path.subpaths union) |> ignore

[<Fact>]
let ``reversed coincident operands remain filled`` () =
    let left = rectangle 0.0 0.0 2.0 2.0
    let union = Csg.union left (Path.reverse left) Nonzero |> output
    Assert.Equal(4.0, float (area union), 6)
    Assert.Equal(Inside, containment union (point 1.0 1.0))
    Assert.Single(Path.subpaths union) |> ignore

[<Fact>]
let ``three coincident contributors emit one boolean boundary`` () =
    let contour = rectangleSubpath 0.0 0.0 2.0 2.0
    let union = Csg.union (Path.ofSubpaths [ contour; contour; contour ]) (Path.ofSubpaths []) Nonzero |> output
    Assert.Equal(4.0, float (area union), 6)
    Assert.Single(Path.subpaths union) |> ignore

let private circleSubpath radius =
    let left, right = point -radius 0.0, point radius 0.0
    Subpath.create
        [ Arc { Start = right; Radius = point radius radius; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = left }
          Arc { Start = left; Radius = point radius radius; XAxisRotation = 0.0<degree>; LargeArc = false; Sweep = true; End = right } ]
    |> Result.bind (Subpath.setClosed true)
    |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``decreasing concentric winding emits outer boundary and hole`` () =
    let input = Path.ofSubpaths [ circleSubpath 40.0; circleSubpath 30.0; Subpath.reverse (circleSubpath 20.0); Subpath.reverse (circleSubpath 10.0) ]
    let union = Csg.union input (Path.ofSubpaths []) Nonzero |> output
    Assert.Equal(2, Path.subpaths union |> List.length)
    Assert.Equal(Outside, containment union (point 0.0 0.0))
    for x in [ 15.0; 25.0; 35.0 ] do Assert.Equal(Inside, containment union (point x 0.0))

[<Fact>]
let ``disjoint rectangles return two components`` () =
    let union = Csg.union (rectangle 0.0 0.0 10.0 10.0) (rectangle 20.0 0.0 30.0 10.0) Nonzero |> output
    Assert.Equal(200.0, float (area union), 6)
    Assert.Equal(2, Path.subpaths union |> List.length)

[<Fact>]
let ``offset adjacent rectangles stitch canonical boundary`` () =
    let union = Csg.union (rectangle 0.0 0.0 1.0 1.0) (rectangle 1.0 0.5 2.0 1.5) Nonzero |> output
    Assert.Equal(2.0, float (area union), 6)
    Assert.Single(Path.subpaths union) |> ignore
    Assert.Equal(Inside, containment union (point 0.5 0.5))
    Assert.Equal(Inside, containment union (point 1.5 1.0))
    Assert.Equal(Outside, containment union (point 1.5 0.25))

[<Fact>]
let ``four square union matches expected boolean semantics`` () =
    let paths = [ rectangle 0.0 0.0 2.0 2.0; rectangle 2.0 1.0 4.0 3.0; rectangle 1.0 3.0 3.0 5.0; rectangle -1.0 2.0 1.0 4.0 ]
    let union = paths.Tail |> List.fold (fun accumulated next -> Csg.union accumulated next Nonzero |> output) paths.Head
    Assert.Equal(16.0, float (area union), 6)
    for sample in [ point 1.0 1.0; point 3.0 2.0; point 2.0 4.0; point 0.0 3.0 ] do Assert.Equal(Inside, containment union sample)

let private has predicate path = path |> Path.subpaths |> List.collect Subpath.segments |> List.exists predicate
let private hasLine = has (function Line _ -> true | _ -> false)
let private hasArc = has (function Arc _ -> true | _ -> false)
let private hasQuadratic = has (function QuadraticBezier _ -> true | _ -> false)
let private hasCubic = has (function CubicBezier _ -> true | _ -> false)

[<Fact>]
let ``circle rectangle union preserves arc and line edges`` () =
    let union = Csg.union (Path.singleton (circleSubpath 10.0)) (rectangle 0.0 -5.0 15.0 5.0) Nonzero |> output
    Assert.Single(Path.subpaths union) |> ignore
    Assert.Equal(Inside, containment union (point -7.0 0.0))
    Assert.Equal(Inside, containment union (point 13.0 0.0))
    Assert.Equal(Outside, containment union (point 0.0 11.0))
    Assert.True(hasArc union && hasLine union)

let private quadraticLoop () =
    let left, top, right, bottom = point -10.0 0.0, point 0.0 -10.0, point 10.0 0.0, point 0.0 10.0
    Subpath.create
        [ QuadraticBezier(left, point -10.0 -10.0, top); QuadraticBezier(top, point 10.0 -10.0, right)
          QuadraticBezier(right, point 10.0 10.0, bottom); QuadraticBezier(bottom, point -10.0 10.0, left) ]
    |> Result.bind (Subpath.setClosed true) |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``quadratic loop rectangle union preserves quadratics and lines`` () =
    let union = Csg.union (Path.singleton (quadraticLoop ())) (rectangle 5.0 -4.0 14.0 4.0) Nonzero |> output
    Assert.Single(Path.subpaths union) |> ignore
    Assert.Equal(Inside, containment union (point 0.0 0.0))
    Assert.Equal(Inside, containment union (point 12.0 0.0))
    Assert.Equal(Outside, containment union (point 0.0 12.0))
    Assert.True(hasQuadratic union && hasLine union)

let private cubicLoop () =
    let r, h = 10.0, 5.522847498307936
    let left, top, right, bottom = point -r 0.0, point 0.0 -r, point r 0.0, point 0.0 r
    Subpath.create
        [ CubicBezier(left, point -r -h, point -h -r, top); CubicBezier(top, point h -r, point r -h, right)
          CubicBezier(right, point r h, point h r, bottom); CubicBezier(bottom, point -h r, point -r h, left) ]
    |> Result.bind (Subpath.setClosed true) |> Result.defaultWith (failwithf "%A")

[<Fact>]
let ``cubic loop rectangle union preserves cubics and lines`` () =
    let union = Csg.union (Path.singleton (cubicLoop ())) (rectangle -14.0 -4.0 -5.0 4.0) Nonzero |> output
    Assert.Single(Path.subpaths union) |> ignore
    Assert.Equal(Inside, containment union (point 0.0 0.0))
    Assert.Equal(Inside, containment union (point -12.0 0.0))
    Assert.Equal(Outside, containment union (point 0.0 12.0))
    Assert.True(hasCubic union && hasLine union)

[<Fact>]
let ``overlapping rectangles intersection matches expected geometry`` () =
    let intersection = Csg.intersection (rectangle 0.0 0.0 10.0 10.0) (rectangle 5.0 0.0 15.0 10.0) Nonzero |> output
    Assert.Equal(50.0, float (area intersection), 6)
    Assert.Single(Path.subpaths intersection) |> ignore
    Assert.Equal(Outside, containment intersection (point 2.5 5.0))
    Assert.Equal(Inside, containment intersection (point 7.5 5.0))
    Assert.Equal(Outside, containment intersection (point 12.5 5.0))

[<Fact>]
let ``disjoint and tangent intersections are empty`` () =
    let left = rectangle 0.0 0.0 10.0 10.0
    for right in [ rectangle 20.0 0.0 30.0 10.0; rectangle 10.0 0.0 20.0 10.0; rectangle 10.0 10.0 20.0 20.0 ] do
        Assert.Empty(Path.subpaths (Csg.intersection left right Nonzero |> output))

[<Fact>]
let ``identical rectangles intersection keeps one boundary`` () =
    let source = rectangle 0.0 0.0 10.0 10.0
    let intersection = Csg.intersection source source Nonzero |> output
    Assert.Equal(100.0, float (area intersection), 6)
    Assert.Single(Path.subpaths intersection) |> ignore

[<Fact>]
let ``circle rectangle intersection preserves arc and line edges`` () =
    let circle =
        let translated = Transform.subpath (circleSubpath 10.0) (Affine.translate 10.0<length> 10.0<length>) |> Result.defaultWith (failwithf "%A")
        Path.singleton translated
    let intersection = Csg.intersection circle (rectangle 5.0 0.0 20.0 20.0) Nonzero |> output
    Assert.Single(Path.subpaths intersection) |> ignore
    Assert.Equal(Inside, containment intersection (point 12.5 10.0))
    Assert.Equal(Outside, containment intersection (point 2.5 10.0))
    Assert.True(hasArc intersection && hasLine intersection)

[<Fact>]
let ``intersection applies nonzero and evenodd fill rules`` () =
    let nested = Path.ofSubpaths [ rectangleSubpath 0.0 0.0 20.0 20.0; rectangleSubpath 5.0 5.0 15.0 15.0 ]
    let probe = rectangle 7.0 7.0 13.0 13.0
    let nonzero = Csg.intersection nested probe Nonzero |> output
    let evenOdd = Csg.intersection nested probe EvenOdd |> output
    Assert.Equal(36.0, float (area nonzero), 6)
    Assert.Empty(Path.subpaths evenOdd)

let private inside rule path sample =
    Path.containment sample path rule
    |> Result.defaultWith (failwithf "%A")
    |> (=) Inside

let private grid xs ys =
    [ for x in xs do
          for y in ys do
              yield point x y ]

let private nestedRectangles () =
    Path.ofSubpaths [ rectangleSubpath 0.0 0.0 20.0 20.0; rectangleSubpath 5.0 5.0 15.0 15.0 ]

let private bowtie () =
    Subpath.polygon [ point 8.0 8.0; point 114.0 112.0; point 114.0 8.0; point 8.0 112.0 ]
    |> Result.defaultWith (failwithf "%A")
    |> Path.singleton

let private translatedCircle x y radius =
    Transform.subpath (circleSubpath radius) (Affine.translate (Length.fromFloat x) (Length.fromFloat y))
    |> Result.defaultWith (failwithf "%A")
    |> Path.singleton

let private assertBinarySemantics operation expected left right rule samples =
    let result = operation left right rule |> output
    for sample in samples do
        Assert.Equal(expected (inside rule left sample) (inside rule right sample), inside rule result sample)

[<Fact>]
let ``intersection semantic matrix`` () =
    let cases =
        [ rectangle 0.0 0.0 10.0 10.0, rectangle 5.0 0.0 15.0 10.0, Nonzero, grid [2.5; 7.5; 12.5; 20.0] [-2.5; 5.0; 12.5]
          translatedCircle 10.0 10.0 10.0, rectangle 5.0 0.0 20.0 20.0, Nonzero, grid [2.5; 7.5; 12.5; 17.5; 22.5] [2.5; 7.5; 12.5; 17.5]
          nestedRectangles (), rectangle 7.0 7.0 13.0 13.0, Nonzero, grid [2.5; 7.5; 10.0; 12.5; 17.5] [2.5; 7.5; 10.0; 12.5; 17.5]
          nestedRectangles (), rectangle 7.0 7.0 13.0 13.0, EvenOdd, grid [2.5; 7.5; 10.0; 12.5; 17.5] [2.5; 7.5; 10.0; 12.5; 17.5]
          translatedCircle 50.0 60.0 40.0, rectangle 90.0 20.0 124.0 100.0, Nonzero, grid [12.5; 50.0; 88.0; 92.0; 110.0] [30.0; 60.0; 90.0]
          bowtie (), rectangle 36.0 28.0 86.0 92.0, Nonzero, grid [16.0; 44.0; 62.0; 78.0; 100.0] [16.0; 40.0; 60.0; 84.0; 104.0] ]
    for left, right, rule, samples in cases do
        assertBinarySemantics Csg.intersection (&&) left right rule samples

[<Fact>]
let ``intersection operation table`` () =
    let source = rectangle 0.0 0.0 10.0 10.0
    Assert.Equal(50.0, float (area (Csg.intersection source (rectangle 5.0 0.0 15.0 10.0) Nonzero |> output)), 6)
    Assert.Empty(Path.subpaths (Csg.intersection source (rectangle 20.0 0.0 30.0 10.0) Nonzero |> output))
    Assert.Equal(100.0, float (area (Csg.intersection source source Nonzero |> output)), 6)
    Assert.Empty(Path.subpaths (Csg.intersection source (rectangle 10.0 0.0 20.0 10.0) Nonzero |> output))

[<Fact>]
let ``difference semantic matrix`` () =
    let cases =
        [ rectangle 0.0 0.0 10.0 10.0, rectangle 5.0 0.0 15.0 10.0, Nonzero, grid [2.5; 7.5; 12.5; 20.0] [-2.5; 5.0; 12.5]
          translatedCircle 10.0 10.0 10.0, rectangle 5.0 0.0 20.0 20.0, Nonzero, grid [2.5; 7.5; 12.5; 17.5; 22.5] [2.5; 7.5; 12.5; 17.5]
          nestedRectangles (), rectangle 7.0 7.0 13.0 13.0, Nonzero, grid [2.5; 7.5; 10.0; 12.5; 17.5] [2.5; 7.5; 10.0; 12.5; 17.5]
          nestedRectangles (), rectangle 7.0 7.0 13.0 13.0, EvenOdd, grid [2.5; 7.5; 10.0; 12.5; 17.5] [2.5; 7.5; 10.0; 12.5; 17.5]
          translatedCircle 50.0 60.0 40.0, rectangle 90.0 20.0 124.0 100.0, Nonzero, grid [12.5; 50.0; 88.0; 92.0; 110.0] [30.0; 60.0; 90.0]
          bowtie (), rectangle 36.0 28.0 86.0 92.0, Nonzero, grid [16.0; 44.0; 62.0; 78.0; 100.0] [16.0; 40.0; 60.0; 84.0; 104.0] ]
    for left, right, rule, samples in cases do
        assertBinarySemantics Csg.difference (fun a b -> a && not b) left right rule samples

[<Fact>]
let ``difference operation table`` () =
    let source = rectangle 0.0 0.0 10.0 10.0
    Assert.Equal(50.0, float (area (Csg.difference source (rectangle 5.0 0.0 15.0 10.0) Nonzero |> output)), 6)
    Assert.Equal(100.0, float (area (Csg.difference source (rectangle 20.0 0.0 30.0 10.0) Nonzero |> output)), 6)
    Assert.Empty(Path.subpaths (Csg.difference source source Nonzero |> output))
    Assert.Equal(100.0, float (area (Csg.difference source (rectangle 10.0 0.0 20.0 10.0) Nonzero |> output)), 6)

[<Fact>]
let ``difference creates hole and preserves mixed curves`` () =
    let holed = Csg.difference (rectangle 0.0 0.0 20.0 20.0) (rectangle 5.0 5.0 15.0 15.0) Nonzero |> output
    Assert.Equal(300.0, float (area holed), 6)
    Assert.Equal(2, Path.subpaths holed |> List.length)
    Assert.Equal(Inside, containment holed (point 2.5 2.5))
    Assert.Equal(Outside, containment holed (point 10.0 10.0))
    let cut = Csg.difference (translatedCircle 10.0 10.0 10.0) (rectangle 5.0 0.0 20.0 20.0) Nonzero |> output
    Assert.True(hasArc cut && hasLine cut)

[<Fact>]
let ``difference adapts old hole orientation`` () =
    let hasBothOrientations path =
        let signedAreas = Path.subpaths path |> List.map Area.signedSubpath
        Assert.Contains(signedAreas, fun signedArea -> signedArea > 0.0<length^2>)
        Assert.Contains(signedAreas, fun signedArea -> signedArea < 0.0<length^2>)

    let probe = rectangle 7.0 7.0 13.0 13.0
    let nonzero = Csg.difference (nestedRectangles ()) probe Nonzero |> output
    Assert.Equal(Outside, containment nonzero (point 10.0 10.0))
    hasBothOrientations nonzero

    let evenOdd =
        Csg.difference
            (rectangle 0.0 0.0 20.0 20.0)
            (rectangle 5.0 5.0 15.0 15.0)
            EvenOdd
        |> output
    hasBothOrientations evenOdd

[<Fact>]
let ``symmetric difference handles basic topologies`` () =
    let left = rectangle 0.0 0.0 10.0 10.0
    let overlapping = Csg.symmetricDifference left (rectangle 5.0 0.0 15.0 10.0) Nonzero |> output
    Assert.Equal(100.0, float (area overlapping), 6)
    Assert.Equal(Inside, containment overlapping (point 2.5 5.0))
    Assert.Equal(Outside, containment overlapping (point 7.5 5.0))
    Assert.Equal(Inside, containment overlapping (point 12.5 5.0))
    Assert.Equal(2, Path.subpaths (Csg.symmetricDifference left (rectangle 20.0 0.0 30.0 10.0) Nonzero |> output) |> List.length)
    Assert.Empty(Path.subpaths (Csg.symmetricDifference left left Nonzero |> output))

[<Fact>]
let ``symmetric difference applies both fill policies`` () =
    let probe = rectangle 7.0 7.0 13.0 13.0
    let nonzero = Csg.symmetricDifference (nestedRectangles ()) probe Nonzero |> output
    let evenOdd = Csg.symmetricDifference (nestedRectangles ()) probe EvenOdd |> output
    Assert.Equal(364.0, float (area nonzero), 6)
    Assert.Equal(Outside, containment nonzero (point 10.0 10.0))
    Assert.Equal(Inside, containment nonzero (point 6.0 6.0))
    Assert.Equal(336.0, float (area evenOdd), 6)
    Assert.Equal(Inside, containment evenOdd (point 10.0 10.0))
    Assert.Equal(Outside, containment evenOdd (point 6.0 6.0))

[<Fact>]
let ``symmetric difference is commutative`` () =
    let left, right = rectangle 0.0 0.0 10.0 10.0, rectangle 5.0 0.0 15.0 10.0
    let forward = Csg.symmetricDifference left right Nonzero |> output
    let reverse = Csg.symmetricDifference right left Nonzero |> output
    Assert.Equal(float (area forward), float (area reverse), 6)
    for sample in grid [2.5; 7.5; 12.5] [2.5; 7.5] do
        Assert.Equal(containment forward sample, containment reverse sample)

let private winding path sample = Path.winding sample path |> Result.defaultWith (failwithf "%A")
let private assertSameWinding input output samples =
    for sample in samples do Assert.Equal(winding input sample, winding output sample)

[<Fact>]
let ``nested contours preserve positive nested winding levels`` () =
    let input = Path.ofSubpaths [ rectangleSubpath 0.0 0.0 30.0 30.0; rectangleSubpath 5.0 5.0 25.0 25.0; rectangleSubpath 10.0 10.0 20.0 20.0 ]
    let result = Csg.nestedContours input |> output
    Assert.Equal(3, Path.subpaths result |> List.length)
    assertSameWinding input result [ point -1.0 -1.0; point 2.0 2.0; point 7.0 7.0; point 15.0 15.0 ]

[<Fact>]
let ``nested contours preserve mixed sign nesting`` () =
    let input = Path.ofSubpaths [ rectangleSubpath 0.0 0.0 30.0 30.0; Subpath.reverse (rectangleSubpath 5.0 5.0 25.0 25.0); rectangleSubpath 10.0 10.0 20.0 20.0 ]
    let result = Csg.nestedContours input |> output
    Assert.Equal(3, Path.subpaths result |> List.length)
    assertSameWinding input result [ point 2.0 2.0; point 7.0 7.0; point 15.0 15.0 ]

[<Fact>]
let ``nested contours decompose overlapping contours by level`` () =
    let input = Path.ofSubpaths [ rectangleSubpath 0.0 0.0 10.0 10.0; rectangleSubpath 4.0 2.0 14.0 12.0 ]
    let result = Csg.nestedContours input |> output
    Assert.Equal(2, Path.subpaths result |> List.length)
    assertSameWinding input result (grid [-1.0; 2.0; 6.0; 12.0; 15.0] [-1.0; 1.0; 6.0; 11.0; 13.0])

[<Fact>]
let ``nested contours drop winding neutral copies`` () =
    let contour = rectangleSubpath 0.0 0.0 10.0 10.0
    let input = Path.ofSubpaths [ contour; Subpath.reverse contour ]
    let result = Csg.nestedContours input |> output
    Assert.Empty(Path.subpaths result)
    assertSameWinding input result [ point -1.0 -1.0; point 5.0 5.0 ]

[<Fact>]
let ``nested contours split self intersection into signed lobes`` () =
    let input = bowtie ()
    let result = Csg.nestedContours input |> output
    Assert.Equal(2, Path.subpaths result |> List.length)
    assertSameWinding input result [ point 61.0 25.0; point 61.0 95.0; point 0.0 0.0 ]
