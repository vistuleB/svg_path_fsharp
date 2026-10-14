namespace SvgPath.Tests

open Xunit
open SvgPath

// Reduced witnesses from the Gleam Paper.js audit. These protect our own API contracts.
module Release310RegressionTests =
    let private unwrap value = value |> Result.defaultWith (failwithf "%A")
    let private p x y = Point.create (x*1.0<length>) (y*1.0<length>)
    let private parse text = Parse.path text |> unwrap
    let private subpath text = parse text |> Path.subpaths |> List.exactlyOne
    let private checkArea expected actual =
        let path = unwrap actual
        Assert.True(abs(abs(float(Area.signedPath path))-expected)<1e-10)
        for subpath in Path.subpaths path do
            Assert.True(Subpath.isClosed subpath)
            let edges = Subpath.segments subpath
            for a,b in List.pairwise (edges @ [List.head edges]) do
                Assert.Equal(Segment.finish a, Segment.start b)

    [<Fact>]
    let ``tangent cubic difference follows arrangement order`` () =
        let outer = parse "M0 0 C0 -2 -4 -2 -4 0 C-4 2 0 2 0 0 Z"
        let inner = parse "M0 0 C0 -1 -2 -1 -2 0 C-2 1 0 1 0 0 Z"
        for a in [outer;Path.reverse outer] do
            for b in [inner;Path.reverse inner] do
                let output = Csg.differencePath a b Nonzero
                checkArea 7.2 output
                let path = unwrap output
                for point in [p -3.0 0.0;p -1.0 1.0;p -1.0 -1.0] do
                    Assert.Equal(Ok Inside, Containment.pathContainment point path Nonzero)
                Assert.Equal(Ok Outside, Containment.pathContainment (p -1.0 0.0) path Nonzero)

    [<Fact>]
    let ``narrow nested faces retain both operand windings`` () =
        let inner = parse "M0 0 L1 1 L0 2Z"
        let outer = parse "M0 0 L1.00004 1 L0 2Z"
        for rule in [Nonzero;EvenOdd] do
            for outside in [outer;Path.reverse outer] do
                checkArea 1.00004 (Csg.unionPath inner outside rule)
                checkArea 1.0 (Csg.intersectionPath inner outside rule)
                checkArea 0.00004 (Csg.differencePath outside inner rule)
                checkArea 0.00004 (Csg.symmetricDifferencePath inner outside rule)

    [<Fact>]
    let ``thin shared edge triangles form valid boundaries`` () =
        let left = parse "M0 0 L0 -.00004 L-1 -1Z"
        let right = parse "M0 -.00004 L0 0 L1 -1Z"
        for rule in [Nonzero;EvenOdd] do
            checkArea 0.00004 (Csg.unionPath left right rule)
            checkArea 0.0 (Csg.intersectionPath left right rule)
            checkArea 0.00002 (Csg.differencePath left right rule)
            checkArea 0.00004 (Csg.symmetricDifferencePath left right rule)

    [<Fact>]
    let ``source multiplicity includes implicit closing edges`` () =
        let source = parse "M0 0 L2 0 L0 2"
        let doubled = Path.ofSubpaths (Path.subpaths source @ Path.subpaths source)
        let cancelled = Path.ofSubpaths (Path.subpaths source @ Path.subpaths (Path.reverse source))
        checkArea 2.0 (Csg.unionPath doubled Path.empty Nonzero)
        checkArea 0.0 (Csg.unionPath doubled Path.empty EvenOdd)
        checkArea 0.0 (Csg.unionPath cancelled Path.empty Nonzero)
        checkArea 2.0 (Csg.intersectionPath source doubled Nonzero)
        checkArea 2.0 (Csg.unionPath Path.empty source Nonzero)
        checkArea 0.0 (Csg.unionPath Path.empty Path.empty Nonzero)

    let private crossingOptions =
        {Containment.defaultCrossingOptions with SignedLineDistanceTolerance=1e-12<length>;MaxIterations=100}

    [<Fact>]
    let ``Bernstein isolation retains three roots and tangent`` () =
        let roots = Root.bernsteinRootsWith [-0.08;0.14;-0.14;0.08] {MaxIterations=100} |> unwrap
        Assert.Equal(3,roots.Length)
        List.iter2 (fun t expected -> Assert.True(abs(float t-expected)<1e-12)) roots [0.2;0.5;0.8]
        Assert.Equal(Ok [0.5<parameter>],Root.bernsteinRootsWith [1.0;-1.0;1.0] {MaxIterations=100})

    [<Fact>]
    let ``near endpoint crossing and endpoint remain distinct at three scales`` () =
        for scale in [0.001;1.0;1000.0] do
            let curve = CubicBezier(p 0.0 (-1000.0*scale),p (1.0/3.0) (-1000.0*scale),p (2.0/3.0) (0.0001*scale),p 1.0 0.0)
            for segment in [curve;Segment.reverse curve] do
                let roots = Containment.segmentRayCrossingsWith segment (p 0.0 0.0) (Point.create 1.0 0.0)
                                {crossingOptions with SignedLineDistanceTolerance=scale*1e-12<length>} |> unwrap
                Assert.Equal(2,roots.Length)
                let a,b = fst roots[0],fst roots[1]
                Assert.True(b-a>9e-8<parameter> && b-a<1.1e-7<parameter>)
                Assert.True(a=0.0<parameter> || b=1.0<parameter>)

    [<Fact>]
    let ``signed zero roots do not duplicate recovered crossing`` () =
        let curve=CubicBezier(p 0.0 0.0,p (1.0/3.0) 0.0,p (2.0/3.0) 1.9,p 1.0 8.0)
        let t,_ = Containment.segmentRayCrossingsWith curve (p 0.0 5e-12) (Point.create -1.0 0.0) crossingOptions |> unwrap |> List.exactlyOne
        Assert.True(abs((Segment.point curve t |> unwrap).Y-5e-12<length>)<=1e-12<length>)

    let private fullOverlap left right reversed =
        let hit=Overlaps.segmentWith left right 5e-7<length> |> unwrap |> List.exactlyOne
        Assert.Equal(0.0<parameter>,hit.LeftFrom)
        Assert.Equal(1.0<parameter>,hit.LeftTo)
        Assert.Equal((if reversed then 1.0<parameter> else 0.0<parameter>),hit.RightFrom)
        Assert.Equal((if reversed then 0.0<parameter> else 1.0<parameter>),hit.RightTo)

    [<Fact>]
    let ``translated monotone cubic retains certified full overlap`` () =
        for origin in [0.0;337.7804798720905] do
            let curve=CubicBezier(p 736.0 (337.7804798720905-origin),p 736.0 (337.780485777646-origin),p 736.0 (337.7804916832015-origin),p 736.0 (337.780497588757-origin))
            fullOverlap curve curve false
            fullOverlap curve (Segment.reverse curve) true

    [<Fact>]
    let ``rounded reverse controls retain full overlap`` () =
        let left=CubicBezier(p 243.02912232136828 272.45353289318496,p 243.02912232136828 272.45353289318496,p 243.02909884308636 272.4535130650999,p 243.0290519201278 272.45347343731044)
        let right=CubicBezier(p 243.0290519201213 272.45347343730504,p 243.02909884308414 272.45351306509815,p 243.02912232136822 272.453532893185,p 243.02912232136822 272.453532893185)
        fullOverlap left right true
        fullOverlap right left true

    [<Fact>]
    let ``zero lines beside arcs do not change strokes`` () =
        let cases =
            ["M10 0L10 0A10 10 0 0 1 0 10","M10 0A10 10 0 0 1 0 10"
             "M10 0A10 10 0 0 1 0 10L0 10","M10 0A10 10 0 0 1 0 10"
             "M10 0L10 0A10 10 0 0 1 0 10L0 10","M10 0A10 10 0 0 1 0 10"
             "M10 0A10 10 0 0 1 0 10L0 10A10 10 0 0 1 -10 0","M10 0A10 10 0 0 1 0 10A10 10 0 0 1 -10 0"]
        for source,expected in cases do
            for a,b in [subpath source,subpath expected;Subpath.reverse(subpath source),Subpath.reverse(subpath expected)] do
                Assert.Equal(Ok b,Offset.normalizeSourceSubpath a Offset.defaultOptions)
                for cap in [Offset.Butt;Offset.RoundCap;Offset.Square] do
                    let expected = Stroke.subpath b 2.0<length> Offset.Round cap |> unwrap
                    Assert.Equal(Ok expected,Stroke.subpath a 2.0<length> Offset.Round cap)

    [<Fact>]
    let ``long construction closure and parsing retain their contracts`` () =
        let lines = [for i in 0..9999 -> Line(p (float i) 0.0,p (float(i+1)) 0.0)]
        let source = Subpath.create lines |> unwrap
        Assert.Equal(10000,Subpath.segments source |> List.length)
        let closed = Subpath.closeWith Bridge source |> unwrap
        Assert.True(Subpath.isClosed closed)
        Assert.Equal(10001,Subpath.segments closed |> List.length)
        let text = "M0 0" + String.replicate 10000 "l1 0"
        Assert.Equal(10000,parse text |> Path.subpaths |> List.exactlyOne |> Subpath.segments |> List.length)
        let many = String.replicate 10000 "M0 0L1 0Z"
        Assert.Equal(10000,parse many |> Path.subpaths |> List.length)

    [<Fact>]
    let ``long implicit and mixed command groups are stack safe`` () =
        let implicit = "M0 0" + String.replicate 10000 " 1 0"
        Assert.Equal(10000,parse implicit |> Path.subpaths |> List.exactlyOne |> Subpath.segments |> List.length)
        let block = "M0 0 1 1l1 0 0 1L4 4h1H6v1V8q1 2 3 4t2 3Q1 2 3 4T5 6c1 2 3 4 5 6s1 2 3 4C1 2 3 4 5 6S1 2 3 4a5 6 0 017 8A5 6 0 109 10z"
        let expected = subpath block
        let actual = parse (String.replicate 1000 block) |> Path.subpaths
        Assert.Equal(1000,actual.Length)
        for subpath in actual do Assert.Equal(expected,subpath)
        let initial="M10 20Q11 22 13 24T15 26C16 27 18 29 20 31Z"
        for tail in ["t1 2";"s1 2 3 4";"l1 2"] do
            Assert.Equal(parse(initial+"M10 20"+tail),parse(initial+String.replicate 5000 "Zz"+tail))

    [<Fact>]
    let ``long parser errors preserve exact suffix`` () =
        let prefix="M0 0"+String.replicate 10000 "h1"
        match Parse.path(prefix+"L1") with
        | Error(Parse.ParseError(Parse.ExpectedNumber,suffix)) -> Assert.Equal("",suffix)
        | other -> failwithf "Unexpected parse result %A" other

    [<Fact>]
    let ``Paper projected cubic witnesses retain geometric crossings`` () =
        let witnesses =
            [ CubicBezier(p 312.1968 265.6776,p 309.55039999999997 271.324,p 299.9904 273.3128,p 294.4 273.3128),p 0.0 273.3127999999945,Point.create 1.0 0.0,[0.9999990436442556]
              CubicBezier(p 512.0 640.0,p 512.0 640.0,p 388.0405751528448 332.07994673131026,p 288.04 332.0799999999999),p 0.0 332.0799999999999,Point.create 1.0 0.0,[0.9999998270048149;1.0]
              CubicBezier(p 487.8821067425946 340.2688405948006,p 487.8821067425946 340.2688405948006,p 529.4025393039343 348.4286400051758,p 529.4025393039343 348.4286400051758),p 529.402539303881 0.0,Point.create 0.0 1.0,[0.9999993457404852]
              CubicBezier(p 100.10301277238074 299.8360064274938,p 100.10301277238074 299.8360064274938,p 192.98476439737567 381.03840618337944,p 192.98476439737567 381.03840618337944),p 0.0 381.03840618333726,Point.create 1.0 0.0,[0.9999995839006389]
              CubicBezier(p 479.86630925710153 107.76493242524054,p 465.2684050136691 111.21284998937328,p 450.6709242911992 114.66066752097116,p 450.6709242911992 114.66066752097116),p 450.67092429122897 0.0,Point.create 0.0 1.0,[0.9999991752805898] ]
        for curve,origin,direction,expected in witnesses do
            let actual=Containment.segmentRayCrossingsWith curve origin direction crossingOptions |> unwrap
            Assert.Equal(expected.Length,actual.Length)
            List.iter2 (fun (t,_) wanted ->
                Assert.True(abs(float t-wanted)<1e-12)
                let point=Segment.point curve t |> unwrap
                let residual=(point.X-origin.X)*direction.Y-(point.Y-origin.Y)*direction.X
                Assert.True(abs residual<=1e-12<length>)) actual expected

    [<Fact>]
    let ``maximum radius rounded rectangles stroke across degenerate seams`` () =
        for radius in [90.0<length>;100.0<length>] do
            let source=BasicShapes.rect 0.0<length> 0.0<length> 300.0<length> 180.0<length> (Some radius) (Some radius) |> unwrap
            let cleaned=Subpath.segments source |> List.filter (function Line(a,b) -> a<>b | _ -> true) |> Subpath.create |> unwrap |> Subpath.close |> unwrap
            let expected=Stroke.subpath cleaned 30.0<length> Offset.Round Offset.Butt |> unwrap
            let actual=Stroke.subpath source 30.0<length> Offset.Round Offset.Butt |> unwrap
            Assert.Equal(expected,actual)
            for point in [p 90.0 -10.0;p 90.0 190.0;p -10.0 90.0] do
                Assert.NotEqual(Outside,Containment.pathContainment point actual Nonzero |> unwrap)

    [<Fact>]
    let ``zero line cleanup retains degenerate source errors`` () =
        let pointLine=Line(p 3.0 4.0,p 3.0 4.0)
        Assert.Equal(Error(Offset.DegenerateTangent 0.0<parameter>),Offset.segment pointLine 1.0<length> Offset.Round)
        let short=Subpath.create [Line(p 0.0 0.0,p 1e-10 0.0)] |> unwrap
        Assert.Equal(Ok short,Offset.normalizeSourceSubpath short Offset.defaultOptions)
        let invalid=subpath "M0 0A10 10 0 0 1 0 0"
        let withZero=subpath "M0 0L0 0A10 10 0 0 1 0 0"
        match Stroke.subpath invalid 2.0<length> Offset.Round Offset.Butt with
        | Error expected -> Assert.Equal(Error expected,Stroke.subpath withZero 2.0<length> Offset.Round Offset.Butt)
        | Ok _ -> failwith "Expected degenerate arc error"
