namespace SvgPath.Tests

open SvgPath
open Xunit

module EncountersTests =
    let private point x y = Point.create x y

    [<Fact>]
    let ``crossing lines report one point intersection and no overlap`` () =
        let left = Line(point 0.0<length> 0.0<length>, point 2.0<length> 2.0<length>)
        let right = Line(point 0.0<length> 2.0<length>, point 2.0<length> 0.0<length>)
        match Encounters.segment left right with
        | Error error -> failwithf "%A" error
        | Ok encounters ->
            Assert.Empty encounters.Overlaps
            Assert.Single encounters.Intersections |> ignore

    [<Fact>]
    let ``coincident lines report overlap without overlap-boundary points`` () =
        let left = Line(point 0.0<length> 0.0<length>, point 2.0<length> 0.0<length>)
        let right = Line(point 1.0<length> 0.0<length>, point 3.0<length> 0.0<length>)
        match Encounters.segment left right with
        | Error error -> failwithf "%A" error
        | Ok encounters ->
            Assert.Single encounters.Overlaps |> ignore
            Assert.Empty encounters.Intersections

    [<Fact>]
    let ``subpath encounters merge shared endpoint aliases`` () =
        let left =
            Subpath.create
                [ Line(point 0.0<length> 0.0<length>, point 1.0<length> 0.0<length>)
                  Line(point 1.0<length> 0.0<length>, point 2.0<length> 0.0<length>) ]
        let right = Subpath.ofSegment (Line(point 1.0<length> -1.0<length>, point 1.0<length> 1.0<length>))
        match left with
        | Error error -> failwithf "%A" error
        | Ok left ->
            match Encounters.subpath left right with
            | Error error -> failwithf "%A" error
            | Ok encounters ->
                Assert.Empty encounters.Overlaps
                let intersection = Assert.Single encounters.Intersections
                Assert.Single intersection.LeftParameters |> ignore

    [<Fact>]
    let ``path encounters retain subpath addresses`` () =
        let left = Path.singleton (Subpath.ofSegment (Line(point 0.0<length> 0.0<length>, point 2.0<length> 2.0<length>)))
        let right = Path.singleton (Subpath.ofSegment (Line(point 0.0<length> 2.0<length>, point 2.0<length> 0.0<length>)))
        match Encounters.path left right with
        | Error error -> failwithf "%A" error
        | Ok encounters ->
            let intersection = Assert.Single encounters.Intersections
            Assert.Equal(0, (List.head intersection.LeftParameters).SubpathIndex)

    [<Fact>]
    let ``subpath filter removes parameters wholly explained by overlap`` () =
        let left = Subpath.ofSegment (Line(point 0.0<length> 0.0<length>, point 2.0<length> 0.0<length>))
        let right = Subpath.ofSegment (Line(point 1.0<length> 0.0<length>, point 3.0<length> 0.0<length>))
        match Encounters.subpath left right with
        | Error error -> failwithf "%A" error
        | Ok encounters ->
            Assert.Single encounters.Overlaps |> ignore
            match Encounters.filterFullyOverlapExplainedSubpathIntersectionParameters encounters left right 1.0e-9<length> with
            | Error error -> failwithf "%A" error
            | Ok filtered -> Assert.Empty filtered.Intersections
