namespace SvgPath

// Mechanical port of Gleam 71b05d1, internal/arcs_join.gleam.
// Corner-local coordinates; signed radii point along the visual left normal.
module internal ArcsJoin =
    type Continuation = { Start: Point<length>; Tangent: Point<1>; Radius: float<length> option }
    let private normal c = Point.rotateCounterclockwise c.Tangent
    let private center c r = Point.add c.Start (Point.scale r (normal c))
    let private sign (x: float<length>) = if x < 0.0<length> then -1.0 else 1.0
    let private sqrtRoundoff (value: float<length^2>) (scale: float<length^2>) =
        if value < -1e-12*scale then None else Some(sqrt(max 0.0<length^2> value))
    let private lineCircle (a: Point<length>) (tangent: Point<1>) (c: Point<length>) (r: float<length>) =
        let delta = Point.subtract c a
        let t = Point.dot delta tangent
        let h = Point.cross delta tangent
        match sqrtRoundoff (r*r-h*h) (r*r+h*h) with
        | None -> []
        | Some v -> [Point.add a (Point.scale (t-v) tangent); Point.add a (Point.scale (t+v) tangent)]
    let private circleCircle a ra b rb =
        let delta = Point.subtract b a
        let d = Point.norm delta
        if d = 0.0<length> then []
        else
            let x = (d*d+(ra-rb)*(ra+rb))/(2.0*d)
            match sqrtRoundoff (ra*ra-x*x) (ra*ra+x*x) with
            | None -> []
            | Some y ->
                let axis = Point.scale (1.0/d) delta
                let mid = Point.add a (Point.scale x axis)
                let side = Point.scale y (Point.rotateCounterclockwise axis)
                [Point.add mid side; Point.subtract mid side]
    let private intersections a b =
        match a.Radius,b.Radius with
        | Some ra,Some rb -> circleCircle (center a ra) (abs ra) (center b rb) (abs rb)
        | None,Some rb -> lineCircle a.Start a.Tangent (center b rb) rb
        | Some ra,None -> lineCircle b.Start b.Tangent (center a ra) ra
        | None,None -> [] // Caller delegates to MiterClip.

    let private adjustCircleLine circle line r =
        let h = Point.cross (Point.subtract circle.Start line.Start) line.Tangent
        let s = Point.cross (normal circle) line.Tangent
        Root.quadratic (s*s-1.0) (2.0*h*s) (h*h)
        |> List.filter (fun v -> System.Double.IsFinite(float v) && v*r > 0.0<_>)
        |> List.sortBy (fun v -> abs(v-r)) |> List.tryHead
        |> Option.map (fun v -> {circle with Radius=Some v})
    let private adjust a b =
        match a.Radius,b.Radius with
        | Some r,None -> adjustCircleLine a b r |> Option.map (fun a -> a,b)
        | None,Some r -> adjustCircleLine b a r |> Option.map (fun b -> a,b)
        | None,None -> None
        | Some ra,Some rb ->
            let ar,br = abs ra,abs rb
            let delta = Point.subtract (center b rb) (center a ra)
            let separate = Point.norm delta > ar+br
            let da = if separate || ar<br then 1.0 else -1.0
            let db = if separate || br<ar then 1.0 else -1.0
            let v = Point.subtract (Point.scale (sign rb*db) (normal b)) (Point.scale (sign ra*da) (normal a))
            let target = if separate then ar+br else ar-br
            let slope = if separate then da+db else da-db
            Root.quadratic (Point.dot v v-slope*slope) (2.0*(Point.dot delta v-target*slope)) (Point.dot delta delta-target*target)
            |> List.filter (fun x -> System.Double.IsFinite(float x) && x>=0.0<length> && ar+da*x>0.0<length> && br+db*x>0.0<length>)
            |> List.sort |> List.tryHead
            |> Option.map (fun amount -> {a with Radius=Some(sign ra*(ar+da*amount))},{b with Radius=Some(sign rb*(br+db*amount))})
    let private angle c tip forward =
        let r = Option.get c.Radius
        let origin = center c r
        let a,b = Point.subtract c.Start origin,Point.subtract tip origin
        let raw = Trig.atan2Degrees (Point.cross a b) (Point.dot a b)
        let clockwise = (r<0.0<length>)=forward
        if clockwise=(raw>=0.0<degree>) then abs raw else 360.0<degree> - abs raw
    // Ordering only: angles and line distances never mix for one continuation.
    let private progress c tip forward =
        match c.Radius with
        | Some _ -> float(angle c tip forward)
        | None -> float(Point.dot (Point.subtract tip c.Start) c.Tangent)*(if forward then 1.0 else -1.0)
    let private piece c tip forward =
        if Point.near 0.0<length> c.Start tip then []
        else
            match c.Radius with
            | None -> [Line(c.Start,tip)]
            | Some r -> [Arc {Start=c.Start;Radius=Point.create (abs r) (abs r);XAxisRotation=0.0<degree>;LargeArc=angle c tip forward>180.0<degree>;Sweep=(r<0.0<length>)=forward;End=tip}]
    let private assemble a b at bt =
        piece a at true @ (if Point.near 0.0<length> at bt then [] else [Line(at,bt)]) @ (piece b bt false |> List.rev |> List.map Segment.reverse)
    let private clipPoint c tip forward origin axis =
        let candidates =
            match c.Radius with
            | Some r -> lineCircle origin (Point.rotateCounterclockwise axis) (center c r) r
            | None ->
                let divisor = Point.dot c.Tangent axis
                if divisor=0.0 then []
                else [Point.add c.Start (Point.scale (Point.dot (Point.subtract origin c.Start) axis/divisor) c.Tangent)]
        let stop = progress c tip forward
        candidates |> List.filter (fun q -> let t=progress c q forward in t>=0.0 && t<=stop+1e-9)
        |> List.sortBy (fun q -> progress c q forward) |> List.tryHead
    let private clipped a b tip axis limit =
        let n = Point.rotateCounterclockwise axis
        let x,y = Point.dot tip axis,Point.dot tip n
        let extent,cut,tangent =
            if y=0.0<length> then Point.norm tip,Point.scale limit axis,axis
            else
                let r = Point.dot tip tip/(2.0*y)
                let theta = 2.0*Trig.atan2Degrees y x
                let phi = Radian.toDegrees ((limit/r)*1.0<radian>)
                let halfSin = Trig.sinDegrees (phi/2.0)
                abs(r*float(Degree.toRadians theta)),
                Point.add (Point.scale (r*Trig.sinDegrees phi) axis) (Point.scale (r*2.0*halfSin*halfSin) n),
                Point.add (Point.scale (Trig.cosDegrees phi) axis) (Point.scale (Trig.sinDegrees phi) n)
        if extent<=limit then Some(assemble a b tip tip)
        elif Point.dot (Point.subtract a.Start cut) tangent>0.0<length> || Point.dot (Point.subtract b.Start cut) tangent>0.0<length> then Some [Line(a.Start,b.Start)]
        else
            match clipPoint a tip true cut tangent,clipPoint b tip false cut tangent with
            | Some at,Some bt -> Some(assemble a b at bt)
            | _ -> None
    let join a b bisector limit =
        let pair = if List.isEmpty(intersections a b) then adjust a b else Some(a,b)
        pair |> Option.bind (fun (a,b) ->
            intersections a b
            |> List.filter (fun q -> System.Double.IsFinite(float q.X) && System.Double.IsFinite(float q.Y) && progress a q true>=0.0 && progress b q false>=0.0)
            |> List.sortBy (fun q -> Point.dot q q) |> List.tryHead
            |> Option.bind (fun tip -> clipped a b tip bisector limit))
