namespace SvgPath

/// Bounding boxes and conservative bounding polygons.
[<RequireQualifiedAccess>]
module Bounds =

    let boundingBoxWidth = BoundingBox.width

    let boundingBoxHeight = BoundingBox.height

    let boundingBoxCenter = BoundingBox.center

    let boundingBoxTaxicabDiameter = BoundingBox.taxicabDiameter

    let boundingBoxUnion = BoundingBox.union

    let boundingBoxUnionMany = BoundingBox.unionMany

    let pointsBoundingBox = BoundingBox.ofPoints

    let segmentBoundingBox = Segment.boundingBox

    let segmentBoundingPolygon = Segment.boundingPolygon

    let segmentBoundingPolygonBetween = Segment.boundingPolygonBetween

    let subpathBoundingBox = Subpath.boundingBox

    let pathBoundingBox = Path.boundingBox
