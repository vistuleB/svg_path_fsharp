namespace SvgPath

type RepeatedRootPolicy =
    | ConsolidateRepeatedRoot
    | PreserveRepeatedRoot

type QuadraticOptions<[<Measure>] 'Value> =
    { CoefficientTolerance: float<'Value>
      RepeatedRootPolicy: RepeatedRootPolicy }

type PolynomialOptions = { MaxIterations: int }

[<Struct>]
type RootIsolation =
    { Lower: float<parameter>
      Estimate: float<parameter>
      Upper: float<parameter> }

type RootKind =
    | NegativeToPositive
    | PositiveToNegative
    | NegativeToNegative
    | PositiveToPositive
    | Ambiguous

[<Struct>]
type ClassifiedRoot =
    { Isolation: RootIsolation
      Kind: RootKind }

type RootError<[<Measure>] 'Value> =
    | InvalidMaxIterations of int
    | NotBracketed of
        left: float<parameter> *
        right: float<parameter> *
        leftValue: float<'Value> *
        rightValue: float<'Value>
    | MaxIterationsReached of estimate: float<parameter> * value: float<'Value>

[<RequireQualifiedAccess>]
module Root =
    let private parameterTolerance = Parameter.fromFloat 1.0e-9
    let private relativeValueTolerance = 1.0e-12

    let private measured<[<Measure>] 'Unit> (value: float) : float<'Unit> =
        LanguagePrimitives.FloatWithMeasure<'Unit> value

    let private orderedBracket left right =
        if left <= right then left, right else right, left

    let private sameSign (a: float<'Unit>) (b: float<'Unit>) =
        (a < measured<'Unit> 0.0 && b < measured<'Unit> 0.0)
        || (a > measured<'Unit> 0.0 && b > measured<'Unit> 0.0)

    let defaultPolynomialOptions () : PolynomialOptions = { MaxIterations = 100 }

    let private coefficientIsZero (value: float<'Value>) (tolerance: float<'Value>) =
        if float tolerance = 0.0 then value = measured<'Value> 0.0 else abs value < tolerance

    let private linearWithTolerance
        (a: float<'Value>)
        (b: float<'Value>)
        (tolerance: float<'Value>)
        : float<parameter> list =
        if coefficientIsZero a tolerance then
            []
        else
            [ Parameter.fromFloat (-float b / float a) ]

    let linear (a: float<'Value>) (b: float<'Value>) : float<parameter> list =
        linearWithTolerance a b (measured<'Value> 0.0)

    let quadraticWith
        (options: QuadraticOptions<'Value>)
        (a: float<'Value>)
        (b: float<'Value>)
        (c: float<'Value>)
        : float<parameter> list =
        let tolerance = max options.CoefficientTolerance (measured<'Value> 0.0)

        if coefficientIsZero a tolerance then
            linearWithTolerance b c tolerance
        else
            let discriminant = b * b - 4.0 * a * c

            if discriminant < 0.0<_> then
                []
            else
                let rootDiscriminant = sqrt discriminant
                let denominator = 2.0 * a
                let repeatedRoot = Parameter.fromFloat (float (-b / denominator))

                if rootDiscriminant = 0.0<_> then
                    match options.RepeatedRootPolicy with
                    | ConsolidateRepeatedRoot -> [ repeatedRoot ]
                    | PreserveRepeatedRoot -> [ repeatedRoot; repeatedRoot ]
                else
                    let q =
                        if b >= 0.0<_> then
                            -0.5 * (b + rootDiscriminant)
                        else
                            -0.5 * (b - rootDiscriminant)

                    let stableRoot = Parameter.fromFloat (float (q / a))
                    let recoveredRoot = Parameter.fromFloat (float (c / q))

                    if b >= 0.0<_> then
                        [ stableRoot; recoveredRoot ]
                    else
                        [ recoveredRoot; stableRoot ]

    let quadratic a b c =
        quadraticWith
            { CoefficientTolerance = measured<'Value> 0.0
              RepeatedRootPolicy = ConsolidateRepeatedRoot }
            a
            b
            c

    let strictlyInside roots lower upper =
        let lower, upper = orderedBracket lower upper
        roots |> List.filter (fun value -> value > lower && value < upper) |> List.sort

    let inside roots lower upper =
        let lower, upper = orderedBracket lower upper
        roots |> List.filter (fun value -> value >= lower && value <= upper) |> List.sort

    /// Evaluate a highest-power-first polynomial at a nominal curve parameter.
    let evaluatePolynomial (coefficients: float<'Value> list) (x: float<parameter>) : float<'Value> =
        let ratio = Parameter.ratio x
        coefficients |> List.fold (fun value coefficient -> value * ratio + coefficient) (measured<'Value> 0.0)

    /// Differentiate with respect to the dimensionless numerical parameter.
    let polynomialDerivative (coefficients: float<'Value> list) : float<'Value> list =
        let degree = List.length coefficients - 1

        coefficients
        |> List.mapi (fun index coefficient -> coefficient * float (degree - index))
        |> List.take (max 0 degree)

    let private polynomialValueScale (coefficients: float<'Value> list) : float<'Value> =
        coefficients |> List.fold (fun scale coefficient -> max scale (abs coefficient)) (measured<'Value> 0.0)

    let private polynomialCoefficientTolerance<[<Measure>] 'Value>
        (coefficients: float<'Value> list)
        : float<'Value> =
        polynomialValueScale coefficients * relativeValueTolerance

    let private valueIsCloseToZero<[<Measure>] 'Value>
        (value: float<'Value>)
        (scale: float<'Value>)
        : bool =
        abs value <= scale * relativeValueTolerance

    let private normalizePolynomialCoefficients<[<Measure>] 'Value>
        (coefficients: float<'Value> list)
        : float<'Value> list =
        let rec loop remaining =
            match remaining with
            | first :: rest when
                not (List.isEmpty rest)
                && coefficientIsZero first (polynomialCoefficientTolerance remaining) ->
                loop rest
            | _ -> remaining

        loop coefficients

    let private distinctIsolations isolations =
        isolations
        |> List.sortBy _.Estimate
        |> List.fold
            (fun kept isolation ->
                match kept with
                | previous :: _ when isolation.Estimate = previous.Estimate -> kept
                | _ -> isolation :: kept)
            []
        |> List.rev

    let rec private refinePolynomialBracket
        (coefficients: float<'Value> list)
        (left: float<parameter>)
        (leftValue: float<'Value>)
        (right: float<parameter>)
        (tolerance: float<parameter>)
        (remainingIterations: int)
        : Result<RootIsolation, RootError<'Value>> =
        let midpoint = left + (right - left) / 2.0
        let midpointValue = evaluatePolynomial coefficients midpoint

        if right - left <= tolerance then
            Ok { Lower = left; Estimate = midpoint; Upper = right }
        elif remainingIterations <= 1 then
            Error(MaxIterationsReached(midpoint, midpointValue))
        elif midpointValue = 0.0<_> then
            Ok { Lower = left; Estimate = midpoint; Upper = right }
        elif sameSign leftValue midpointValue then
            refinePolynomialBracket coefficients midpoint midpointValue right tolerance (remainingIterations - 1)
        else
            refinePolynomialBracket coefficients left leftValue midpoint tolerance (remainingIterations - 1)

    let private crossingRoots
        (coefficients: float<'Value> list)
        (boundaries: float<parameter> list)
        (options: PolynomialOptions)
        : Result<RootIsolation list, RootError<'Value>> =
        let rec loop remaining found =
            match remaining with
            | left :: (right :: _ as tail) ->
                let leftValue = evaluatePolynomial coefficients left
                let rightValue = evaluatePolynomial coefficients right
                let valueScale = polynomialValueScale coefficients

                if
                    sameSign leftValue rightValue
                    || valueIsCloseToZero leftValue valueScale
                    || valueIsCloseToZero rightValue valueScale
                then
                    loop tail found
                else
                    match
                        refinePolynomialBracket
                            coefficients
                            left
                            leftValue
                            right
                            parameterTolerance
                            options.MaxIterations
                    with
                    | Error error -> Error error
                    | Ok root -> loop tail (root :: found)
            | _ -> Ok found

        loop boundaries []

    let rec private polynomialRootIsolationsValid
        (coefficients: float<'Value> list)
        (lower: float<parameter>)
        (upper: float<parameter>)
        (options: PolynomialOptions)
        : Result<RootIsolation list, RootError<'Value>> =
        match coefficients with
        | []
        | [ _ ] -> Ok []
        | [ a; b ] ->
            linearWithTolerance a b (polynomialCoefficientTolerance coefficients)
            |> fun roots -> inside roots lower upper
            |> List.map (fun root -> { Lower = root; Estimate = root; Upper = root })
            |> Ok
        | [ a; b; c ] ->
            quadraticWith
                { CoefficientTolerance = polynomialCoefficientTolerance coefficients
                  RepeatedRootPolicy = ConsolidateRepeatedRoot }
                a
                b
                c
            |> fun roots -> inside roots lower upper
            |> List.map (fun root -> { Lower = root; Estimate = root; Upper = root })
            |> Ok
        | _ ->
            match polynomialRootIsolationsValid (polynomialDerivative coefficients) lower upper options with
            | Error error -> Error error
            | Ok derivativeRoots ->
                let critical = distinctIsolations derivativeRoots
                let criticalValues = critical |> List.map _.Estimate
                let valueScale = polynomialValueScale coefficients

                let repeated =
                    critical
                    |> List.filter (fun isolation ->
                        valueIsCloseToZero (evaluatePolynomial coefficients isolation.Estimate) valueScale)
                    |> List.map (fun isolation ->
                        { Lower = isolation.Estimate
                          Estimate = isolation.Estimate
                          Upper = isolation.Estimate })

                let endpoints =
                    [ lower; upper ]
                    |> List.filter (fun value ->
                        valueIsCloseToZero (evaluatePolynomial coefficients value) valueScale)
                    |> List.map (fun root -> { Lower = root; Estimate = root; Upper = root })

                match crossingRoots coefficients (lower :: (criticalValues @ [ upper ])) options with
                | Error error -> Error error
                | Ok crossing ->
                    endpoints @ repeated @ crossing
                    |> distinctIsolations
                    |> Ok

    let private finalizeRootWindows isolations domainLower domainUpper =
        let isolations = distinctIsolations isolations

        let rec loop remaining previousEstimate finalized =
            match remaining with
            | [] -> List.rev finalized
            | isolation :: rest ->
                let leftLimit =
                    match previousEstimate with
                    | None -> domainLower
                    | Some previous -> previous + (isolation.Estimate - previous) / 2.0

                let rightLimit =
                    match rest with
                    | [] -> domainUpper
                    | next :: _ -> isolation.Estimate + (next.Estimate - isolation.Estimate) / 2.0

                let windowLower, windowUpper =
                    if isolation.Lower = isolation.Upper then
                        isolation.Estimate - parameterTolerance / 2.0,
                        isolation.Estimate + parameterTolerance / 2.0
                    else
                        isolation.Lower, isolation.Upper

                let finalizedIsolation =
                    { Lower = max leftLimit windowLower
                      Estimate = isolation.Estimate
                      Upper = min rightLimit windowUpper }

                loop rest (Some isolation.Estimate) (finalizedIsolation :: finalized)

        loop isolations None []

    let private validatePolynomialOptions (options: PolynomialOptions) =
        if options.MaxIterations <= 0 then
            Error(InvalidMaxIterations options.MaxIterations)
        else
            Ok()

    let polynomialRootIsolationsWith
        (coefficients: float<'Value> list)
        (lower: float<parameter>)
        (upper: float<parameter>)
        (options: PolynomialOptions) =
        match validatePolynomialOptions options with
        | Error error -> Error error
        | Ok() ->
            let normalized = normalizePolynomialCoefficients coefficients
            let lower, upper = orderedBracket lower upper
            polynomialRootIsolationsValid normalized lower upper options
            |> Result.map (fun isolations -> finalizeRootWindows isolations lower upper)

    let polynomialRootsWith
        (coefficients: float<'Value> list)
        (lower: float<parameter>)
        (upper: float<parameter>)
        (options: PolynomialOptions) =
        polynomialRootIsolationsWith coefficients lower upper options
        |> Result.map (List.map _.Estimate)

    let private signedNonzero<[<Measure>] 'Value>
        (value: float<'Value>)
        (scale: float<'Value>) =
        if valueIsCloseToZero value scale then None elif value < 0.0<_> then Some -1 else Some 1

    let private classifyRootSigns<[<Measure>] 'Value>
        (leftValue: float<'Value>)
        (rightValue: float<'Value>)
        (valueScale: float<'Value>) =
        match signedNonzero leftValue valueScale, signedNonzero rightValue valueScale with
        | Some -1, Some 1 -> NegativeToPositive
        | Some 1, Some -1 -> PositiveToNegative
        | Some -1, Some -1 -> NegativeToNegative
        | Some 1, Some 1 -> PositiveToPositive
        | _ -> Ambiguous

    let private classifyRootFromDerivatives<[<Measure>] 'Value>
        (coefficients: float<'Value> list)
        (estimate: float<parameter>) =
        let rec loop derivative order =
            match derivative with
            | [] -> Ambiguous
            | _ ->
                let value = evaluatePolynomial derivative estimate

                if valueIsCloseToZero value (polynomialValueScale derivative) then
                    loop (polynomialDerivative derivative) (order + 1)
                else
                    match order % 2 = 1, value > 0.0<_> with
                    | true, true -> NegativeToPositive
                    | true, false -> PositiveToNegative
                    | false, true -> PositiveToPositive
                    | false, false -> NegativeToNegative

        loop (polynomialDerivative coefficients) 1

    let private classifyRoot
        (coefficients: float<'Value> list)
        (isolation: RootIsolation)
        : RootKind =
        let valueScale = polynomialValueScale coefficients
        let leftValue = evaluatePolynomial coefficients isolation.Lower
        let rightValue = evaluatePolynomial coefficients isolation.Upper

        match classifyRootSigns leftValue rightValue valueScale with
        | Ambiguous -> classifyRootFromDerivatives coefficients isolation.Estimate
        | kind -> kind

    let classifiedPolynomialRootsWith
        (coefficients: float<'Value> list)
        (lower: float<parameter>)
        (upper: float<parameter>)
        (options: PolynomialOptions)
        : Result<ClassifiedRoot list, RootError<'Value>> =
        let normalized = normalizePolynomialCoefficients coefficients
        let lower, upper = orderedBracket lower upper

        polynomialRootIsolationsWith normalized lower upper options
        |> Result.map (
            List.map (fun isolation ->
                { Isolation = isolation
                  Kind = classifyRoot normalized isolation })
        )

    let realLinear01Roots a b options =
        classifiedPolynomialRootsWith [ a; b ] (Parameter.fromFloat 0.0) (Parameter.fromFloat 1.0) options

    let realQuadratic01Roots a b c options =
        classifiedPolynomialRootsWith [ a; b; c ] (Parameter.fromFloat 0.0) (Parameter.fromFloat 1.0) options

    let realCubic01Roots a b c d options =
        classifiedPolynomialRootsWith [ a; b; c; d ] (Parameter.fromFloat 0.0) (Parameter.fromFloat 1.0) options

    let isSignChangeRoot kind =
        match kind with
        | NegativeToPositive
        | PositiveToNegative -> true
        | _ -> false

    let isCrossingRoot = isSignChangeRoot

    let private polynomialRootBound (leading: float<'Value>) (rest: float<'Value> list) =
        rest
        |> List.fold (fun largest coefficient -> max largest (abs (float coefficient / float leading))) 0.0
        |> (+) 1.0
        |> Parameter.fromFloat

    let cubicWith
        (options: PolynomialOptions)
        (a: float<'Value>)
        (b: float<'Value>)
        (c: float<'Value>)
        (d: float<'Value>)
        : Result<float<parameter> list, RootError<'Value>> =
        match validatePolynomialOptions options with
        | Error error -> Error error
        | Ok() ->
            let coefficients = normalizePolynomialCoefficients [ a; b; c; d ]

            match coefficients with
            | []
            | [ _ ] -> Ok []
            | [ linearA; linearB ] -> Ok(linear linearA linearB)
            | [ quadraticA; quadraticB; quadraticC ] ->
                quadraticWith
                    { CoefficientTolerance = polynomialCoefficientTolerance coefficients
                      RepeatedRootPolicy = ConsolidateRepeatedRoot }
                    quadraticA
                    quadraticB
                    quadraticC
                |> Ok
            | leading :: rest ->
                let bound = polynomialRootBound leading rest
                polynomialRootsWith coefficients -bound bound options

    let cubic a b c d = cubicWith (defaultPolynomialOptions ()) a b c d

    let rec private bisectIsolationLoop
        (f: float<parameter> -> float<'Value>)
        (left: float<parameter>)
        (leftValue: float<'Value>)
        (right: float<parameter>)
        (remainingIterations: int)
        (certified: float<parameter> -> float<parameter> -> bool)
        : Result<RootIsolation, RootError<'Value>> =
        let midpoint = left + (right - left) / 2.0
        let midpointValue = f midpoint

        if certified left right || midpoint = left || midpoint = right then
            Ok { Lower = left; Estimate = midpoint; Upper = right }
        elif remainingIterations <= 1 then
            Error(MaxIterationsReached(midpoint, midpointValue))
        elif midpointValue = 0.0<_> then
            Ok { Lower = midpoint; Estimate = midpoint; Upper = midpoint }
        elif sameSign leftValue midpointValue then
            bisectIsolationLoop f midpoint midpointValue right (remainingIterations - 1) certified
        else
            bisectIsolationLoop f left leftValue midpoint (remainingIterations - 1) certified

    let bisectIsolationUntil
        (f: float<parameter> -> float<'Value>)
        (left: float<parameter>)
        (right: float<parameter>)
        (maxIterations: int)
        (certified: float<parameter> -> float<parameter> -> bool)
        : Result<RootIsolation, RootError<'Value>> =
        if maxIterations <= 0 then
            Error(InvalidMaxIterations maxIterations)
        else
            let left, right = orderedBracket left right
            let leftValue = f left
            let rightValue = f right

            if leftValue = 0.0<_> then
                Ok { Lower = left; Estimate = left; Upper = left }
            elif rightValue = 0.0<_> then
                Ok { Lower = right; Estimate = right; Upper = right }
            elif sameSign leftValue rightValue then
                Error(NotBracketed(left, right, leftValue, rightValue))
            else
                bisectIsolationLoop f left leftValue right maxIterations certified
