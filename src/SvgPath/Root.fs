namespace SvgPath

type RepeatedRootPolicy =
    | ConsolidateRepeatedRoot
    | PreserveRepeatedRoot

type QuadraticOptions<[<Measure>] 'Value> =
    { CoefficientTolerance: float<'Value>
      RepeatedRootPolicy: RepeatedRootPolicy }

type PolynomialOptions<[<Measure>] 'Value> =
    { CoefficientTolerance: float<'Value>
      ParameterTolerance: float<parameter>
      ValueTolerance: float<'Value>
      MaxIterations: int }

type BisectionOptions<[<Measure>] 'Value> =
    { ParameterTolerance: float<parameter>
      ValueTolerance: float<'Value>
      MaxIterations: int }

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
    | InvalidParameterTolerance of float<parameter>
    | InvalidValueTolerance of float<'Value>
    | InvalidMaxIterations of int
    | NotBracketed of
        left: float<parameter> *
        right: float<parameter> *
        leftValue: float<'Value> *
        rightValue: float<'Value>
    | MaxIterationsReached of estimate: float<parameter> * value: float<'Value>

[<RequireQualifiedAccess>]
module Root =
    let private measured<[<Measure>] 'Unit> (value: float) : float<'Unit> =
        LanguagePrimitives.FloatWithMeasure<'Unit> value

    let private orderedBracket left right =
        if left <= right then left, right else right, left

    let private isFinite (value: float<'Unit>) = System.Double.IsFinite(float value)

    let private isCloseToZero (value: float<'Unit>) (tolerance: float<'Unit>) =
        abs value <= tolerance

    let private sameSign (a: float<'Unit>) (b: float<'Unit>) =
        (a < measured<'Unit> 0.0 && b < measured<'Unit> 0.0)
        || (a > measured<'Unit> 0.0 && b > measured<'Unit> 0.0)

    let defaultPolynomialOptions<[<Measure>] 'Value> () : PolynomialOptions<'Value> =
        { CoefficientTolerance = measured<'Value> 1.0e-12
          ParameterTolerance = Parameter.fromFloat 1.0e-9
          ValueTolerance = measured<'Value> 1.0e-9
          MaxIterations = 100 }

    let defaultBisectionOptions<[<Measure>] 'Value> () : BisectionOptions<'Value> =
        { ParameterTolerance = Parameter.fromFloat 1.0e-9
          ValueTolerance = measured<'Value> 1.0e-9
          MaxIterations = 100 }

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

    let private normalizePolynomialCoefficients coefficients tolerance =
        let rec loop remaining =
            match remaining with
            | first :: rest when not (List.isEmpty rest) && coefficientIsZero first tolerance -> loop rest
            | _ -> remaining

        loop coefficients

    let private consolidateIsolations tolerance isolations =
        let tolerance = max tolerance (Parameter.fromFloat 0.0)

        isolations
        |> List.sortBy _.Estimate
        |> List.fold
            (fun kept isolation ->
                match kept with
                | previous :: _ when abs (isolation.Estimate - previous.Estimate) <= tolerance -> kept
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

        if (right - left) / 2.0 <= tolerance then
            Ok { Lower = left; Estimate = midpoint; Upper = right }
        elif remainingIterations <= 1 then
            Error(MaxIterationsReached(midpoint, midpointValue))
        elif midpointValue = 0.0<_> then
            Ok { Lower = midpoint; Estimate = midpoint; Upper = midpoint }
        elif sameSign leftValue midpointValue then
            refinePolynomialBracket coefficients midpoint midpointValue right tolerance (remainingIterations - 1)
        else
            refinePolynomialBracket coefficients left leftValue midpoint tolerance (remainingIterations - 1)

    let private crossingRoots
        (coefficients: float<'Value> list)
        (boundaries: float<parameter> list)
        (options: PolynomialOptions<'Value>)
        : Result<RootIsolation list, RootError<'Value>> =
        let rec loop remaining found =
            match remaining with
            | left :: (right :: _ as tail) ->
                let leftValue = evaluatePolynomial coefficients left
                let rightValue = evaluatePolynomial coefficients right

                if sameSign leftValue rightValue || leftValue = 0.0<_> || rightValue = 0.0<_> then
                    loop tail found
                else
                    match
                        refinePolynomialBracket
                            coefficients
                            left
                            leftValue
                            right
                            options.ParameterTolerance
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
        (options: PolynomialOptions<'Value>)
        : Result<RootIsolation list, RootError<'Value>> =
        match coefficients with
        | []
        | [ _ ] -> Ok []
        | [ a; b ] ->
            linearWithTolerance a b options.CoefficientTolerance
            |> fun roots -> inside roots lower upper
            |> List.map (fun root -> { Lower = root; Estimate = root; Upper = root })
            |> Ok
        | _ ->
            match polynomialRootIsolationsValid (polynomialDerivative coefficients) lower upper options with
            | Error error -> Error error
            | Ok derivativeRoots ->
                let critical = consolidateIsolations options.ParameterTolerance derivativeRoots
                let criticalValues = critical |> List.map _.Estimate

                let repeated =
                    critical
                    |> List.filter (fun isolation ->
                        isCloseToZero
                            (evaluatePolynomial coefficients isolation.Estimate)
                            options.ValueTolerance)

                let endpoints =
                    [ lower; upper ]
                    |> List.filter (fun value ->
                        isCloseToZero (evaluatePolynomial coefficients value) options.ValueTolerance)
                    |> List.map (fun root -> { Lower = root; Estimate = root; Upper = root })

                match crossingRoots coefficients (lower :: (criticalValues @ [ upper ])) options with
                | Error error -> Error error
                | Ok crossing ->
                    endpoints @ repeated @ crossing
                    |> consolidateIsolations options.ParameterTolerance
                    |> Ok

    let private validatePolynomialOptions (options: PolynomialOptions<'Value>) =
        if options.ParameterTolerance <= 0.0<parameter> || not (isFinite options.ParameterTolerance) then
            Error(InvalidParameterTolerance options.ParameterTolerance)
        elif options.MaxIterations <= 0 then
            Error(InvalidMaxIterations options.MaxIterations)
        else
            Ok()

    let polynomialRootIsolationsWith
        (coefficients: float<'Value> list)
        (lower: float<parameter>)
        (upper: float<parameter>)
        (options: PolynomialOptions<'Value>) =
        match validatePolynomialOptions options with
        | Error error -> Error error
        | Ok() ->
            let tolerance = max options.CoefficientTolerance (measured<'Value> 0.0)
            let normalized = normalizePolynomialCoefficients coefficients tolerance
            let lower, upper = orderedBracket lower upper
            polynomialRootIsolationsValid normalized lower upper options

    let polynomialRootsWith
        (coefficients: float<'Value> list)
        (lower: float<parameter>)
        (upper: float<parameter>)
        (options: PolynomialOptions<'Value>) =
        polynomialRootIsolationsWith coefficients lower upper options
        |> Result.map (List.map _.Estimate)

    let rec private sampleRootSide
        (coefficients: float<'Value> list)
        (candidate: float<parameter>)
        (fallbackFrom: float<parameter>)
        (intervalEdge: float<parameter>)
        (direction: int)
        (valueTolerance: float<'Value>)
        (parameterTolerance: float<parameter>)
        (remainingExpansions: int)
        : float<'Value> option =
        let beyondEdge =
            (direction < 0 && candidate <= intervalEdge)
            || (direction > 0 && candidate >= intervalEdge)

        if beyondEdge then
            None
        else
            let value = evaluatePolynomial coefficients candidate

            if not (isCloseToZero value valueTolerance) then
                Some value
            elif remainingExpansions <= 0 then
                None
            else
                let distance = max (abs (candidate - fallbackFrom) * 2.0) (parameterTolerance * 2.0)
                let nextCandidate = fallbackFrom + float direction * distance

                sampleRootSide
                    coefficients
                    nextCandidate
                    fallbackFrom
                    intervalEdge
                    direction
                    valueTolerance
                    parameterTolerance
                    (remainingExpansions - 1)

    let private signedNonzero value tolerance =
        if isCloseToZero value tolerance then None elif value < 0.0<_> then Some -1 else Some 1

    let private classifyRoot
        (coefficients: float<'Value> list)
        (isolation: RootIsolation)
        (lower: float<parameter>)
        (upper: float<parameter>)
        (options: PolynomialOptions<'Value>)
        : RootKind =
        let valueTolerance = max options.ValueTolerance (measured<'Value> 0.0)

        let leftCandidate =
            if isolation.Lower < isolation.Estimate then
                isolation.Lower
            else
                isolation.Estimate - options.ParameterTolerance

        let rightCandidate =
            if isolation.Upper > isolation.Estimate then
                isolation.Upper
            else
                isolation.Estimate + options.ParameterTolerance

        let left =
            sampleRootSide
                coefficients
                leftCandidate
                isolation.Estimate
                lower
                -1
                valueTolerance
                options.ParameterTolerance
                32

        let right =
            sampleRootSide
                coefficients
                rightCandidate
                isolation.Estimate
                upper
                1
                valueTolerance
                options.ParameterTolerance
                32

        match left |> Option.bind (fun value -> signedNonzero value valueTolerance), right |> Option.bind (fun value -> signedNonzero value valueTolerance) with
        | Some -1, Some 1 -> NegativeToPositive
        | Some 1, Some -1 -> PositiveToNegative
        | Some -1, Some -1 -> NegativeToNegative
        | Some 1, Some 1 -> PositiveToPositive
        | _ -> Ambiguous

    let classifiedPolynomialRootsWith
        (coefficients: float<'Value> list)
        (lower: float<parameter>)
        (upper: float<parameter>)
        (options: PolynomialOptions<'Value>)
        : Result<ClassifiedRoot list, RootError<'Value>> =
        let tolerance = max options.CoefficientTolerance (measured<'Value> 0.0)
        let normalized = normalizePolynomialCoefficients coefficients tolerance
        let lower, upper = orderedBracket lower upper

        polynomialRootIsolationsWith normalized lower upper options
        |> Result.map (
            List.map (fun isolation ->
                { Isolation = isolation
                  Kind = classifyRoot normalized isolation lower upper options })
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
        (options: PolynomialOptions<'Value>)
        (a: float<'Value>)
        (b: float<'Value>)
        (c: float<'Value>)
        (d: float<'Value>)
        : Result<float<parameter> list, RootError<'Value>> =
        match validatePolynomialOptions options with
        | Error error -> Error error
        | Ok() ->
            let tolerance = max options.CoefficientTolerance (measured<'Value> 0.0)
            let coefficients = normalizePolynomialCoefficients [ a; b; c; d ] tolerance

            match coefficients with
            | []
            | [ _ ] -> Ok []
            | [ linearA; linearB ] -> Ok(linear linearA linearB)
            | [ quadraticA; quadraticB; quadraticC ] ->
                quadraticWith
                    { CoefficientTolerance = options.CoefficientTolerance
                      RepeatedRootPolicy = ConsolidateRepeatedRoot }
                    quadraticA
                    quadraticB
                    quadraticC
                |> Ok
            | leading :: rest ->
                let bound = polynomialRootBound leading rest
                polynomialRootsWith coefficients -bound bound options

    let cubic a b c d = cubicWith (defaultPolynomialOptions ()) a b c d

    let rec private bisectLoop
        (f: float<parameter> -> float<'Value>)
        (left: float<parameter>)
        (leftValue: float<'Value>)
        (right: float<parameter>)
        (options: BisectionOptions<'Value>)
        (remainingIterations: int)
        : Result<float<parameter>, RootError<'Value>> =
        let midpoint = left + (right - left) / 2.0
        let midpointValue = f midpoint

        if
            isCloseToZero midpointValue options.ValueTolerance
            || (right - left) / 2.0 <= options.ParameterTolerance
        then
            Ok midpoint
        elif remainingIterations <= 1 then
            Error(MaxIterationsReached(midpoint, midpointValue))
        elif sameSign leftValue midpointValue then
            bisectLoop f midpoint midpointValue right options (remainingIterations - 1)
        else
            bisectLoop f left leftValue midpoint options (remainingIterations - 1)

    let bisectWith
        (f: float<parameter> -> float<'Value>)
        (left: float<parameter>)
        (right: float<parameter>)
        (options: BisectionOptions<'Value>)
        : Result<float<parameter>, RootError<'Value>> =
        if options.ParameterTolerance <= 0.0<parameter> || not (isFinite options.ParameterTolerance) then
            Error(InvalidParameterTolerance options.ParameterTolerance)
        elif options.ValueTolerance < measured<'Value> 0.0 || not (isFinite options.ValueTolerance) then
            Error(InvalidValueTolerance options.ValueTolerance)
        elif options.MaxIterations <= 0 then
            Error(InvalidMaxIterations options.MaxIterations)
        else
            let left, right = orderedBracket left right
            let leftValue = f left
            let rightValue = f right

            if isCloseToZero leftValue options.ValueTolerance then
                Ok left
            elif isCloseToZero rightValue options.ValueTolerance then
                Ok right
            elif sameSign leftValue rightValue then
                Error(NotBracketed(left, right, leftValue, rightValue))
            else
                bisectLoop f left leftValue right options options.MaxIterations

    let bisect f left right = bisectWith f left right (defaultBisectionOptions ())

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
