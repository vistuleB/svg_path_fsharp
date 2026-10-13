<!-- Mathematical derivation shared with the Gleam implementation. -->
# Affine factorization: F# port and numerical contract

## Implemented

`Affine.decompose` defaults to `T R H S`; `Affine.decomposeWithOrder` supports
all 24 algebraic orders, and `Affine.recompose` uses the order stored in the
returned record. Matrices act on column vectors. Here `H = [[1,h],[0,1]]` and
`S = diag(sx,sy)`. Angles are degrees; `h` is a coefficient, not an angle.
The transform serializer preserves direct recognition of simple special cases
and uses this engine for general matrices. Ordinary output uses the standard
order with matrix fallback; `TransformSerialize.toStringInOrder` honors an explicit order or
returns an error. Both paths check the actual rounded SVG text, including the
conversion from shear coefficient to skew angle. See the Transform Attributes
section of the README for the separate formatting error budget.

For `A = [[a,c],[b,d]]`, normalize the first column to `(u,v)` and use:

```shell
sx = hypot(a,b)
q  = u*c + v*d
sy = u*d - v*c
theta = atan2(v,u)
h = q/sy
A = R(theta) [[sx,q],[0,sy]] = R(theta) H(h) S(sx,sy)
```

Implementation normalizes each column separately before the dot/cross products,
avoiding determinant overflow/underflow and global scaling that erases the
smaller column. Returned factors must reconstruct each column to relative
max-norm error at most 1e-12. This is a backward-error check, not a forward-error
bound on the factors. No small-determinant tolerance silently changes rank.
Computed rank and solvability failures return `NoFactorizationInOrder order`;
floating-point rank classification is not an exact arithmetic certificate.

Zero columns are handled explicitly: both zero selects zero rotation/shear and
zero scales; one zero selects zero shear, a suitable rotation, and a nonnegative
remaining scale. Two nonzero collinear columns cannot have finite RHS factors:
`sx != 0`, so singularity forces `sy = 0`, erasing the second column entirely.
For example `[[1,1],[0,0]]` is not representable in this order. This limitation
is algebraic, not an omitted numerical fallback.

## Sources and conventions

- [SVG 1.1 coordinate transformations](https://www.w3.org/TR/SVG11/coords.html#TransformMatrixDefined)
  defines the six coefficients and homogeneous affine matrix conventions.
- [LAPACK Users' Guide: QR factorization](https://www.netlib.org/lapack/lug/node40.html)
  supplies the orthogonal/upper-triangular framework for the standard order.
- [CSS Transforms 1 decomposition](https://www.w3.org/TR/css-transforms-1/#decomposing-a-2d-matrix)
  is useful background, but retains a residual 2x2 matrix and has its own
  reflection convention. Its pseudocode is not an implementation of all 24
  permutations of these four factors.

The order classification and counterexamples below are derived here by direct
matrix multiplication, not attributed to the CSS specification.

## Why arbitrary order cannot mean unconditional success

Write `C = cos(theta)`, `N = sin(theta)`, and `D = ad-bc`.
There are six linear orders and four insertion positions for translation.

| Linear order | All invertible 2x2 matrices? | Construction |
| --- | --- | --- |
| R H S | Yes, in exact arithmetic | QR, `h=q/sy` |
| R S H | Yes, in exact arithmetic | Same QR, `h=q/sx` |
| H S R | Yes, in exact arithmetic | Row-based triangularization, `h=q/sy` |
| S H R | Yes, in exact arithmetic | Same row-based triangularization, `h=q/sx` |
| S R H | No | Quadratic rotation constraint |
| H R S | No | Quadratic rotation constraint |

For the row-based construction, `sy=hypot(b,d)`, `N=b/sy`, `C=d/sy`,
`sx=a*C-c*N`, `q=a*N+c*C`. This gives `A=[[sx,q],[0,sy]] R`.
The two shear denominators in the table follow by multiplying H and S.

For `S R H`, with `a*b != 0`, direct multiplication gives:

```shell
a = sx*C              b = sy*N
c = sx*(C*h - N)      d = sy*(N*h + C)
d/b - c/a = tan(theta) + cot(theta) = D/(a*b)
```

For real nonzero `z=tan(theta)`, `|z+1/z| >= 2`.
Thus `[[1,0],[1,1]]`, whose ratio is 1, cannot have this factorization,
even though its determinant is 1. Equivalently, the equation
`a*b*z*z - D*z + a*b = 0` has no real root for this matrix.

For `H R S`, with `b*d != 0`, similarly:

```shell
a = (C+h*N)*sx       b = N*sx
c = (-N+h*C)*sy      d = C*sy
a/b - c/d = cot(theta) + tan(theta) = D/(b*d)
```

The same matrix is a counterexample. The quadratic is
`b*d*z*z - D*z + b*d = 0`. Zero denominator cases require separate branches;
they must not be rejected merely because these quotient formulas are undefined.
For nonsingular matrices the inequalities above are also sufficient when their
stated denominators are nonzero: solve the quadratic, recover scales from the
nonzero sine/cosine, and then recover shear.

Translation does not repair an impossible linear order. For any chosen factors
write the full product `L T(t) B`, where L contains the linear factors left of T.
Its translation column is `L*t`. In the invertible case solve `L*t = (e,f)`.
For singular L, first test whether `(e,f)` lies in its image, then select a
documented solution if solutions exist. For example, a zero linear map followed
on its right by translation cannot produce a nonzero output translation.

## Implementation of the general-order API

The public order type spells out all 24 permutations. `Decomposition` carries
that order and six numerical fields. Errors distinguish absence of factors in
the requested order from non-finite arithmetic and uncertified reconstruction.
No fallback to another order occurs.

The implementation uses three core solvers. The map
`F(A) = J transpose(A) J`, where J swaps the two axes, reverses products,
preserves R and H, and swaps the two diagonal scales. It therefore converts
RHS to SHR, RSH to HSR, and SRH to HRS without inverting the input matrix.

- Triangularization handles RHS/RSH, with explicit zero-column branches.
  RSH also permits rank-one inputs with two nonzero columns, using zero y-scale
  and finite shear. RHS rejects that case.
- SRH normalizes rows independently and solves the quadratic in homogeneous
  tangent coordinates. Both roots are considered. Near the discriminant-zero
  boundary (relative allowance 1e-14), a double-root candidate is tried rather
  than rejected solely on rounded discriminant sign; it must still pass the
  reconstruction bound. Clearly negative discriminants are rejected.
- SRH uses positive x-scale and prefers the smallest absolute rotation, then
  signed angle; HRS inherits positive y-scale. Other triangular orders use the
  sign conventions documented on `decomposeWithOrder`. Angles are normalized
  to (-180,180]. These conventions are deterministic, not continuous.
- Each SRH row scale is recovered from its larger coefficient, avoiding
  division by a near-zero sine/cosine close to quarter turns. A candidate with
  numerical failure does not prevent trying the other quadratic branch.
- Nonsingular translation prefixes are inverted factor by factor. Singular
  prefixes use their largest matrix entry as pivot and set one free coordinate
  to zero. The translation residual must pass certification. A zero prefix
  supports only zero output translation.
- Every successful result is recomposed and checked per linear column and per
  translation vector against relative max-norm error 1e-12. Exactly zero vectors
  must stay zero. Finite but extreme or ill-conditioned cases can return a
  numerical error rather than a factorization.

Ported tests independently multiply the ordinary affine constructors for all
24 orders: 2,880 nonsingular and 648 singular generated inputs, plus identity,
translation placement, impossible-order, scale-range, branch-selection,
near-half-turn, and zero-translation cases, together with the 189-case
standard-order grid. Serializer regressions cover all 24
orders, direct skewY output, reflections, singular fallback, precision failures,
and small shear amplified by anisotropic scale.

If every affine matrix, including every singular matrix, must always succeed,
the factor vocabulary/order must be relaxed. A two-rotation SVD form
`T R1 S R2` can cover all real 2x2 linear maps with signed scales, but that is
not a permutation of one translation, one rotation, one x-shear, and one scale.

## Numerical audit and port validation

The Gleam numerical audit found normalization-underflow misclassification:
`[[1e-300, 1e300], [1e-300, 2e300]]` in S R H and
`[[1e300, 1e300], [0, 1e-300]]` in R H S lost nonzero normalized entries.
This port mirrors the correction: report `DecompositionUnstable`, not an
impossible order. Cancellation-based rank tests remain floating-point tests,
not exact proofs.

For `[[1, 0], [1, 2 + delta]]` in S R H, exact solvability near zero requires
`delta >= 0`. The `-1e-14` case can still succeed within the documented boundary
and reconstruction tolerances; `-1e-13` and `-1e-10` are rejected. Changing
`delta` from zero to `1e-10` changes rotation from 45 degrees to approximately
44.999713521 degrees, illustrating sensitive factors despite accurate
reconstruction. Very unequal row scales can instead return a numerical error.

F# regressions cover the same 350-case boundary sweep and normalization cases,
2,880 nonsingular and 648 singular independently composed all-order cases, a
189-case standard-order grid, the near-half-turn regression, and extreme scales.
The 52 Paper.js assertions preserve signed scale expectations and adapt only
the five -180/+180 endpoint conventions. The 26 input cases additionally check
recomposition and both ordinary and explicit-order serializer round trips.
These Paper.js fixtures do not cover arbitrary shear or all 24 orders.

A seeded 4,800-case comparison against Gleam's JavaScript build also checked
canonical factors and error classifications for all 24 orders. No differences
were found (factor comparison tolerance `1e-8 × max(1, abs(expected))`).
This diagnostic supplements the independent reconstruction tests.
