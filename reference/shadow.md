# Shadow layer effect

Draws dark, low-opacity copies of a stroked or point mark beneath the
original, offset by (`x`, `y`) and softened by stacking a few widened
copies — a drop shadow (with an offset) or an ambient shadow (offset
`0`). Applies to the same marks as
[`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md).

## Usage

``` r
shadow(
  x = 0.5,
  y = -0.5,
  color = "black",
  alpha = 0.3,
  spread = 1.5,
  layers = 3L
)
```

## Arguments

- x, y:

  Shadow offset in millimetres (`+x` right, `+y` up). Defaults to a
  small down-right drop.

- color:

  Shadow colour.

- alpha:

  Opacity of each copy.

- spread:

  Softening spread, in millimetres, over which the copies widen.

- layers:

  Number of stacked copies (more = softer).

## Value

A `ShadowSpec` object for a mark's `effects` list.

## Details

The offset is an **absolute distance in millimetres** (`+x` right, `+y`
up), resolved device-side, so a drop shadow stays the same physical
distance and is isotropic regardless of the panel's size or aspect (via
`vellum`'s compound `npc + mm` unit).

## See also

[`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md),
[`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md)

## Examples

``` r
df <- data.frame(x = 1:20, y = cumsum(rnorm(20)))
vplot(df) |> mark_line(x = x, y = y, effects = list(shadow()))
```
