# Density-shape marks

Marks that draw a variable's distribution as a filled shape.
`mark_violin()` draws a mirrored kernel density of `y` per categorical
`x` (like a boxplot's footprint); `mark_ridgeline()` draws a kernel
density of `x` per categorical `y` as overlapping ridges.
`mark_dotplot()` bins `x` and stacks one dot per observation.

## Usage

``` r
mark_violin(plot, ..., adjust = 1, blend = NULL, sketch = NULL, data = NULL)

mark_ridgeline(
  plot,
  ...,
  adjust = 1,
  height = 1.4,
  blend = NULL,
  sketch = NULL,
  data = NULL
)

mark_dotplot(
  plot,
  ...,
  binwidth = NULL,
  blend = NULL,
  sketch = NULL,
  data = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- ...:

  Encodings. `mark_violin()` needs categorical `x` and numeric `y`;
  `mark_ridgeline()` needs numeric `x` and categorical `y`;
  `mark_dotplot()` needs `x`. A mapped `color`/`fill` sets the shape
  fill.

- adjust:

  Kernel-density bandwidth multiplier (violin/ridgeline).

- blend, sketch, data:

  Standard layer arguments (see
  [`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)).

- height:

  Ridge height as a multiple of the row band (ridgeline; default `1.4`,
  so adjacent ridges overlap slightly).

- binwidth:

  Dot-plot bin width, or `NULL` to use ~1/30 of the data range.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Examples

``` r
df <- data.frame(g = rep(letters[1:3], each = 50), v = rnorm(150))
vplot(df) |> mark_violin(x = g, y = v)

vplot(df) |> mark_ridgeline(x = v, y = g)

vplot(df) |> mark_dotplot(x = v)
```
