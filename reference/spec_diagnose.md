# Diagnose a spec against its data

`spec_diagnose()` checks a spec (a list, JSON string, or path) or a
built
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
without throwing: it parses it, validates every referenced field against
the data, runs a few pre-compile checks, dry-run compiles it, and
reports per layer how many rows will not be shown. It returns a
structured verdict. `vplot_from_spec()` is the strict form — it returns
the
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
on success and raises a classed `vellumplot_spec_invalid` error
(carrying the same diagnostics) on failure.

## Usage

``` r
spec_diagnose(spec, data = NULL, env = globalenv())

vplot_from_spec(spec, data = NULL, env = globalenv())
```

## Arguments

- spec:

  A spec list (from
  [`as_spec()`](https://r-vellum.github.io/vellumplot/reference/as_spec.md)),
  a JSON string, a `.json` path, or a
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- data:

  The data frame the spec is drawn from. Required when the spec stores
  data by reference; optional (for field validation) when inlined.
  Ignored for a
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md),
  which carries its own data.

- env:

  Environment channel expressions are re-quoted in.

## Value

`spec_diagnose()` returns a list of class `vellumplot_diagnosis` with
`ok` (logical), `plot` (the
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
or `NULL`), `diagnostics` (a list of records with `severity`, `field`,
`message`, `hint`), and `report` (a data frame with one row per layer:
`layer`, `mark`, `rows`, the counts `missing`, `transform`, `stat`,
`limits`, their total `dropped`, and `oob`, the rows outside a scale's
limits that are still drawn; `NULL` when the plot could not be built).
Printing it shows a summary. `vplot_from_spec()` returns a
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

This is the agent repair loop: a failure comes back as data an agent can
act on (which field is unknown, the nearest real field) rather than a
traceback.

## Pre-compile checks

Before compiling, the plot is checked for mistakes that would compile to
a wrong plot or fail with an opaque error:

- a **facet variable missing** from the plot data (an error), or from a
  layer's own data (a warning: that layer is then repeated across that
  variable's panels);

- **`color` (or `fill`) mapped to variables of different type** in two
  layers (one continuous, one discrete), although the layers share one
  scale and legend (an error, or a warning when the scale kind is set
  explicitly);

- a **manual or discrete palette**
  ([`scale_color_manual()`](https://r-vellum.github.io/vellumplot/reference/scale_color_continuous.md),
  ...) on a continuous colour channel, which the compile refuses (an
  error).

## Compile report

Some rows produce no visible mark. The `report` element counts them per
layer, on the layer's input rows, attributing each row to the first
reason that applies:

- `missing`: a continuous position value (`x`, `y`, `xend`, `ymin`, ...)
  is `NA`. Such a row is removed from the plot with a warning, or leaves
  a gap in a line, step or area;

- `transform`: the axis transform cannot map the value, e.g. a value
  `<= 0` on a log axis;

- `stat`: the layer's statistic leaves the row out, e.g. an infinite
  value in a histogram or density, or a group with too few points for a
  density, a smooth or an ellipse;

- `limits`: the value is outside explicit scale limits
  ([`lims()`](https://r-vellum.github.io/vellumplot/reference/lims.md),
  `scale_*(limits = )`), so it is clipped away. Coordinate limits
  (`coord_cartesian(xlim = )`) zoom instead and are not counted.

Their total is `dropped`. Limits follow each scale's `oob` handler: a
position outside its limits counts under `limits` when it is clipped
(the default) or censored, and so does a censored size. A separate
column, `oob`, counts rows that are outside a scale's `limits` but still
drawn: a squished position, or a colour, fill, size, alpha or linewidth
value (clamped to the end of the palette, given the NA colour, or
extrapolated). They are not part of `dropped`.

A missing *non-position* value (colour, size, ...) is drawn with the
scale's `NA` value, a missing discrete position as a trailing `"NA"`
category, and a missing facet value in a trailing `"NA"` panel, so none
of them is counted. Each layer that loses rows also adds a `"warning"`
diagnostic, so it shows in the printed summary.

## See also

[`spec_fields()`](https://r-vellum.github.io/vellumplot/reference/spec_fields.md),
[`spec_schema()`](https://r-vellum.github.io/vellumplot/reference/spec_schema.md),
[`vplot_ask()`](https://r-vellum.github.io/vellumplot/reference/vplot_ask.md)

## Examples

``` r
spec <- as_spec(vplot(mtcars) |> mark_point(x = wt, y = mpg))
d <- spec_diagnose(spec, data = mtcars)
d$ok
#> [1] TRUE

# a PlotSpec works too; the report shows rows a log axis cannot draw
df <- data.frame(x = 1:4, y = c(-1, 1, 10, NA))
p <- vplot(df) |>
  mark_point(x = x, y = y) |>
  scale_y_continuous(trans = "log10")
spec_diagnose(p)
#> Warning: NaNs produced
#> Warning: NaNs produced
#> <vellumplot diagnosis> ok: 0 errors, 1 warning
#> * [warning] Layer 1 (point): 2 of 4 rows will not be shown: 1 missing a position value, 1 not mappable on a transformed axis (e.g. <= 0 on a log scale).
#>     See `$report` for the per-layer counts.
#> 
#> Rows not shown, per layer (`oob`: drawn, outside a scale's limits):
#>  layer  mark rows missing transform stat limits dropped oob
#>      1 point    4       1         1    0      0       2   0
```
