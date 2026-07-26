# Add a vellumplot sparkline column to a `gt` table

`gt_vsparkline()` renders a **list-column of numeric vectors** as a
per-row
[`vsparkline()`](https://r-vellum.github.io/vellumplot/reference/vsparkline.md),
embedded as inline SVG in a
[`gt::gt()`](https://gt.rstudio.com/reference/gt.html) table — so a gt
table (with all of gt's formatting and theming) gains a vellum sparkline
column. Needs the gt package, and HTML output (inline SVG is not
embedded by gt's LaTeX / Word backends).

## Usage

``` r
gt_vsparkline(
  gt_object,
  column,
  type = "line",
  ...,
  width = 30,
  height = 8,
  units = "mm"
)
```

## Arguments

- gt_object:

  A [`gt::gt()`](https://gt.rstudio.com/reference/gt.html) object.

- column:

  The list-column to render (bare name or string).

- type:

  Sparkline type: `"line"` (default), `"bar"`, or `"winloss"`.

- ...:

  Further
  [`vsparkline()`](https://r-vellum.github.io/vellumplot/reference/vsparkline.md)
  arguments (e.g. `color`, `points`).

- width, height, units:

  Sparkline size (default `30 x 8` mm).

## Value

The modified `gt` object.

## Details

The vectors are recovered from the gt object's data in its current row
order; apply `gt_vsparkline()` **before** any gt row reordering /
grouping.

## See also

[`vsparkline()`](https://r-vellum.github.io/vellumplot/reference/vsparkline.md),
[`vtable()`](https://r-vellum.github.io/vellumplot/reference/vtable.md),
[`plot_svg()`](https://r-vellum.github.io/vellumplot/reference/plot_svg.md)

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(metric = c("A", "B"))
df$trend <- list(cumsum(rnorm(20)), cumsum(rnorm(20)))
gt::gt(df) |> gt_vsparkline(trend, type = "line")
} # }
```
