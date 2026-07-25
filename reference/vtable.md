# Tables with sparkline columns

`vtable()` lays a data frame out as a grid of cells where an ordinary
column renders as text and a **list-column of numeric vectors** renders
as a per-row
[`vsparkline()`](https://r-vellum.github.io/vellumplot/reference/vsparkline.md)
— chart-in-table, drawn as one vector scene (so it renders on PNG / SVG
/ PDF like any plot). It returns a compiled object;
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
it, [`print()`](https://rdrr.io/r/base/print.html) it, or drop it into a
composition.

## Usage

``` r
vtable(
  data,
  spark = list(),
  cols = NULL,
  align = NULL,
  header = TRUE,
  font_size = 9,
  spark_width = 24,
  row_height = 7,
  cell_pad = 1.5,
  units = "mm",
  dpi = 96
)
```

## Arguments

- data:

  A data frame. Sparkline columns must be **list-columns** whose cells
  are numeric vectors.

- spark:

  A named list mapping a (list-)column name to how it draws: a `type`
  string (`"line"`/`"bar"`/`"winloss"`) or a function `function(values)`
  returning a
  [`vsparkline()`](https://r-vellum.github.io/vellumplot/reference/vsparkline.md).

- cols:

  Character vector of columns to show, in order (default all).

- align:

  Optional named list of per-column alignment
  (`"left"`/`"right"`/`"centre"`); defaults to right for numeric, left
  otherwise.

- header:

  Draw a bold header row with an underline (default `TRUE`).

- font_size:

  Text size in points (default `9`).

- spark_width:

  Width of a sparkline column, in `units` (default `24`).

- row_height:

  Row height, in `units` (default `7`).

- cell_pad:

  Horizontal padding inside a text cell, in `units` (default `1.5`).

- units:

  Length unit for the sizes above: `"mm"` (default), `"cm"`, `"in"`,
  `"pt"`.

- dpi:

  Resolution for raster output.

## Value

A `VTable` (renders via
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)).

## See also

[`vsparkline()`](https://r-vellum.github.io/vellumplot/reference/vsparkline.md)

## Examples

``` r
df <- data.frame(name = c("A", "B", "C"), mean = c(3.1, 5.4, 2.2))
df$trend <- list(cumsum(rnorm(20)), cumsum(rnorm(20)), cumsum(rnorm(20)))
vtable(df, spark = list(trend = "line"))
```
