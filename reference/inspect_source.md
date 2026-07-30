# Inspect the data behind a clicked element (click-to-source)

Declare that a host should surface the **source data rows** behind an
element when the user clicks (or hovers) it — the "click-to-source"
affordance. It is inert on a static render; a host
(`vellumwidget::as_widget()`) reads the compiled scene's provenance
([`provenance_join()`](https://r-vellum.github.io/vellumplot/reference/provenance_join.md)
/
[`provenance_payload()`](https://r-vellum.github.io/vellumplot/reference/provenance_payload.md))
to answer the gesture, and under Shiny reports the clicked element's
rows to `input$<id>_source`.

## Usage

``` r
inspect_source(plot, on = c("click", "hover"), values = FALSE)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

- on:

  The gesture that reveals the source, `"click"` (default) or `"hover"`.

- values:

  If `TRUE`, also ship the referenced data rows so the host can display
  their values (a heavier payload); `FALSE` (default) ships only row
  indices.

## Value

The
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md),
with source inspection registered.

## Details

This is opt-in because the row mapping adds to the widget payload;
declaring it on the plot (rather than as a host flag) keeps
interactivity a property of the spec.

## See also

[`provenance_payload()`](https://r-vellum.github.io/vellumplot/reference/provenance_payload.md),
[`select_point()`](https://r-vellum.github.io/vellumplot/reference/select_point.md)

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  inspect_source()
```
