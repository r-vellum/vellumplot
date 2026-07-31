# Lint a plot for legibility and accessibility problems

`plot_lint()` compiles a plot and reports the design problems a static
render hides from a green test suite: text too small to read, colour
contrast below the WCAG threshold, labels that overlap or fall off the
panel, and grammar-level mistakes such as an encoding with a single
level or a legend too long to read. It is the "flag it" step of the
accessibility workflow — pair it with `render_plot(cvd = )` to *see* a
failing palette and
[`scale_pattern()`](https://r-vellum.github.io/vellumplot/reference/scale_pattern.md)
/
[`pattern_hatch()`](https://r-vellum.github.io/vellumplot/reference/pattern_hatch.md)
to *fix* it with a redundant non-colour encoding.

## Usage

``` r
plot_lint(x, min_text_px = 7, min_contrast = 3)
```

## Arguments

- x:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  (or anything
  [`vellum::as_vellum_scene()`](https://r-vellum.github.io/vellum/reference/as_vellum_scene.html)
  accepts).

- min_text_px:

  Minimum legible text height in pixels (default `7`); text below it is
  flagged.

- min_contrast:

  Minimum acceptable contrast ratio between a mark and its background
  (default `3`, the WCAG AA threshold for graphical objects).

## Value

A data frame (class `vellum_lint`) with one row per finding: `rule`,
`severity` (`"warning"`/`"note"`), `node`, and a human `message`. Zero
rows when the plot is clean.

## Details

The geometric rules (contrast, text size, overlap, off-canvas) come from
the engine's
[`vellum::vl_lint()`](https://r-vellum.github.io/vellum/reference/vl_lint.html),
which judges them in resolved device pixels; the grammar rules are added
here from the trained scales. Findings are returned most-severe first.

## See also

[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
(`cvd =`),
[`scale_pattern()`](https://r-vellum.github.io/vellumplot/reference/scale_pattern.md),
[`pattern_hatch()`](https://r-vellum.github.io/vellumplot/reference/pattern_hatch.md),
[`vellum::vl_lint()`](https://r-vellum.github.io/vellum/reference/vl_lint.html)

## Examples

``` r
# a tiny-text, single-level-scale plot trips the linter
p <- vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = "one group") |>
  theme(axis.text = element_text(size = 2))
plot_lint(p)
#> Error in grDevices::col2rgb(x, alpha = TRUE): invalid color name 'one group'
```
