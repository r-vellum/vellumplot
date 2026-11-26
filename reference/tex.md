# Math labels

`tex()` builds a label from TeX math notation and `plotmath()` from R's
plotmath notation (as ggplot2's `parse = TRUE` reads it); `tex()` also
takes plotmath unevaluated, `tex(quote(x[i]^2))`. They are
[`vellum::tex()`](https://r-vellum.github.io/vellum/reference/tex.html)
and
[`vellum::plotmath()`](https://r-vellum.github.io/vellum/reference/plotmath.html),
re-exported. A math label goes wherever an
[`md()`](https://r-vellum.github.io/vellumplot/reference/md.md) label
does:
[`labs()`](https://r-vellum.github.io/vellumplot/reference/labs.md)
titles, scale names, discrete scale `labels =` (a list, one label per
key),
[`mark_text()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
and
[`mark_label()`](https://r-vellum.github.io/vellumplot/reference/mark_text.md)
labels (one label, or a list with one per row; or `parse = TRUE` to
parse a column), facet strips through
[`label_parsed()`](https://r-vellum.github.io/vellumplot/reference/labellers.md),
and
[`annotate()`](https://r-vellum.github.io/vellumplot/reference/annotate.md).
[`md()`](https://r-vellum.github.io/vellumplot/reference/md.md) labels
can carry inline math between `$` signs. The formula is typeset by
vellum, the same in PNG, SVG and PDF; alt text and the widget's tooltips
show its TeX source.

## Examples

``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  labs(
    title = md("Fuel use against weight, $\\hat{y} = \\beta_0 + \\beta_1 x$"),
    x = tex("w\\ (10^3\\,\\mathrm{lb})"),
    y = plotmath("mpg ~ (mi~gal^-1)")
  )
```
