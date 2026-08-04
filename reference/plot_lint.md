# Lint a plot for legibility and accessibility problems

`plot_lint()` compiles a plot and reports the design problems a static
render hides from a green test suite: text too small to read, colour
contrast below the WCAG threshold, two palette colours a colour-blind
reader cannot tell apart, labels that overlap or fall off the panel, and
grammar-level mistakes such as an encoding with a single level or a
legend too long to read. It is the "flag it" step of the accessibility
workflow — pair it with `render_plot(cvd = )` to *see* a failing palette
and
[`scale_pattern()`](https://r-vellum.github.io/vellumplot/reference/scale_pattern.md)
/
[`pattern_hatch()`](https://r-vellum.github.io/vellumplot/reference/pattern_hatch.md)
to *fix* it with a redundant non-colour encoding.

## Usage

``` r
plot_lint(x, ..., min_text_px = 7, min_contrast = 3)
```

## Arguments

- x:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  (or anything
  [`vellum::as_vellum_scene()`](https://r-vellum.github.io/vellum/reference/as_vellum_scene.html)
  accepts).

- ...:

  Passed to
  [`vellum::vl_lint()`](https://r-vellum.github.io/vellum/reference/vl_lint.html):
  `rules`, `exclude`, `severity`, `cvd`, `min_text_pt`, `max_overplot`
  and the rest of the thresholds.

- min_text_px:

  Minimum legible text height in pixels (default `7`); text below it is
  flagged.

- min_contrast:

  Minimum acceptable contrast ratio between a mark and its background
  (default `3`, the WCAG AA threshold for graphical objects).

## Value

A data frame (class `vellum_lint`) with one row per finding: `rule`,
`severity` (`"warning"`/`"note"`), `node`, a human `message`, and the
device-px box `x0`/`y0`/`x1`/`y1` — `NA` for a grammar finding, which is
about a scale and so has no box. Zero rows when the plot is clean.

## Details

The geometric rules come from the engine's
[`vellum::vl_lint()`](https://r-vellum.github.io/vellum/reference/vl_lint.html),
which judges them in resolved device pixels. The grammar rules are
registered into the same registry by vellumplot, so they are not
special:
[`vellum::vl_lint_rules()`](https://r-vellum.github.io/vellum/reference/vl_lint_rule.html)
lists them, `rules =` selects them, and
[`vellum::vl_lint()`](https://r-vellum.github.io/vellum/reference/vl_lint.html)
on a compiled plot reports them too. Findings are returned most-severe
first.

Because every finding carries the box it refers to,
[`vellum::vl_lint_overlay()`](https://r-vellum.github.io/vellum/reference/vl_lint_overlay.html)
can draw the report onto the plot, and
[`vellum::vl_lint_assert()`](https://r-vellum.github.io/vellum/reference/vl_lint_assert.html)
can fail a test on it.

A composition or table has no single set of trained scales, so it
reports the geometric findings only — lint the cells individually for
the grammar ones (vellumplot#147).

## See also

[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)
(`cvd =`),
[`scale_pattern()`](https://r-vellum.github.io/vellumplot/reference/scale_pattern.md),
[`pattern_hatch()`](https://r-vellum.github.io/vellumplot/reference/pattern_hatch.md),
[`vellum::vl_lint()`](https://r-vellum.github.io/vellum/reference/vl_lint.html),
[`vellum::vl_lint_overlay()`](https://r-vellum.github.io/vellum/reference/vl_lint_overlay.html),
[`vellum::vl_lint_assert()`](https://r-vellum.github.io/vellum/reference/vl_lint_assert.html)

## Examples

``` r
# a tiny-text, single-level-scale plot trips the linter
p <- vplot(transform(mtcars, grp = "one group")) |>
  mark_point(x = wt, y = mpg, color = grp) |>
  theme(axis.text = element_text(size = 2))
plot_lint(p)
#> 9 lint findings (8 warnings):
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ℹ [single_level_scale] scale:color: The color scale has a single level (one
#>   group): the encoding conveys nothing and its legend is redundant.

# the engine's arguments come through, so a project can accept a finding
plot_lint(p, exclude = "scale:color")
#> 8 lint findings (8 warnings):
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 2.7 px tall - below the 7 px legibility floor
```
