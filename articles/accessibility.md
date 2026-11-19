# Accessibility

A chart that a screen-reader user cannot perceive, or that a keyboard
user cannot operate, excludes people. The vellum ecosystem treats
accessibility as part of the output contract rather than an
afterthought: **every plot you compile starts from a usable baseline** —
a title and a generated text alternative, an SVG that announces itself
as an image, a tagged PDF, and a widget you can operate without a mouse.

A baseline is not compliance. A generated description can say what is
plotted but not what it means.
[`plot_lint()`](https://r-vellum.github.io/vellumplot/reference/plot_lint.md)
now checks your colour contrast and text legibility (see below), but
nothing decides whether your *takeaway* is legible. What the ecosystem
removes is the boilerplate excuse: the mechanics are already in place,
so the remaining work is editorial. This article is the cross-package
guide to both halves.

The work is split across the three packages along the same seam as
everything else:

- **vellumplot** (this package) is the *author*. It generates the plot’s
  text alternative and hands it, with the title, to the scene.
- **vellum** is the *emitter*. It turns that title/description into an
  accessible SVG and a tagged PDF.
- **vellumwidget** is the *interactive layer*. It makes the widget
  keyboard- and screen-reader-navigable.

## Alt text: the text alternative

The single most important thing a chart needs is a **text alternative**,
a prose sentence or two describing what it shows (WCAG 1.1.1).
vellumplot writes one for every plot automatically, from the spec it
already has:

``` r

p <- vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = hp)

plot_alt(p)
#> [1] "A scatter plot. It plots mpg (vertical axis) against wt (horizontal axis), where colour shows hp. Based on 32 observations."
```

The generated description names the chart type, what is mapped to each
axis and to colour/size, the number of observations, and any faceting.
It is a *sensible default*, not a substitute for editorial judgement.
The auto text says *what* is plotted, not *what it means*. When you know
the takeaway, write it:

``` r

p <- p |>
  labs(
    title = "Heavier cars use more fuel",
    alt = paste(
      "Scatter plot of fuel economy against weight for 32 cars.",
      "Fuel economy falls roughly linearly as weight rises, from about",
      "34 mpg at 1.5 tons to about 10 mpg at 5 tons."
    )
  )

plot_alt(p)
#> [1] "Scatter plot of fuel economy against weight for 32 cars. Fuel economy falls roughly linearly as weight rises, from about 34 mpg at 1.5 tons to about 10 mpg at 5 tons."
```

`labs(alt =)` always wins over the automatic text.
[`plot_alt()`](https://r-vellum.github.io/vellumplot/reference/plot_alt.md)
returns whichever applies, so you can inspect exactly what assistive
technology will receive.

## What the accessible SVG emits

At the compile seam the plot **title** becomes the scene’s accessible
*name* and
[`plot_alt()`](https://r-vellum.github.io/vellumplot/reference/plot_alt.md)
becomes its *description*. vellum emits these as the reliable
screen-reader pattern: `role="img"` with `<title>` and `<desc>`
referenced by `aria-labelledby`:

``` r

f <- tempfile(fileext = ".svg")
render_plot(p, f)

svg <- paste(readLines(f), collapse = "\n")
# the opening <svg> tag plus its <title>/<desc>
regmatches(svg, regexpr("<svg[^>]*>.*?</desc>", svg, perl = TRUE))
#> [1] "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"576\" height=\"384\" viewBox=\"0 0 576 384\" role=\"img\" aria-labelledby=\"vledd10f39-t vledd10f39-d\"><title id=\"vledd10f39-t\">Heavier cars use more fuel</title><desc id=\"vledd10f39-d\">Scatter plot of fuel economy against weight for 32 cars. Fuel economy falls roughly linearly as weight rises, from about 34 mpg at 1.5 tons to about 10 mpg at 5 tons.</desc>"
```

A screen reader announces the title as the image’s name and the
description on request. This happens for every compiled plot; you never
have to remember to turn it on. (A plot with no title still carries the
auto-generated description, so it is never a mute image.)

## Accessible PDF

The same title/description drive a **tagged PDF** (the basis of PDF/UA)
when you export to `.pdf`. The chart becomes a `Figure` in the
document’s structure tree, carrying the description as its `Alt` text,
so a PDF reader can announce it:

``` r

render_plot(p, "fuel.pdf") # a tagged PDF: /StructTreeRoot + a Figure with Alt
```

Tagging is automatic and additive. A plot with no title/description
exports as an ordinary, untagged PDF exactly as before. (Strict PDF/UA-1
*validation* is a planned follow-up; the tag tree and `Alt` ship today.)

The chart is a **single** `Figure` carrying the one description — the
individual marks are its visual content, tagged as *artifacts* rather
than each becoming its own announced figure. That is deliberate: a
screen reader should hear the plot’s text alternative once, not read out
every line, point, and label (still less the internal identifiers a mark
carries for provenance and interactivity). Gridlines and other furniture
are artifacts for the same reason.

## Colour, contrast, and legibility

Alt text and tags make a plot *perceivable to assistive technology*. A
separate question is whether the picture itself is legible — text large
enough to read, contrast high enough to see, and information that does
not vanish for a colour-blind viewer. Three tools form a loop: **see**
the problem, **flag** it, **fix** it.

### See it: `render_plot(cvd = )`

About 1 in 12 men has some form of colour-vision deficiency, most
commonly red–green. `render_plot(cvd = )` renders a `.png` through a
simulation of how a palette reads for such a viewer — `"protanopia"`,
`"deuteranopia"`, `"tritanopia"`, or `"achromatopsia"` — so you can see
a failing palette instead of guessing:

``` r

p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = factor(cyl))

render_plot(p, "normal.png")
render_plot(p, "deuteranopia.png", cvd = "deuteranopia") # red-green preview
```

Simulation is raster-only (it is a pixel operation), so it is ignored
for `.svg`/`.pdf`.

### Flag it: `plot_lint()`

[`plot_lint()`](https://r-vellum.github.io/vellumplot/reference/plot_lint.md)
compiles the plot and reports the legibility problems a green test suite
hides: text below a readable size, contrast under the WCAG threshold,
two palette colours a colour-blind reader cannot tell apart, labels that
overlap or fall off the panel (all judged in real device pixels by the
engine), plus grammar-level mistakes such as an encoding with a single
level or a legend too long to read.

``` r

p_bad <- vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(rep("all cars", nrow(mtcars)))) |>
  theme(axis.text = element_text(size = 4))

plot_lint(p_bad)
#> 9 lint findings (8 warnings):
#> ✖ [tiny_text] text: 5.3 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 5.3 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 5.3 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 5.3 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 5.3 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 5.3 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 5.3 px tall - below the 7 px legibility floor
#> ✖ [tiny_text] text: 5.3 px tall - below the 7 px legibility floor
#> ℹ [single_level_scale] scale:color: The color scale has a single level (all
#>   cars): the encoding conveys nothing and its legend is redundant.
```

A well-made plot lints clean:

``` r

plot_lint(vplot(mtcars) |> mark_point(x = wt, y = mpg))
#> ✔ No lint findings.
```

Each finding is a row: `rule`, `severity`, `node`, `message`, and the
device-px box the finding refers to. `min_text_px` and `min_contrast`
set the thresholds, and every other argument of
[`vellum::vl_lint()`](https://r-vellum.github.io/vellum/reference/vl_lint.html)
comes through too — `exclude` to accept a finding you have already
judged, `cvd` to choose which colour-vision deficiency to check, `rules`
to run one rule on its own.

Because the findings carry their boxes, the report can be drawn onto the
plot instead of read:

``` r

scene <- vellum::as_vellum_scene(p_bad)
vellum::display(vellum::vl_lint_overlay(scene, plot_lint(p_bad)))
```

![The plot above with boxes drawn around each linted
element.](accessibility_files/figure-html/unnamed-chunk-9-1.png)

And it can gate a test suite, which is the point of flagging things at
all:

``` r

test_that("the figure is legible", {
  vellum::vl_lint_assert(my_plot())
})
```

The grammar rules are not a separate mechanism. vellumplot registers
them in the engine’s own rule registry, so
[`vellum::vl_lint_rules()`](https://r-vellum.github.io/vellum/reference/vl_lint_rule.html)
lists them, `rules =` selects them, and a plain
[`vellum::vl_lint()`](https://r-vellum.github.io/vellum/reference/vl_lint.html)
of a compiled plot reports the encoding problems alongside the geometric
ones:

``` r

subset(vellum::vl_lint_rules(), tags == "grammar", c("rule", "description"))
#>                  rule
#> 14    legend_overflow
#> 19 single_level_scale
#>                                                    description
#> 14 A discrete encoding has more levels than a legend can show.
#> 19   A discrete encoding has one level, so it encodes nothing.
```

### Fix it: a redundant, non-colour encoding

The robust fix for a colour encoding that fails under CVD or in
greyscale is to carry the same information a second way. Map the
`pattern` aesthetic alongside `fill`, and reach for
[`pattern_hatch()`](https://r-vellum.github.io/vellumplot/reference/pattern_hatch.md)
— a crisp **vector** hatch that stays sharp in PDF and survives being
printed in black and white (unlike the raster tile patterns):

``` r

d <- data.frame(grp = c("A", "B", "C"), n = c(8, 5, 11))

vplot(d) |>
  mark_bar(x = grp, y = n, fill = grp, pattern = grp) |>
  scale_pattern(values = list(
    pattern_hatch(angle = 0),
    pattern_hatch(angle = 45),
    pattern_hatch(angle = 90)
  ))
```

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAlIiBoZWlnaHQ9IjEwMCUiIHZpZXdib3g9IjAgMCA1NzYgMzg0IiByb2xlPSJpbWciIGFyaWEtbGFiZWxsZWRieT0idmw5NGQwMTlmNi1kIj48ZGVzYyBpZD0idmw5NGQwMTlmNi1kIj5BIGJhciBjaGFydC4gSXQgcGxvdHMgbiAodmVydGljYWwgYXhpcykgYWdhaW5zdCBncnAgKGhvcml6b250YWwgYXhpcyksIHdoZXJlIGNvbG91ciBzaG93cyBncnAuIEJhc2VkIG9uIDMgb2JzZXJ2YXRpb25zLjwvZGVzYz48ZGVmcz48Y2xpcHBhdGggaWQ9InZsOTRkMDE5ZjYtYzAiIGNsaXBwYXRodW5pdHM9InVzZXJTcGFjZU9uVXNlIj48cGF0aCBkPSJNODAuNzg5MzY3Njc1NzgxMjUgMjAuNzg3NDAxMTk5MzQwODIgTDUwNS4yNjI3OTI4NzI2MzE2IDIwLjc4NzQwMTE5OTM0MDgyIEw1MDUuMjYyNzkyODcyNjMxNiAzMTUuOTE4OTYyODc5MTMwODUgTDgwLjc4OTM2NzY3NTc4MTI1IDMxNS45MTg5NjI4NzkxMzA4NSBaIiAvPjwvY2xpcHBhdGg+PC9kZWZzPjxyZWN0IHdpZHRoPSI1NzYiIGhlaWdodD0iMzg0IiBmaWxsPSIjZmZmZmZmIiBmaWxsLW9wYWNpdHk9IjEiIC8+PGcgZGF0YS12ZWxsdW0tcGFuZWw9InBsb3QiPjxnIGRhdGEtdmVsbHVtLXBhbmVsPSJwbG90LWJhY2tncm91bmQiPjxnIHJvbGU9InByZXNlbnRhdGlvbiI+PHBhdGggZD0iTTAgMCBMNTc2IDAgTDU3NiAzODQgTDAgMzg0IFoiIGZpbGw9IiNmZmZmZmYiIC8+PC9nPjwvZz48ZyBkYXRhLXZlbGx1bS1wYW5lbD0icGFuZWwtYXJlYSI+PGcgZGF0YS12ZWxsdW0tcGFuZWw9InBhbmVsLTEtMSIgY2xpcC1wYXRoPSJ1cmwoI3ZsOTRkMDE5ZjYtYzApIj48ZyBkYXRhLXZlbGx1bS1wYW49InBhbmVsLTEtMSI+PHBhdGggZD0iTTgwLjc5IDIwLjc5IEw1MDUuMjYgMjAuNzkgTDUwNS4yNiAzMTUuOTIgTDgwLjc5IDMxNS45MiBaIiBmaWxsPSIjZWJlYmViIiAvPjxnIHJvbGU9ImdyaWQiPjxwYXRoIGQ9Ik04MC43OSAzMzIuOTkgTDUwNS4yNiAzMzIuOTkgTTgwLjc5IDI3Mi4wMiBMNTA1LjI2IDI3Mi4wMiBNODAuNzkgMjExLjA0IEw1MDUuMjYgMjExLjA0IE04MC43OSAxNTAuMDYgTDUwNS4yNiAxNTAuMDYgTTgwLjc5IDg5LjA4IEw1MDUuMjYgODkuMDggTTgwLjc5IDI4LjEgTDUwNS4yNiAyOC4xIiBmaWxsPSJub25lIiBzdHJva2U9IiNmZmZmZmYiIHN0cm9rZS13aWR0aD0iMC41IiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIC8+PC9nPjxnIHJvbGU9ImdyaWQiPjxwYXRoIGQ9Ik0xNTEuNTMgMzE1LjkyIEwxNTEuNTMgMjAuNzkgTTI5My4wMyAzMTUuOTIgTDI5My4wMyAyMC43OSBNNDM0LjUyIDMxNS45MiBMNDM0LjUyIDIwLjc5IiBmaWxsPSJub25lIiBzdHJva2U9IiNmZmZmZmYiIHN0cm9rZS13aWR0aD0iMSIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIiBzdHJva2UtbGluZWpvaW49InJvdW5kIiAvPjwvZz48ZyByb2xlPSJncmlkIj48cGF0aCBkPSJNODAuNzkgMzAyLjUgTDUwNS4yNiAzMDIuNSBNODAuNzkgMjQxLjUzIEw1MDUuMjYgMjQxLjUzIE04MC43OSAxODAuNTUgTDUwNS4yNiAxODAuNTUgTTgwLjc5IDExOS41NyBMNTA1LjI2IDExOS41NyBNODAuNzkgNTguNTkgTDUwNS4yNiA1OC41OSIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjZmZmZmZmIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgLz48L2c+PGcgcm9sZT0icHJlc2VudGF0aW9uIj48cGF0aCBkPSJNMTUxLjUzIDMxNS45MiBMMTUxLjUzIDMxMC4yNSBNMjkzLjAzIDMxNS45MiBMMjkzLjAzIDMxMC4yNSBNNDM0LjUyIDMxNS45MiBMNDM0LjUyIDMxMC4yNSIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMzMzMzMzIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgLz48L2c+PGcgcm9sZT0icHJlc2VudGF0aW9uIj48cGF0aCBkPSJNODAuNzkgMzAyLjUgTDg2LjQ2IDMwMi41IE04MC43OSAyNDEuNTMgTDg2LjQ2IDI0MS41MyBNODAuNzkgMTgwLjU1IEw4Ni40NiAxODAuNTUgTTgwLjc5IDExOS41NyBMODYuNDYgMTE5LjU3IE04MC43OSA1OC41OSBMODYuNDYgNTguNTkiIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzMzMzMzMyIgc3Ryb2tlLXdpZHRoPSIxIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIC8+PC9nPjxnIGRhdGEtdmVsbHVtLWlkPSJsYXllci0xLWJhci1nMSIgcm9sZT0icHJlc2VudGF0aW9uIj48cGF0aCBkPSJNODcuODYgMTA3LjM4IEwyMTUuMjEgMTA3LjM4IEwyMTUuMjEgMzAyLjUgTDg3Ljg2IDMwMi41IFoiIGZpbGw9IiNmZmZmZmYiIGZpbGwtb3BhY2l0eT0iMCIgLz48cGF0aCBkPSJNODcuODYgMTA4Ljc5IEwyMTUuMjEgMTA4Ljc5IE04Ny44NiAxMTIuNzkgTDIxNS4yMSAxMTIuNzkgTTg3Ljg2IDExNi43OSBMMjE1LjIxIDExNi43OSBNODcuODYgMTIwLjc5IEwyMTUuMjEgMTIwLjc5IE04Ny44NiAxMjQuNzkgTDIxNS4yMSAxMjQuNzkgTTg3Ljg2IDEyOC43OSBMMjE1LjIxIDEyOC43OSBNODcuODYgMTMyLjc5IEwyMTUuMjEgMTMyLjc5IE04Ny44NiAxMzYuNzkgTDIxNS4yMSAxMzYuNzkgTTg3Ljg2IDE0MC43OSBMMjE1LjIxIDE0MC43OSBNODcuODYgMTQ0Ljc5IEwyMTUuMjEgMTQ0Ljc5IE04Ny44NiAxNDguNzkgTDIxNS4yMSAxNDguNzkgTTg3Ljg2IDE1Mi43OSBMMjE1LjIxIDE1Mi43OSBNODcuODYgMTU2Ljc5IEwyMTUuMjEgMTU2Ljc5IE04Ny44NiAxNjAuNzkgTDIxNS4yMSAxNjAuNzkgTTg3Ljg2IDE2NC43OSBMMjE1LjIxIDE2NC43OSBNODcuODYgMTY4Ljc5IEwyMTUuMjEgMTY4Ljc5IE04Ny44NiAxNzIuNzkgTDIxNS4yMSAxNzIuNzkgTTg3Ljg2IDE3Ni43OSBMMjE1LjIxIDE3Ni43OSBNODcuODYgMTgwLjc5IEwyMTUuMjEgMTgwLjc5IE04Ny44NiAxODQuNzkgTDIxNS4yMSAxODQuNzkgTTg3Ljg2IDE4OC43OSBMMjE1LjIxIDE4OC43OSBNODcuODYgMTkyLjc5IEwyMTUuMjEgMTkyLjc5IE04Ny44NiAxOTYuNzkgTDIxNS4yMSAxOTYuNzkgTTg3Ljg2IDIwMC43OSBMMjE1LjIxIDIwMC43OSBNODcuODYgMjA0Ljc5IEwyMTUuMjEgMjA0Ljc5IE04Ny44NiAyMDguNzkgTDIxNS4yMSAyMDguNzkgTTg3Ljg2IDIxMi43OSBMMjE1LjIxIDIxMi43OSBNODcuODYgMjE2Ljc5IEwyMTUuMjEgMjE2Ljc5IE04Ny44NiAyMjAuNzkgTDIxNS4yMSAyMjAuNzkgTTg3Ljg2IDIyNC43OSBMMjE1LjIxIDIyNC43OSBNODcuODYgMjI4Ljc5IEwyMTUuMjEgMjI4Ljc5IE04Ny44NiAyMzIuNzkgTDIxNS4yMSAyMzIuNzkgTTg3Ljg2IDIzNi43OSBMMjE1LjIxIDIzNi43OSBNODcuODYgMjQwLjc5IEwyMTUuMjEgMjQwLjc5IE04Ny44NiAyNDQuNzkgTDIxNS4yMSAyNDQuNzkgTTg3Ljg2IDI0OC43OSBMMjE1LjIxIDI0OC43OSBNODcuODYgMjUyLjc5IEwyMTUuMjEgMjUyLjc5IE04Ny44NiAyNTYuNzkgTDIxNS4yMSAyNTYuNzkgTTg3Ljg2IDI2MC43OSBMMjE1LjIxIDI2MC43OSBNODcuODYgMjY0Ljc5IEwyMTUuMjEgMjY0Ljc5IE04Ny44NiAyNjguNzkgTDIxNS4yMSAyNjguNzkgTTg3Ljg2IDI3Mi43OSBMMjE1LjIxIDI3Mi43OSBNODcuODYgMjc2Ljc5IEwyMTUuMjEgMjc2Ljc5IE04Ny44NiAyODAuNzkgTDIxNS4yMSAyODAuNzkgTTg3Ljg2IDI4NC43OSBMMjE1LjIxIDI4NC43OSBNODcuODYgMjg4Ljc5IEwyMTUuMjEgMjg4Ljc5IE04Ny44NiAyOTIuNzkgTDIxNS4yMSAyOTIuNzkgTTg3Ljg2IDI5Ni43OSBMMjE1LjIxIDI5Ni43OSBNODcuODYgMzAwLjc5IEwyMTUuMjEgMzAwLjc5IiBmaWxsPSJub25lIiBzdHJva2U9IiMzMzMzMzMiIHN0cm9rZS13aWR0aD0iMSIgc3Ryb2tlLWxpbmVjYXA9ImJ1dHQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIC8+PC9nPjxnIGRhdGEtdmVsbHVtLWlkPSJsYXllci0xLWJhci1nMiIgcm9sZT0icHJlc2VudGF0aW9uIj48cGF0aCBkPSJNMjI5LjM2IDE4MC41NSBMMzU2LjcgMTgwLjU1IEwzNTYuNyAzMDIuNSBMMjI5LjM2IDMwMi41IFoiIGZpbGw9IiNmZmZmZmYiIGZpbGwtb3BhY2l0eT0iMCIgLz48cGF0aCBkPSJNMzUzLjY5IDE4MC41NSBMMzU2LjcgMTgzLjU2IE0zNDguMDMgMTgwLjU1IEwzNTYuNyAxODkuMjEgTTM0Mi4zNyAxODAuNTUgTDM1Ni43IDE5NC44NyBNMzM2LjcyIDE4MC41NSBMMzU2LjcgMjAwLjUzIE0zMzEuMDYgMTgwLjU1IEwzNTYuNyAyMDYuMTkgTTMyNS40IDE4MC41NSBMMzU2LjcgMjExLjg0IE0zMTkuNzUgMTgwLjU1IEwzNTYuNyAyMTcuNSBNMzE0LjA5IDE4MC41NSBMMzU2LjcgMjIzLjE2IE0zMDguNDMgMTgwLjU1IEwzNTYuNyAyMjguODEgTTMwMi43OCAxODAuNTUgTDM1Ni43IDIzNC40NyBNMjk3LjEyIDE4MC41NSBMMzU2LjcgMjQwLjEzIE0yOTEuNDYgMTgwLjU1IEwzNTYuNyAyNDUuNzggTTI4NS44MSAxODAuNTUgTDM1Ni43IDI1MS40NCBNMjgwLjE1IDE4MC41NSBMMzU2LjcgMjU3LjEgTTI3NC40OSAxODAuNTUgTDM1Ni43IDI2Mi43NSBNMjY4LjgzIDE4MC41NSBMMzU2LjcgMjY4LjQxIE0yNjMuMTggMTgwLjU1IEwzNTYuNyAyNzQuMDcgTTI1Ny41MiAxODAuNTUgTDM1Ni43IDI3OS43MiBNMjUxLjg2IDE4MC41NSBMMzU2LjcgMjg1LjM4IE0yNDYuMjEgMTgwLjU1IEwzNTYuNyAyOTEuMDQgTTI0MC41NSAxODAuNTUgTDM1Ni43IDI5Ni43IE0yMzQuODkgMTgwLjU1IEwzNTYuNyAzMDIuMzUgTTIyOS4zNiAxODAuNjcgTDM1MS4xOSAzMDIuNSBNMjI5LjM2IDE4Ni4zMiBMMzQ1LjU0IDMwMi41IE0yMjkuMzYgMTkxLjk4IEwzMzkuODggMzAyLjUgTTIyOS4zNiAxOTcuNjQgTDMzNC4yMiAzMDIuNSBNMjI5LjM2IDIwMy4yOSBMMzI4LjU2IDMwMi41IE0yMjkuMzYgMjA4Ljk1IEwzMjIuOTEgMzAyLjUgTTIyOS4zNiAyMTQuNjEgTDMxNy4yNSAzMDIuNSBNMjI5LjM2IDIyMC4yNiBMMzExLjU5IDMwMi41IE0yMjkuMzYgMjI1LjkyIEwzMDUuOTQgMzAyLjUgTTIyOS4zNiAyMzEuNTggTDMwMC4yOCAzMDIuNSBNMjI5LjM2IDIzNy4yNCBMMjk0LjYyIDMwMi41IE0yMjkuMzYgMjQyLjg5IEwyODguOTcgMzAyLjUgTTIyOS4zNiAyNDguNTUgTDI4My4zMSAzMDIuNSBNMjI5LjM2IDI1NC4yMSBMMjc3LjY1IDMwMi41IE0yMjkuMzYgMjU5Ljg2IEwyNzIgMzAyLjUgTTIyOS4zNiAyNjUuNTIgTDI2Ni4zNCAzMDIuNSBNMjI5LjM2IDI3MS4xOCBMMjYwLjY4IDMwMi41IE0yMjkuMzYgMjc2LjgzIEwyNTUuMDMgMzAyLjUgTTIyOS4zNiAyODIuNDkgTDI0OS4zNyAzMDIuNSBNMjI5LjM2IDI4OC4xNSBMMjQzLjcxIDMwMi41IE0yMjkuMzYgMjkzLjggTDIzOC4wNiAzMDIuNSBNMjI5LjM2IDI5OS40NiBMMjMyLjQgMzAyLjUiIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzMzMzMzMyIgc3Ryb2tlLXdpZHRoPSIxIiBzdHJva2UtbGluZWNhcD0iYnV0dCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgLz48L2c+PGcgZGF0YS12ZWxsdW0taWQ9ImxheWVyLTEtYmFyLWczIiByb2xlPSJwcmVzZW50YXRpb24iPjxwYXRoIGQ9Ik0zNzAuODUgMzQuMiBMNDk4LjE5IDM0LjIgTDQ5OC4xOSAzMDIuNSBMMzcwLjg1IDMwMi41IFoiIGZpbGw9IiNmZmZmZmYiIGZpbGwtb3BhY2l0eT0iMCIgLz48cGF0aCBkPSJNNDk2Ljc5IDM0LjIgTDQ5Ni43OSAzMDIuNSBNNDkyLjc5IDM0LjIgTDQ5Mi43OSAzMDIuNSBNNDg4Ljc5IDM0LjIgTDQ4OC43OSAzMDIuNSBNNDg0Ljc5IDM0LjIgTDQ4NC43OSAzMDIuNSBNNDgwLjc5IDM0LjIgTDQ4MC43OSAzMDIuNSBNNDc2Ljc5IDM0LjIgTDQ3Ni43OSAzMDIuNSBNNDcyLjc5IDM0LjIgTDQ3Mi43OSAzMDIuNSBNNDY4Ljc5IDM0LjIgTDQ2OC43OSAzMDIuNSBNNDY0Ljc5IDM0LjIgTDQ2NC43OSAzMDIuNSBNNDYwLjc5IDM0LjIgTDQ2MC43OSAzMDIuNSBNNDU2Ljc5IDM0LjIgTDQ1Ni43OSAzMDIuNSBNNDUyLjc5IDM0LjIgTDQ1Mi43OSAzMDIuNSBNNDQ4Ljc5IDM0LjIgTDQ0OC43OSAzMDIuNSBNNDQ0Ljc5IDM0LjIgTDQ0NC43OSAzMDIuNSBNNDQwLjc5IDM0LjIgTDQ0MC43OSAzMDIuNSBNNDM2Ljc5IDM0LjIgTDQzNi43OSAzMDIuNSBNNDMyLjc5IDM0LjIgTDQzMi43OSAzMDIuNSBNNDI4Ljc5IDM0LjIgTDQyOC43OSAzMDIuNSBNNDI0Ljc5IDM0LjIgTDQyNC43OSAzMDIuNSBNNDIwLjc5IDM0LjIgTDQyMC43OSAzMDIuNSBNNDE2Ljc5IDM0LjIgTDQxNi43OSAzMDIuNSBNNDEyLjc5IDM0LjIgTDQxMi43OSAzMDIuNSBNNDA4Ljc5IDM0LjIgTDQwOC43OSAzMDIuNSBNNDA0Ljc5IDM0LjIgTDQwNC43OSAzMDIuNSBNNDAwLjc5IDM0LjIgTDQwMC43OSAzMDIuNSBNMzk2Ljc5IDM0LjIgTDM5Ni43OSAzMDIuNSBNMzkyLjc5IDM0LjIgTDM5Mi43OSAzMDIuNSBNMzg4Ljc5IDM0LjIgTDM4OC43OSAzMDIuNSBNMzg0Ljc5IDM0LjIgTDM4NC43OSAzMDIuNSBNMzgwLjc5IDM0LjIgTDM4MC43OSAzMDIuNSBNMzc2Ljc5IDM0LjIgTDM3Ni43OSAzMDIuNSBNMzcyLjc5IDM0LjIgTDM3Mi43OSAzMDIuNSIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMzMzMzMzIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJidXR0IiBzdHJva2UtbGluZWpvaW49InJvdW5kIiAvPjwvZz48L2c+PC9nPjxnIGRhdGEtdmVsbHVtLXBhbmVsPSJheGlzLXktMSI+PHRleHQgeD0iMzYuMTQyMjA0NzI0NDA5NDQ2IiB5PSIyODEuNzE2NDkwNjk0MzQ1IiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDQzLjE0MTI0IDIwLjc4NzQwMSkiIGZpbGw9IiMzMzMzMzMiIGZpbGwtb3BhY2l0eT0iMSIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIHRleHQtYW5jaG9yPSJlbmQiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj4wLjA8L3RleHQ+PHRleHQgeD0iMzYuMTQyMjA0NzI0NDA5NDQ2IiB5PSIyMjAuNzM4ODk1MzA1OTU4NyIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA0My4xNDEyNCAyMC43ODc0MDEpIiBmaWxsPSIjMzMzMzMzIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiB0ZXh0LWFuY2hvcj0iZW5kIiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+Mi41PC90ZXh0Pjx0ZXh0IHg9IjM2LjE0MjIwNDcyNDQwOTQ0NiIgeT0iMTU5Ljc2MTI5OTkxNzU3MjMiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgNDMuMTQxMjQgMjAuNzg3NDAxKSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9ImVuZCIgZG9taW5hbnQtYmFzZWxpbmU9ImNlbnRyYWwiPjUuMDwvdGV4dD48dGV4dCB4PSIzNi4xNDIyMDQ3MjQ0MDk0NDYiIHk9Ijk4Ljc4MzcwNDUyOTE4NTkxIiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDQzLjE0MTI0IDIwLjc4NzQwMSkiIGZpbGw9IiMzMzMzMzMiIGZpbGwtb3BhY2l0eT0iMSIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIHRleHQtYW5jaG9yPSJlbmQiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj43LjU8L3RleHQ+PHRleHQgeD0iMzYuMTQyMjA0NzI0NDA5NDQ2IiB5PSIzNy44MDYxMDkxNDA3OTk1NSIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA0My4xNDEyNCAyMC43ODc0MDEpIiBmaWxsPSIjMzMzMzMzIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiB0ZXh0LWFuY2hvcj0iZW5kIiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+MTAuMDwvdGV4dD48L2c+PGcgZGF0YS12ZWxsdW0tcGFuZWw9ImF4aXMteC0zIj48dGV4dCB4PSI3MC43NDU1NzA4NjYxNDE3MiIgeT0iNC40ODkxNjMzODU4MjY3NjciIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgODAuNzg5MzcgMzE1LjkxODk4KSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZG9taW5hbnQtYmFzZWxpbmU9InRleHQtYmVmb3JlLWVkZ2UiPkE8L3RleHQ+PHRleHQgeD0iMjEyLjIzNjcxMjU5ODQyNTE5IiB5PSI0LjQ4OTE2MzM4NTgyNjc2NyIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA4MC43ODkzNyAzMTUuOTE4OTgpIiBmaWxsPSIjMzMzMzMzIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBkb21pbmFudC1iYXNlbGluZT0idGV4dC1iZWZvcmUtZWRnZSI+QjwvdGV4dD48dGV4dCB4PSIzNTMuNzI3ODU0MzMwNzA4NyIgeT0iNC40ODkxNjMzODU4MjY3NjciIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgODAuNzg5MzcgMzE1LjkxODk4KSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZG9taW5hbnQtYmFzZWxpbmU9InRleHQtYmVmb3JlLWVkZ2UiPkM8L3RleHQ+PC9nPjxnIGRhdGEtdmVsbHVtLXBhbmVsPSJheGlzLXRpdGxlLXkiPjx0ZXh0IHg9IjExLjE3NjkxOTI5MTMzODU4MiIgeT0iMTQ3LjU2NTc4MDgzOTg5NSIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSAyMC43ODc0MDEgMjAuNzg3NDAxKSByb3RhdGUoLTkwIDExLjE3NjkxOTI5MTMzODU4MiAxNDcuNTY1NzgwODM5ODk1KSIgZmlsbD0iIzAwMDAwMCIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxNC42NjY2NjY2NjY2NjY2NjYiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj5uPC90ZXh0PjwvZz48ZyBkYXRhLXZlbGx1bS1wYW5lbD0iYXhpcy10aXRsZS14Ij48dGV4dCB4PSIyMTIuMjM2NzEyNTk4NDI1MTkiIHk9IjExLjE3NjkxOTI5MTMzODU3OCIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA4MC43ODkzNyAzNDAuODU4NzYpIiBmaWxsPSIjMDAwMDAwIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjE0LjY2NjY2NjY2NjY2NjY2NiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZG9taW5hbnQtYmFzZWxpbmU9ImNlbnRyYWwiPmdycDwvdGV4dD48L2c+PGcgZGF0YS12ZWxsdW0tcGFuZWw9ImxlZ2VuZCI+PHRleHQgeD0iMCIgeT0iMTIuMDMzNzU5ODQyNTE5Njg0IiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDUxMi44MjE5IDEwNC4xMDEyMikiIGZpbGw9IiMwMDAwMDAiIGZpbGwtb3BhY2l0eT0iMSIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTMuMzMzMzMzMzMzMzMzMzM0IiB0ZXh0LWFuY2hvcj0ic3RhcnQiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj5ncnA8L3RleHQ+PHBhdGggZD0iTTUxOC40OSAxMjYuOCBMNTMzLjYxIDEyNi44IEw1MzMuNjEgMTQxLjkyIEw1MTguNDkgMTQxLjkyIFoiIGZpbGw9IiMyZjQ4NTgiIC8+PHRleHQgeD0iMCIgeT0iMTAuMjA0NzI0NDA5NDQ4ODIiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgNTM5LjI3ODU2IDEyNC4xNTc0ODYpIiBmaWxsPSIjMzMzMzMzIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiB0ZXh0LWFuY2hvcj0ic3RhcnQiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj5BPC90ZXh0PjxwYXRoIGQ9Ik01MTguNDkgMTQ3LjIxIEw1MzMuNjEgMTQ3LjIxIEw1MzMuNjEgMTYyLjMzIEw1MTguNDkgMTYyLjMzIFoiIGZpbGw9IiMwMDcyYjIiIC8+PHRleHQgeD0iMCIgeT0iMTAuMjA0NzI0NDA5NDQ4ODE5IiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDUzOS4yNzg1NiAxNDQuNTY2OTMpIiBmaWxsPSIjMzMzMzMzIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiB0ZXh0LWFuY2hvcj0ic3RhcnQiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj5CPC90ZXh0PjxwYXRoIGQ9Ik01MTguNDkgMTY3LjYyIEw1MzMuNjEgMTY3LjYyIEw1MzMuNjEgMTgyLjc0IEw1MTguNDkgMTgyLjc0IFoiIGZpbGw9IiMwMDllNzMiIC8+PHRleHQgeD0iMCIgeT0iMTAuMjA0NzI0NDA5NDQ4ODIyIiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDUzOS4yNzg1NiAxNjQuOTc2MzgpIiBmaWxsPSIjMzMzMzMzIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiB0ZXh0LWFuY2hvcj0ic3RhcnQiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj5DPC90ZXh0Pjx0ZXh0IHg9IjAiIHk9IjEyLjAzMzc1OTg0MjUxOTY4NCIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA1MTIuODIxOSAxOTguNjE0MTgpIiBmaWxsPSIjMDAwMDAwIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEzLjMzMzMzMzMzMzMzMzMzNCIgdGV4dC1hbmNob3I9InN0YXJ0IiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+Z3JwPC90ZXh0PjxwYXRoIGQ9Ik01MTQuNjcgMjIwLjEgTDUzNy40MyAyMjAuMSBMNTM3LjQzIDIzNy42NSBMNTE0LjY3IDIzNy42NSBaIiBmaWxsPSIjZmZmZmZmIiBmaWxsLW9wYWNpdHk9IjAiIC8+PHBhdGggZD0iTTUxNC42NyAyMjIuNjcgTDUzNy40MyAyMjIuNjcgTTUxNC42NyAyMjYuNjcgTDUzNy40MyAyMjYuNjcgTTUxNC42NyAyMzAuNjcgTDUzNy40MyAyMzAuNjcgTTUxNC42NyAyMzQuNjcgTDUzNy40MyAyMzQuNjciIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzMzMzMzMyIgc3Ryb2tlLXdpZHRoPSIxIiBzdHJva2UtbGluZWNhcD0iYnV0dCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgLz48cGF0aCBkPSJNNTE0LjY3IDIyMC4xIEw1MzcuNDMgMjIwLjEgTDUzNy40MyAyMzcuNjUgTDUxNC42NyAyMzcuNjUgWiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjOGM4YzhjIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgLz48dGV4dCB4PSIwIiB5PSIxMC4yMDQ3MjQ0MDk0NDg4MiIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA1MzkuMjc4NTYgMjE4LjY3MDQ0KSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9InN0YXJ0IiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+QTwvdGV4dD48cGF0aCBkPSJNNTE0LjY3IDI0MC41MSBMNTM3LjQzIDI0MC41MSBMNTM3LjQzIDI1OC4wNiBMNTE0LjY3IDI1OC4wNiBaIiBmaWxsPSIjZmZmZmZmIiBmaWxsLW9wYWNpdHk9IjAiIC8+PHBhdGggZD0iTTUzNi44OCAyNDAuNTEgTDUzNy40MyAyNDEuMDYgTTUzMS4yMiAyNDAuNTEgTDUzNy40MyAyNDYuNzEgTTUyNS41NiAyNDAuNTEgTDUzNy40MyAyNTIuMzcgTTUxOS45MSAyNDAuNTEgTDUzNy40MyAyNTguMDMgTTUxNC42NyAyNDAuOTMgTDUzMS44IDI1OC4wNiBNNTE0LjY3IDI0Ni41OSBMNTI2LjE1IDI1OC4wNiBNNTE0LjY3IDI1Mi4yNSBMNTIwLjQ5IDI1OC4wNiBNNTE0LjY3IDI1Ny45IEw1MTQuODMgMjU4LjA2IiBmaWxsPSJub25lIiBzdHJva2U9IiMzMzMzMzMiIHN0cm9rZS13aWR0aD0iMSIgc3Ryb2tlLWxpbmVjYXA9ImJ1dHQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIC8+PHBhdGggZD0iTTUxNC42NyAyNDAuNTEgTDUzNy40MyAyNDAuNTEgTDUzNy40MyAyNTguMDYgTDUxNC42NyAyNTguMDYgWiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjOGM4YzhjIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgLz48dGV4dCB4PSIwIiB5PSIxMC4yMDQ3MjQ0MDk0NDg4MTkiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgNTM5LjI3ODU2IDIzOS4wNzk5KSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9InN0YXJ0IiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+QjwvdGV4dD48cGF0aCBkPSJNNTE0LjY3IDI2MC45MiBMNTM3LjQzIDI2MC45MiBMNTM3LjQzIDI3OC40NyBMNTE0LjY3IDI3OC40NyBaIiBmaWxsPSIjZmZmZmZmIiBmaWxsLW9wYWNpdHk9IjAiIC8+PHBhdGggZD0iTTUzNi44MiAyNjAuOTIgTDUzNi44MiAyNzguNDcgTTUzMi44MiAyNjAuOTIgTDUzMi44MiAyNzguNDcgTTUyOC44MiAyNjAuOTIgTDUyOC44MiAyNzguNDcgTTUyNC44MiAyNjAuOTIgTDUyNC44MiAyNzguNDcgTTUyMC44MiAyNjAuOTIgTDUyMC44MiAyNzguNDcgTTUxNi44MiAyNjAuOTIgTDUxNi44MiAyNzguNDciIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzMzMzMzMyIgc3Ryb2tlLXdpZHRoPSIxIiBzdHJva2UtbGluZWNhcD0iYnV0dCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgLz48cGF0aCBkPSJNNTE0LjY3IDI2MC45MiBMNTM3LjQzIDI2MC45MiBMNTM3LjQzIDI3OC40NyBMNTE0LjY3IDI3OC40NyBaIiBmaWxsPSJub25lIiBzdHJva2U9IiM4YzhjOGMiIHN0cm9rZS13aWR0aD0iMSIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIiBzdHJva2UtbGluZWpvaW49InJvdW5kIiAvPjx0ZXh0IHg9IjAiIHk9IjEwLjIwNDcyNDQwOTQ0ODgyMiIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA1MzkuMjc4NTYgMjU5LjQ4OTM1KSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9InN0YXJ0IiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+QzwvdGV4dD48L2c+PC9nPjwvZz48L3N2Zz4=)

Now each bar is distinguished by hatch *and* colour: the plot survives
the CVD preview above, and
[`plot_lint()`](https://r-vellum.github.io/vellumplot/reference/plot_lint.md)’s
contrast rule has less to complain about.

## The interactive widget

A static image with alt text is enough for a picture, but an
*interactive* chart must also be operable and explorable without a
mouse. `vellumwidget::as_widget()` (accessibility on by default,
`a11y = TRUE`) makes the widget a first-class interactive chart:

``` r

library(vellumwidget)

vplot(mtcars) |>
  mark_point(x = wt, y = mpg, tooltip = rownames(mtcars), data_id = rownames(mtcars)) |>
  labs(title = "Fuel economy", alt = "Fuel economy against weight for 32 cars.") |>
  as_widget()
```

That widget:

- announces itself as an **interactive chart**
  (`role="graphics-document"`), labelled from the title/description
  vellumplot already emitted (or an explicit `as_widget(alt =)`);
- makes **every mark focusable**. Tab into the chart, then use the
  **arrow keys** to move between marks; each is a `graphics-symbol`
  labelled with its tooltip. **Enter** or **Space** selects the focused
  mark; **Escape** leaves traversal mode;
- announces the focused and selected mark through a polite
  **`aria-live`** region, so a screen-reader user hears what they land
  on;
- ships a visually-hidden **data table** listing every mark, so the
  underlying data is available even without traversing the chart.

Setting `a11y = FALSE` restores the previous behaviour exactly, but
there is rarely a reason to.

## Authoring checklist

- **Give plots a `title`.** It becomes the accessible name.
- **Write a real `alt`** when the automatic one misses the point: say
  what the chart *shows*, not only what it plots. Inspect it with
  [`plot_alt()`](https://r-vellum.github.io/vellumplot/reference/plot_alt.md).
- **Set `tooltip` / `data_id`** on interactive marks: the tooltip is
  what a screen-reader user hears when focusing a mark, and appears in
  the data table.
- **Don’t rely on colour alone.** Pair a colour encoding with a
  redundant non-colour one — `shape`, or a `pattern` mapped through
  [`scale_pattern()`](https://r-vellum.github.io/vellumplot/reference/scale_pattern.md)
  with
  [`pattern_hatch()`](https://r-vellum.github.io/vellumplot/reference/pattern_hatch.md)
  — or label the extremes, so the chart is legible without colour
  perception. Preview it with `render_plot(cvd = )`.
- **Lint before you ship.** Run
  [`plot_lint()`](https://r-vellum.github.io/vellumplot/reference/plot_lint.md)
  to catch tiny text, low contrast, overlaps and dead encodings while
  they are still cheap to fix.

## See also

- vellum’s *The scene contract* vignette: the SVG/PDF a11y attributes
  and the additivity invariant, from the emitter’s side.
- vellumwidget’s interactivity article: the widget’s full keyboard and
  screen-reader model.
