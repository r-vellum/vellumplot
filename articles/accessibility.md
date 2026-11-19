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
#> [1] "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"576\" height=\"384\" viewBox=\"0 0 576 384\" role=\"img\" aria-labelledby=\"vl2-t vl2-d\"><title id=\"vl2-t\">Heavier cars use more fuel</title><desc id=\"vl2-d\">Scatter plot of fuel economy against weight for 32 cars. Fuel economy falls roughly linearly as weight rises, from about 34 mpg at 1.5 tons to about 10 mpg at 5 tons.</desc>"
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

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAlIiBoZWlnaHQ9IjEwMCUiIHZpZXdib3g9IjAgMCA1NzYgMzg0IiByb2xlPSJpbWciIGFyaWEtbGFiZWxsZWRieT0idmw0NDktZCI+PGRlc2MgaWQ9InZsNDQ5LWQiPkEgYmFyIGNoYXJ0LiBJdCBwbG90cyBuICh2ZXJ0aWNhbCBheGlzKSBhZ2FpbnN0IGdycCAoaG9yaXpvbnRhbCBheGlzKSwgd2hlcmUgY29sb3VyIHNob3dzIGdycC4gQmFzZWQgb24gMyBvYnNlcnZhdGlvbnMuPC9kZXNjPjxkZWZzPjxjbGlwcGF0aCBpZD0iYzAiIGNsaXBwYXRodW5pdHM9InVzZXJTcGFjZU9uVXNlIj48cGF0aCBkPSJNODAuNzg5MzY3Njc1NzgxMjUgMjAuNzg3NDAxMTk5MzQwODIgTDUwNS4yNjI3OTI4NzI2MzE2IDIwLjc4NzQwMTE5OTM0MDgyIEw1MDUuMjYyNzkyODcyNjMxNiAzMTUuOTE4OTYyODc5MTMwODUgTDgwLjc4OTM2NzY3NTc4MTI1IDMxNS45MTg5NjI4NzkxMzA4NSBaIiAvPjwvY2xpcHBhdGg+PC9kZWZzPjxyZWN0IHdpZHRoPSI1NzYiIGhlaWdodD0iMzg0IiBmaWxsPSIjZmZmZmZmIiBmaWxsLW9wYWNpdHk9IjEiIC8+PGcgZGF0YS12ZWxsdW0tcGFuZWw9InBsb3QiPjxnIGRhdGEtdmVsbHVtLXBhbmVsPSJwbG90LWJhY2tncm91bmQiPjxnIHJvbGU9InByZXNlbnRhdGlvbiI+PHBhdGggZD0iTTAgMCBMNTc2IDAgTDU3NiAzODQgTDAgMzg0IFoiIGZpbGw9IiNmZmZmZmYiIC8+PC9nPjwvZz48ZyBkYXRhLXZlbGx1bS1wYW5lbD0icGFuZWwtYXJlYSI+PGcgZGF0YS12ZWxsdW0tcGFuZWw9InBhbmVsLTEtMSIgY2xpcC1wYXRoPSJ1cmwoI2MwKSI+PGcgZGF0YS12ZWxsdW0tcGFuPSJwYW5lbC0xLTEiPjxwYXRoIGQ9Ik04MC43OSAyMC43OSBMNTA1LjI2IDIwLjc5IEw1MDUuMjYgMzE1LjkyIEw4MC43OSAzMTUuOTIgWiIgZmlsbD0iI2ViZWJlYiIgLz48ZyByb2xlPSJncmlkIj48cGF0aCBkPSJNODAuNzkgMzMyLjk5IEw1MDUuMjYgMzMyLjk5IE04MC43OSAyNzIuMDIgTDUwNS4yNiAyNzIuMDIgTTgwLjc5IDIxMS4wNCBMNTA1LjI2IDIxMS4wNCBNODAuNzkgMTUwLjA2IEw1MDUuMjYgMTUwLjA2IE04MC43OSA4OS4wOCBMNTA1LjI2IDg5LjA4IE04MC43OSAyOC4xIEw1MDUuMjYgMjguMSIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjZmZmZmZmIiBzdHJva2Utd2lkdGg9IjAuNSIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIiBzdHJva2UtbGluZWpvaW49InJvdW5kIiAvPjwvZz48ZyByb2xlPSJncmlkIj48cGF0aCBkPSJNMTUxLjUzIDMxNS45MiBMMTUxLjUzIDIwLjc5IE0yOTMuMDMgMzE1LjkyIEwyOTMuMDMgMjAuNzkgTTQzNC41MiAzMTUuOTIgTDQzNC41MiAyMC43OSIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjZmZmZmZmIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgLz48L2c+PGcgcm9sZT0iZ3JpZCI+PHBhdGggZD0iTTgwLjc5IDMwMi41IEw1MDUuMjYgMzAyLjUgTTgwLjc5IDI0MS41MyBMNTA1LjI2IDI0MS41MyBNODAuNzkgMTgwLjU1IEw1MDUuMjYgMTgwLjU1IE04MC43OSAxMTkuNTcgTDUwNS4yNiAxMTkuNTcgTTgwLjc5IDU4LjU5IEw1MDUuMjYgNTguNTkiIGZpbGw9Im5vbmUiIHN0cm9rZT0iI2ZmZmZmZiIgc3Ryb2tlLXdpZHRoPSIxIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIC8+PC9nPjxnIHJvbGU9InByZXNlbnRhdGlvbiI+PHBhdGggZD0iTTE1MS41MyAzMTUuOTIgTDE1MS41MyAzMTAuMjUgTTI5My4wMyAzMTUuOTIgTDI5My4wMyAzMTAuMjUgTTQzNC41MiAzMTUuOTIgTDQzNC41MiAzMTAuMjUiIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzMzMzMzMyIgc3Ryb2tlLXdpZHRoPSIxIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIC8+PC9nPjxnIHJvbGU9InByZXNlbnRhdGlvbiI+PHBhdGggZD0iTTgwLjc5IDMwMi41IEw4Ni40NiAzMDIuNSBNODAuNzkgMjQxLjUzIEw4Ni40NiAyNDEuNTMgTTgwLjc5IDE4MC41NSBMODYuNDYgMTgwLjU1IE04MC43OSAxMTkuNTcgTDg2LjQ2IDExOS41NyBNODAuNzkgNTguNTkgTDg2LjQ2IDU4LjU5IiBmaWxsPSJub25lIiBzdHJva2U9IiMzMzMzMzMiIHN0cm9rZS13aWR0aD0iMSIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIiBzdHJva2UtbGluZWpvaW49InJvdW5kIiAvPjwvZz48ZyBkYXRhLXZlbGx1bS1pZD0ibGF5ZXItMS1iYXItZzEiIHJvbGU9InByZXNlbnRhdGlvbiI+PHBhdGggZD0iTTg3Ljg2IDEwNy4zOCBMMjE1LjIxIDEwNy4zOCBMMjE1LjIxIDMwMi41IEw4Ny44NiAzMDIuNSBaIiBmaWxsPSIjZmZmZmZmIiBmaWxsLW9wYWNpdHk9IjAiIC8+PHBhdGggZD0iTTg3Ljg2IDEwOC43OSBMMjE1LjIxIDEwOC43OSBNODcuODYgMTEyLjc5IEwyMTUuMjEgMTEyLjc5IE04Ny44NiAxMTYuNzkgTDIxNS4yMSAxMTYuNzkgTTg3Ljg2IDEyMC43OSBMMjE1LjIxIDEyMC43OSBNODcuODYgMTI0Ljc5IEwyMTUuMjEgMTI0Ljc5IE04Ny44NiAxMjguNzkgTDIxNS4yMSAxMjguNzkgTTg3Ljg2IDEzMi43OSBMMjE1LjIxIDEzMi43OSBNODcuODYgMTM2Ljc5IEwyMTUuMjEgMTM2Ljc5IE04Ny44NiAxNDAuNzkgTDIxNS4yMSAxNDAuNzkgTTg3Ljg2IDE0NC43OSBMMjE1LjIxIDE0NC43OSBNODcuODYgMTQ4Ljc5IEwyMTUuMjEgMTQ4Ljc5IE04Ny44NiAxNTIuNzkgTDIxNS4yMSAxNTIuNzkgTTg3Ljg2IDE1Ni43OSBMMjE1LjIxIDE1Ni43OSBNODcuODYgMTYwLjc5IEwyMTUuMjEgMTYwLjc5IE04Ny44NiAxNjQuNzkgTDIxNS4yMSAxNjQuNzkgTTg3Ljg2IDE2OC43OSBMMjE1LjIxIDE2OC43OSBNODcuODYgMTcyLjc5IEwyMTUuMjEgMTcyLjc5IE04Ny44NiAxNzYuNzkgTDIxNS4yMSAxNzYuNzkgTTg3Ljg2IDE4MC43OSBMMjE1LjIxIDE4MC43OSBNODcuODYgMTg0Ljc5IEwyMTUuMjEgMTg0Ljc5IE04Ny44NiAxODguNzkgTDIxNS4yMSAxODguNzkgTTg3Ljg2IDE5Mi43OSBMMjE1LjIxIDE5Mi43OSBNODcuODYgMTk2Ljc5IEwyMTUuMjEgMTk2Ljc5IE04Ny44NiAyMDAuNzkgTDIxNS4yMSAyMDAuNzkgTTg3Ljg2IDIwNC43OSBMMjE1LjIxIDIwNC43OSBNODcuODYgMjA4Ljc5IEwyMTUuMjEgMjA4Ljc5IE04Ny44NiAyMTIuNzkgTDIxNS4yMSAyMTIuNzkgTTg3Ljg2IDIxNi43OSBMMjE1LjIxIDIxNi43OSBNODcuODYgMjIwLjc5IEwyMTUuMjEgMjIwLjc5IE04Ny44NiAyMjQuNzkgTDIxNS4yMSAyMjQuNzkgTTg3Ljg2IDIyOC43OSBMMjE1LjIxIDIyOC43OSBNODcuODYgMjMyLjc5IEwyMTUuMjEgMjMyLjc5IE04Ny44NiAyMzYuNzkgTDIxNS4yMSAyMzYuNzkgTTg3Ljg2IDI0MC43OSBMMjE1LjIxIDI0MC43OSBNODcuODYgMjQ0Ljc5IEwyMTUuMjEgMjQ0Ljc5IE04Ny44NiAyNDguNzkgTDIxNS4yMSAyNDguNzkgTTg3Ljg2IDI1Mi43OSBMMjE1LjIxIDI1Mi43OSBNODcuODYgMjU2Ljc5IEwyMTUuMjEgMjU2Ljc5IE04Ny44NiAyNjAuNzkgTDIxNS4yMSAyNjAuNzkgTTg3Ljg2IDI2NC43OSBMMjE1LjIxIDI2NC43OSBNODcuODYgMjY4Ljc5IEwyMTUuMjEgMjY4Ljc5IE04Ny44NiAyNzIuNzkgTDIxNS4yMSAyNzIuNzkgTTg3Ljg2IDI3Ni43OSBMMjE1LjIxIDI3Ni43OSBNODcuODYgMjgwLjc5IEwyMTUuMjEgMjgwLjc5IE04Ny44NiAyODQuNzkgTDIxNS4yMSAyODQuNzkgTTg3Ljg2IDI4OC43OSBMMjE1LjIxIDI4OC43OSBNODcuODYgMjkyLjc5IEwyMTUuMjEgMjkyLjc5IE04Ny44NiAyOTYuNzkgTDIxNS4yMSAyOTYuNzkgTTg3Ljg2IDMwMC43OSBMMjE1LjIxIDMwMC43OSIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMzMzMzMzIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJidXR0IiBzdHJva2UtbGluZWpvaW49InJvdW5kIiAvPjwvZz48ZyBkYXRhLXZlbGx1bS1pZD0ibGF5ZXItMS1iYXItZzIiIHJvbGU9InByZXNlbnRhdGlvbiI+PHBhdGggZD0iTTIyOS4zNiAxODAuNTUgTDM1Ni43IDE4MC41NSBMMzU2LjcgMzAyLjUgTDIyOS4zNiAzMDIuNSBaIiBmaWxsPSIjZmZmZmZmIiBmaWxsLW9wYWNpdHk9IjAiIC8+PHBhdGggZD0iTTM1My42OSAxODAuNTUgTDM1Ni43IDE4My41NiBNMzQ4LjAzIDE4MC41NSBMMzU2LjcgMTg5LjIxIE0zNDIuMzcgMTgwLjU1IEwzNTYuNyAxOTQuODcgTTMzNi43MiAxODAuNTUgTDM1Ni43IDIwMC41MyBNMzMxLjA2IDE4MC41NSBMMzU2LjcgMjA2LjE5IE0zMjUuNCAxODAuNTUgTDM1Ni43IDIxMS44NCBNMzE5Ljc1IDE4MC41NSBMMzU2LjcgMjE3LjUgTTMxNC4wOSAxODAuNTUgTDM1Ni43IDIyMy4xNiBNMzA4LjQzIDE4MC41NSBMMzU2LjcgMjI4LjgxIE0zMDIuNzggMTgwLjU1IEwzNTYuNyAyMzQuNDcgTTI5Ny4xMiAxODAuNTUgTDM1Ni43IDI0MC4xMyBNMjkxLjQ2IDE4MC41NSBMMzU2LjcgMjQ1Ljc4IE0yODUuODEgMTgwLjU1IEwzNTYuNyAyNTEuNDQgTTI4MC4xNSAxODAuNTUgTDM1Ni43IDI1Ny4xIE0yNzQuNDkgMTgwLjU1IEwzNTYuNyAyNjIuNzUgTTI2OC44MyAxODAuNTUgTDM1Ni43IDI2OC40MSBNMjYzLjE4IDE4MC41NSBMMzU2LjcgMjc0LjA3IE0yNTcuNTIgMTgwLjU1IEwzNTYuNyAyNzkuNzIgTTI1MS44NiAxODAuNTUgTDM1Ni43IDI4NS4zOCBNMjQ2LjIxIDE4MC41NSBMMzU2LjcgMjkxLjA0IE0yNDAuNTUgMTgwLjU1IEwzNTYuNyAyOTYuNyBNMjM0Ljg5IDE4MC41NSBMMzU2LjcgMzAyLjM1IE0yMjkuMzYgMTgwLjY3IEwzNTEuMTkgMzAyLjUgTTIyOS4zNiAxODYuMzIgTDM0NS41NCAzMDIuNSBNMjI5LjM2IDE5MS45OCBMMzM5Ljg4IDMwMi41IE0yMjkuMzYgMTk3LjY0IEwzMzQuMjIgMzAyLjUgTTIyOS4zNiAyMDMuMjkgTDMyOC41NiAzMDIuNSBNMjI5LjM2IDIwOC45NSBMMzIyLjkxIDMwMi41IE0yMjkuMzYgMjE0LjYxIEwzMTcuMjUgMzAyLjUgTTIyOS4zNiAyMjAuMjYgTDMxMS41OSAzMDIuNSBNMjI5LjM2IDIyNS45MiBMMzA1Ljk0IDMwMi41IE0yMjkuMzYgMjMxLjU4IEwzMDAuMjggMzAyLjUgTTIyOS4zNiAyMzcuMjQgTDI5NC42MiAzMDIuNSBNMjI5LjM2IDI0Mi44OSBMMjg4Ljk3IDMwMi41IE0yMjkuMzYgMjQ4LjU1IEwyODMuMzEgMzAyLjUgTTIyOS4zNiAyNTQuMjEgTDI3Ny42NSAzMDIuNSBNMjI5LjM2IDI1OS44NiBMMjcyIDMwMi41IE0yMjkuMzYgMjY1LjUyIEwyNjYuMzQgMzAyLjUgTTIyOS4zNiAyNzEuMTggTDI2MC42OCAzMDIuNSBNMjI5LjM2IDI3Ni44MyBMMjU1LjAzIDMwMi41IE0yMjkuMzYgMjgyLjQ5IEwyNDkuMzcgMzAyLjUgTTIyOS4zNiAyODguMTUgTDI0My43MSAzMDIuNSBNMjI5LjM2IDI5My44IEwyMzguMDYgMzAyLjUgTTIyOS4zNiAyOTkuNDYgTDIzMi40IDMwMi41IiBmaWxsPSJub25lIiBzdHJva2U9IiMzMzMzMzMiIHN0cm9rZS13aWR0aD0iMSIgc3Ryb2tlLWxpbmVjYXA9ImJ1dHQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIC8+PC9nPjxnIGRhdGEtdmVsbHVtLWlkPSJsYXllci0xLWJhci1nMyIgcm9sZT0icHJlc2VudGF0aW9uIj48cGF0aCBkPSJNMzcwLjg1IDM0LjIgTDQ5OC4xOSAzNC4yIEw0OTguMTkgMzAyLjUgTDM3MC44NSAzMDIuNSBaIiBmaWxsPSIjZmZmZmZmIiBmaWxsLW9wYWNpdHk9IjAiIC8+PHBhdGggZD0iTTQ5Ni43OSAzNC4yIEw0OTYuNzkgMzAyLjUgTTQ5Mi43OSAzNC4yIEw0OTIuNzkgMzAyLjUgTTQ4OC43OSAzNC4yIEw0ODguNzkgMzAyLjUgTTQ4NC43OSAzNC4yIEw0ODQuNzkgMzAyLjUgTTQ4MC43OSAzNC4yIEw0ODAuNzkgMzAyLjUgTTQ3Ni43OSAzNC4yIEw0NzYuNzkgMzAyLjUgTTQ3Mi43OSAzNC4yIEw0NzIuNzkgMzAyLjUgTTQ2OC43OSAzNC4yIEw0NjguNzkgMzAyLjUgTTQ2NC43OSAzNC4yIEw0NjQuNzkgMzAyLjUgTTQ2MC43OSAzNC4yIEw0NjAuNzkgMzAyLjUgTTQ1Ni43OSAzNC4yIEw0NTYuNzkgMzAyLjUgTTQ1Mi43OSAzNC4yIEw0NTIuNzkgMzAyLjUgTTQ0OC43OSAzNC4yIEw0NDguNzkgMzAyLjUgTTQ0NC43OSAzNC4yIEw0NDQuNzkgMzAyLjUgTTQ0MC43OSAzNC4yIEw0NDAuNzkgMzAyLjUgTTQzNi43OSAzNC4yIEw0MzYuNzkgMzAyLjUgTTQzMi43OSAzNC4yIEw0MzIuNzkgMzAyLjUgTTQyOC43OSAzNC4yIEw0MjguNzkgMzAyLjUgTTQyNC43OSAzNC4yIEw0MjQuNzkgMzAyLjUgTTQyMC43OSAzNC4yIEw0MjAuNzkgMzAyLjUgTTQxNi43OSAzNC4yIEw0MTYuNzkgMzAyLjUgTTQxMi43OSAzNC4yIEw0MTIuNzkgMzAyLjUgTTQwOC43OSAzNC4yIEw0MDguNzkgMzAyLjUgTTQwNC43OSAzNC4yIEw0MDQuNzkgMzAyLjUgTTQwMC43OSAzNC4yIEw0MDAuNzkgMzAyLjUgTTM5Ni43OSAzNC4yIEwzOTYuNzkgMzAyLjUgTTM5Mi43OSAzNC4yIEwzOTIuNzkgMzAyLjUgTTM4OC43OSAzNC4yIEwzODguNzkgMzAyLjUgTTM4NC43OSAzNC4yIEwzODQuNzkgMzAyLjUgTTM4MC43OSAzNC4yIEwzODAuNzkgMzAyLjUgTTM3Ni43OSAzNC4yIEwzNzYuNzkgMzAyLjUgTTM3Mi43OSAzNC4yIEwzNzIuNzkgMzAyLjUiIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzMzMzMzMyIgc3Ryb2tlLXdpZHRoPSIxIiBzdHJva2UtbGluZWNhcD0iYnV0dCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgLz48L2c+PC9nPjwvZz48ZyBkYXRhLXZlbGx1bS1wYW5lbD0iYXhpcy15LTEiPjx0ZXh0IHg9IjM2LjE0MjIwNDcyNDQwOTQ0NiIgeT0iMjgxLjcxNjQ5MDY5NDM0NSIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA0My4xNDEyNCAyMC43ODc0MDEpIiBmaWxsPSIjMzMzMzMzIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiB0ZXh0LWFuY2hvcj0iZW5kIiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+MC4wPC90ZXh0Pjx0ZXh0IHg9IjM2LjE0MjIwNDcyNDQwOTQ0NiIgeT0iMjIwLjczODg5NTMwNTk1ODciIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgNDMuMTQxMjQgMjAuNzg3NDAxKSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9ImVuZCIgZG9taW5hbnQtYmFzZWxpbmU9ImNlbnRyYWwiPjIuNTwvdGV4dD48dGV4dCB4PSIzNi4xNDIyMDQ3MjQ0MDk0NDYiIHk9IjE1OS43NjEyOTk5MTc1NzIzIiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDQzLjE0MTI0IDIwLjc4NzQwMSkiIGZpbGw9IiMzMzMzMzMiIGZpbGwtb3BhY2l0eT0iMSIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIHRleHQtYW5jaG9yPSJlbmQiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj41LjA8L3RleHQ+PHRleHQgeD0iMzYuMTQyMjA0NzI0NDA5NDQ2IiB5PSI5OC43ODM3MDQ1MjkxODU5MSIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA0My4xNDEyNCAyMC43ODc0MDEpIiBmaWxsPSIjMzMzMzMzIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiB0ZXh0LWFuY2hvcj0iZW5kIiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+Ny41PC90ZXh0Pjx0ZXh0IHg9IjM2LjE0MjIwNDcyNDQwOTQ0NiIgeT0iMzcuODA2MTA5MTQwNzk5NTUiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgNDMuMTQxMjQgMjAuNzg3NDAxKSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9ImVuZCIgZG9taW5hbnQtYmFzZWxpbmU9ImNlbnRyYWwiPjEwLjA8L3RleHQ+PC9nPjxnIGRhdGEtdmVsbHVtLXBhbmVsPSJheGlzLXgtMyI+PHRleHQgeD0iNzAuNzQ1NTcwODY2MTQxNzIiIHk9IjQuNDg5MTYzMzg1ODI2NzY3IiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDgwLjc4OTM3IDMxNS45MTg5OCkiIGZpbGw9IiMzMzMzMzMiIGZpbGwtb3BhY2l0eT0iMSIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGRvbWluYW50LWJhc2VsaW5lPSJ0ZXh0LWJlZm9yZS1lZGdlIj5BPC90ZXh0Pjx0ZXh0IHg9IjIxMi4yMzY3MTI1OTg0MjUxOSIgeT0iNC40ODkxNjMzODU4MjY3NjciIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgODAuNzg5MzcgMzE1LjkxODk4KSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZG9taW5hbnQtYmFzZWxpbmU9InRleHQtYmVmb3JlLWVkZ2UiPkI8L3RleHQ+PHRleHQgeD0iMzUzLjcyNzg1NDMzMDcwODciIHk9IjQuNDg5MTYzMzg1ODI2NzY3IiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDgwLjc4OTM3IDMxNS45MTg5OCkiIGZpbGw9IiMzMzMzMzMiIGZpbGwtb3BhY2l0eT0iMSIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGRvbWluYW50LWJhc2VsaW5lPSJ0ZXh0LWJlZm9yZS1lZGdlIj5DPC90ZXh0PjwvZz48ZyBkYXRhLXZlbGx1bS1wYW5lbD0iYXhpcy10aXRsZS15Ij48dGV4dCB4PSIxMS4xNzY5MTkyOTEzMzg1ODIiIHk9IjE0Ny41NjU3ODA4Mzk4OTUiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgMjAuNzg3NDAxIDIwLjc4NzQwMSkgcm90YXRlKC05MCAxMS4xNzY5MTkyOTEzMzg1ODIgMTQ3LjU2NTc4MDgzOTg5NSkiIGZpbGw9IiMwMDAwMDAiIGZpbGwtb3BhY2l0eT0iMSIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTQuNjY2NjY2NjY2NjY2NjY2IiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+bjwvdGV4dD48L2c+PGcgZGF0YS12ZWxsdW0tcGFuZWw9ImF4aXMtdGl0bGUteCI+PHRleHQgeD0iMjEyLjIzNjcxMjU5ODQyNTE5IiB5PSIxMS4xNzY5MTkyOTEzMzg1NzgiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgODAuNzg5MzcgMzQwLjg1ODc2KSIgZmlsbD0iIzAwMDAwMCIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxNC42NjY2NjY2NjY2NjY2NjYiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj5ncnA8L3RleHQ+PC9nPjxnIGRhdGEtdmVsbHVtLXBhbmVsPSJsZWdlbmQiPjx0ZXh0IHg9IjAiIHk9IjEyLjAzMzc1OTg0MjUxOTY4NCIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA1MTIuODIxOSAxMDQuMTAxMjIpIiBmaWxsPSIjMDAwMDAwIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEzLjMzMzMzMzMzMzMzMzMzNCIgdGV4dC1hbmNob3I9InN0YXJ0IiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+Z3JwPC90ZXh0PjxwYXRoIGQ9Ik01MTguNDkgMTI2LjggTDUzMy42MSAxMjYuOCBMNTMzLjYxIDE0MS45MiBMNTE4LjQ5IDE0MS45MiBaIiBmaWxsPSIjMmY0ODU4IiAvPjx0ZXh0IHg9IjAiIHk9IjEwLjIwNDcyNDQwOTQ0ODgyIiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDUzOS4yNzg1NiAxMjQuMTU3NDg2KSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9InN0YXJ0IiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+QTwvdGV4dD48cGF0aCBkPSJNNTE4LjQ5IDE0Ny4yMSBMNTMzLjYxIDE0Ny4yMSBMNTMzLjYxIDE2Mi4zMyBMNTE4LjQ5IDE2Mi4zMyBaIiBmaWxsPSIjMDA3MmIyIiAvPjx0ZXh0IHg9IjAiIHk9IjEwLjIwNDcyNDQwOTQ0ODgxOSIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA1MzkuMjc4NTYgMTQ0LjU2NjkzKSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9InN0YXJ0IiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+QjwvdGV4dD48cGF0aCBkPSJNNTE4LjQ5IDE2Ny42MiBMNTMzLjYxIDE2Ny42MiBMNTMzLjYxIDE4Mi43NCBMNTE4LjQ5IDE4Mi43NCBaIiBmaWxsPSIjMDA5ZTczIiAvPjx0ZXh0IHg9IjAiIHk9IjEwLjIwNDcyNDQwOTQ0ODgyMiIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA1MzkuMjc4NTYgMTY0Ljk3NjM4KSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9InN0YXJ0IiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+QzwvdGV4dD48dGV4dCB4PSIwIiB5PSIxMi4wMzM3NTk4NDI1MTk2ODQiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgNTEyLjgyMTkgMTk4LjYxNDE4KSIgZmlsbD0iIzAwMDAwMCIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMy4zMzMzMzMzMzMzMzMzMzQiIHRleHQtYW5jaG9yPSJzdGFydCIgZG9taW5hbnQtYmFzZWxpbmU9ImNlbnRyYWwiPmdycDwvdGV4dD48cGF0aCBkPSJNNTE0LjY3IDIyMC4xIEw1MzcuNDMgMjIwLjEgTDUzNy40MyAyMzcuNjUgTDUxNC42NyAyMzcuNjUgWiIgZmlsbD0iI2ZmZmZmZiIgZmlsbC1vcGFjaXR5PSIwIiAvPjxwYXRoIGQ9Ik01MTQuNjcgMjIyLjY3IEw1MzcuNDMgMjIyLjY3IE01MTQuNjcgMjI2LjY3IEw1MzcuNDMgMjI2LjY3IE01MTQuNjcgMjMwLjY3IEw1MzcuNDMgMjMwLjY3IE01MTQuNjcgMjM0LjY3IEw1MzcuNDMgMjM0LjY3IiBmaWxsPSJub25lIiBzdHJva2U9IiMzMzMzMzMiIHN0cm9rZS13aWR0aD0iMSIgc3Ryb2tlLWxpbmVjYXA9ImJ1dHQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIC8+PHBhdGggZD0iTTUxNC42NyAyMjAuMSBMNTM3LjQzIDIyMC4xIEw1MzcuNDMgMjM3LjY1IEw1MTQuNjcgMjM3LjY1IFoiIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzhjOGM4YyIgc3Ryb2tlLXdpZHRoPSIxIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIC8+PHRleHQgeD0iMCIgeT0iMTAuMjA0NzI0NDA5NDQ4ODIiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgNTM5LjI3ODU2IDIxOC42NzA0NCkiIGZpbGw9IiMzMzMzMzMiIGZpbGwtb3BhY2l0eT0iMSIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIHRleHQtYW5jaG9yPSJzdGFydCIgZG9taW5hbnQtYmFzZWxpbmU9ImNlbnRyYWwiPkE8L3RleHQ+PHBhdGggZD0iTTUxNC42NyAyNDAuNTEgTDUzNy40MyAyNDAuNTEgTDUzNy40MyAyNTguMDYgTDUxNC42NyAyNTguMDYgWiIgZmlsbD0iI2ZmZmZmZiIgZmlsbC1vcGFjaXR5PSIwIiAvPjxwYXRoIGQ9Ik01MzYuODggMjQwLjUxIEw1MzcuNDMgMjQxLjA2IE01MzEuMjIgMjQwLjUxIEw1MzcuNDMgMjQ2LjcxIE01MjUuNTYgMjQwLjUxIEw1MzcuNDMgMjUyLjM3IE01MTkuOTEgMjQwLjUxIEw1MzcuNDMgMjU4LjAzIE01MTQuNjcgMjQwLjkzIEw1MzEuOCAyNTguMDYgTTUxNC42NyAyNDYuNTkgTDUyNi4xNSAyNTguMDYgTTUxNC42NyAyNTIuMjUgTDUyMC40OSAyNTguMDYgTTUxNC42NyAyNTcuOSBMNTE0LjgzIDI1OC4wNiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMzMzMzMzIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJidXR0IiBzdHJva2UtbGluZWpvaW49InJvdW5kIiAvPjxwYXRoIGQ9Ik01MTQuNjcgMjQwLjUxIEw1MzcuNDMgMjQwLjUxIEw1MzcuNDMgMjU4LjA2IEw1MTQuNjcgMjU4LjA2IFoiIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzhjOGM4YyIgc3Ryb2tlLXdpZHRoPSIxIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIC8+PHRleHQgeD0iMCIgeT0iMTAuMjA0NzI0NDA5NDQ4ODE5IiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDUzOS4yNzg1NiAyMzkuMDc5OSkiIGZpbGw9IiMzMzMzMzMiIGZpbGwtb3BhY2l0eT0iMSIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIHRleHQtYW5jaG9yPSJzdGFydCIgZG9taW5hbnQtYmFzZWxpbmU9ImNlbnRyYWwiPkI8L3RleHQ+PHBhdGggZD0iTTUxNC42NyAyNjAuOTIgTDUzNy40MyAyNjAuOTIgTDUzNy40MyAyNzguNDcgTDUxNC42NyAyNzguNDcgWiIgZmlsbD0iI2ZmZmZmZiIgZmlsbC1vcGFjaXR5PSIwIiAvPjxwYXRoIGQ9Ik01MzYuODIgMjYwLjkyIEw1MzYuODIgMjc4LjQ3IE01MzIuODIgMjYwLjkyIEw1MzIuODIgMjc4LjQ3IE01MjguODIgMjYwLjkyIEw1MjguODIgMjc4LjQ3IE01MjQuODIgMjYwLjkyIEw1MjQuODIgMjc4LjQ3IE01MjAuODIgMjYwLjkyIEw1MjAuODIgMjc4LjQ3IE01MTYuODIgMjYwLjkyIEw1MTYuODIgMjc4LjQ3IiBmaWxsPSJub25lIiBzdHJva2U9IiMzMzMzMzMiIHN0cm9rZS13aWR0aD0iMSIgc3Ryb2tlLWxpbmVjYXA9ImJ1dHQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIC8+PHBhdGggZD0iTTUxNC42NyAyNjAuOTIgTDUzNy40MyAyNjAuOTIgTDUzNy40MyAyNzguNDcgTDUxNC42NyAyNzguNDcgWiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjOGM4YzhjIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgLz48dGV4dCB4PSIwIiB5PSIxMC4yMDQ3MjQ0MDk0NDg4MjIiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgNTM5LjI3ODU2IDI1OS40ODkzNSkiIGZpbGw9IiMzMzMzMzMiIGZpbGwtb3BhY2l0eT0iMSIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIHRleHQtYW5jaG9yPSJzdGFydCIgZG9taW5hbnQtYmFzZWxpbmU9ImNlbnRyYWwiPkM8L3RleHQ+PC9nPjwvZz48L2c+PC9zdmc+)

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
