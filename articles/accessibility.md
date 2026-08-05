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

![](data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSIxMDAlIiBoZWlnaHQ9IjEwMCUiIHZpZXdib3g9IjAgMCA1NzYgMzg0IiByb2xlPSJpbWciIGFyaWEtbGFiZWxsZWRieT0idmw0NDQtZCI+PGRlc2MgaWQ9InZsNDQ0LWQiPkEgYmFyIGNoYXJ0LiBJdCBwbG90cyBuICh2ZXJ0aWNhbCBheGlzKSBhZ2FpbnN0IGdycCAoaG9yaXpvbnRhbCBheGlzKSwgd2hlcmUgY29sb3VyIHNob3dzIGdycC4gQmFzZWQgb24gMyBvYnNlcnZhdGlvbnMuPC9kZXNjPjxkZWZzPjxjbGlwcGF0aCBpZD0iYzAiIGNsaXBwYXRodW5pdHM9InVzZXJTcGFjZU9uVXNlIj48cGF0aCBkPSJNODAuNzg5MzY3Njc1NzgxMjUgMjAuNzg3NDAxMTk5MzQwODIgTDUwNS4yNjI3OTI4NzI2MzE2IDIwLjc4NzQwMTE5OTM0MDgyIEw1MDUuMjYyNzkyODcyNjMxNiAzMTUuOTE4OTYyODc5MTMwODUgTDgwLjc4OTM2NzY3NTc4MTI1IDMxNS45MTg5NjI4NzkxMzA4NSBaIiAvPjwvY2xpcHBhdGg+PC9kZWZzPjxyZWN0IHdpZHRoPSI1NzYiIGhlaWdodD0iMzg0IiBmaWxsPSIjZmZmZmZmIiBmaWxsLW9wYWNpdHk9IjEiIC8+PGcgZGF0YS12ZWxsdW0tcGFuZWw9InBsb3QiPjxnIGRhdGEtdmVsbHVtLXBhbmVsPSJwbG90LWJhY2tncm91bmQiPjxnIHJvbGU9InByZXNlbnRhdGlvbiI+PHBhdGggZD0iTS0wLjUgLTAuNSBMMC41IC0wLjUgTDAuNSAwLjUgTC0wLjUgMC41IFoiIGZpbGw9IiNmZmZmZmYiIGZpbGwtb3BhY2l0eT0iMSIgdHJhbnNmb3JtPSJtYXRyaXgoNTc2IDAgMCAzODQgMjg4IDE5MikiIC8+PC9nPjwvZz48ZyBkYXRhLXZlbGx1bS1wYW5lbD0icGFuZWwtYXJlYSI+PGcgZGF0YS12ZWxsdW0tcGFuZWw9InBhbmVsLTEtMSIgY2xpcC1wYXRoPSJ1cmwoI2MwKSI+PGcgZGF0YS12ZWxsdW0tcGFuPSJwYW5lbC0xLTEiPjxwYXRoIGQ9Ik0tMC41IC0wLjUgTDAuNSAtMC41IEwwLjUgMC41IEwtMC41IDAuNSBaIiBmaWxsPSIjZWJlYmViIiBmaWxsLW9wYWNpdHk9IjEiIHRyYW5zZm9ybT0ibWF0cml4KDQyNC40NzM0MiAwIDAgMjk1LjEzMTU2IDI5My4wMjYwNiAxNjguMzUzMTgpIiAvPjxnIHJvbGU9ImdyaWQiPjxwYXRoIGQ9Ik0wIDMxMi4yMDUzIEw0MjQuNDczNDIgMzEyLjIwNTMgTTAgMjUxLjIyNzY5IEw0MjQuNDczNDIgMjUxLjIyNzY5IE0wIDE5MC4yNTAwOSBMNDI0LjQ3MzQyIDE5MC4yNTAwOSBNMCAxMjkuMjcyNSBMNDI0LjQ3MzQyIDEyOS4yNzI1IE0wIDY4LjI5NDkxIEw0MjQuNDczNDIgNjguMjk0OTEgTTAgNy4zMTczMTEzIEw0MjQuNDczNDIgNy4zMTczMTEzIiBmaWxsPSJub25lIiBzdHJva2U9IiNmZmZmZmYiIHN0cm9rZS1vcGFjaXR5PSIxIiBzdHJva2Utd2lkdGg9IjAuNSIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIiBzdHJva2UtbGluZWpvaW49InJvdW5kIiBzdHJva2UtbWl0ZXJsaW1pdD0iMTAiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgODAuNzg5MzcgMjAuNzg3NDAxKSIgLz48L2c+PGcgcm9sZT0iZ3JpZCI+PHBhdGggZD0iTTcwLjc0NTU3IDI5NS4xMzE1NiBMNzAuNzQ1NTcgMCBNMjEyLjIzNjcxIDI5NS4xMzE1NiBMMjEyLjIzNjcxIDAgTTM1My43Mjc4NCAyOTUuMTMxNTYgTDM1My43Mjc4NCAwIiBmaWxsPSJub25lIiBzdHJva2U9IiNmZmZmZmYiIHN0cm9rZS1vcGFjaXR5PSIxIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgc3Ryb2tlLW1pdGVybGltaXQ9IjEwIiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDgwLjc4OTM3IDIwLjc4NzQwMSkiIC8+PC9nPjxnIHJvbGU9ImdyaWQiPjxwYXRoIGQ9Ik0wIDI4MS43MTY1IEw0MjQuNDczNDIgMjgxLjcxNjUgTTAgMjIwLjczODg5IEw0MjQuNDczNDIgMjIwLjczODg5IE0wIDE1OS43NjEzIEw0MjQuNDczNDIgMTU5Ljc2MTMgTTAgOTguNzgzNzEgTDQyNC40NzM0MiA5OC43ODM3MSBNMCAzNy44MDYxMSBMNDI0LjQ3MzQyIDM3LjgwNjExIiBmaWxsPSJub25lIiBzdHJva2U9IiNmZmZmZmYiIHN0cm9rZS1vcGFjaXR5PSIxIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgc3Ryb2tlLW1pdGVybGltaXQ9IjEwIiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDgwLjc4OTM3IDIwLjc4NzQwMSkiIC8+PC9nPjxnIHJvbGU9InByZXNlbnRhdGlvbiI+PHBhdGggZD0iTTcwLjc0NTU3IDI5NS4xMzE1NiBMNzAuNzQ1NTcgMjg5LjQ2MjI4IE0yMTIuMjM2NzEgMjk1LjEzMTU2IEwyMTIuMjM2NzEgMjg5LjQ2MjI4IE0zNTMuNzI3ODQgMjk1LjEzMTU2IEwzNTMuNzI3ODQgMjg5LjQ2MjI4IiBmaWxsPSJub25lIiBzdHJva2U9IiMzMzMzMzMiIHN0cm9rZS1vcGFjaXR5PSIxIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgc3Ryb2tlLW1pdGVybGltaXQ9IjEwIiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDgwLjc4OTM3IDIwLjc4NzQwMSkiIC8+PC9nPjxnIHJvbGU9InByZXNlbnRhdGlvbiI+PHBhdGggZD0iTTAgMjgxLjcxNjUgTDUuNjY5MjkxNSAyODEuNzE2NSBNMCAyMjAuNzM4ODkgTDUuNjY5MjkxNSAyMjAuNzM4ODkgTTAgMTU5Ljc2MTMgTDUuNjY5MjkxNSAxNTkuNzYxMyBNMCA5OC43ODM3MSBMNS42NjkyOTE1IDk4Ljc4MzcxIE0wIDM3LjgwNjExIEw1LjY2OTI5MTUgMzcuODA2MTEiIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzMzMzMzMyIgc3Ryb2tlLW9wYWNpdHk9IjEiIHN0cm9rZS13aWR0aD0iMSIgc3Ryb2tlLWxpbmVjYXA9InJvdW5kIiBzdHJva2UtbGluZWpvaW49InJvdW5kIiBzdHJva2UtbWl0ZXJsaW1pdD0iMTAiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgODAuNzg5MzcgMjAuNzg3NDAxKSIgLz48L2c+PGcgZGF0YS12ZWxsdW0taWQ9ImxheWVyLTEtYmFyLWcxIiByb2xlPSJwcmVzZW50YXRpb24iPjxwYXRoIGQ9Ik03LjA3NDU1NzMgODYuNTg4MTkgTDEzNC40MTY1OCA4Ni41ODgxOSBMMTM0LjQxNjU4IDI4MS43MTY1IEw3LjA3NDU1NzMgMjgxLjcxNjUgWiIgZmlsbD0iI2ZmZmZmZiIgZmlsbC1vcGFjaXR5PSIwIiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDgwLjc4OTM3IDIwLjc4NzQwMSkiIC8+PHBhdGggZD0iTTcuMDc0NTU3MyA4OCBMMTM0LjQxNjU4IDg4IE03LjA3NDU1NzMgOTIgTDEzNC40MTY1OCA5MiBNNy4wNzQ1NTczIDk2IEwxMzQuNDE2NTggOTYgTTcuMDc0NTU3MyAxMDAgTDEzNC40MTY1OCAxMDAgTTcuMDc0NTU3MyAxMDQgTDEzNC40MTY1OCAxMDQgTTcuMDc0NTU3MyAxMDggTDEzNC40MTY1OCAxMDggTTcuMDc0NTU3MyAxMTIgTDEzNC40MTY1OCAxMTIgTTcuMDc0NTU3MyAxMTYgTDEzNC40MTY1OCAxMTYgTTcuMDc0NTU3MyAxMjAgTDEzNC40MTY1OCAxMjAgTTcuMDc0NTU3MyAxMjQgTDEzNC40MTY1OCAxMjQgTTcuMDc0NTU3MyAxMjggTDEzNC40MTY1OCAxMjggTTcuMDc0NTU3MyAxMzIgTDEzNC40MTY1OCAxMzIgTTcuMDc0NTU3MyAxMzYgTDEzNC40MTY1OCAxMzYgTTcuMDc0NTU3MyAxNDAgTDEzNC40MTY1OCAxNDAgTTcuMDc0NTU3MyAxNDQgTDEzNC40MTY1OCAxNDQgTTcuMDc0NTU3MyAxNDggTDEzNC40MTY1OCAxNDggTTcuMDc0NTU3MyAxNTIgTDEzNC40MTY1OCAxNTIgTTcuMDc0NTU3MyAxNTYgTDEzNC40MTY1OCAxNTYgTTcuMDc0NTU3MyAxNjAgTDEzNC40MTY1OCAxNjAgTTcuMDc0NTU3MyAxNjQgTDEzNC40MTY1OCAxNjQgTTcuMDc0NTU3MyAxNjggTDEzNC40MTY1OCAxNjggTTcuMDc0NTU3MyAxNzIgTDEzNC40MTY1OCAxNzIgTTcuMDc0NTU3MyAxNzYgTDEzNC40MTY1OCAxNzYgTTcuMDc0NTU3MyAxODAgTDEzNC40MTY1OCAxODAgTTcuMDc0NTU3MyAxODQgTDEzNC40MTY1OCAxODQgTTcuMDc0NTU3MyAxODggTDEzNC40MTY1OCAxODggTTcuMDc0NTU3MyAxOTIgTDEzNC40MTY1OCAxOTIgTTcuMDc0NTU3MyAxOTYgTDEzNC40MTY1OCAxOTYgTTcuMDc0NTU3MyAyMDAgTDEzNC40MTY1OCAyMDAgTTcuMDc0NTU3MyAyMDQgTDEzNC40MTY1OCAyMDQgTTcuMDc0NTU3MyAyMDggTDEzNC40MTY1OCAyMDggTTcuMDc0NTU3MyAyMTIgTDEzNC40MTY1OCAyMTIgTTcuMDc0NTU3MyAyMTYgTDEzNC40MTY1OCAyMTYgTTcuMDc0NTU3MyAyMjAgTDEzNC40MTY1OCAyMjAgTTcuMDc0NTU3MyAyMjQgTDEzNC40MTY1OCAyMjQgTTcuMDc0NTU3MyAyMjggTDEzNC40MTY1OCAyMjggTTcuMDc0NTU3MyAyMzIgTDEzNC40MTY1OCAyMzIgTTcuMDc0NTU3MyAyMzYgTDEzNC40MTY1OCAyMzYgTTcuMDc0NTU3MyAyNDAgTDEzNC40MTY1OCAyNDAgTTcuMDc0NTU3MyAyNDQgTDEzNC40MTY1OCAyNDQgTTcuMDc0NTU3MyAyNDggTDEzNC40MTY1OCAyNDggTTcuMDc0NTU3MyAyNTIgTDEzNC40MTY1OCAyNTIgTTcuMDc0NTU3MyAyNTYgTDEzNC40MTY1OCAyNTYgTTcuMDc0NTU3MyAyNjAgTDEzNC40MTY1OCAyNjAgTTcuMDc0NTU3MyAyNjQgTDEzNC40MTY1OCAyNjQgTTcuMDc0NTU3MyAyNjggTDEzNC40MTY1OCAyNjggTTcuMDc0NTU3MyAyNzIgTDEzNC40MTY1OCAyNzIgTTcuMDc0NTU3MyAyNzYgTDEzNC40MTY1OCAyNzYgTTcuMDc0NTU3MyAyODAgTDEzNC40MTY1OCAyODAiIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzMzMzMzMyIgc3Ryb2tlLW9wYWNpdHk9IjEiIHN0cm9rZS13aWR0aD0iMSIgc3Ryb2tlLWxpbmVjYXA9ImJ1dHQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIHN0cm9rZS1taXRlcmxpbWl0PSIxMCIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA4MC43ODkzNyAyMC43ODc0MDEpIiAvPjwvZz48ZyBkYXRhLXZlbGx1bS1pZD0ibGF5ZXItMS1iYXItZzIiIHJvbGU9InByZXNlbnRhdGlvbiI+PHBhdGggZD0iTTE0OC41NjU3IDE1OS43NjEzIEwyNzUuOTA3NyAxNTkuNzYxMyBMMjc1LjkwNzcgMjgxLjcxNjUgTDE0OC41NjU3IDI4MS43MTY1IFoiIGZpbGw9IiNmZmZmZmYiIGZpbGwtb3BhY2l0eT0iMCIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA4MC43ODkzNyAyMC43ODc0MDEpIiAvPjxwYXRoIGQ9Ik0yNzIuODk4MzggMTU5Ljc2MTMgTDI3NS45MDc3IDE2Mi43NzA2MSBNMjY3LjI0MTU1IDE1OS43NjEzIEwyNzUuOTA3NyAxNjguNDI3NDcgTTI2MS41ODQ3IDE1OS43NjEzMiBMMjc1LjkwNzY4IDE3NC4wODQzMiBNMjU1LjkyNzgzIDE1OS43NjEzMiBMMjc1LjkwNzY4IDE3OS43NDExOCBNMjUwLjI3MDk4IDE1OS43NjEzIEwyNzUuOTA3NjggMTg1LjM5ODAzIE0yNDQuNjE0MTIgMTU5Ljc2MTMgTDI3NS45MDc3IDE5MS4wNTQ4OSBNMjM4Ljk1NzI2IDE1OS43NjEzIEwyNzUuOTA3NyAxOTYuNzExNzUgTTIzMy4zMDA0MiAxNTkuNzYxMzIgTDI3NS45MDc2OCAyMDIuMzY4NTkgTTIyNy42NDM1MiAxNTkuNzYxMjkgTDI3NS45MDc2OCAyMDguMDI1NDUgTTIyMS45ODY3MSAxNTkuNzYxMyBMMjc1LjkwNzY4IDIxMy42ODIzIE0yMTYuMzI5ODMgMTU5Ljc2MTI5IEwyNzUuOTA3NyAyMTkuMzM5MTYgTTIxMC42NzI5NyAxNTkuNzYxMjkgTDI3NS45MDc3IDIyNC45OTYwMiBNMjA1LjAxNjExIDE1OS43NjEyOSBMMjc1LjkwNzcgMjMwLjY1MjkgTTE5OS4zNTkyOCAxNTkuNzYxMjkgTDI3NS45MDc3IDIzNi4zMDk3NCBNMTkzLjcwMjQyIDE1OS43NjEyOSBMMjc1LjkwNzcgMjQxLjk2NjYgTTE4OC4wNDU1NiAxNTkuNzYxMjkgTDI3NS45MDc3IDI0Ny42MjM0NiBNMTgyLjM4ODcyIDE1OS43NjEzIEwyNzUuOTA3NzUgMjUzLjI4MDMyIE0xNzYuNzMxODYgMTU5Ljc2MTMgTDI3NS45MDc3IDI1OC45MzcxMyBNMTcxLjA3NTAxIDE1OS43NjEyOSBMMjc1LjkwNzcgMjY0LjU5NCBNMTY1LjQxODE1IDE1OS43NjEyOSBMMjc1LjkwNzcgMjcwLjI1MDg1IE0xNTkuNzYxMjkgMTU5Ljc2MTI5IEwyNzUuOTA3NyAyNzUuOTA3NyBNMTU0LjEwNDQ1IDE1OS43NjEzIEwyNzUuOTA3NyAyODEuNTY0NTggTTE0OC41NjU3IDE1OS44Nzk0MyBMMjcwLjQwMjc3IDI4MS43MTY1IE0xNDguNTY1NyAxNjUuNTM2MjUgTDI2NC43NDU5IDI4MS43MTY1IE0xNDguNTY1NyAxNzEuMTkzMTIgTDI1OS4wODkwNSAyODEuNzE2NSBNMTQ4LjU2NTcgMTc2Ljg0OTk4IEwyNTMuNDMyMiAyODEuNzE2NSBNMTQ4LjU2NTcgMTgyLjUwNjg0IEwyNDcuNzc1MzggMjgxLjcxNjUgTTE0OC41NjU3IDE4OC4xNjM3IEwyNDIuMTE4NTIgMjgxLjcxNjUgTTE0OC41NjU3IDE5My44MjA1MyBMMjM2LjQ2MTY3IDI4MS43MTY1IE0xNDguNTY1NyAxOTkuNDc3MzkgTDIzMC44MDQ4MSAyODEuNzE2NSBNMTQ4LjU2NTcgMjA1LjEzNDI1IEwyMjUuMTQ3OTUgMjgxLjcxNjUgTTE0OC41NjU3IDIxMC43OTExIEwyMTkuNDkxMDkgMjgxLjcxNjUgTTE0OC41NjU3IDIxNi40NDc5NyBMMjEzLjgzNDIzIDI4MS43MTY1IE0xNDguNTY1NyAyMjIuMTA0OCBMMjA4LjE3NzQgMjgxLjcxNjUgTTE0OC41NjU3IDIyNy43NjE2NiBMMjAyLjUyMDU0IDI4MS43MTY1IE0xNDguNTY1NyAyMzMuNDE4NTIgTDE5Ni44NjM2OCAyODEuNzE2NSBNMTQ4LjU2NTcgMjM5LjA3NTM4IEwxOTEuMjA2ODIgMjgxLjcxNjUgTTE0OC41NjU3IDI0NC43MzIyNCBMMTg1LjU0OTk2IDI4MS43MTY1IE0xNDguNTY1NyAyNTAuMzg5MDcgTDE3OS44OTMxMyAyODEuNzE2NSBNMTQ4LjU2NTcgMjU2LjA0NTkzIEwxNzQuMjM2MjcgMjgxLjcxNjUgTTE0OC41NjU3IDI2MS43MDI4IEwxNjguNTc5NCAyODEuNzE2NSBNMTQ4LjU2NTcgMjY3LjM1OTY1IEwxNjIuOTIyNTUgMjgxLjcxNjUgTTE0OC41NjU3IDI3My4wMTY1IEwxNTcuMjY1NjkgMjgxLjcxNjUgTTE0OC41NjU3IDI3OC42NzMzNCBMMTUxLjYwODg2IDI4MS43MTY1IiBmaWxsPSJub25lIiBzdHJva2U9IiMzMzMzMzMiIHN0cm9rZS1vcGFjaXR5PSIxIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJidXR0IiBzdHJva2UtbGluZWpvaW49InJvdW5kIiBzdHJva2UtbWl0ZXJsaW1pdD0iMTAiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgODAuNzg5MzcgMjAuNzg3NDAxKSIgLz48L2c+PGcgZGF0YS12ZWxsdW0taWQ9ImxheWVyLTEtYmFyLWczIiByb2xlPSJwcmVzZW50YXRpb24iPjxwYXRoIGQ9Ik0yOTAuMDU2ODUgMTMuNDE1MDcxIEw0MTcuMzk4ODYgMTMuNDE1MDcxIEw0MTcuMzk4ODYgMjgxLjcxNjUgTDI5MC4wNTY4NSAyODEuNzE2NSBaIiBmaWxsPSIjZmZmZmZmIiBmaWxsLW9wYWNpdHk9IjAiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgODAuNzg5MzcgMjAuNzg3NDAxKSIgLz48cGF0aCBkPSJNNDE2IDEzLjQxNTA3MSBMNDE2IDI4MS43MTY1IE00MTIgMTMuNDE1MDcxIEw0MTIgMjgxLjcxNjUgTTQwOCAxMy40MTUwNzEgTDQwOCAyODEuNzE2NSBNNDA0IDEzLjQxNTA3MTUgTDQwNCAyODEuNzE2NSBNNDAwIDEzLjQxNTA3MSBMNDAwIDI4MS43MTY1IE0zOTYgMTMuNDE1MDcxIEwzOTYgMjgxLjcxNjUgTTM5MiAxMy40MTUwNzEgTDM5MiAyODEuNzE2NSBNMzg4IDEzLjQxNTA3MSBMMzg4IDI4MS43MTY1IE0zODQgMTMuNDE1MDcxNSBMMzg0IDI4MS43MTY1IE0zODAgMTMuNDE1MDcxIEwzODAgMjgxLjcxNjUgTTM3NiAxMy40MTUwNzEgTDM3NiAyODEuNzE2NSBNMzcyIDEzLjQxNTA3MSBMMzcyIDI4MS43MTY1IE0zNjggMTMuNDE1MDcxIEwzNjggMjgxLjcxNjUgTTM2NCAxMy40MTUwNzE1IEwzNjQgMjgxLjcxNjUgTTM2MCAxMy40MTUwNzE1IEwzNjAgMjgxLjcxNjUgTTM1NiAxMy40MTUwNzEgTDM1NiAyODEuNzE2NSBNMzUyIDEzLjQxNTA3MSBMMzUyIDI4MS43MTY1MiBNMzQ4IDEzLjQxNTA3MSBMMzQ4IDI4MS43MTY1IE0zNDQgMTMuNDE1MDcxIEwzNDQgMjgxLjcxNjUgTTM0MCAxMy40MTUwNzE1IEwzNDAgMjgxLjcxNjUgTTMzNiAxMy40MTUwNzEgTDMzNiAyODEuNzE2NSBNMzMyIDEzLjQxNTA3MSBMMzMyIDI4MS43MTY1IE0zMjggMTMuNDE1MDcxIEwzMjggMjgxLjcxNjUgTTMyNCAxMy40MTUwNzEgTDMyNCAyODEuNzE2NSBNMzIwIDEzLjQxNTA3MTUgTDMyMCAyODEuNzE2NSBNMzE2IDEzLjQxNTA3MSBMMzE2IDI4MS43MTY1IE0zMTIgMTMuNDE1MDcxIEwzMTIgMjgxLjcxNjUgTTMwOCAxMy40MTUwNzEgTDMwOCAyODEuNzE2NSBNMzA0IDEzLjQxNTA3MSBMMzA0IDI4MS43MTY1IE0zMDAgMTMuNDE1MDcxNSBMMzAwIDI4MS43MTY1IE0yOTYgMTMuNDE1MDcxNSBMMjk2IDI4MS43MTY1IE0yOTIgMTMuNDE1MDcxIEwyOTIgMjgxLjcxNjUiIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzMzMzMzMyIgc3Ryb2tlLW9wYWNpdHk9IjEiIHN0cm9rZS13aWR0aD0iMSIgc3Ryb2tlLWxpbmVjYXA9ImJ1dHQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIHN0cm9rZS1taXRlcmxpbWl0PSIxMCIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA4MC43ODkzNyAyMC43ODc0MDEpIiAvPjwvZz48L2c+PC9nPjxnIGRhdGEtdmVsbHVtLXBhbmVsPSJheGlzLXktMSI+PHRleHQgeD0iMzYuMTQyMjA0NzI0NDA5NDQ2IiB5PSIyODEuNzE2NDkwNjk0MzQ1IiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDQzLjE0MTI0IDIwLjc4NzQwMSkiIGZpbGw9IiMzMzMzMzMiIGZpbGwtb3BhY2l0eT0iMSIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIHRleHQtYW5jaG9yPSJlbmQiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj4wLjA8L3RleHQ+PHRleHQgeD0iMzYuMTQyMjA0NzI0NDA5NDQ2IiB5PSIyMjAuNzM4ODk1MzA1OTU4NyIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA0My4xNDEyNCAyMC43ODc0MDEpIiBmaWxsPSIjMzMzMzMzIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiB0ZXh0LWFuY2hvcj0iZW5kIiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+Mi41PC90ZXh0Pjx0ZXh0IHg9IjM2LjE0MjIwNDcyNDQwOTQ0NiIgeT0iMTU5Ljc2MTI5OTkxNzU3MjMiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgNDMuMTQxMjQgMjAuNzg3NDAxKSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9ImVuZCIgZG9taW5hbnQtYmFzZWxpbmU9ImNlbnRyYWwiPjUuMDwvdGV4dD48dGV4dCB4PSIzNi4xNDIyMDQ3MjQ0MDk0NDYiIHk9Ijk4Ljc4MzcwNDUyOTE4NTkxIiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDQzLjE0MTI0IDIwLjc4NzQwMSkiIGZpbGw9IiMzMzMzMzMiIGZpbGwtb3BhY2l0eT0iMSIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIHRleHQtYW5jaG9yPSJlbmQiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj43LjU8L3RleHQ+PHRleHQgeD0iMzYuMTQyMjA0NzI0NDA5NDQ2IiB5PSIzNy44MDYxMDkxNDA3OTk1NSIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA0My4xNDEyNCAyMC43ODc0MDEpIiBmaWxsPSIjMzMzMzMzIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiB0ZXh0LWFuY2hvcj0iZW5kIiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+MTAuMDwvdGV4dD48L2c+PGcgZGF0YS12ZWxsdW0tcGFuZWw9ImF4aXMteC0zIj48dGV4dCB4PSI3MC43NDU1NzA4NjYxNDE3MiIgeT0iNC40ODkxNjMzODU4MjY3NjciIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgODAuNzg5MzcgMzE1LjkxODk4KSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZG9taW5hbnQtYmFzZWxpbmU9InRleHQtYmVmb3JlLWVkZ2UiPkE8L3RleHQ+PHRleHQgeD0iMjEyLjIzNjcxMjU5ODQyNTE5IiB5PSI0LjQ4OTE2MzM4NTgyNjc2NyIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA4MC43ODkzNyAzMTUuOTE4OTgpIiBmaWxsPSIjMzMzMzMzIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIiBkb21pbmFudC1iYXNlbGluZT0idGV4dC1iZWZvcmUtZWRnZSI+QjwvdGV4dD48dGV4dCB4PSIzNTMuNzI3ODU0MzMwNzA4NyIgeT0iNC40ODkxNjMzODU4MjY3NjciIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgODAuNzg5MzcgMzE1LjkxODk4KSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZG9taW5hbnQtYmFzZWxpbmU9InRleHQtYmVmb3JlLWVkZ2UiPkM8L3RleHQ+PC9nPjxnIGRhdGEtdmVsbHVtLXBhbmVsPSJheGlzLXRpdGxlLXkiPjx0ZXh0IHg9IjExLjE3NjkxOTI5MTMzODU4MiIgeT0iMTQ3LjU2NTc4MDgzOTg5NSIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSAyMC43ODc0MDEgMjAuNzg3NDAxKSByb3RhdGUoLTkwIDExLjE3NjkxOTI5MTMzODU4MiAxNDcuNTY1NzgwODM5ODk1KSIgZmlsbD0iIzAwMDAwMCIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxNC42NjY2NjY2NjY2NjY2NjYiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj5uPC90ZXh0PjwvZz48ZyBkYXRhLXZlbGx1bS1wYW5lbD0iYXhpcy10aXRsZS14Ij48dGV4dCB4PSIyMTIuMjM2NzEyNTk4NDI1MTkiIHk9IjExLjE3NjkxOTI5MTMzODU3OCIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA4MC43ODkzNyAzNDAuODU4NzYpIiBmaWxsPSIjMDAwMDAwIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjE0LjY2NjY2NjY2NjY2NjY2NiIgdGV4dC1hbmNob3I9Im1pZGRsZSIgZG9taW5hbnQtYmFzZWxpbmU9ImNlbnRyYWwiPmdycDwvdGV4dD48L2c+PGcgZGF0YS12ZWxsdW0tcGFuZWw9ImxlZ2VuZCI+PHRleHQgeD0iMCIgeT0iMTIuMDMzNzU5ODQyNTE5Njg0IiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDUxMi44MjE5IDEwNC4xMDEyMikiIGZpbGw9IiMwMDAwMDAiIGZpbGwtb3BhY2l0eT0iMSIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTMuMzMzMzMzMzMzMzMzMzM0IiB0ZXh0LWFuY2hvcj0ic3RhcnQiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj5ncnA8L3RleHQ+PHBhdGggZD0iTS0wLjUgLTAuNSBMMC41IC0wLjUgTDAuNSAwLjUgTC0wLjUgMC41IFoiIGZpbGw9IiMyZjQ4NTgiIGZpbGwtb3BhY2l0eT0iMSIgdHJhbnNmb3JtPSJtYXRyaXgoMTUuMTE4MTExIDAgMCAxNS4xMTgxMTEgNTI2LjA1MDIzIDEzNC4zNjIyMSkiIC8+PHRleHQgeD0iMCIgeT0iMTAuMjA0NzI0NDA5NDQ4ODIiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgNTM5LjI3ODU2IDEyNC4xNTc0ODYpIiBmaWxsPSIjMzMzMzMzIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiB0ZXh0LWFuY2hvcj0ic3RhcnQiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj5BPC90ZXh0PjxwYXRoIGQ9Ik0tMC41IC0wLjUgTDAuNSAtMC41IEwwLjUgMC41IEwtMC41IDAuNSBaIiBmaWxsPSIjMDA3MmIyIiBmaWxsLW9wYWNpdHk9IjEiIHRyYW5zZm9ybT0ibWF0cml4KDE1LjExODExMSAwIDAgMTUuMTE4MTExIDUyNi4wNTAyMyAxNTQuNzcxNjUpIiAvPjx0ZXh0IHg9IjAiIHk9IjEwLjIwNDcyNDQwOTQ0ODgxOSIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA1MzkuMjc4NTYgMTQ0LjU2NjkzKSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9InN0YXJ0IiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+QjwvdGV4dD48cGF0aCBkPSJNLTAuNSAtMC41IEwwLjUgLTAuNSBMMC41IDAuNSBMLTAuNSAwLjUgWiIgZmlsbD0iIzAwOWU3MyIgZmlsbC1vcGFjaXR5PSIxIiB0cmFuc2Zvcm09Im1hdHJpeCgxNS4xMTgxMTEgMCAwIDE1LjExODExMSA1MjYuMDUwMjMgMTc1LjE4MTEpIiAvPjx0ZXh0IHg9IjAiIHk9IjEwLjIwNDcyNDQwOTQ0ODgyMiIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA1MzkuMjc4NTYgMTY0Ljk3NjM4KSIgZmlsbD0iIzMzMzMzMyIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMiIgdGV4dC1hbmNob3I9InN0YXJ0IiBkb21pbmFudC1iYXNlbGluZT0iY2VudHJhbCI+QzwvdGV4dD48dGV4dCB4PSIwIiB5PSIxMi4wMzM3NTk4NDI1MTk2ODQiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgNTEyLjgyMTkgMTk4LjYxNDE4KSIgZmlsbD0iIzAwMDAwMCIgZmlsbC1vcGFjaXR5PSIxIiBmb250LWZhbWlseT0ic2Fucy1zZXJpZiIgZm9udC1zaXplPSIxMy4zMzMzMzMzMzMzMzMzMzQiIHRleHQtYW5jaG9yPSJzdGFydCIgZG9taW5hbnQtYmFzZWxpbmU9ImNlbnRyYWwiPmdycDwvdGV4dD48cGF0aCBkPSJNMS44NTE5Njg1IDEuNDI4NjYxNSBMMjQuNjA0NzI1IDEuNDI4NjYxNSBMMjQuNjA0NzI1IDE4Ljk4MDc4NyBMMS44NTE5Njg1IDE4Ljk4MDc4NyBaIiBmaWxsPSIjZmZmZmZmIiBmaWxsLW9wYWNpdHk9IjAiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgNTEyLjgyMTkgMjE4LjY3MDQ0KSIgLz48cGF0aCBkPSJNMS44NTE5Njg1IDQgTDI0LjYwNDcyNSA0IE0xLjg1MTk2ODUgOCBMMjQuNjA0NzI1IDggTTEuODUxOTY4NSAxMiBMMjQuNjA0NzI1IDEyIE0xLjg1MTk2ODUgMTYgTDI0LjYwNDcyNSAxNiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjMzMzMzMzIiBzdHJva2Utb3BhY2l0eT0iMSIgc3Ryb2tlLXdpZHRoPSIxIiBzdHJva2UtbGluZWNhcD0iYnV0dCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgc3Ryb2tlLW1pdGVybGltaXQ9IjEwIiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDUxMi44MjE5IDIxOC42NzA0NCkiIC8+PHBhdGggZD0iTTEuODUxOTY4NSAxLjQyODY2MTUgTDI0LjYwNDcyNSAxLjQyODY2MTUgTDI0LjYwNDcyNSAxOC45ODA3ODcgTDEuODUxOTY4NSAxOC45ODA3ODcgWiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjOGM4YzhjIiBzdHJva2Utb3BhY2l0eT0iMSIgc3Ryb2tlLXdpZHRoPSIxIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIHN0cm9rZS1taXRlcmxpbWl0PSIxMCIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA1MTIuODIxOSAyMTguNjcwNDQpIiAvPjx0ZXh0IHg9IjAiIHk9IjEwLjIwNDcyNDQwOTQ0ODgyIiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDUzOS4yNzg1NiAyMTguNjcwNDQpIiBmaWxsPSIjMzMzMzMzIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiB0ZXh0LWFuY2hvcj0ic3RhcnQiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj5BPC90ZXh0PjxwYXRoIGQ9Ik0xLjg1MTk2ODUgMS40Mjg2NjE1IEwyNC42MDQ3MjUgMS40Mjg2NjE1IEwyNC42MDQ3MjUgMTguOTgwNzg3IEwxLjg1MTk2ODUgMTguOTgwNzg3IFoiIGZpbGw9IiNmZmZmZmYiIGZpbGwtb3BhY2l0eT0iMCIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA1MTIuODIxOSAyMzkuMDc5OSkiIC8+PHBhdGggZD0iTTI0LjA1NjA3NiAxLjQyODY2MDQgTDI0LjYwNDcyMyAxLjk3NzMwNjQgTTE4LjM5OTIyMyAxLjQyODY2MTMgTDI0LjYwNDcyMyA3LjYzNDE2MSBNMTIuNzQyMzcgMS40Mjg2NjEzIEwyNC42MDQ3MjMgMTMuMjkxMDE0IE03LjA4NTUxNiAxLjQyODY2MTYgTDI0LjYwNDcyMyAxOC45NDc4NyBNMS44NTE5NjgzIDEuODUxOTY4MyBMMTguOTgwNzg3IDE4Ljk4MDc4NyBNMS44NTE5NjggNy41MDg4MjI0IEwxMy4zMjM5MzQgMTguOTgwNzg3IE0xLjg1MTk2ODggMTMuMTY1Njc3IEw3LjY2NzA3OTQgMTguOTgwNzg3IE0xLjg1MTk2ODggMTguODIyNTMgTDIuMDEwMjI1MyAxOC45ODA3ODciIGZpbGw9Im5vbmUiIHN0cm9rZT0iIzMzMzMzMyIgc3Ryb2tlLW9wYWNpdHk9IjEiIHN0cm9rZS13aWR0aD0iMSIgc3Ryb2tlLWxpbmVjYXA9ImJ1dHQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIHN0cm9rZS1taXRlcmxpbWl0PSIxMCIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA1MTIuODIxOSAyMzkuMDc5OSkiIC8+PHBhdGggZD0iTTEuODUxOTY4NSAxLjQyODY2MTUgTDI0LjYwNDcyNSAxLjQyODY2MTUgTDI0LjYwNDcyNSAxOC45ODA3ODcgTDEuODUxOTY4NSAxOC45ODA3ODcgWiIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjOGM4YzhjIiBzdHJva2Utb3BhY2l0eT0iMSIgc3Ryb2tlLXdpZHRoPSIxIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiIHN0cm9rZS1taXRlcmxpbWl0PSIxMCIgdHJhbnNmb3JtPSJtYXRyaXgoMSAwIDAgMSA1MTIuODIxOSAyMzkuMDc5OSkiIC8+PHRleHQgeD0iMCIgeT0iMTAuMjA0NzI0NDA5NDQ4ODE5IiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDUzOS4yNzg1NiAyMzkuMDc5OSkiIGZpbGw9IiMzMzMzMzMiIGZpbGwtb3BhY2l0eT0iMSIgZm9udC1mYW1pbHk9InNhbnMtc2VyaWYiIGZvbnQtc2l6ZT0iMTIiIHRleHQtYW5jaG9yPSJzdGFydCIgZG9taW5hbnQtYmFzZWxpbmU9ImNlbnRyYWwiPkI8L3RleHQ+PHBhdGggZD0iTTEuODUxOTY4NSAxLjQyODY2MTUgTDI0LjYwNDcyNSAxLjQyODY2MTUgTDI0LjYwNDcyNSAxOC45ODA3ODcgTDEuODUxOTY4NSAxOC45ODA3ODcgWiIgZmlsbD0iI2ZmZmZmZiIgZmlsbC1vcGFjaXR5PSIwIiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDUxMi44MjE5IDI1OS40ODkzNSkiIC8+PHBhdGggZD0iTTI0IDEuNDI4NjYxNSBMMjQgMTguOTgwNzg3IE0yMCAxLjQyODY2MTUgTDIwIDE4Ljk4MDc4NSBNMTYgMS40Mjg2NjE1IEwxNS45OTk5OTkgMTguOTgwNzg1IE0xMiAxLjQyODY2MTMgTDExLjk5OTk5OSAxOC45ODA3ODcgTTggMS40Mjg2NjE1IEw3Ljk5OTk5OSAxOC45ODA3ODcgTTQgMS40Mjg2NjEzIEwzLjk5OTk5OTMgMTguOTgwNzg3IiBmaWxsPSJub25lIiBzdHJva2U9IiMzMzMzMzMiIHN0cm9rZS1vcGFjaXR5PSIxIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJidXR0IiBzdHJva2UtbGluZWpvaW49InJvdW5kIiBzdHJva2UtbWl0ZXJsaW1pdD0iMTAiIHRyYW5zZm9ybT0ibWF0cml4KDEgMCAwIDEgNTEyLjgyMTkgMjU5LjQ4OTM1KSIgLz48cGF0aCBkPSJNMS44NTE5Njg1IDEuNDI4NjYxNSBMMjQuNjA0NzI1IDEuNDI4NjYxNSBMMjQuNjA0NzI1IDE4Ljk4MDc4NyBMMS44NTE5Njg1IDE4Ljk4MDc4NyBaIiBmaWxsPSJub25lIiBzdHJva2U9IiM4YzhjOGMiIHN0cm9rZS1vcGFjaXR5PSIxIiBzdHJva2Utd2lkdGg9IjEiIHN0cm9rZS1saW5lY2FwPSJyb3VuZCIgc3Ryb2tlLWxpbmVqb2luPSJyb3VuZCIgc3Ryb2tlLW1pdGVybGltaXQ9IjEwIiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDUxMi44MjE5IDI1OS40ODkzNSkiIC8+PHRleHQgeD0iMCIgeT0iMTAuMjA0NzI0NDA5NDQ4ODIyIiB0cmFuc2Zvcm09Im1hdHJpeCgxIDAgMCAxIDUzOS4yNzg1NiAyNTkuNDg5MzUpIiBmaWxsPSIjMzMzMzMzIiBmaWxsLW9wYWNpdHk9IjEiIGZvbnQtZmFtaWx5PSJzYW5zLXNlcmlmIiBmb250LXNpemU9IjEyIiB0ZXh0LWFuY2hvcj0ic3RhcnQiIGRvbWluYW50LWJhc2VsaW5lPSJjZW50cmFsIj5DPC90ZXh0PjwvZz48L2c+PC9nPjwvc3ZnPg==)

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
