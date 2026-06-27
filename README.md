
<!-- README.md is generated from README.Rmd. Please edit that file -->



# vellumplot

<!-- badges: start -->
<!-- badges: end -->

vellumplot is a declarative, pipe-first grammar of graphics built on the
[vellum](https://github.com/schochastics/vellum) graphics backend. You describe
a plot as an inspectable, serializable *spec*; nothing is drawn until the spec is
compiled into a vellum scene and rendered.

It is a real compiler — spec → resolve encodings → train scales → measure layout
→ compile guides → compile marks → vellum scene — not a thin wrapper around the
drawing primitives.

## Installation

```r
# install vellum first, then:
# pak::pak("schochastics/vellumplot")
```

## Usage


``` r
library(vellumplot)

# a scatter with a continuous colour legend
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = hp) |>
  scale_color_continuous() |>
  render_plot("cars.png")
```

Layer marks on a single panel; scales train across every layer:


``` r
vplot(economics) |>
  mark_line(x = date, y = unemploy) |>
  mark_point(x = date, y = unemploy) |>
  render_plot("unemployment.png")
```

The spec is just data — inspect it before drawing:


``` r
library(vellumplot)
vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
#> <PlotSpec> 32x11 (11 columns), page 6x4 in
#> 
#> ── layers
#> • mark_point(x = wt, y = mpg, color = hp)
```

## What's included (v1)

* Marks: `mark_point()`, `mark_line()`, `mark_rule()`.
* Encodings (tidy-eval): `x`, `y`, `color`/`fill`, `size`, `shape`, `alpha`.
* Position scales (`scale_x_continuous()`, `scale_y_continuous()`; linear and
  `log10`) with auto-trained, expanded domains.
* Colour scales (`scale_color_continuous()`, `scale_color_discrete()`) with a
  legend.
* Trained axes, a panel with gridlines, and layering on one panel.

Faceting, statistical transforms, and other features are planned for v2 — see
`NEWS.md`.
