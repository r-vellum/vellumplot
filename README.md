
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

Building a plot returns a spec; printing it draws into the Plots pane (and
embeds in a knitr/Quarto chunk), like ggplot2. Use `render_plot()` to write a
file.


``` r
library(vellumplot)

# a scatter with a continuous colour legend
vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = hp) |>
  scale_color_continuous()
```

<div class="figure">
<img src="man/figures/README-example-1.png" alt="plot of chunk example" width="100%" />
<p class="caption">plot of chunk example</p>
</div>

Layer marks on a single panel; scales train across every layer:


``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  mark_smooth(x = wt, y = mpg)
```

<div class="figure">
<img src="man/figures/README-layers-1.png" alt="plot of chunk layers" width="100%" />
<p class="caption">plot of chunk layers</p>
</div>

Facet into a grid of panels (`facet_wrap()` / `facet_grid()`), with shared or
free scales:


``` r
vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  facet_wrap(~cyl)
```

<div class="figure">
<img src="man/figures/README-facet-1.png" alt="plot of chunk facet" width="100%" />
<p class="caption">plot of chunk facet</p>
</div>

The spec is just data — `summary()` shows its structure without drawing:


``` r
summary(vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp))
#> <PlotSpec> 32x11 (11 columns), page 6x4 in
#> 
#> ── layers
#> • mark_point(x = wt, y = mpg, color = hp)
```

Write to a file with `render_plot()` (the format follows the extension):


``` r
p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
render_plot(p, "cars.png")
```

## What's included

* Marks: `mark_point()`, `mark_line()`, `mark_rule()`, `mark_bar()` (explicit
  heights, or row counts per category).
* Encodings (tidy-eval): `x`, `y`, `color`/`fill`, `size`, `shape`, `alpha`.
* Position scales (`scale_x_continuous()`, `scale_y_continuous()`; linear and
  `log10`) with auto-trained, expanded domains; discrete band scales for
  categorical axes.
* Colour scales (`scale_color_continuous()`, `scale_color_discrete()`) and a
  trained size scale, with stacked legends.
* Trained axes, a panel with gridlines, and layering on one panel.
* Faceting (`facet_wrap()`, `facet_grid()`) with shared or free scales, via the
  `resolve_scale()` lattice.
* Statistical marks: `mark_histogram()`, `mark_density()`, `mark_summary()`,
  `mark_smooth()` (with `after_stat()`).
* Coordinate systems: `coord_flip()`, `coord_fixed()`, `coord_polar()`
  (pie / coxcomb / radar).
* Position adjustments: stack / dodge / fill bars, jittered points.
* `mark_datashade()` for million-point density rasters.
* Themes (`theme_gray()` default, `theme_minimal()`, `theme_bw()`,
  `theme_classic()`, `theme_void()`, `theme()` / `set_theme()`) and multi-plot
  composition (`hconcat()`, `vconcat()`, `concat()`, `wrap_plots()`, `inset()`,
  `repeat_()`).

Spatial (`sf`) and network (`igraph`) layers are on the roadmap — see `NEWS.md`
and `_docs/DESIGN.md`.
