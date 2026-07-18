# Statistical marks

Most marks draw the data as given. A statistical mark computes something
from the data first, then draws the result. vellumplot runs that
transform as a stage of the compiler (see **[The
compiler](https://r-vellum.github.io/vellumplot/articles/the-compiler.md)**),
so a histogram is a mark that bins the data as part of being drawn,
rather than something you bin by hand.

## Histograms and densities

[`mark_histogram()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
bins a continuous `x` and draws the per-bin counts as bars. Control the
resolution with `bins`.

``` r

vplot(faithful) |>
  mark_histogram(x = waiting, bins = 25)
```

![](statistical-marks_files/figure-html/unnamed-chunk-2-1.png)

[`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md)
draws a smooth kernel-density estimate of `x` as a filled curve;
`adjust` scales the bandwidth. Densities are on a comparable vertical
scale, so mapping `fill` to a group and overlaying works well.

``` r

vplot(penguins) |>
  mark_density(x = bill_len, fill = species, alpha = 0.4)
```

![](statistical-marks_files/figure-html/unnamed-chunk-3-1.png)

## Marginal distributions

A scatter shows the joint distribution of two variables but hides each
one on its own.
[`add_marginal()`](https://r-vellum.github.io/vellumplot/reference/add_marginal.md)
puts them back: it draws a distribution of `x` along the top edge and of
`y` along the right edge, each on the same scale as the panel so it
lines up with the points. This is the vellumplot counterpart of
`ggExtra::ggMarginal()`.

``` r

vplot(faithful) |>
  mark_point(x = eruptions, y = waiting) |>
  add_marginal()
```

![](statistical-marks_files/figure-html/unnamed-chunk-4-1.png)

Unlike the other marks in this article,
[`add_marginal()`](https://r-vellum.github.io/vellumplot/reference/add_marginal.md)
is a plot modifier rather than a layer, closer to
[`facet_wrap()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
than to
[`mark_density()`](https://r-vellum.github.io/vellumplot/reference/mark_tile.md).
It takes no encoding of its own; it reads `x` and `y` from the first
plain layer (here the
[`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md))
and computes the margins from those. `type = "histogram"` bins the
values instead of smoothing them, and `sides` picks which edges to draw.

``` r

vplot(faithful) |>
  mark_point(x = eruptions, y = waiting) |>
  add_marginal(type = "histogram", sides = "t", bins = 20)
```

![](statistical-marks_files/figure-html/unnamed-chunk-5-1.png)

When the scatter maps a discrete `color` or `fill`, `group = TRUE`
splits each margin the same way, so a per-group density sits above its
points in the matching colour. The scatter’s legend already names the
groups, so none is added.

``` r

vplot(penguins) |>
  mark_point(x = bill_len, y = bill_dep, color = species) |>
  add_marginal(group = TRUE)
```

![](statistical-marks_files/figure-html/unnamed-chunk-6-1.png)

Margins share the panel’s scales and reserve space around a single
panel, so this version does not combine with facets, a flipped or polar
coordinate system, or a fixed aspect ratio.

## Per-group summaries

[`mark_summary()`](https://r-vellum.github.io/vellumplot/reference/mark_boxplot.md)
aggregates `y` within each `x` category using `fun` (the mean by
default) and draws the result. It is the quick way to add group means
over raw data.

``` r

vplot(mtcars) |>
  mark_point(x = factor(cyl), y = mpg, color = "grey60") |>
  mark_summary(x = factor(cyl), y = mpg, fun = median)
```

![](statistical-marks_files/figure-html/unnamed-chunk-7-1.png)

## Smooths

[`mark_smooth()`](https://r-vellum.github.io/vellumplot/reference/mark_histogram.md)
fits a model of `y` on `x` (per group) and draws the fitted line with a
confidence ribbon when `se = TRUE`. It layers naturally over the raw
points.

``` r

vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  mark_smooth(x = wt, y = mpg, se = TRUE, level = 0.95)
```

![](statistical-marks_files/figure-html/unnamed-chunk-8-1.png)

The `method` chooses the fit: `"lm"` (linear), `"loess"` (local
regression, with `span` controlling the neighbourhood), `"glm"` (a
`family` via `method.args`, e.g. logistic), `"gam"` (a penalised smooth,
needs ), and `"rq"` (quantile regression, needs ). The default `"auto"`
picks `loess` for small groups and `gam` for large ones.

``` r

vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  mark_smooth(x = wt, y = mpg, method = "loess", span = 0.9)
```

![](statistical-marks_files/figure-html/unnamed-chunk-9-1.png)

## Group regions: ellipses and hulls

[`mark_ellipse()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md)
and
[`mark_hull()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md)
enclose a set of points in a single region — a way to read group
separation on a scatter without drawing every point’s membership. When a
`color` or `fill` is mapped, each group gets its own region.

[`mark_ellipse()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md)
draws a covariance ellipse (a robust `t` distribution by default, or
`"norm"` / `"euclid"`), following ggplot2’s `stat_ellipse()`:

``` r

vplot(iris) |>
  mark_point(x = Sepal.Length, y = Sepal.Width, color = Species) |>
  mark_ellipse(x = Sepal.Length, y = Sepal.Width, color = Species)
```

![](statistical-marks_files/figure-html/unnamed-chunk-10-1.png)

[`mark_hull()`](https://r-vellum.github.io/vellumplot/reference/mark_ellipse.md)
draws the convex hull instead. Both are unfilled boundaries by default
(matching ggplot2); map or set a `fill` to shade them:

``` r

vplot(iris) |>
  mark_point(x = Sepal.Length, y = Sepal.Width, color = Species) |>
  mark_hull(x = Sepal.Length, y = Sepal.Width, fill = Species, alpha = 0.2)
```

![](statistical-marks_files/figure-html/unnamed-chunk-11-1.png)

## Distributions

Several marks summarise how a single variable is distributed.
[`mark_ecdf()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
draws the empirical cumulative distribution of `x` as a step, which is a
scale-free way to compare groups without choosing a bandwidth.

``` r

vplot(penguins) |>
  mark_ecdf(x = bill_len, color = species)
```

![](statistical-marks_files/figure-html/unnamed-chunk-12-1.png)

[`mark_qq()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
plots the sorted `sample` against the quantiles of a reference
distribution (normal by default);
[`mark_qq_line()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
adds the reference line through the quartiles. Points on the line mean
the sample matches the reference.

``` r

vplot(mtcars) |>
  mark_qq(sample = mpg) |>
  mark_qq_line(sample = mpg)
```

![](statistical-marks_files/figure-html/unnamed-chunk-13-1.png)

[`mark_rug()`](https://r-vellum.github.io/vellumplot/reference/mark_ecdf.md)
adds marginal ticks at each observation, a compact companion to a
scatter or density. `sides` picks the edges (`"b"`, `"l"`, `"t"`,
`"r"`).

``` r

vplot(faithful) |>
  mark_point(x = waiting, y = eruptions) |>
  mark_rug()
```

![](statistical-marks_files/figure-html/unnamed-chunk-14-1.png)

## Density shapes

[`mark_violin()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
draws a mirrored kernel density of `y` for each categorical `x`, a
boxplot’s silhouette that shows the full shape of each group.

``` r

vplot(penguins) |>
  mark_violin(x = species, y = bill_len, fill = species)
```

![](statistical-marks_files/figure-html/unnamed-chunk-15-1.png)

[`mark_ridgeline()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
turns that on its side: a density of `x` per categorical `y`, with the
ridges overlapping so many groups fit in little vertical space. Use the
`scale` argument to tune how much they overlap.

``` r

vplot(penguins) |>
  mark_ridgeline(x = bill_len, y = species, fill = species)
```

![](statistical-marks_files/figure-html/unnamed-chunk-16-1.png)

[`mark_dotplot()`](https://r-vellum.github.io/vellumplot/reference/mark_violin.md)
bins `x` and stacks one dot per observation, so the height of each stack
is a count you can read dot by dot.

``` r

vplot(faithful) |>
  mark_dotplot(x = waiting)
```

![](statistical-marks_files/figure-html/unnamed-chunk-17-1.png)

## 2-D density contours

[`mark_contour()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
estimates the 2-D density of a point cloud and draws its iso-density
contour lines;
[`mark_contour_filled()`](https://r-vellum.github.io/vellumplot/reference/mark_contour.md)
fills the bands between them. Both are coloured by level automatically.
They read best over the points they summarise:

``` r

vplot(faithful) |>
  mark_point(x = eruptions, y = waiting, color = "grey70") |>
  mark_contour(x = eruptions, y = waiting)
```

![](statistical-marks_files/figure-html/unnamed-chunk-18-1.png)

``` r

vplot(faithful) |>
  mark_contour_filled(x = eruptions, y = waiting)
```

![](statistical-marks_files/figure-html/unnamed-chunk-19-1.png)

The density estimate uses
[`MASS::kde2d()`](https://rdrr.io/pkg/MASS/man/kde2d.html); tune the
levels with `bins`, `binwidth`, or explicit `breaks`. To contour a
*surface* you already have (a `z` value on a regular `x`/`y` grid)
rather than a point density, map `z`:

``` r

grid <- expand.grid(x = seq(-3, 3, 0.1), y = seq(-3, 3, 0.1))
grid$z <- with(grid, dnorm(x) * dnorm(y))
vplot(grid) |> mark_contour(x = x, y = y, z = z)
```

![](statistical-marks_files/figure-html/unnamed-chunk-20-1.png)

Contour tracing needs the `isoband` package (and `MASS` for the density
estimate).

## Reaching computed variables with after_stat

A statistical mark produces new variables that were not in your data: a
histogram computes a `count` and a `density`, for example.
[`after_stat()`](https://r-vellum.github.io/vellumplot/reference/after_stat.md)
lets an encoding refer to one of those computed variables instead of a
data column. The classic use is a density-scaled histogram.

``` r

vplot(faithful) |>
  mark_histogram(x = waiting, bins = 25, fill = after_stat(density))
```

![](statistical-marks_files/figure-html/unnamed-chunk-21-1.png)

Because the aesthetic is now driven by a computed value, its scale
trains on that value like any other, and you get the matching legend.

## Millions of points with mark_datashade

When there are too many points to draw one marker each (overplotted, up
to millions), individual markers become uninformative and slow to draw.
[`mark_datashade()`](https://r-vellum.github.io/vellumplot/reference/mark_datashade.md)
bins the points into a canvas-sized grid in a single pass and colours
each cell by density, drawing one raster. Its cost is decoupled from the
number of points.

``` r

n <- 5e5
big <- data.frame(
  x = rnorm(n),
  y = rnorm(n) + rep(c(-1, 1), each = n / 2)
)
vplot(big) |>
  mark_datashade(x = y, y = x, how = "eq_hist")
```

![](statistical-marks_files/figure-html/unnamed-chunk-22-1.png)

Here `how = "eq_hist"` uses histogram equalisation so both dense and
sparse regions stay visible; the grid resolution is set by `width` and
`height`.

Map a discrete `color` (or `fill`) aesthetic to shade **categorically**
(datashader’s `count_cat`): each category is aggregated separately and
every cell is coloured by the count-weighted mix of the categories it
holds, opacity by density — with a colour legend.

``` r

big$g <- sample(c("a", "b"), n, replace = TRUE)
vplot(big) |>
  mark_datashade(x = y, y = x, color = g)
```

![](statistical-marks_files/figure-html/unnamed-chunk-23-1.png)

[`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
also has an `auto = TRUE` switch that falls back to a datashaded raster
automatically when a layer has very many rows, so you can keep writing
[`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md)
and let vellumplot choose the representation.
