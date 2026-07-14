# Effects and themes

Everything so far has been about mapping data. This article is about the
rest: the panel, the gridlines, the fonts, and a set of render effects
that change how marks are painted. vellumplot keeps these separate. A
*theme* controls the non-data furniture of the whole plot; a *layer
effect* changes how one mark is drawn; a *gradient* is a fancy fill
value; and *sketch* mode turns the whole thing hand-drawn.

## Built-in themes

A theme sets the look of everything that is not a mark. vellumplot ships
several.
[`theme_gray()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
is the default (grey panel, white gridlines);
[`theme_minimal()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
drops the panel fill;
[`theme_bw()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
is a white panel with light grey gridlines;
[`theme_classic()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
gives axis lines and no gridlines;
[`theme_void()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
strips everything but the marks, legend, and titles.

``` r

vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(cyl)) |>
  theme_minimal()
```

![](effects-and-themes_files/figure-html/unnamed-chunk-2-1.png)

## Customising with theme elements

[`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
overrides individual slots, and each slot is described by a typed
*element*:
[`element_text()`](https://r-vellum.github.io/vellumplot/reference/element.md),
[`element_line()`](https://r-vellum.github.io/vellumplot/reference/element.md),
[`element_rect()`](https://r-vellum.github.io/vellumplot/reference/element.md),
or
[`element_blank()`](https://r-vellum.github.io/vellumplot/reference/element.md)
to draw nothing. Slot names follow the familiar dotted scheme
(`panel.grid.minor`, `axis.title`, `plot.title`, and so on), and any
property left `NULL` is inherited from its parent in the theme tree.

``` r

vplot(mtcars) |>
  mark_point(x = wt, y = mpg) |>
  theme_bw() |>
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(color = "grey30")
  ) |>
  labs(title = "Fuel economy")
```

![](effects-and-themes_files/figure-html/unnamed-chunk-3-1.png)

## Layer effects

Effects change how a single mark is painted, and they are passed to a
mark’s `effects` argument as a list. They apply to stroked and point
marks.
[`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md)
draws widened, low-opacity copies beneath the mark for a neon halo;
[`shadow()`](https://r-vellum.github.io/vellumplot/reference/shadow.md)
adds an offset drop shadow;
[`outline()`](https://r-vellum.github.io/vellumplot/reference/outline.md)
puts a contrasting halo behind the mark so it stays legible over a busy
backdrop.
[`motion()`](https://r-vellum.github.io/vellumplot/reference/motion.md)
and
[`echo()`](https://r-vellum.github.io/vellumplot/reference/motion.md)
draw a fading trail of copies marching off along a direction — a
speed-blur
([`motion()`](https://r-vellum.github.io/vellumplot/reference/motion.md),
many close copies) or discrete ghost repeats
([`echo()`](https://r-vellum.github.io/vellumplot/reference/motion.md)).

``` r

vplot(pressure) |>
  mark_line(
    x = temperature, y = pressure,
    effects = list(shadow(), outline(color = "white", size = 2))
  )
```

![](effects-and-themes_files/figure-html/unnamed-chunk-4-1.png)

``` r

vplot(mtcars) |>
  mark_point(x = wt, y = mpg, size = 4, effects = list(motion(x = 4)))
```

![](effects-and-themes_files/figure-html/unnamed-chunk-5-1.png)

[`glow()`](https://r-vellum.github.io/vellumplot/reference/glow.md)
pairs naturally with
[`theme_cyberpunk()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md),
a dark neon theme.

``` r

vplot(mtcars) |>
  mark_point(x = wt, y = mpg, color = factor(cyl), effects = list(glow())) |>
  theme_cyberpunk()
```

![](effects-and-themes_files/figure-html/unnamed-chunk-6-1.png)

## Gradient fills

A gradient is an unscaled *value* for the `fill` aesthetic, not a mapped
scale. Pass
[`linear_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md)
or
[`radial_gradient()`](https://r-vellum.github.io/vellumplot/reference/gradients.md)
directly as a fill and the region is painted with it as one paint. Using
a transparent stop gives the “fade out under a line” look.

``` r

vplot(pressure) |>
  mark_area(
    x = temperature, y = pressure,
    fill = linear_gradient(c("#00e5ff", "#00e5ff00"))
  ) |>
  mark_line(x = temperature, y = pressure, color = "#00e5ff") |>
  theme_cyberpunk()
```

![](effects-and-themes_files/figure-html/unnamed-chunk-7-1.png)

Because a gradient is one paint per region, it cannot be mapped to a
data column; for that you want a colour scale (see **[Scales and
guides](https://r-vellum.github.io/vellumplot/articles/scales-and-guides.md)**).

## Hand-drawn sketch mode

[`theme_sketch()`](https://r-vellum.github.io/vellumplot/reference/theme_sketch.md)
turns an entire plot hand-drawn in one line: wobbly gridlines, axis
lines, and marks, on a warm paper background. The look is generated
natively in the engine, so it is exact and works across PNG, SVG, and
PDF.

``` r

vplot(mtcars) |>
  mark_bar(x = factor(cyl), fill = factor(cyl)) |>
  theme_sketch()
```

![](effects-and-themes_files/figure-html/unnamed-chunk-8-1.png)

For finer control,
[`sketch()`](https://r-vellum.github.io/vellumplot/reference/sketch.md)
is the vocabulary: `roughness`, `bowing`, `fill_style` (`"hachure"`,
`"crosshatch"`, `"zigzag"`, and more), and so on. Pass a
[`sketch()`](https://r-vellum.github.io/vellumplot/reference/sketch.md)
value to a single mark’s `sketch =` argument to rough up just that
layer, or to an
[`element_line()`](https://r-vellum.github.io/vellumplot/reference/element.md)
/
[`element_rect()`](https://r-vellum.github.io/vellumplot/reference/element.md)
slot inside
[`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md).
Set `sketch = NA` on a mark to force it crisp even under a plot-wide
[`theme_sketch()`](https://r-vellum.github.io/vellumplot/reference/theme_sketch.md).

``` r

vplot(mtcars) |>
  mark_bar(
    x = factor(cyl), fill = factor(cyl),
    sketch = sketch(roughness = 2, fill_style = "crosshatch")
  )
```

![](effects-and-themes_files/figure-html/unnamed-chunk-9-1.png)
