# Draw images at data points

`mark_image()` draws a bitmap image (e.g. a flag or logo) at each
`(x, y)`, replacing the usual point marker. `src` is a column of local
image file paths (per datum) or a single constant path (the same image
at every point). Images are sized by `size` (height in millimetres);
each image's native aspect ratio is preserved.

## Usage

``` r
mark_image(
  plot,
  ...,
  position = "identity",
  src = NULL,
  size = 5,
  nudge_x = 0,
  nudge_y = 0,
  interpolate = TRUE,
  blend = NULL,
  data = NULL
)
```

## Arguments

- plot:

  A
  [PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md)
  (from
  [`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)).

- ...:

  Encodings (tidy-eval): `x`, `y`, and `src` (the image path column).

- position:

  Position adjustment: a string — `"identity"` (default), `"nudge"`,
  `"stack"`, `"fill"`, `"dodge"`, `"dodge2"`, `"jitter"`,
  `"jitterdodge"`, `"sina"` — or a parameterised
  [position](https://r-vellum.github.io/vellumplot/reference/position.md)
  object such as
  [`position_dodge()`](https://r-vellum.github.io/vellumplot/reference/position.md)
  or
  [`position_stack()`](https://r-vellum.github.io/vellumplot/reference/position.md).
  Every mark takes `"identity"` and `"nudge"`. `"stack"`/`"fill"` work
  on bars, areas, points, text and labels (so a label can sit on its
  stacked bar); `"dodge"`/`"dodge2"` on bars, points, text, labels,
  error bars, line/point ranges, cross-bars, box plots, violins and
  slab-intervals (so error bars dodge over dodged bars); `"jitter"` on
  points, text and labels; `"jitterdodge"` and `"sina"` on points. A
  position a mark does not support is an error naming the ones it does.

- src:

  Image source: a column of file paths (mapped, one per datum) or a
  single constant path (the same image at every point).

- size:

  Image height in millimetres (constant or mapped); the width follows
  the image's own aspect ratio. Default `5`.

- nudge_x, nudge_y:

  Shift each image by an exact absolute distance in millimetres (`+x`
  right, `+y` up). Default `0`.

- interpolate:

  Smoothly interpolate when scaling (default `TRUE`)? `FALSE` keeps hard
  pixel edges.

- blend:

  Optional blend mode for compositing this layer against what is already
  drawn beneath it (the panel and earlier layers), one of the CSS
  `mix-blend-mode` names, e.g. `"multiply"`, `"screen"`, `"darken"`. The
  whole layer composites as one isolated group (not per element).

- data:

  Optional layer data frame; overrides the plot data for this layer.

## Value

The modified
[PlotSpec](https://r-vellum.github.io/vellumplot/reference/PlotSpec.md).

## Details

Reading image files requires the suggested
[magick](https://docs.ropensci.org/magick/) package (which decodes PNG,
JPEG, SVG, GIF and more); `mark_image()` errors with an install hint if
it is not available.

## Examples

``` r
if (FALSE) { # \dontrun{
d <- data.frame(x = 1:3, y = c(2, 1, 3),
                flag = c("de.png", "fr.png", "it.png"))
vplot(d) |> mark_image(x = x, y = y, src = flag, size = 8)
} # }
```
