# The plot specification

`PlotSpec` is the S7 class that
[`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md)
creates and the `mark_*()` / `scale_*()` functions extend. It is a
plain, inspectable, serializable data object: data, a list of layers, a
list of scale overrides, and the page size. Nothing is drawn until it is
compiled with
[`vellum::as_vellum_scene()`](https://r-vellum.github.io/vellum/reference/as_vellum_scene.html)
(e.g. via
[`render_plot()`](https://r-vellum.github.io/vellumplot/reference/render_plot.md)).
Printing it draws the plot; inspect its structure with
[`base::summary()`](https://rdrr.io/r/base/summary.html).

## Usage

``` r
PlotSpec(
  data = NULL,
  edge_data = NULL,
  layers = list(),
  scales = list(),
  facet = NULL,
  coord = NULL,
  resolve = list(),
  width = 6,
  height = 4,
  dpi = 96,
  theme = NULL,
  labels = list(),
  marginal = NULL,
  selections = list(),
  filters = list(),
  binds = list(),
  clip = NULL,
  transition = NULL,
  ease = NULL
)
```

## Arguments

- data:

  The data frame.

- edge_data:

  For a graph plot (from
  [`vgraph()`](https://r-vellum.github.io/vellumplot/reference/vgraph.md)),
  the edge table; the default data for
  [`mark_edges()`](https://r-vellum.github.io/vellumplot/reference/mark_graph.md).
  `NULL` for ordinary plots.

- layers:

  A list of layer specifications (one per `mark_*()`).

- scales:

  A list of declared scale overrides.

- facet:

  A faceting specification (from
  [`facet_wrap()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)
  /
  [`facet_grid()`](https://r-vellum.github.io/vellumplot/reference/facet_wrap.md)),
  or `NULL` for a single panel.

- coord:

  A coordinate specification (from
  [`coord_cartesian()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  /
  [`coord_flip()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)
  /
  [`coord_fixed()`](https://r-vellum.github.io/vellumplot/reference/coord_cartesian.md)),
  or `NULL` for the default Cartesian system.

- resolve:

  A named list mapping an aesthetic to `"shared"` or `"independent"`
  (the scale-resolution lattice; see
  [`resolve_scale()`](https://r-vellum.github.io/vellumplot/reference/resolve_scale.md)).

- width, height:

  Page size in inches.

- dpi:

  Output resolution in dots per inch (pixels per inch). The exported
  PNG's pixel dimensions are `width * dpi` by `height * dpi`.

- theme:

  A theme (a named list of resolved element/setting overrides, from
  [`theme()`](https://r-vellum.github.io/vellumplot/reference/theme_gray.md)
  / a `theme_*()` preset), or `NULL` for the default theme.

- labels:

  A named list of plot/axis/legend label overrides (see
  [`labs()`](https://r-vellum.github.io/vellumplot/reference/labs.md)).

- marginal:

  A marginal-distribution specification (from
  [`add_marginal()`](https://r-vellum.github.io/vellumplot/reference/add_marginal.md)),
  or `NULL` for no marginals.

- selections:

  A list of interactive selection declarations (from
  [`select_point()`](https://r-vellum.github.io/vellumplot/reference/select_point.md)
  /
  [`select_interval()`](https://r-vellum.github.io/vellumplot/reference/select_point.md)
  /
  [`add_selection()`](https://r-vellum.github.io/vellumplot/reference/add_selection.md)).

- filters:

  A list of filter declarations (from
  [`filter_by()`](https://r-vellum.github.io/vellumplot/reference/filter_by.md)).

- binds:

  A list of scale-domain bind declarations (from
  [`bind_scale()`](https://r-vellum.github.io/vellumplot/reference/bind_scale.md)).

- clip:

  A
  [`clip_to()`](https://r-vellum.github.io/vellumplot/reference/clip.md)
  /
  [`set_mask()`](https://r-vellum.github.io/vellumplot/reference/clip.md)
  geometry clip (a `ClipSpec`), or `NULL` for none.

- transition:

  A keyframe-animation transition (from
  [`transition_states()`](https://r-vellum.github.io/vellumplot/reference/transition_states.md)
  /
  [`transition_time()`](https://r-vellum.github.io/vellumplot/reference/transition_time.md)
  /
  [`transition_reveal()`](https://r-vellum.github.io/vellumplot/reference/transition_reveal.md)),
  or `NULL`. Read only by
  [`animate()`](https://r-vellum.github.io/vellumplot/reference/animate.md);
  inert on a static render.

- ease:

  An animation easing (from
  [`ease_aes()`](https://r-vellum.github.io/vellumplot/reference/ease_aes.md)),
  or `NULL`.

## Value

A `PlotSpec`.

## See also

[`vplot()`](https://r-vellum.github.io/vellumplot/reference/vplot.md),
[`mark_point()`](https://r-vellum.github.io/vellumplot/reference/mark_point.md),
[`scale_x_continuous()`](https://r-vellum.github.io/vellumplot/reference/scale_x_continuous.md)
