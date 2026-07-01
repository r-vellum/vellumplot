#' @include classes.R
NULL

# Split captured aesthetic quosures into data-mapped channels vs constant
# params. A quosure whose expression is a bare literal (number, string,
# logical) is a constant aesthetic (e.g. `size = 3`, `color = "red"`); anything
# referring to data (a symbol like `wt`, or a call like `factor(cyl)`) is a
# channel evaluated against the data at compile time.
.split_encodings <- function(quos) {
  encoding <- list()
  params <- list()
  for (nm in names(quos)) {
    q <- quos[[nm]]
    if (rlang::quo_is_null(q)) {
      next
    }
    e <- rlang::quo_get_expr(q)
    if (is.call(e) && identical(rlang::call_name(e), "after_stat")) {
      # after_stat(expr): a stage-2 channel evaluated against the stat output.
      inner <- rlang::new_quosure(e[[2]], rlang::quo_get_env(q))
      encoding[[nm]] <- channel(expr = inner, after = TRUE)
    } else if (!is.symbol(e) && !is.call(e)) {
      # syntactic literal -> constant aesthetic
      params[[nm]] <- rlang::eval_tidy(q)
    } else {
      encoding[[nm]] <- channel(expr = q)
    }
  }
  list(encoding = encoding, params = params)
}

#' Map an aesthetic to a statistic computed by a stat
#'
#' Used inside a `mark_*()` encoding to reference a variable produced by the
#' layer's statistical transform rather than a raw data column, e.g.
#' `y = after_stat(count)` or `y = after_stat(density)`.
#'
#' @param x An expression in terms of the stat's computed variables.
#' @return Its argument (the marker is interpreted at compile time).
#' @export
after_stat <- function(x) x

# The CSS mix-blend-mode set vellum's viewport(blend=) accepts.
.BLEND_MODES <- c(
  "normal",
  "multiply",
  "screen",
  "overlay",
  "darken",
  "lighten",
  "color-dodge",
  "color-burn",
  "hard-light",
  "soft-light",
  "difference",
  "exclusion",
  "hue",
  "saturation",
  "color",
  "luminosity"
)

.check_blend <- function(blend, call = rlang::caller_env()) {
  if (is.null(blend)) {
    return("normal")
  }
  modes <- .BLEND_MODES
  if (!is.character(blend) || length(blend) != 1L || !blend %in% modes) {
    cli::cli_abort(
      "{.arg blend} must be one of {.or {.val {modes}}}.",
      call = call
    )
  }
  blend
}

# Capture `...` plus the explicit geometry args, append a LayerSpec.
.add_layer <- function(
  plot,
  mark,
  dots,
  extra = list(),
  stat = "identity",
  stat_params = list(),
  position = "identity",
  blend = NULL,
  data = NULL,
  z = 0L
) {
  if (!is.null(data) && !is.data.frame(data)) {
    cli::cli_abort(
      "Layer {.arg data} must be a data frame, not {.obj_type_friendly {data}}."
    )
  }
  quos <- c(dots, extra)
  split <- .split_encodings(quos)
  layer <- LayerSpec(
    mark = mark,
    encoding = split$encoding,
    params = split$params,
    stat = stat,
    stat_params = stat_params,
    position = position,
    blend = .check_blend(blend),
    data = data,
    z = as.integer(z)
  )
  plot@layers <- c(plot@layers, list(layer))
  plot
}

#' Add marks to a plot
#'
#' Each `mark_*()` appends a drawing layer to a [PlotSpec]. Encodings are bare
#' column names (or expressions) captured with tidy evaluation, e.g.
#' `x = wt, y = mpg, color = hp`. Scalar values (e.g. `size = 3`,
#' `color = "red"`) are treated as constant aesthetics rather than data
#' mappings.
#'
#' @param plot A [PlotSpec] (from [vplot()]).
#' @param ... Encodings: named channel expressions such as `x`, `y`, `color`,
#'   `fill`, `size`, `shape`, `alpha`.
#' @param size,shape Convenience arguments for the point size (in mm) / shape;
#'   may be a constant or a mapped expression. One of `"circle"`, `"square"`,
#'   `"triangle"`, `"diamond"`, `"plus"`, `"cross"`.
#' @param position Position adjustment: `"identity"` (default), `"jitter"`
#'   (points), or `"stack"` / `"dodge"` / `"fill"` (bars).
#' @param auto For `mark_point()`, when `TRUE` and the layer has very many rows,
#'   automatically render it as a datashaded density raster (see
#'   [mark_datashade()]) instead of individual markers.
#' @param seed For `mark_point(position = "jitter")`, an optional integer seed
#'   making the jitter reproducible. The global RNG stream is restored afterwards.
#' @param blend Optional blend mode for compositing this layer against what is
#'   already drawn beneath it (the panel and earlier layers), one of the CSS
#'   `mix-blend-mode` names, e.g. `"multiply"`, `"screen"`, `"darken"`. The whole
#'   layer composites as one isolated group (not per element).
#' @param data Optional layer data frame; overrides the plot data for this layer.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
#' @export
mark_point <- function(
  plot,
  ...,
  size = NULL,
  shape = NULL,
  position = "identity",
  auto = FALSE,
  seed = NULL,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "point",
    rlang::enquos(...),
    rlang::enquos(size = size, shape = shape),
    position = position,
    stat_params = list(auto = isTRUE(auto), seed = seed),
    blend = blend,
    data = data
  )
}

#' @rdname mark_point
#' @export
mark_line <- function(plot, ..., blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(plot, "line", rlang::enquos(...), blend = blend, data = data)
}

#' Draw simple-feature (sf) geometries
#'
#' `mark_sf()` draws the geometry column of an `sf` object as a map layer:
#' `POINT`/`MULTIPOINT` render as points, `LINESTRING`/`MULTILINESTRING` as
#' polylines, and `POLYGON`/`MULTIPOLYGON` as filled paths (holes cut with the
#' even-odd rule, so ring winding need not be canonical). Coordinates come from
#' the geometry, so there are no `x`/`y` encodings; other aesthetics map feature
#' attributes as usual, e.g. `fill = AREA` for a choropleth. Pair with
#' [coord_sf()] to reproject and lock the map aspect ratio.
#'
#' `sf` is an optional dependency (in `Suggests`); `mark_sf()` errors with an
#' install hint if it is not available.
#'
#' @param plot A [PlotSpec] (from [vplot()]).
#' @param ... Encodings mapping feature attributes to aesthetics: `fill`,
#'   `color`, `alpha`, `linewidth`, `size`. A geometry column is not encoded —
#'   it is read from the data.
#' @param fill,color,alpha,linewidth,size Convenience aesthetic arguments; a
#'   constant or a mapped expression.
#' @param na_value Fill colour for features whose mapped `fill`/`color` value is
#'   `NA` (drawn as a distinct legend swatch). Default `"grey80"`.
#' @param blend Optional blend mode (see [mark_point()]).
#' @param data Optional layer data (an `sf` object); overrides the plot data.
#' @return The modified [PlotSpec].
#' @seealso [coord_sf()], [scale_fill_binned()]
#' @examples
#' \dontrun{
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' vplot(nc) |> mark_sf(fill = BIR74) |> coord_sf()
#' }
#' @export
mark_sf <- function(
  plot,
  ...,
  fill = NULL,
  color = NULL,
  alpha = NULL,
  linewidth = NULL,
  size = NULL,
  na_value = "grey80",
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  .need_pkg("sf", "mark_sf()")
  .add_layer(
    plot,
    "sf",
    rlang::enquos(...),
    rlang::enquos(
      fill = fill,
      color = color,
      alpha = alpha,
      linewidth = linewidth,
      size = size
    ),
    stat_params = list(na_value = na_value),
    blend = blend,
    data = data
  )
}

#' @rdname mark_point
#' @export
mark_rule <- function(plot, ..., blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(plot, "rule", rlang::enquos(...), blend = blend, data = data)
}

#' @rdname mark_point
#' @details
#' `mark_bar()` draws bars from a zero baseline. With an explicit `y` it uses the
#' `y` values as heights; with no `y` it counts rows per category (the `"count"`
#' stat). When `color`/`fill` is mapped, grouped bars are stacked by default; use
#' `position = "dodge"` for side-by-side bars or `"fill"` to normalise to 1.
#' @export
mark_bar <- function(plot, ..., position = "stack", blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "bar",
    rlang::enquos(...),
    position = position,
    blend = blend,
    data = data
  )
}

#' Pie and donut charts
#'
#' Convenience marks for part-of-whole charts. `mark_pie()` draws a pie: each
#' `value` becomes a wedge whose angle is its share of the total, coloured by
#' `fill`. `mark_donut()` is a pie with a hollow centre (`hole`, a fraction of
#' the radius). Both are shorthand for a stacked bar projected through
#' [coord_polar()] with `theta = "y"`, which they set on the plot; they error if
#' the plot already carries a non-polar coordinate.
#'
#' @param plot A [PlotSpec].
#' @param value Encoding (tidy-eval) for each slice's magnitude.
#' @param fill Encoding (tidy-eval) for the slice colour. Omit for a single
#'   slice.
#' @param hole For `mark_donut()`, the inner-hole radius as a fraction of the rim
#'   (`0` is a pie, the default `0.5` a medium donut).
#' @param ... Further constant aesthetics (e.g. `alpha`).
#' @param data Optional per-layer data frame.
#' @return The modified [PlotSpec].
#' @seealso [coord_polar()]
#' @examples
#' df <- data.frame(part = c("a", "b", "c"), n = c(3, 5, 2))
#' vplot(df) |> mark_pie(value = n, fill = part)
#' vplot(df) |> mark_donut(value = n, fill = part, hole = 0.6)
#' @export
mark_pie <- function(plot, value, fill = NULL, ..., data = NULL) {
  .pie_layer(plot, rlang::enquos(value = value, fill = fill, ...), 0, data)
}

#' @rdname mark_pie
#' @export
mark_donut <- function(plot, value, fill = NULL, hole = 0.5, ..., data = NULL) {
  if (!is.numeric(hole) || hole < 0 || hole >= 1) {
    cli::cli_abort("{.arg hole} must be a fraction in {.val [0, 1)}.")
  }
  .pie_layer(plot, rlang::enquos(value = value, fill = fill, ...), hole, data)
}

# Shared body of mark_pie/mark_donut: a stacked bar (value -> y, a constant
# single-band x) forced through polar theta = "y".
.pie_layer <- function(plot, enc, hole, data) {
  .check_plot(plot)
  if (!is.null(plot@coord) && !identical(plot@coord@kind, "polar")) {
    cli::cli_abort(c(
      "{.fn mark_pie} / {.fn mark_donut} imply a polar coordinate.",
      x = "The plot already has a {.val {plot@coord@kind}} coordinate.",
      i = "Remove the conflicting {.fn coord_*} call."
    ))
  }
  names(enc)[names(enc) == "value"] <- "y"
  plot <- .add_layer(
    plot,
    "bar",
    enc,
    extra = rlang::quos(x = factor(1)),
    position = "stack",
    data = data
  )
  if (is.null(plot@coord)) {
    plot@coord <- CoordSpec(kind = "polar", theta = "y", rmin = hole)
  }
  plot
}

#' Statistical marks
#'
#' Marks that apply a statistical transform before drawing. `mark_histogram()`
#' bins a continuous `x` and draws the per-bin counts as bars. `mark_smooth()`
#' fits a model (`"lm"` for now) of `y` on `x` and draws the fitted line, with a
#' confidence ribbon when `se = TRUE`.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval), e.g. `x`, `y`, `color`/`fill`.
#' @param bins Number of histogram bins.
#' @param method Smoothing method; `"lm"` (linear) for now.
#' @param se Draw a confidence ribbon around the smooth?
#' @param level Confidence level for the ribbon.
#' @param position Position adjustment for the histogram bars (`"stack"`,
#'   `"dodge"`, `"fill"`).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_histogram(x = mpg, bins = 10)
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> mark_smooth(x = wt, y = mpg)
#' @export
mark_histogram <- function(
  plot,
  ...,
  bins = 30,
  position = "stack",
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "bar",
    rlang::enquos(...),
    stat = "bin",
    stat_params = list(bins = bins),
    position = position,
    blend = blend,
    data = data
  )
}

#' @rdname mark_histogram
#' @export
mark_smooth <- function(
  plot,
  ...,
  method = "lm",
  se = TRUE,
  level = 0.95,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "smooth",
    rlang::enquos(...),
    stat = "smooth",
    stat_params = list(method = method, se = se, level = level),
    blend = blend,
    data = data
  )
}

#' Datashade a large point cloud
#'
#' For data too dense to draw one marker each (overplotted, up to millions of
#' points), `mark_datashade()` bins the points into a canvas-sized grid in one
#' pass and colours each cell by density (via [vellum::datashade()]), drawing a
#' single raster that fills the panel. Cost is decoupled from point count and
#' overplotting. Per-point colour/size aesthetics do not apply; cell colour
#' encodes density.
#'
#' @inheritParams mark_point
#' @param ... Encodings; `x` and `y` are required.
#' @param width,height Aggregation grid size in cells (output raster pixels).
#' @param colors Two or more colours forming the low-to-high density ramp. For
#'   an additive per-category overlay, ramp from a transparent/black low end to
#'   the category hue and composite with `blend = "screen"` (see details).
#' @param how Density-to-colour mapping: `"eq_hist"` (default), `"log"`,
#'   `"cbrt"`, or `"linear"`.
#' @details
#' Categorical shading (à la datashader's `count_cat`) has no single-call form,
#' but is reproduced by stacking one datashade layer per category — each with a
#' `colors` ramp from black to its hue — composited with `blend = "screen"`, so
#' overlapping densities mix additively.
#' @return The modified [PlotSpec].
#' @examples
#' n <- 1e5
#' d <- data.frame(x = rnorm(n), y = rnorm(n))
#' vplot(d) |> mark_datashade(x = x, y = y)
#' @export
mark_datashade <- function(
  plot,
  ...,
  width = 400,
  height = 300,
  colors = NULL,
  how = "eq_hist",
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "datashade",
    rlang::enquos(...),
    stat_params = list(
      width = width,
      height = height,
      colors = colors,
      how = how
    ),
    blend = blend,
    data = data
  )
}

#' Area, ribbon, and step marks
#'
#' `mark_area()` fills the region between a `y` line and the zero baseline;
#' `mark_ribbon()` fills between `ymin` and `ymax`; `mark_step()` draws a
#' staircase line. All connect points in `x` order.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x` and `y` for area/step; `x`, `ymin`,
#'   `ymax` for ribbon; plus `color`/`fill`/`alpha`.
#' @param direction For `mark_step()`, `"hv"` (horizontal then vertical, default)
#'   or `"vh"`.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(pressure) |> mark_area(x = temperature, y = pressure)
#' @export
mark_area <- function(plot, ..., blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(plot, "area", rlang::enquos(...), blend = blend, data = data)
}

#' @rdname mark_area
#' @export
mark_ribbon <- function(plot, ..., blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(plot, "ribbon", rlang::enquos(...), blend = blend, data = data)
}

#' @rdname mark_area
#' @export
mark_step <- function(plot, ..., direction = "hv", blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "step",
    rlang::enquos(...),
    stat_params = list(direction = direction),
    blend = blend,
    data = data
  )
}

#' Text marks
#'
#' `mark_text()` draws the `label` aesthetic as text at each `(x, y)`;
#' `mark_label()` adds a filled rounded background behind each label. `size` is
#' the font size in points; `angle` (degrees) may be mapped or constant.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y`, `label` (+ `color`, `angle`).
#' @param size Font size in points.
#' @param family,fontface Font family / face (`"plain"`, `"bold"`, `"italic"`,
#'   `"bold.italic"`).
#' @param hjust,vjust Horizontal / vertical justification (constant; `"left"`,
#'   `"centre"`, `"right"`, `"bottom"`, `"top"`, or numeric in `[0, 1]`).
#' @param angle Text rotation in degrees.
#' @param fill For `mark_label()`, the background fill colour.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_text(x = wt, y = mpg, label = rownames(mtcars))
#' @export
mark_text <- function(
  plot,
  ...,
  size = NULL,
  family = NULL,
  fontface = NULL,
  hjust = "centre",
  vjust = "centre",
  angle = NULL,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "text",
    rlang::enquos(...),
    rlang::enquos(
      size = size,
      family = family,
      fontface = fontface,
      hjust = hjust,
      vjust = vjust,
      angle = angle
    ),
    blend = blend,
    data = data
  )
}

#' @rdname mark_text
#' @export
mark_label <- function(
  plot,
  ...,
  size = NULL,
  family = NULL,
  fontface = NULL,
  hjust = "centre",
  vjust = "centre",
  angle = NULL,
  fill = "white",
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "label",
    rlang::enquos(...),
    rlang::enquos(
      size = size,
      family = family,
      fontface = fontface,
      hjust = hjust,
      vjust = vjust,
      angle = angle,
      fill = fill
    ),
    blend = blend,
    data = data
  )
}

#' Heatmap marks
#'
#' `mark_tile()` draws a rectangular tile at each `(x, y)` coloured by `fill`;
#' `mark_raster()` draws the same as one raster image (a fast path requiring a
#' complete regular grid). `mark_bin2d()` bins continuous `x`/`y` into a grid and
#' colours each cell by count. `mark_density()` draws a 1-D kernel density of `x`
#' as a filled curve.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y`, `fill` for tile/raster; `x`, `y`
#'   for bin2d; `x` (+ `fill`/`color`) for density.
#' @param bins Number of bins per axis for `mark_bin2d()` / hex columns for
#'   `mark_hex()`.
#' @param adjust Bandwidth multiplier for `mark_density()`.
#' @return The modified [PlotSpec].
#' @examples
#' d <- expand.grid(x = 1:5, y = 1:5)
#' d$z <- d$x * d$y
#' vplot(d) |> mark_tile(x = x, y = y, fill = z)
#' @export
mark_tile <- function(plot, ..., blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(plot, "tile", rlang::enquos(...), blend = blend, data = data)
}

#' @rdname mark_tile
#' @export
mark_raster <- function(plot, ..., blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(plot, "raster", rlang::enquos(...), blend = blend, data = data)
}

#' @rdname mark_tile
#' @export
mark_bin2d <- function(plot, ..., bins = 30, blend = NULL, data = NULL) {
  .check_plot(plot)
  dots <- rlang::enquos(...)
  if (is.null(dots$fill) && is.null(dots$color)) {
    dots$fill <- rlang::quo(after_stat(count))
  }
  .add_layer(
    plot,
    "tile",
    dots,
    stat = "bin2d",
    stat_params = list(bins = bins),
    blend = blend,
    data = data
  )
}

#' @rdname mark_tile
#' @export
mark_density <- function(plot, ..., adjust = 1, blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "area",
    rlang::enquos(...),
    stat = "density",
    stat_params = list(adjust = adjust),
    blend = blend,
    data = data
  )
}

#' @rdname mark_tile
#' @export
mark_hex <- function(plot, ..., bins = 30, blend = NULL, data = NULL) {
  .check_plot(plot)
  dots <- rlang::enquos(...)
  if (is.null(dots$fill) && is.null(dots$color)) {
    dots$fill <- rlang::quo(after_stat(count))
  }
  .add_layer(
    plot,
    "hex",
    dots,
    stat = "hexbin",
    stat_params = list(bins = bins),
    blend = blend,
    data = data
  )
}

#' Boxplot, error bar, and summary marks
#'
#' `mark_boxplot()` draws a box-and-whisker per `x` category from the raw `y`
#' values (box = Q1-Q3, median line, 1.5*IQR whiskers, outlier points).
#' `mark_errorbar()` draws vertical bars from `ymin` to `ymax` with horizontal
#' caps; `mark_linerange()` omits the caps. `mark_summary()` aggregates `y` per
#' `x` with `fun` (default mean) and draws the result as points.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y` for boxplot/summary; `x`, `ymin`,
#'   `ymax` for errorbar/linerange; plus `color`/`fill`.
#' @param width For `mark_errorbar()`, the cap width as a fraction of the band.
#' @param fun For `mark_summary()`, the aggregation function (default `mean`).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg)
#' @export
mark_boxplot <- function(plot, ..., blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(plot, "boxplot", rlang::enquos(...), blend = blend, data = data)
}

#' @rdname mark_boxplot
#' @export
mark_errorbar <- function(plot, ..., width = 0.5, blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "errorbar",
    rlang::enquos(...),
    rlang::enquos(width = width),
    blend = blend,
    data = data
  )
}

#' @rdname mark_boxplot
#' @export
mark_linerange <- function(plot, ..., blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(plot, "linerange", rlang::enquos(...), blend = blend, data = data)
}

#' @rdname mark_boxplot
#' @export
mark_summary <- function(plot, ..., fun = mean, blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "point",
    rlang::enquos(...),
    stat = "aggregate",
    stat_params = list(fun = fun),
    blend = blend,
    data = data
  )
}

#' Segment mark
#'
#' `mark_segment()` draws a straight line from `(x, y)` to `(xend, yend)` per row.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y`, `xend`, `yend` (+ `color`,
#'   `linewidth`, `alpha`).
#' @return The modified [PlotSpec].
#' @examples
#' d <- data.frame(x = 1, y = 1, xend = 5, yend = 4)
#' vplot(d) |> mark_segment(x = x, y = y, xend = xend, yend = yend)
#' @export
mark_segment <- function(plot, ..., blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(plot, "segment", rlang::enquos(...), blend = blend, data = data)
}

# Fill in default encoding channels the user did not supply (used by the graph
# marks, whose x/y/xend/yend/label columns are produced by vgraph()).
.with_default_aes <- function(dots, defaults) {
  c(dots, defaults[setdiff(names(defaults), names(dots))])
}

#' Network (graph) marks
#'
#' Draw a node-link diagram on a [PlotSpec] from [vgraph()]. `mark_edges()` draws
#' the edges (straight lines, batched), `mark_nodes()` the vertices (points), and
#' `mark_node_text()` the vertex labels. Draw order is fixed regardless of the
#' order you pipe them: edges under nodes under labels. Edges default to the edge
#' table (`vgraph()`'s `edge_data`), nodes and labels to the node table; the
#' `x`/`y`/`xend`/`yend`/`label`/`name` columns those tables carry are mapped
#' automatically, so bare `mark_edges() |> mark_nodes()` just works.
#'
#' These are thin over the point / segment / text marks; `igraph` need not be
#' installed to use them (only [vgraph()] needs it).
#'
#' @param plot A [PlotSpec], normally from [vgraph()].
#' @param ... Encodings mapping node/edge attributes to aesthetics. Nodes: `size`,
#'   `color`/`fill`, `shape`, `alpha`. Edges: `color`, `linewidth`, `alpha`. The
#'   position channels are supplied by `vgraph()` and need not be mapped.
#' @param size,shape For `mark_nodes()`, the node size (mm) / shape; a constant or
#'   a mapped expression.
#' @param fill,color,alpha Convenience aesthetics; a constant or a mapped
#'   expression. For nodes, `fill` (or `color`) is the marker colour.
#' @param linewidth For `mark_edges()`, the edge width; a constant or (via
#'   [scale_edge_width()]) a mapped expression such as `linewidth = weight`.
#' @param arrow For `mark_edges()`, `TRUE` to draw an arrowhead at each edge's
#'   target end (directed graphs). Heads are not yet capped to the node boundary,
#'   so on very large nodes they may sit slightly under the node.
#' @param label For `mark_node_text()`, the label expression (default the vertex
#'   `name`).
#' @param blend Optional blend mode (see [mark_point()]).
#' @param data Optional layer data; overrides the default table.
#' @return The modified [PlotSpec].
#' @seealso [vgraph()], [scale_edge_width()]
#' @name mark_graph
#' @examples
#' \dontrun{
#' g <- igraph::make_graph("Zachary")
#' vgraph(g) |>
#'   mark_edges(alpha = 0.5) |>
#'   mark_nodes(size = 4, fill = "steelblue")
#' }
NULL

#' @rdname mark_graph
#' @export
mark_edges <- function(
  plot,
  ...,
  color = NULL,
  linewidth = NULL,
  alpha = NULL,
  arrow = FALSE,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  dots <- .with_default_aes(
    rlang::enquos(...),
    rlang::quos(x = x, y = y, xend = xend, yend = yend)
  )
  .add_layer(
    plot,
    "edges",
    dots,
    rlang::enquos(color = color, linewidth = linewidth, alpha = alpha),
    stat_params = list(arrow = isTRUE(arrow)),
    blend = blend,
    data = data %||% plot@edge_data,
    z = 1L
  )
}

#' @rdname mark_graph
#' @export
mark_nodes <- function(
  plot,
  ...,
  size = NULL,
  shape = NULL,
  fill = NULL,
  color = NULL,
  alpha = NULL,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  dots <- .with_default_aes(rlang::enquos(...), rlang::quos(x = x, y = y))
  .add_layer(
    plot,
    "nodes",
    dots,
    rlang::enquos(
      size = size,
      shape = shape,
      fill = fill,
      color = color,
      alpha = alpha
    ),
    blend = blend,
    data = data,
    z = 2L
  )
}

#' @rdname mark_graph
#' @export
mark_node_text <- function(
  plot,
  ...,
  label = NULL,
  color = NULL,
  size = NULL,
  alpha = NULL,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  dots <- .with_default_aes(
    rlang::enquos(...),
    rlang::quos(x = x, y = y, label = name)
  )
  .add_layer(
    plot,
    "node_text",
    dots,
    rlang::enquos(label = label, color = color, size = size, alpha = alpha),
    blend = blend,
    data = data,
    z = 3L
  )
}

.check_plot <- function(plot, call = rlang::caller_env()) {
  if (!S7::S7_inherits(plot, PlotSpec)) {
    cli::cli_abort(
      "{.arg plot} must be a {.cls PlotSpec} from {.fn vplot}.",
      call = call
    )
  }
}
