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
  blend = NULL
) {
  quos <- c(dots, extra)
  split <- .split_encodings(quos)
  layer <- LayerSpec(
    mark = mark,
    encoding = split$encoding,
    params = split$params,
    stat = stat,
    stat_params = stat_params,
    position = position,
    blend = .check_blend(blend)
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
  blend = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "point",
    rlang::enquos(...),
    rlang::enquos(size = size, shape = shape),
    position = position,
    stat_params = list(auto = isTRUE(auto), seed = seed),
    blend = blend
  )
}

#' @rdname mark_point
#' @export
mark_line <- function(plot, ..., blend = NULL) {
  .check_plot(plot)
  .add_layer(plot, "line", rlang::enquos(...), blend = blend)
}

#' @rdname mark_point
#' @export
mark_rule <- function(plot, ..., blend = NULL) {
  .check_plot(plot)
  .add_layer(plot, "rule", rlang::enquos(...), blend = blend)
}

#' @rdname mark_point
#' @details
#' `mark_bar()` draws bars from a zero baseline. With an explicit `y` it uses the
#' `y` values as heights; with no `y` it counts rows per category (the `"count"`
#' stat). When `color`/`fill` is mapped, grouped bars are stacked by default; use
#' `position = "dodge"` for side-by-side bars or `"fill"` to normalise to 1.
#' @export
mark_bar <- function(plot, ..., position = "stack", blend = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "bar",
    rlang::enquos(...),
    position = position,
    blend = blend
  )
}

#' Statistical marks
#'
#' Marks that apply a statistical transform before drawing. `mark_histogram()`
#' bins a continuous `x` and draws the per-bin counts as bars. `mark_smooth()`
#' fits a model (`"lm"` for now) of `y` on `x` and draws the fitted line, with a
#' confidence ribbon when `se = TRUE`.
#'
#' @param plot A [PlotSpec].
#' @param ... Encodings (tidy-eval), e.g. `x`, `y`, `color`/`fill`.
#' @param bins Number of histogram bins.
#' @param method Smoothing method; `"lm"` (linear) for now.
#' @param se Draw a confidence ribbon around the smooth?
#' @param level Confidence level for the ribbon.
#' @param position Position adjustment for the histogram bars (`"stack"`,
#'   `"dodge"`, `"fill"`).
#' @param blend Optional blend mode (CSS `mix-blend-mode`) for compositing the
#'   layer against the backdrop; see [mark_point()].
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
  blend = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "bar",
    rlang::enquos(...),
    stat = "bin",
    stat_params = list(bins = bins),
    position = position,
    blend = blend
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
  blend = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "smooth",
    rlang::enquos(...),
    stat = "smooth",
    stat_params = list(method = method, se = se, level = level),
    blend = blend
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
#' @param plot A [PlotSpec].
#' @param ... Encodings; `x` and `y` are required.
#' @param width,height Aggregation grid size in cells (output raster pixels).
#' @param colors Two or more colours forming the low-to-high density ramp.
#' @param how Density-to-colour mapping: `"eq_hist"` (default), `"log"`,
#'   `"cbrt"`, or `"linear"`.
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
  how = "eq_hist"
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
    )
  )
}

#' Area, ribbon, and step marks
#'
#' `mark_area()` fills the region between a `y` line and the zero baseline;
#' `mark_ribbon()` fills between `ymin` and `ymax`; `mark_step()` draws a
#' staircase line. All connect points in `x` order.
#'
#' @param plot A [PlotSpec].
#' @param ... Encodings (tidy-eval): `x` and `y` for area/step; `x`, `ymin`,
#'   `ymax` for ribbon; plus `color`/`fill`/`alpha`.
#' @param direction For `mark_step()`, `"hv"` (horizontal then vertical, default)
#'   or `"vh"`.
#' @param blend Optional blend mode; see [mark_point()].
#' @return The modified [PlotSpec].
#' @examples
#' vplot(pressure) |> mark_area(x = temperature, y = pressure)
#' @export
mark_area <- function(plot, ..., blend = NULL) {
  .check_plot(plot)
  .add_layer(plot, "area", rlang::enquos(...), blend = blend)
}

#' @rdname mark_area
#' @export
mark_ribbon <- function(plot, ..., blend = NULL) {
  .check_plot(plot)
  .add_layer(plot, "ribbon", rlang::enquos(...), blend = blend)
}

#' @rdname mark_area
#' @export
mark_step <- function(plot, ..., direction = "hv", blend = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "step",
    rlang::enquos(...),
    stat_params = list(direction = direction),
    blend = blend
  )
}

#' Text marks
#'
#' `mark_text()` draws the `label` aesthetic as text at each `(x, y)`;
#' `mark_label()` adds a filled rounded background behind each label. `size` is
#' the font size in points; `angle` (degrees) may be mapped or constant.
#'
#' @param plot A [PlotSpec].
#' @param ... Encodings (tidy-eval): `x`, `y`, `label` (+ `color`, `angle`).
#' @param size Font size in points.
#' @param family,fontface Font family / face (`"plain"`, `"bold"`, `"italic"`,
#'   `"bold.italic"`).
#' @param hjust,vjust Horizontal / vertical justification (constant; `"left"`,
#'   `"centre"`, `"right"`, `"bottom"`, `"top"`, or numeric in `[0, 1]`).
#' @param angle Text rotation in degrees.
#' @param fill For `mark_label()`, the background fill colour.
#' @param blend Optional blend mode; see [mark_point()].
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
  blend = NULL
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
    blend = blend
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
  blend = NULL
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
    blend = blend
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
#' @param plot A [PlotSpec].
#' @param ... Encodings (tidy-eval): `x`, `y`, `fill` for tile/raster; `x`, `y`
#'   for bin2d; `x` (+ `fill`/`color`) for density.
#' @param bins Number of bins per axis for `mark_bin2d()`.
#' @param adjust Bandwidth multiplier for `mark_density()`.
#' @param blend Optional blend mode; see [mark_point()].
#' @return The modified [PlotSpec].
#' @examples
#' d <- expand.grid(x = 1:5, y = 1:5)
#' d$z <- d$x * d$y
#' vplot(d) |> mark_tile(x = x, y = y, fill = z)
#' @export
mark_tile <- function(plot, ..., blend = NULL) {
  .check_plot(plot)
  .add_layer(plot, "tile", rlang::enquos(...), blend = blend)
}

#' @rdname mark_tile
#' @export
mark_raster <- function(plot, ..., blend = NULL) {
  .check_plot(plot)
  .add_layer(plot, "raster", rlang::enquos(...), blend = blend)
}

#' @rdname mark_tile
#' @export
mark_bin2d <- function(plot, ..., bins = 30, blend = NULL) {
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
    blend = blend
  )
}

#' @rdname mark_tile
#' @export
mark_density <- function(plot, ..., adjust = 1, blend = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "area",
    rlang::enquos(...),
    stat = "density",
    stat_params = list(adjust = adjust),
    blend = blend
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
#' @param plot A [PlotSpec].
#' @param ... Encodings (tidy-eval): `x`, `y` for boxplot/summary; `x`, `ymin`,
#'   `ymax` for errorbar/linerange; plus `color`/`fill`.
#' @param width For `mark_errorbar()`, the cap width as a fraction of the band.
#' @param fun For `mark_summary()`, the aggregation function (default `mean`).
#' @param blend Optional blend mode; see [mark_point()].
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg)
#' @export
mark_boxplot <- function(plot, ..., blend = NULL) {
  .check_plot(plot)
  .add_layer(plot, "boxplot", rlang::enquos(...), blend = blend)
}

#' @rdname mark_boxplot
#' @export
mark_errorbar <- function(plot, ..., width = 0.5, blend = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "errorbar",
    rlang::enquos(...),
    rlang::enquos(width = width),
    blend = blend
  )
}

#' @rdname mark_boxplot
#' @export
mark_linerange <- function(plot, ..., blend = NULL) {
  .check_plot(plot)
  .add_layer(plot, "linerange", rlang::enquos(...), blend = blend)
}

#' @rdname mark_boxplot
#' @export
mark_summary <- function(plot, ..., fun = mean, blend = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "point",
    rlang::enquos(...),
    stat = "aggregate",
    stat_params = list(fun = fun),
    blend = blend
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
