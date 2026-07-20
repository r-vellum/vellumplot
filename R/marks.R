#' @include classes.R
NULL

# A call to a paint constructor (linear_gradient / radial_gradient), whose result
# is an unscaled fill *value*, not a data-mapped channel. Namespace-qualified
# calls (`vellum::linear_gradient(...)`) match too (call_name strips the `::`).
.is_paint_call <- function(e) {
  nm <- rlang::call_name(e)
  !is.null(nm) && nm %in% c("linear_gradient", "radial_gradient")
}

# If a channel quosure is a bare *symbol* bound (in its own environment) to a
# paint object, return it; else NULL. Used to treat `fill = g` -- a paint held
# in a variable -- as a value. Restricted to symbols so arbitrary channel calls
# (e.g. `y = rnorm(20)`) are never eagerly evaluated here (no double eval / RNG
# side effects); inline `linear_gradient(...)` calls go through `.is_paint_call`.
# A data-column symbol resolves only under the data mask, so it errors here and
# safely falls through to a channel.
.paint_value <- function(q) {
  if (!is.symbol(rlang::quo_get_expr(q))) {
    return(NULL)
  }
  val <- tryCatch(rlang::eval_tidy(q), error = function(e) NULL)
  if (inherits(val, "vellum_gradient")) val else NULL
}

# Split captured aesthetic quosures into data-mapped channels vs constant
# params. A quosure whose expression is a bare literal (number, string,
# logical) is a constant aesthetic (e.g. `size = 3`, `color = "red"`); anything
# referring to data (a symbol like `wt`, or a call like `factor(cyl)`) is a
# channel evaluated against the data at compile time.
# Positional (coordinate) aesthetics. A bare literal on one of these is a
# constant-valued coordinate that must train the position scale (e.g. a segment
# baseline `y = 0`), so it is kept as a channel rather than a style param.
.POSITION_AES <- c(
  "x",
  "y",
  "xend",
  "yend",
  "xmin",
  "xmax",
  "ymin",
  "ymax"
)

.split_encodings <- function(quos) {
  # British spelling: accept `colour` (and compounds like `hover_colour`) as an
  # alias for the American `color` the compiler reads. Normalise once here so
  # every downstream path -- scale training, provenance, alt text -- sees a
  # single spelling; otherwise `mark_*(colour = ...)` is silently dropped.
  if (length(quos)) {
    names(quos) <- sub("colour", "color", names(quos), fixed = TRUE)
  }
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
    } else if (is.call(e) && identical(rlang::call_name(e), "condition")) {
      # color = condition(sel, if_true, if_false): a conditional encoding. Store the
      # `if_true` branch as the channel expr (so labelling / type / training /
      # drawing see the plain encoding -- transparency), and carry the selection +
      # `if_false` + `empty` alongside for the interactive host.
      encoding[[nm]] <- .condition_channel(e, rlang::quo_get_env(q))
    } else if (is.call(e) && .is_paint_call(e)) {
      # fill = linear_gradient(...) -> a paint *value* (constant aesthetic), not a
      # data channel: evaluate it now and store as a param.
      params[[nm]] <- rlang::eval_tidy(q)
    } else if (!is.symbol(e) && !is.call(e)) {
      # syntactic literal. A positional literal (e.g. `y = 0` on a segment
      # baseline) is a constant-valued *coordinate*, not a style param: keep it
      # as a channel so it populates `values`, recycles per row, and trains the
      # position scale. Every other literal (e.g. `size = 3`) is a constant
      # aesthetic.
      if (nm %in% .POSITION_AES) {
        encoding[[nm]] <- channel(expr = q)
      } else {
        params[[nm]] <- rlang::eval_tidy(q)
      }
    } else if (!is.null(pv <- .paint_value(q))) {
      # a pre-built paint bound to a variable (fill = g) is a value, not a channel
      params[[nm]] <- pv
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

# The CSS mix-blend-mode set vellum's vl_viewport(blend=) accepts.
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

# Normalise a mark's / element's `sketch` argument to what the spec stores:
# NULL (inherit), NA (forced crisp), or a `vellum_sketch`. `FALSE` is an alias
# for `NA` (crisp). Anything else is an error.
.check_sketch <- function(sketch, call = rlang::caller_env()) {
  if (is.null(sketch)) {
    return(NULL)
  }
  if (inherits(sketch, "vellum_sketch")) {
    return(sketch)
  }
  if (
    length(sketch) == 1L && is.logical(sketch) && (is.na(sketch) || !sketch)
  ) {
    return(NA) # NA / FALSE -> forced crisp
  }
  cli::cli_abort(
    c(
      "{.arg sketch} must be a {.fn sketch} object, {.code NA} (crisp), or {.code NULL}.",
      i = "See {.fn sketch} and {.fn theme_sketch}."
    ),
    call = call
  )
}

# Normalise a `mark_line(window=)` argument into window stat params, or NULL for
# no window. Accepts a bare op string (`"mean"`) or a list `(op, k, align,
# partial)`. Validates the op and alignment; leaves `k` defaulting per-op to the
# stat.
.check_window <- function(window, call = rlang::caller_env()) {
  if (is.null(window)) {
    return(NULL)
  }
  if (is.character(window) && length(window) == 1L) {
    window <- list(op = window)
  }
  if (!is.list(window) || is.null(window$op)) {
    cli::cli_abort(
      c(
        "{.arg window} must be a window op string or a list with an {.field op}.",
        i = 'For example {.code window = "mean"} or {.code window = list(op = "mean", k = 7)}.'
      ),
      call = call
    )
  }
  ok_ops <- .WINDOW_OPS
  if (!window$op %in% ok_ops) {
    cli::cli_abort(
      "{.arg window} {.field op} must be one of {.or {.val {ok_ops}}}.",
      call = call
    )
  }
  align <- window$align %||% "right"
  if (!align %in% c("right", "left", "center")) {
    cli::cli_abort(
      '{.arg window} {.field align} must be {.val right}, {.val left}, or {.val center}.',
      call = call
    )
  }
  list(
    op = window$op,
    k = window$k,
    align = align,
    partial = window$partial %||% TRUE
  )
}

# Capture `...` plus the explicit geometry args, append a LayerSpec.
.add_layer <- function(
  plot,
  mark,
  dots,
  extra = list(),
  const_params = list(),
  stat = "identity",
  stat_params = list(),
  position = "identity",
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL,
  z = 0L
) {
  if (!is.null(data) && !is.data.frame(data)) {
    cli::cli_abort(
      "Layer {.arg data} must be a data frame, not {.obj_type_friendly {data}}."
    )
  }
  quos <- c(dots, extra)
  # Normalise British spelling up front (`colour` -> `color`, incl. compounds
  # like `hover_colour`) so the interactivity split below -- keyed on the
  # American `.INTERACT_ARGS` -- catches `hover_colour`/`selected_colour` too.
  # `.split_encodings` repeats this normalisation harmlessly (it is idempotent).
  if (length(quos)) {
    names(quos) <- sub("colour", "color", names(quos), fixed = TRUE)
  }
  # Interactivity args (`tooltip`/`data_id`/`hover_group`/`hover_color`/
  # `selected_color`, see `.INTERACT_ARGS`) are reserved, not aesthetics: pull
  # them out before encoding-splitting so they are never scale-trained. They are
  # captured as quosures and resolved per row at compile.
  interactivity <- quos[intersect(names(quos), .INTERACT_ARGS)]
  interactivity <- interactivity[
    !vapply(interactivity, rlang::quo_is_null, logical(1))
  ]
  quos <- quos[setdiff(names(quos), .INTERACT_ARGS)]
  split <- .split_encodings(quos)
  # Pre-evaluated always-constant params (e.g. mm nudges) bypass encoding
  # splitting, so a negative literal like `nudge_y = -3` is not mistaken for a
  # data channel.
  if (length(const_params)) {
    split$params <- utils::modifyList(split$params, const_params)
  }
  # `effects` is a reserved argument, not an aesthetic: catch it arriving via `...`
  # on a mark that does not expose an `effects` argument.
  if ("effects" %in% c(names(split$encoding), names(split$params))) {
    cli::cli_abort(c(
      "{.arg effects} is not an aesthetic.",
      i = "This mark does not take an {.arg effects} argument.",
      i = "Effects are available on stroked and point marks (line/point/step/rule/segment/edges/nodes)."
    ))
  }
  if ("sketch" %in% c(names(split$encoding), names(split$params))) {
    cli::cli_abort(c(
      "{.arg sketch} is not an aesthetic.",
      i = "This mark does not take a {.arg sketch} argument (text, image, raster, hex and datashade marks are never sketched).",
      i = "For a plot-wide hand-drawn look, use {.fn theme_sketch}."
    ))
  }
  # A `position_*()` object splits into the type string (stored on the layer)
  # plus parameters merged into `stat_params`, where the emitters read them.
  pos <- .normalize_position(position)
  stat_params <- utils::modifyList(stat_params, pos$args)
  layer <- LayerSpec(
    mark = mark,
    encoding = split$encoding,
    params = split$params,
    stat = stat,
    stat_params = stat_params,
    position = pos$type,
    blend = .check_blend(blend),
    effects = .check_effects(effects, mark),
    sketch = .check_sketch(sketch),
    data = data,
    z = as.integer(z),
    interactivity = interactivity
  )
  plot@layers <- c(plot@layers, list(layer))
  plot
}

# Reserved, per-row interactivity arguments (declared on any mark_*() via `...`).
# `data_id` = the join key (SVG `data-key`); `tooltip` = per-row tooltip text;
# `hover_group` = a field that groups elements for linked emphasis (consumed by a
# host in a later phase). They flow into the vellum scene as element key/metadata.
.INTERACT_ARGS <- c(
  "tooltip",
  "data_id",
  "hover_group",
  "hover_color",
  "selected_color"
)

# Parse a `condition(selection, if_true, if_false, empty)` call into a `channel`
# whose `expr` is the `if_true` branch (so everything downstream sees a plain
# encoding) and whose `condition` slot carries the selection name (a constant,
# resolved now), the `if_false` quosure (or NULL = theme dim), and `empty`.
.condition_channel <- function(e, env, call = rlang::caller_env()) {
  m <- rlang::call_match(e, condition)
  args <- rlang::call_args(m)
  if (is.null(args$selection) || is.null(args$if_true)) {
    cli::cli_abort(
      "{.fn condition} needs a selection name and an {.arg if_true} value.",
      call = call
    )
  }
  selection <- rlang::eval_tidy(rlang::new_quosure(args$selection, env))
  if (!is.character(selection) || length(selection) != 1L) {
    cli::cli_abort(
      "{.fn condition} selection must be a single string.",
      call = call
    )
  }
  empty <- if (!is.null(args$empty)) {
    isTRUE(rlang::eval_tidy(rlang::new_quosure(args$empty, env)))
  } else {
    TRUE
  }
  if_false <- if (!is.null(args$if_false)) {
    rlang::new_quosure(args$if_false, env)
  } else {
    NULL
  }
  channel(
    expr = rlang::new_quosure(args$if_true, env),
    condition = list(selection = selection, if_false = if_false, empty = empty)
  )
}

# The slab/interval marks name their interval probabilities `.width` (dotted) to
# avoid colliding with the `width` aesthetic other marks use. A bare `width =`
# here is almost certainly a typo for `.width`; catch it rather than let it pass
# through as an inert aesthetic (`L$values$width`) that silently does nothing.
# Default an unmapped primary channel to a computed (`after_stat()`) variable.
# `quo` is the fallback quosure (e.g. `after_stat(count)`). A `fill` default also
# yields to an explicit `color`/`colour` (the two are easily confused on a filled
# mark); a `color` default yields to `colour`. No-op if already mapped. Shared by
# the binning/contour marks so their defaults stay in step.
.default_channel <- function(dots, channel, quo) {
  mapped <- !is.null(dots[[channel]]) || !is.null(dots$colour)
  if (channel == "fill") {
    mapped <- mapped || !is.null(dots$color)
  }
  if (!mapped) {
    dots[[channel]] <- quo
  }
  dots
}

.reject_width_arg <- function(dots, call = rlang::caller_env()) {
  if ("width" %in% names(dots)) {
    cli::cli_abort(
      c(
        "{.arg width} is not an aesthetic of this mark.",
        i = "Did you mean {.arg .width} (the interval probabilities)?"
      ),
      call = call
    )
  }
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
#'   `fill`, `size`, `shape`, `alpha`. Stroked marks (line, step, segment, rule,
#'   linerange) also take `linewidth` and `linetype`.
#' @param size,shape Convenience arguments for the point size (in mm) / shape;
#'   may be a constant or a mapped expression. One of `"circle"`, `"square"`,
#'   `"triangle"`, `"diamond"`, `"plus"`, `"cross"`.
#' @param position Position adjustment: a string — `"identity"` (default),
#'   `"jitter"` / `"jitterdodge"` (points), `"stack"` / `"fill"` / `"dodge"` /
#'   `"dodge2"` (bars), `"nudge"` — or a parameterised [position_jitter()] /
#'   [position_dodge()] / [position_dodge2()] / [position_jitterdodge()] /
#'   [position_nudge()] object.
#' @param auto For `mark_point()`, `mark_line()`, `mark_step()`,
#'   `mark_segment()`, and `mark_edges()`, when `TRUE` and the layer has very many
#'   rows, automatically render it as a datashaded density raster (see
#'   [mark_datashade()]) instead of individual vector marks: points bin into a
#'   density grid (`vellum::datashade()`), dense lines/steps rasterise as connected
#'   polylines (`vellum::datashade_lines()`), and segments/edges as independent
#'   segments (`vellum::datashade_segments()`). The datashaded line/segment output
#'   is `dynspread`-ed so thin marks stay visible (see the `spread` argument of
#'   [mark_datashade()]). The fallback is skipped under a warped coordinate system
#'   (`coord_polar()` / `coord_trans()`), which draws the vector marks instead.
#' @param seed For `mark_point(position = "jitter")`, an optional integer seed
#'   making the jitter reproducible. The global RNG stream is restored afterwards.
#' @param window For `mark_line()`, an optional window (rolling / cumulative /
#'   offset) transform of `y` computed per group over rows ordered by `x`, before
#'   the line is drawn. Either an op name (`"mean"`, `"sum"`, `"median"`, `"min"`,
#'   `"max"`, `"cumsum"`, `"cummean"`, `"cummax"`, `"cummin"`, `"lag"`, `"lead"`,
#'   `"rank"`) or a list `list(op=, k=, align=, partial=)`: `k` is the window size
#'   (rolling; default 7) or shift (`lag`/`lead`; default 1), `align` is
#'   `"right"` (trailing, default), `"left"`, or `"center"`, and `partial`
#'   (default `TRUE`) computes at the edges from the shorter available window. For
#'   example `window = list(op = "mean", k = 7)` is a 7-point trailing average.
#' @param blend Optional blend mode for compositing this layer against what is
#'   already drawn beneath it (the panel and earlier layers), one of the CSS
#'   `mix-blend-mode` names, e.g. `"multiply"`, `"screen"`, `"darken"`. The whole
#'   layer composites as one isolated group (not per element).
#' @param effects A list of layer render effects applied to the mark at draw
#'   time — [glow()], [outline()], and [shadow()]. Available on stroked and point
#'   marks.
#' @param sketch A [sketch()] spec giving this layer a hand-drawn look (wobbly
#'   outlines, hachure fills), `NA`/`FALSE` to force it crisp (overriding a
#'   plot-wide [theme_sketch()]), or `NULL` (default) to inherit. Geometry marks
#'   accept it; text, raster, hex and datashade marks do not.
#' @param data Optional layer data frame; overrides the plot data for this layer.
#' @section Interactivity:
#' Any mark accepts reserved, per-row arguments (captured like encodings, via tidy
#' evaluation) that make its elements addressable — and stylable — by an
#' interactive host without changing what a static render draws:
#'
#' * `data_id` — a per-element **data key** (e.g. `data_id = model`). Emitted by
#'   the SVG backend as `data-key` on each element and returned by
#'   `vellum::scene_model()`; it is the join key a host uses to map a hover/click
#'   back to a datum, and to link the same datum across views.
#' * `tooltip` — per-element tooltip text (a column expression or a constant),
#'   surfaced in `scene_model()` metadata.
#' * `hover_group` — a field grouping elements for linked emphasis (consumed by a
#'   host in a later phase).
#' * `hover_color`, `selected_color` — per-element outline colours applied by the
#'   host when the element is hovered / selected (a constant, or mapped from a
#'   column so different marks highlight differently). They override the widget-wide
#'   theme set by `vellumwidget::as_widget(hover_color=, selected_color=)`.
#'
#' These are inert for PNG/PDF and for an SVG opened without a JS host: a plot
#' with none of them compiles and renders exactly as before. Declaring any of them
#' without `data_id` defaults the key to the row index, so the element is still
#' addressable. They currently apply to `stat = "identity"` marks (points, bars,
#' tiles, segments, edges, hexbins, sf features, …); aggregating stats
#' (histogram/count/density) drop them, since rows no longer map 1:1 to elements.
#'
#' How these flow into the vellum scene (the `scene_model()` element table, the
#' SVG `data-key` / `data-vellum-*` attributes, and the reserved `meta` key
#' vocabulary) is described in vellum's "The scene contract" vignette
#' (`vignette("scene-contract", package = "vellum")`).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
#'
#' # Declare interactivity (inert on a static render):
#' df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, model = rownames(mtcars))
#' vplot(df) |> mark_point(x = wt, y = mpg, tooltip = model, data_id = model)
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
  effects = list(),
  sketch = NULL,
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
    effects = effects,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_point
#' @export
mark_line <- function(
  plot,
  ...,
  window = NULL,
  auto = FALSE,
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  win <- .check_window(window)
  .add_layer(
    plot,
    "line",
    rlang::enquos(...),
    stat = if (is.null(win)) "identity" else "window",
    stat_params = c(list(auto = isTRUE(auto)), win),
    blend = blend,
    effects = effects,
    sketch = sketch,
    data = data
  )
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
#' @param sketch A [sketch()] spec giving the layer a hand-drawn look, or `NULL`
#'   (default) to inherit.
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
  sketch = NULL,
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
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_point
#' @export
mark_rule <- function(
  plot,
  ...,
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "rule",
    rlang::enquos(...),
    blend = blend,
    effects = effects,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_point
#' @details
#' `mark_bar()` draws bars from a zero baseline. With an explicit `y` it uses the
#' `y` values as heights; with no `y` it counts rows per category (the `"count"`
#' stat). When `color`/`fill` is mapped, grouped bars are stacked by default; use
#' `position = "dodge"` for side-by-side bars or `"fill"` to normalise to 1.
#' @export
mark_bar <- function(
  plot,
  ...,
  position = "stack",
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "bar",
    rlang::enquos(...),
    position = position,
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' Pie and donut charts
#'
#' Convenience marks for part-of-whole charts. `mark_pie()` draws a pie: each
#' `value` becomes a wedge whose angle is its share of the total, coloured by
#' `fill`. `mark_donut()` is a pie with a hollow centre (`inner_radius`, a
#' fraction of the radius). Both are shorthand for a stacked bar projected through
#' [coord_polar()] with `theta = "y"`, which they set on the plot; they error if
#' the plot already carries a non-polar coordinate.
#'
#' @param plot A [PlotSpec].
#' @param value Encoding (tidy-eval) for each slice's magnitude.
#' @param fill Encoding (tidy-eval) for the slice colour. Omit for a single
#'   slice.
#' @param inner_radius For `mark_donut()`, the inner-hole radius as a fraction of
#'   the rim (`0` is a pie, the default `0.5` a medium donut).
#' @param ... Further constant aesthetics (e.g. `alpha`).
#' @param blend Optional blend mode for compositing the layer (see [mark_point()]).
#' @param data Optional per-layer data frame.
#' @param sketch A [sketch()] spec giving the layer a hand-drawn look, or `NULL`
#'   (default) to inherit.
#' @return The modified [PlotSpec].
#' @seealso [coord_polar()]
#' @examples
#' df <- data.frame(part = c("a", "b", "c"), n = c(3, 5, 2))
#' vplot(df) |> mark_pie(value = n, fill = part)
#' vplot(df) |> mark_donut(value = n, fill = part, inner_radius = 0.6)
#' @export
mark_pie <- function(
  plot,
  value,
  fill = NULL,
  ...,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .pie_layer(
    plot,
    rlang::enquos(value = value, fill = fill, ...),
    0,
    blend,
    sketch,
    data
  )
}

#' @rdname mark_pie
#' @export
mark_donut <- function(
  plot,
  value,
  fill = NULL,
  inner_radius = 0.5,
  ...,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_inner_radius(inner_radius)
  .pie_layer(
    plot,
    rlang::enquos(value = value, fill = fill, ...),
    inner_radius,
    blend,
    sketch,
    data
  )
}

# Shared body of mark_pie/mark_donut: a stacked bar (value -> y, a constant
# single-band x) forced through polar theta = "y".
.pie_layer <- function(plot, enc, inner_radius, blend, sketch, data) {
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
    blend = blend,
    sketch = sketch,
    data = data
  )
  if (is.null(plot@coord)) {
    plot@coord <- CoordSpec(kind = "polar", theta = "y", rmin = inner_radius)
  }
  plot
}

#' Statistical marks
#'
#' Marks that apply a statistical transform before drawing. `mark_histogram()`
#' bins a continuous `x` and draws the per-bin counts as bars. `mark_smooth()`
#' fits a model of `y` on `x` (per group) and draws the fitted line, with a
#' confidence ribbon when `se = TRUE`.
#'
#' `mark_smooth()` supports several `method`s:
#' * `"auto"` (default) picks `"loess"` for small groups (< 1000 points) and
#'   `"gam"` for large ones.
#' * `"lm"` / `"glm"` — linear and generalised linear fits (`glm` takes a
#'   `family` via `method.args`, e.g. `binomial()` for logistic).
#' * `"loess"` — local regression, controlled by `span`.
#' * `"gam"` — a generalised additive model with a smooth term (default
#'   `y ~ s(x)`); needs the \pkg{mgcv} package.
#' * `"rq"` — quantile regression at a single `method.args$tau` (default the
#'   median); needs the \pkg{quantreg} package and draws the fitted line only
#'   (no confidence ribbon). For several quantiles, add one layer per `tau`.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval), e.g. `x`, `y`, `color`/`fill`.
#' @param bins Number of histogram bins.
#' @param method Smoothing method: one of `"auto"`, `"lm"`, `"loess"`, `"glm"`,
#'   `"gam"`, `"rq"`.
#' @param formula Model formula in terms of `x` and `y` (e.g. `y ~ poly(x, 2)`,
#'   `y ~ s(x)` for `gam`). Defaults to `y ~ x` (`y ~ s(x)` for `gam`).
#' @param span `loess` neighbourhood size (larger = smoother).
#' @param se Draw a confidence ribbon around the smooth? Ignored for `"rq"`.
#' @param level Confidence level for the ribbon.
#' @param method.args Extra arguments to the fitting function, e.g.
#'   `list(family = binomial())` for `glm`, or `list(tau = 0.9)` for `rq`.
#' @param position Position adjustment for the histogram bars (`"stack"`,
#'   `"dodge"`, `"fill"`).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_histogram(x = mpg, bins = 10)
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> mark_smooth(x = wt, y = mpg)
#' # local regression with a wider neighbourhood
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   mark_smooth(x = wt, y = mpg, method = "loess", span = 0.9)
#' @export
mark_histogram <- function(
  plot,
  ...,
  bins = 30,
  position = "stack",
  blend = NULL,
  sketch = NULL,
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
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_histogram
#' @export
mark_smooth <- function(
  plot,
  ...,
  method = "auto",
  formula = NULL,
  span = 0.75,
  se = TRUE,
  level = 0.95,
  method.args = list(),
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "smooth",
    rlang::enquos(...),
    stat = "smooth",
    stat_params = list(
      method = method,
      formula = formula,
      span = span,
      se = se,
      level = level,
      method.args = method.args
    ),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' Group summary regions
#'
#' Marks that summarise a set of `(x, y)` points (per group, when a `color` or
#' `fill` is mapped) as a single enclosing region drawn over the raw points.
#'
#' * `mark_ellipse()` draws a covariance ellipse: a `t` distribution
#'   (`type = "t"`, the default, robustly estimated via \pkg{MASS}), a
#'   multivariate normal (`"norm"`), or a Euclidean circle of radius `level`
#'   (`"euclid"`). The algorithm follows ggplot2's `stat_ellipse()`.
#' * `mark_hull()` draws the convex hull of the points.
#'
#' Both are unfilled by default (a boundary), matching ggplot2; map or set a
#' `fill` to shade the region. A region needs at least 3 points per group;
#' smaller groups are skipped with a warning.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y`, and optionally `color`/`fill`
#'   (which also splits the points into per-group regions).
#' @param type Ellipse type: `"t"`, `"norm"`, or `"euclid"`.
#' @param level Confidence level (`"t"`/`"norm"`) or circle radius (`"euclid"`).
#' @param segments Number of line segments approximating the ellipse.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(iris) |>
#'   mark_point(x = Sepal.Length, y = Sepal.Width, color = Species) |>
#'   mark_ellipse(x = Sepal.Length, y = Sepal.Width, color = Species)
#' vplot(iris) |>
#'   mark_point(x = Sepal.Length, y = Sepal.Width, color = Species) |>
#'   mark_hull(x = Sepal.Length, y = Sepal.Width, color = Species)
#' @export
mark_ellipse <- function(
  plot,
  ...,
  type = "t",
  level = 0.95,
  segments = 51L,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "ellipse",
    rlang::enquos(...),
    stat = "ellipse",
    stat_params = list(type = type, level = level, segments = segments),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_ellipse
#' @export
mark_hull <- function(
  plot,
  ...,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "hull",
    rlang::enquos(...),
    stat = "hull",
    stat_params = list(),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' Datashade a large point cloud
#'
#' For data too dense to draw one marker each (overplotted, up to millions of
#' points), `mark_datashade()` bins the points into a canvas-sized grid in one
#' pass and colours each cell by density (via [vellum::datashade()]), drawing a
#' single raster that fills the panel. Cost is decoupled from point count and
#' overplotting.
#'
#' Map a discrete `color` (or `fill`) aesthetic to shade **categorically**
#' (datashader's `count_cat`): each category is aggregated separately and every
#' cell is coloured by the count-weighted average of the category hues it holds,
#' with opacity from its total density — so you see which category dominates
#' where, and where they mix, with a colour legend. The category hues come from
#' the usual discrete colour scale (so [scale_color_manual()] etc. apply). Without
#' a mapped colour, cell colour encodes plain density via the `colors` ramp.
#'
#' @inheritParams mark_point
#' @param ... Encodings; `x` and `y` are required. Map `color`/`fill` to a
#'   discrete variable for categorical (`count_cat`) shading.
#' @param width,height Aggregation grid size in cells (output raster pixels).
#' @param colors Two or more colours forming the low-to-high density ramp (plain
#'   density shading only; ignored when a `color`/`fill` aesthetic is mapped).
#' @param how Density-to-colour mapping (and, categorically, per-cell opacity):
#'   `"eq_hist"` (default), `"log"`, `"cbrt"`, or `"linear"`.
#' @param span,clip Optional density-range clamping passed to [vellum::datashade()]:
#'   `span = c(lo, hi)` absolute limits, or `clip = c(0.01, 0.99)` percentiles, so
#'   a few extreme cells don't flatten the rest. Both default `NULL`.
#' @param spread Optional post-aggregation spreading to keep sparse output
#'   visible (passed to [vellum::datashade()]): `NULL` (default) none — the raw
#'   one-pass aggregation; a positive integer dilates each shaded pixel by that
#'   radius ([vellum::spread()]); `"auto"` picks the radius from the image density
#'   ([vellum::dynspread()]). Datashaded lines/segments (`auto = TRUE` on
#'   `mark_line()` / `mark_segment()` etc.) default to `"auto"` since single-pixel
#'   lines otherwise vanish.
#' @return The modified [PlotSpec].
#' @examples
#' n <- 1e5
#' d <- data.frame(x = rnorm(n), y = rnorm(n), g = sample(c("a", "b"), n, TRUE))
#' vplot(d) |> mark_datashade(x = x, y = y)
#' # categorical: colour by group
#' vplot(d) |> mark_datashade(x = x, y = y, color = g)
#' @export
mark_datashade <- function(
  plot,
  ...,
  width = 400,
  height = 300,
  colors = NULL,
  how = "eq_hist",
  span = NULL,
  clip = NULL,
  spread = NULL,
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
      how = how,
      span = span,
      clip = clip,
      spread = spread
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
#' @param position For `mark_area()`, how areas sharing an `x` combine when
#'   `fill`/`color` is mapped: `"stack"` (default) stacks them into a band,
#'   `"fill"` normalises each `x` to 1, `"identity"` overlays them from the zero
#'   baseline. With no fill mapping all three are equivalent (a single area).
#' @param direction For `mark_step()`, `"hv"` (horizontal then vertical, default)
#'   or `"vh"`.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(pressure) |> mark_area(x = temperature, y = pressure)
#' @export
mark_area <- function(
  plot,
  ...,
  position = "stack",
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "area",
    rlang::enquos(...),
    position = position,
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_area
#' @export
mark_ribbon <- function(plot, ..., blend = NULL, sketch = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "ribbon",
    rlang::enquos(...),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_area
#' @export
mark_step <- function(
  plot,
  ...,
  direction = "hv",
  auto = FALSE,
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "step",
    rlang::enquos(...),
    stat_params = list(direction = direction, auto = isTRUE(auto)),
    blend = blend,
    effects = effects,
    sketch = sketch,
    data = data
  )
}

#' Text marks
#'
#' `mark_text()` draws the `label` aesthetic as text at each `(x, y)`;
#' `mark_label()` adds a filled rounded background behind each label. `size` is
#' the font size in points; `angle` (degrees) may be mapped or constant.
#'
#' The `label` for `mark_text()` may be plain text (embedded newlines `\n` wrap
#' onto stacked lines) or rich [vellum::md()] labels — map `label = md(<expr>)`
#' for a per-datum styled label (bold/italic/super-/subscript/colour). (Rich
#' labels are not yet supported by `mark_label()`'s background box.)
#'
#' Set `repel = TRUE` to move overlapping labels apart with a force-directed
#' layout (like ggrepel), drawing a thin leader line back to each label's point.
#' Repulsion is resolved exactly against the true rendered panel size, so it does
#' not depend on the data scale. It is currently limited to a single cartesian
#' panel (no facets / composition / polar).
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y`, `label` (+ `color`, `angle`).
#' @param size Font size in points.
#' @param family,fontface Font family / face (`"plain"`, `"bold"`, `"italic"`,
#'   `"bold.italic"`).
#' @param hjust,vjust Horizontal / vertical justification (constant; `"left"`,
#'   `"centre"`, `"right"`, `"bottom"`, `"top"`, or numeric in `[0, 1]`).
#' @param angle Text rotation in degrees.
#' @param nudge_x,nudge_y Shift each label by an exact absolute distance in
#'   millimetres (`+x` right, `+y` up), device-resolved so the nudge is constant
#'   regardless of scale or panel aspect. Default `0`.
#' @param fill For `mark_label()`, the label background: a constant colour, or a
#'   mapped encoding (e.g. `fill = group`) coloured through the fill/colour scale.
#' @param repel Move overlapping labels apart (force-directed, ggrepel-style),
#'   with leader lines to the points? Single cartesian panel only.
#' @param box_padding,point_padding Extra space (mm) kept around each label box
#'   and around each anchor point during repulsion.
#' @param min_segment_length Shortest leader line (mm) worth drawing; a label
#'   that barely moved gets none.
#' @param max_overlaps Reserved for a future overlap cap (currently unused).
#' @param seed Integer seed making the repel layout reproducible (the global RNG
#'   stream is restored afterwards).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_text(x = wt, y = mpg, label = rownames(mtcars))
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |>
#'   mark_text(x = wt, y = mpg, label = rownames(mtcars), nudge_y = 2)
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
  nudge_x = 0,
  nudge_y = 0,
  repel = FALSE,
  box_padding = 1,
  point_padding = 1,
  min_segment_length = 2,
  max_overlaps = 10,
  seed = NULL,
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
    const_params = list(
      nudge_x = as.numeric(nudge_x),
      nudge_y = as.numeric(nudge_y)
    ),
    stat_params = list(
      repel = .repel_params(
        repel,
        box_padding,
        point_padding,
        min_segment_length,
        max_overlaps,
        seed
      )
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
  nudge_x = 0,
  nudge_y = 0,
  fill = "white",
  repel = FALSE,
  box_padding = 1,
  point_padding = 1,
  min_segment_length = 2,
  max_overlaps = 10,
  seed = NULL,
  blend = NULL,
  sketch = NULL,
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
    const_params = list(
      nudge_x = as.numeric(nudge_x),
      nudge_y = as.numeric(nudge_y)
    ),
    stat_params = list(
      repel = .repel_params(
        repel,
        box_padding,
        point_padding,
        min_segment_length,
        max_overlaps,
        seed
      )
    ),
    blend = blend,
    sketch = sketch,
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
mark_tile <- function(plot, ..., blend = NULL, sketch = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "tile",
    rlang::enquos(...),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_tile
#' @export
mark_raster <- function(plot, ..., blend = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(plot, "raster", rlang::enquos(...), blend = blend, data = data)
}

#' Draw images at data points
#'
#' `mark_image()` draws a bitmap image (e.g. a flag or logo) at each `(x, y)`,
#' replacing the usual point marker. `src` is a column of local image file paths
#' (per datum) or a single constant path (the same image at every point). Images
#' are sized by `size` (height in millimetres); each image's native aspect ratio
#' is preserved.
#'
#' Reading image files requires the suggested
#' [magick](https://docs.ropensci.org/magick/) package (which decodes PNG, JPEG,
#' SVG, GIF and more); `mark_image()` errors with an install hint if it is not
#' available.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y`, and `src` (the image path
#'   column).
#' @param src Image source: a column of file paths (mapped, one per datum) or a
#'   single constant path (the same image at every point).
#' @param size Image height in millimetres (constant or mapped); the width
#'   follows the image's own aspect ratio. Default `5`.
#' @param nudge_x,nudge_y Shift each image by an exact absolute distance in
#'   millimetres (`+x` right, `+y` up). Default `0`.
#' @param interpolate Smoothly interpolate when scaling (default `TRUE`)?
#'   `FALSE` keeps hard pixel edges.
#' @return The modified [PlotSpec].
#' @examples
#' \dontrun{
#' d <- data.frame(x = 1:3, y = c(2, 1, 3),
#'                 flag = c("de.png", "fr.png", "it.png"))
#' vplot(d) |> mark_image(x = x, y = y, src = flag, size = 8)
#' }
#' @export
mark_image <- function(
  plot,
  ...,
  src = NULL,
  size = 5,
  nudge_x = 0,
  nudge_y = 0,
  interpolate = TRUE,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "image",
    rlang::enquos(...),
    rlang::enquos(src = src, size = size),
    const_params = list(
      nudge_x = as.numeric(nudge_x),
      nudge_y = as.numeric(nudge_y),
      interpolate = isTRUE(interpolate)
    ),
    blend = blend,
    data = data
  )
}

#' @rdname mark_tile
#' @export
mark_bin2d <- function(plot, ..., bins = 30, blend = NULL, data = NULL) {
  .check_plot(plot)
  dots <- rlang::enquos(...)
  dots <- .default_channel(dots, "fill", rlang::quo(after_stat(count)))
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
mark_density <- function(
  plot,
  ...,
  adjust = 1,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "area",
    rlang::enquos(...),
    stat = "density",
    stat_params = list(adjust = adjust),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' 2-D density contours
#'
#' `mark_contour()` draws iso-density contour **lines** of a 2-D point cloud
#' (`x`, `y`); `mark_contour_filled()` fills the bands between them. By default the
#' field is a kernel density estimate (needs the \pkg{MASS} package); map a `z`
#' aesthetic to instead contour a supplied surface over a regular `x`/`y` grid.
#' Contours are coloured by level automatically — `mark_contour()` maps
#' `color = after_stat(level)`, `mark_contour_filled()` maps `fill`. Requires the
#' \pkg{isoband} package.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y` (+ optional `z` surface, `color` /
#'   `fill`, `linewidth`, `linetype`).
#' @param bins Target number of contour levels (when neither `breaks` nor
#'   `binwidth` is given).
#' @param binwidth Spacing between contour levels, or `NULL`.
#' @param breaks Explicit contour levels, or `NULL` to derive from `bins` /
#'   `binwidth`.
#' @param n Density-estimate grid resolution per axis (KDE mode only).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(faithful) |>
#'   mark_point(x = eruptions, y = waiting) |>
#'   mark_contour(x = eruptions, y = waiting)
#' vplot(faithful) |> mark_contour_filled(x = eruptions, y = waiting)
#' @export
mark_contour <- function(
  plot,
  ...,
  bins = 10,
  binwidth = NULL,
  breaks = NULL,
  n = 100,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  dots <- rlang::enquos(...)
  dots <- .default_channel(dots, "color", rlang::quo(after_stat(level)))
  .add_layer(
    plot,
    "contour",
    dots,
    stat = "density_2d",
    stat_params = list(
      bins = bins,
      binwidth = binwidth,
      breaks = breaks,
      n = n
    ),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_contour
#' @export
mark_contour_filled <- function(
  plot,
  ...,
  bins = 10,
  binwidth = NULL,
  breaks = NULL,
  n = 100,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  dots <- rlang::enquos(...)
  dots <- .default_channel(dots, "fill", rlang::quo(after_stat(level)))
  .add_layer(
    plot,
    "contour_filled",
    dots,
    stat = "density_2d",
    stat_params = list(
      bins = bins,
      binwidth = binwidth,
      breaks = breaks,
      n = n,
      filled = TRUE
    ),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' Distribution marks
#'
#' Marks that summarise the distribution of a variable.
#' `mark_ecdf()` draws the empirical cumulative distribution of `x` as a step;
#' `mark_rug()` draws marginal ticks at each datum; `mark_qq()` draws a
#' quantile-quantile plot of a `sample` against a theoretical distribution, with
#' `mark_qq_line()` adding the reference line. All respect a mapped `color`/`fill`
#' grouping.
#'
#' @param plot A [PlotSpec].
#' @param ... Encodings. `mark_ecdf()` needs `x`; `mark_qq()`/`mark_qq_line()`
#'   need `sample`; `mark_rug()` takes `x` and/or `y` (+ `color`, `alpha`,
#'   `linewidth`, `linetype`).
#' @param sides Which edges `mark_rug()` draws ticks on: any of `"b"` (bottom),
#'   `"l"` (left), `"t"` (top), `"r"` (right); default `"bl"`.
#' @param length Rug tick length as a fraction of the panel (default `0.03`).
#' @param distribution Quantile function of the reference distribution for
#'   `mark_qq()` / `mark_qq_line()` (default [stats::qnorm]).
#' @param blend,sketch,data Standard layer arguments (see [mark_point()]).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_ecdf(x = mpg)
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> mark_rug()
#' vplot(mtcars) |> mark_qq(sample = mpg) |> mark_qq_line(sample = mpg)
#' @export
mark_ecdf <- function(plot, ..., blend = NULL, sketch = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "step",
    rlang::enquos(...),
    stat = "ecdf",
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_ecdf
#' @export
mark_rug <- function(
  plot,
  ...,
  sides = "bl",
  length = 0.03,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "rug",
    rlang::enquos(...),
    stat = "identity",
    stat_params = list(sides = sides, length = length),
    blend = blend,
    data = data
  )
}

#' @rdname mark_ecdf
#' @export
mark_qq <- function(
  plot,
  ...,
  distribution = "qnorm",
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "point",
    rlang::enquos(...),
    stat = "qq",
    stat_params = list(distribution = distribution),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_ecdf
#' @export
mark_qq_line <- function(
  plot,
  ...,
  distribution = "qnorm",
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "line",
    rlang::enquos(...),
    stat = "qq_line",
    stat_params = list(distribution = distribution),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' Density-shape marks
#'
#' Marks that draw a variable's distribution as a filled shape.
#' `mark_violin()` draws a mirrored kernel density of `y` per categorical `x`
#' (like a boxplot's footprint); `mark_ridgeline()` draws a kernel density of `x`
#' per categorical `y` as overlapping ridges. `mark_dotplot()` bins `x` and
#' stacks one dot per observation.
#'
#' @param plot A [PlotSpec].
#' @param ... Encodings. `mark_violin()` needs categorical `x` and numeric `y`;
#'   `mark_ridgeline()` needs numeric `x` and categorical `y`; `mark_dotplot()`
#'   needs `x`. A mapped `color`/`fill` sets the shape fill.
#' @param adjust Kernel-density bandwidth multiplier (violin/ridgeline).
#' @param height Ridge height as a multiple of the row band (ridgeline; default
#'   `1.4`, so adjacent ridges overlap slightly).
#' @param binwidth Dot-plot bin width, or `NULL` to use ~1/30 of the data range.
#' @param blend,sketch,data Standard layer arguments (see [mark_point()]).
#' @return The modified [PlotSpec].
#' @examples
#' df <- data.frame(g = rep(letters[1:3], each = 50), v = rnorm(150))
#' vplot(df) |> mark_violin(x = g, y = v)
#' vplot(df) |> mark_ridgeline(x = v, y = g)
#' vplot(df) |> mark_dotplot(x = v)
#' @export
mark_violin <- function(
  plot,
  ...,
  adjust = 1,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "violin",
    rlang::enquos(...),
    stat = "identity",
    stat_params = list(adjust = adjust),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_violin
#' @export
mark_ridgeline <- function(
  plot,
  ...,
  adjust = 1,
  height = 1.4,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "ridgeline",
    rlang::enquos(...),
    stat = "identity",
    stat_params = list(adjust = adjust, height = height),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_violin
#' @export
mark_dotplot <- function(
  plot,
  ...,
  binwidth = NULL,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "point",
    rlang::enquos(...),
    stat = "dotplot",
    stat_params = list(binwidth = binwidth),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' Uncertainty marks (slab + interval)
#'
#' Marks for visualising a distribution's shape *and* its summary intervals
#' together, in the style of the \pkg{ggdist} package — a natural fit for
#' posterior draws or bootstrap samples (one row per draw, grouped by a
#' categorical `x`).
#'
#' * `mark_halfeye()` draws, per `x` category, a one-sided density "slab" (a half
#'   violin) with a **point-interval** at its base: the median (or mean) as a
#'   point, a thick bar for the inner interval and a thin bar for the outer one.
#' * `mark_interval()` draws the point-interval alone (no slab).
#'
#' Intervals are equal-tailed quantile intervals at the `.width` probabilities
#' (widest drawn thinnest). Supply many draws per `x` (e.g. posterior samples).
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): a categorical `x` and the sample `y`
#'   (+ `color`/`fill`).
#' @param .width Interval probabilities, widest last (default
#'   `c(0.66, 0.95)`).
#' @param point Central summary: `"median"` (default) or `"mean"`.
#' @param adjust `mark_halfeye()` density bandwidth multiplier.
#' @param scale Slab width as a fraction of the category band (`mark_halfeye()`).
#' @return The modified [PlotSpec].
#' @examples
#' set.seed(1)
#' draws <- data.frame(
#'   grp = rep(c("a", "b", "c"), each = 400),
#'   val = rnorm(1200, rep(c(0, 1, 2), each = 400))
#' )
#' vplot(draws) |> mark_halfeye(x = grp, y = val)
#' vplot(draws) |> mark_interval(x = grp, y = val)
#' @export
mark_halfeye <- function(
  plot,
  ...,
  .width = c(0.66, 0.95),
  point = "median",
  adjust = 1,
  scale = 0.9,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  dots <- rlang::enquos(...)
  .reject_width_arg(dots)
  .add_layer(
    plot,
    "halfeye",
    dots,
    stat_params = list(
      width = .width,
      point = point,
      adjust = adjust,
      scale = scale
    ),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_halfeye
#' @export
mark_interval <- function(
  plot,
  ...,
  .width = c(0.66, 0.95),
  point = "median",
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  dots <- rlang::enquos(...)
  .reject_width_arg(dots)
  .add_layer(
    plot,
    "interval",
    dots,
    stat_params = list(width = .width, point = point),
    blend = blend,
    data = data
  )
}

#' @rdname mark_tile
#' @export
mark_hex <- function(plot, ..., bins = 30, blend = NULL, data = NULL) {
  .check_plot(plot)
  dots <- rlang::enquos(...)
  dots <- .default_channel(dots, "fill", rlang::quo(after_stat(count)))
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
#'   `ymax` for errorbar/linerange; plus `color`/`fill` (and `linetype` for
#'   errorbar/linerange).
#' @param width For `mark_errorbar()`, the cap width as a fraction of the band.
#' @param fun For `mark_summary()`, the aggregation function (default `mean`).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg)
#' @export
mark_boxplot <- function(plot, ..., blend = NULL, sketch = NULL, data = NULL) {
  .check_plot(plot)
  .add_layer(
    plot,
    "boxplot",
    rlang::enquos(...),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_boxplot
#' @export
mark_errorbar <- function(
  plot,
  ...,
  width = 0.5,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "errorbar",
    rlang::enquos(...),
    rlang::enquos(width = width),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_boxplot
#' @export
mark_linerange <- function(
  plot,
  ...,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "linerange",
    rlang::enquos(...),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_boxplot
#' @export
mark_summary <- function(
  plot,
  ...,
  fun = mean,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "point",
    rlang::enquos(...),
    stat = "aggregate",
    stat_params = list(fun = fun),
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' Segment mark
#'
#' `mark_segment()` draws a straight line from `(x, y)` to `(xend, yend)` per row.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y`, `xend`, `yend` (+ `color`,
#'   `linewidth`, `linetype`, `alpha`).
#' @return The modified [PlotSpec].
#' @examples
#' d <- data.frame(x = 1, y = 1, xend = 5, yend = 4)
#' vplot(d) |> mark_segment(x = x, y = y, xend = xend, yend = yend)
#' @export
mark_segment <- function(
  plot,
  ...,
  auto = FALSE,
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "segment",
    rlang::enquos(...),
    stat_params = list(auto = isTRUE(auto)),
    blend = blend,
    effects = effects,
    sketch = sketch,
    data = data
  )
}

# Fill in default encoding channels the user did not supply (used by the graph
# marks, whose x/y/xend/yend/label columns are produced by vgraph()).
.with_default_aes <- function(dots, defaults) {
  c(dots, defaults[setdiff(names(defaults), names(dots))])
}

# Move a graph edge's colour / alpha / line-type encodings onto dedicated edge
# channels (`edge_color`/`edge_alpha`/`edge_linetype`) so they train and legend
# independently of the node scales (see _docs/GAPS-NETWORKS.md N1a). Position
# channels (x/y/xend/yend) and edge width (`linewidth` -> the `edge_width` scale)
# are left untouched; `colour`/`fill` both fold into `edge_color`.
.EDGE_AES_MAP <- c(
  color = "edge_color",
  colour = "edge_color",
  fill = "edge_color",
  alpha = "edge_alpha",
  linetype = "edge_linetype"
)
.rename_edge_aes <- function(quos) {
  if (!length(quos)) {
    return(quos)
  }
  nm <- names(quos)
  hit <- nm %in% names(.EDGE_AES_MAP)
  nm[hit] <- unname(.EDGE_AES_MAP[nm[hit]])
  names(quos) <- nm
  quos
}

# Resolve `mark_edges(arrow = )` to a vellum arrow spec, or NULL. `FALSE`/`NULL`
# draws no head; `TRUE` the default closed head at the target end (the directed
# convention); a `vl_arrow()` object is used verbatim (custom ends/type/size).
.resolve_edge_arrow <- function(arrow) {
  if (is.null(arrow) || isFALSE(arrow)) {
    return(NULL)
  }
  if (isTRUE(arrow)) {
    return(vellum::vl_arrow(type = "closed", length = vellum::vl_unit(2, "mm")))
  }
  arrow
}

#' Network (graph) marks
#'
#' Draw a node-link diagram on a [PlotSpec] from [vgraph()]. `mark_edges()` draws
#' the edges (straight lines, batched), `mark_nodes()` the vertices (points),
#' `mark_node_text()` the vertex labels, and `mark_edge_text()` labels on the
#' edges. Draw order is fixed regardless of the order you pipe them: edges under
#' edge labels under nodes under node labels. Edges (and edge labels) default to
#' the edge table (`vgraph()`'s `edge_data`), nodes and node labels to the node
#' table; the `x`/`y`/`xend`/`yend`/`label`/`name` columns those tables carry are
#' mapped automatically, so bare `mark_edges() |> mark_nodes()` just works.
#'
#' Edge colour, opacity, and line type train on **their own scales**
#' ([scale_edge_color()], [scale_edge_alpha()], [scale_edge_linetype()], plus
#' [scale_edge_width()]), independent of the node colour/alpha/linetype scales --
#' so a plot can map node fill to a discrete community and edge colour to a
#' continuous weight without the two legends colliding.
#'
#' `mark_node_text()` has the label-legibility tools a dense graph needs:
#' `repel = TRUE` nudges overlapping labels apart with leader lines, `dist` pushes
#' labels radially clear of the node markers, `top_n`/`by` label only the hubs,
#' and `effects = list(outline())` draws a halo so labels stay readable over
#' edges. `mark_edge_text()` takes the same `effects`.
#'
#' These are thin over the point / segment / text marks; `igraph` need not be
#' installed to use them (only [vgraph()] needs it).
#'
#' @param plot A [PlotSpec], normally from [vgraph()].
#' @param ... Encodings mapping node/edge attributes to aesthetics. Nodes: `size`,
#'   `color`/`fill`, `shape`, `alpha`. Edges: `color`, `linewidth`, `linetype`,
#'   `alpha`. The position channels are supplied by `vgraph()` and need not be
#'   mapped.
#' @param size,shape For `mark_nodes()`, the node size (mm) / shape; a constant or
#'   a mapped expression. `size` is also the label font size for
#'   `mark_node_text()`/`mark_edge_text()`.
#' @param fill,color,alpha Convenience aesthetics; a constant or a mapped
#'   expression. For nodes, `fill` (or `color`) is the marker colour; for
#'   `mark_edges()`, `color`/`alpha` map through the edge scales.
#' @param linewidth For `mark_edges()`, the edge width; a constant or (via
#'   [scale_edge_width()]) a mapped expression such as `linewidth = weight`.
#' @param linetype For `mark_edges()`, the edge line type; a constant or (via
#'   [scale_edge_linetype()]) a mapped expression.
#' @param arrow For `mark_edges()`, `TRUE` to draw a closed arrowhead at each
#'   edge's target end (the directed convention), `FALSE`/`NULL` for none, or a
#'   [vellum::vl_arrow()] spec for full control (`ends`, `type`, `length`,
#'   `angle`) -- e.g. `arrow = vellum::vl_arrow(ends = "both", type = "open")`.
#'   Edges are capped exactly at each endpoint's node boundary (per vertex, at any
#'   size/resolution), so the head sits on the node edge; self-loops are drawn as
#'   teardrop loops sized to the node, with the head on the node boundary.
#' @param routing For `mark_edges()`, edge routing: `"straight"` (default) or
#'   `"elbow"` -- orthogonal right-angle steps for tree / DAG / dendrogram
#'   layouts (still straight segments, no curvature), stepping along whichever
#'   axis the endpoints are farther apart on. Elbows keep node-boundary caps and
#'   arrowheads.
#' @param gradient For `mark_edges()`, `TRUE` to fade each (straight) edge from
#'   faint at its source to opaque at its target -- a direction cue that needs no
#'   arrowhead (igraph's `edge.gradient`). Ignored with `routing = "elbow"`.
#' @param auto For `mark_edges()`, `TRUE` to datashade a large graph's edges as a
#'   density raster ([vellum::datashade_segments()]) once the edge count exceeds
#'   the datashade threshold, instead of drawing each edge as a vector segment —
#'   the fast, overplotting-honest path for hairballs. The device-space refinements
#'   of the vector path (parallel-edge offsets, node-boundary caps, arrowheads,
#'   teardrop self-loops) do not apply to the rasterised edges.
#' @param label For `mark_node_text()`, the label expression (default the vertex
#'   `name`); for `mark_edge_text()`, the edge label expression (no default -- map
#'   an edge attribute, e.g. `label = weight`).
#' @param angle For `mark_edge_text()`, the label rotation: a constant in degrees,
#'   or `"along"` to rotate each label along its edge. `NULL` (default) draws
#'   horizontal labels.
#' @param dist For `mark_node_text()`, a radial offset (mm) pushing each label
#'   outward from the layout centroid, so labels clear the node markers instead of
#'   sitting on them. `0` (default) centres the label on the vertex.
#' @param top_n,by For `mark_node_text()`, label only the `top_n` vertices with
#'   the largest `by` (an edge/vertex metric column, e.g. `by = degree`) -- the
#'   idiomatic "label just the hubs" filter. `NULL` (default) labels every vertex.
#' @param repel For `mark_node_text()`, `TRUE` to move overlapping labels apart
#'   with a force-directed layout (ggrepel-style), drawing a thin leader line back
#'   to each vertex. `box_padding`, `point_padding`, `min_segment_length`,
#'   `max_overlaps`, and `seed` tune it exactly as in [mark_text()].
#' @param box_padding,point_padding,min_segment_length,max_overlaps,seed Repel
#'   tuning for `mark_node_text(repel = TRUE)`; see [mark_text()].
#' @param blend Optional blend mode (see [mark_point()]).
#' @param data Optional layer data; overrides the default table.
#' @param effects A list of layer render effects ([glow()], [outline()],
#'   [shadow()]) applied to the mark at draw time.
#' @param sketch A [sketch()] spec giving the layer a hand-drawn look,
#'   `NA`/`FALSE` to force it crisp, or `NULL` (default) to inherit.
#' @return The modified [PlotSpec].
#' @seealso [vgraph()], [scale_edge_width()], [scale_edge_color()]
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
  linetype = NULL,
  arrow = FALSE,
  routing = c("straight", "elbow"),
  gradient = FALSE,
  auto = FALSE,
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  routing <- match.arg(routing)
  gradient <- isTRUE(gradient)
  if (identical(routing, "elbow") && gradient) {
    cli::cli_warn(
      "{.arg gradient} is ignored with {.code routing = \"elbow\"}; drawing elbows."
    )
    gradient <- FALSE
  }
  dots <- .with_default_aes(
    rlang::enquos(...),
    rlang::quos(x = x, y = y, xend = xend, yend = yend)
  )
  extra <- rlang::enquos(
    color = color,
    linewidth = linewidth,
    alpha = alpha,
    linetype = linetype
  )
  .add_layer(
    plot,
    "edges",
    .rename_edge_aes(dots),
    .rename_edge_aes(extra),
    stat_params = list(
      arrow = .resolve_edge_arrow(arrow),
      routing = routing,
      gradient = gradient,
      auto = isTRUE(auto)
    ),
    blend = blend,
    effects = effects,
    sketch = sketch,
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
  effects = list(),
  sketch = NULL,
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
    effects = effects,
    sketch = sketch,
    data = data,
    z = 3L
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
  dist = 0,
  top_n = NULL,
  by = NULL,
  repel = FALSE,
  box_padding = 1,
  point_padding = 1,
  min_segment_length = 2,
  max_overlaps = 10,
  seed = NULL,
  effects = list(),
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  # `top_n`/`by`: label only the n vertices with the largest `by` (a per-layer
  # data filter -- the idiomatic "label the hubs" move made a one-liner). The
  # centroid for the radial offset stays the *full* layout, so kept labels still
  # fan outward correctly.
  full <- data %||% plot@data
  lab <- full
  by_q <- rlang::enquo(by)
  if (!is.null(top_n)) {
    if (rlang::quo_is_null(by_q)) {
      cli::cli_abort(c(
        "{.arg top_n} needs a {.arg by} metric.",
        i = "e.g. {.code mark_node_text(top_n = 10, by = degree)}."
      ))
    }
    vals <- rlang::eval_tidy(by_q, lab)
    keep <- utils::head(order(vals, decreasing = TRUE), as.integer(top_n))
    lab <- lab[sort(keep), , drop = FALSE]
  }
  lab <- .node_label_offsets(full, lab, as.numeric(dist))
  dots <- .with_default_aes(
    rlang::enquos(...),
    rlang::quos(x = x, y = y, label = name)
  )
  extra <- rlang::enquos(
    label = label,
    color = color,
    size = size,
    alpha = alpha
  )
  if (as.numeric(dist) != 0) {
    extra <- c(
      extra,
      rlang::quos(nudge_x = .node_nudge_x, nudge_y = .node_nudge_y)
    )
  }
  .add_layer(
    plot,
    "node_text",
    dots,
    extra,
    stat_params = list(
      repel = .repel_params(
        repel,
        box_padding,
        point_padding,
        min_segment_length,
        max_overlaps,
        seed
      )
    ),
    effects = effects,
    blend = blend,
    data = lab,
    z = 4L
  )
}

# Attach the per-row radial nudges (mm) a `mark_node_text(dist=)` uses to push
# each label outward from the full layout's centroid, so labels clear the node
# markers instead of sitting on them. No-op when `dist == 0`.
.node_label_offsets <- function(full, lab, dist, call = rlang::caller_env()) {
  if (dist == 0) {
    return(lab)
  }
  if (!all(c("x", "y") %in% names(lab))) {
    cli::cli_abort(
      "{.arg dist} needs {.field x}/{.field y} columns (use it on a {.fn vgraph} plot).",
      call = call
    )
  }
  cx <- mean(full$x)
  cy <- mean(full$y)
  dx <- lab$x - cx
  dy <- lab$y - cy
  r <- sqrt(dx^2 + dy^2)
  r[r == 0] <- 1 # a node at the centroid has no outward direction
  lab$.node_nudge_x <- dist * dx / r
  lab$.node_nudge_y <- dist * dy / r
  lab
}

#' @rdname mark_graph
#' @export
mark_edge_text <- function(
  plot,
  ...,
  label = NULL,
  color = NULL,
  size = NULL,
  alpha = NULL,
  angle = NULL,
  effects = list(),
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  if (rlang::quo_is_null(rlang::enquo(label))) {
    cli::cli_abort(c(
      "{.fn mark_edge_text} needs a {.arg label} mapping.",
      i = "Map an edge attribute, e.g. {.code mark_edge_text(label = weight)}."
    ))
  }
  # Position defaults to the edge midpoint (mean of the two endpoints). Under the
  # aspect-locked graph panel a native slope equals the device slope, so an angle
  # computed from the raw endpoints rotates the label along the edge exactly.
  dots <- .with_default_aes(
    rlang::enquos(...),
    rlang::quos(x = (x + xend) / 2, y = (y + yend) / 2)
  )
  extra <- rlang::enquos(
    label = label,
    color = color,
    size = size,
    alpha = alpha
  )
  if (identical(angle, "along")) {
    extra <- c(
      extra,
      rlang::quos(angle = atan2(yend - y, xend - x) * 180 / pi)
    )
  } else if (!is.null(angle)) {
    extra <- c(extra, rlang::quos(angle = !!angle))
  }
  .add_layer(
    plot,
    "edge_text",
    dots,
    extra,
    effects = effects,
    blend = blend,
    data = data %||% plot@edge_data,
    z = 2L
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
