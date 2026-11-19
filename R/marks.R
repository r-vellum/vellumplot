#' @include classes.R
NULL

# A call to a paint constructor (a gradient or a pattern builder), whose result
# is an unscaled fill *value*, not a data-mapped channel. Namespace-qualified
# calls (`vellum::linear_gradient(...)`) match too (call_name strips the `::`).
.PAINT_CALLS <- c(
  "linear_gradient",
  "radial_gradient",
  "vl_pattern",
  "pattern_stripe",
  "pattern_crosshatch",
  "pattern_grid",
  "pattern_dot",
  "pattern_checker"
)
.is_paint_call <- function(e) {
  nm <- rlang::call_name(e)
  !is.null(nm) && nm %in% .PAINT_CALLS
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
  if (inherits(val, c("vellum_gradient", "vellum_pattern"))) val else NULL
}

# Split captured aesthetic quosures into data-mapped channels vs constant
# params. A quosure whose expression is a bare literal (number, string,
# logical) is a constant aesthetic (e.g. `size = 3`, `color = "red"`); anything
# referring to data (a symbol like `wt`, or a call like `factor(cyl)`) is a
# channel evaluated against the data at compile time.
# Positional (coordinate) aesthetics. A bare literal on one of these is a
# constant-valued coordinate that must train the position scale (e.g. a segment
# baseline `y = 0`), so it is kept as a channel rather than a style param.
#
# `width`/`height` are deliberately *not* here, even though they are geometric.
# They are not coordinates: they never train a position scale, and their meaning
# is mark-specific -- a band width in *data* units on `mark_bar`/`mark_boxplot`,
# but a box size in *physical* units on `mark_label`. So each emitter reads them
# from `params` (via `%||% band` or `.aes_param()`) and interprets them in its
# own space; a constant width already works through that path. Promoting them to
# a channel here would force one global meaning and misroute the physical-size
# case, so the `params` path is the intended design (vellumplot#3).
#
# (Named apart from trained-scale.R's `.POSITION_AES`, the two position scales:
# sharing the name let that later definition shadow this one, so a literal
# `xend`/`ymin` became a style param that trained nothing.)
.COORD_AES <- c(
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
      if (nm %in% .COORD_AES) {
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

# Shared checks for the numeric stat parameters of the mark constructors
# (`bins`, `binwidth`, `adjust`, `level`, ...), so a bad value fails where it is
# written, naming the argument, instead of deep inside the stat as a cryptic
# crash or a silently wrong plot. `NULL` passes when `null_ok` (a parameter whose
# default is derived from the data).
.stat_arg_bad <- function(x, null_ok) {
  if (is.null(x)) {
    return(!null_ok)
  }
  !is.numeric(x) || length(x) != 1L || !is.finite(x)
}

# The "Got ..." bullet of a stat-parameter error: the value itself when it is a
# single number, else its type.
.stat_arg_got <- function(x) {
  if (is.numeric(x) && length(x) == 1L) {
    cli::format_inline("Got {.val {x}}.")
  } else {
    cli::format_inline("Got {.obj_type_friendly {x}}.")
  }
}

# A count: a single whole number >= `min` (e.g. `bins`).
.check_count <- function(
  x,
  arg = rlang::caller_arg(x),
  min = 1L,
  null_ok = FALSE,
  call = rlang::caller_env()
) {
  if (is.null(x) && null_ok) {
    return(invisible(x))
  }
  if (.stat_arg_bad(x, null_ok) || x < min || x != round(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a whole number of at least {min}.",
        x = .stat_arg_got(x)
      ),
      call = call
    )
  }
  invisible(x)
}

# A strictly positive, finite number (e.g. `binwidth`, `adjust`, `span`).
.check_positive <- function(
  x,
  arg = rlang::caller_arg(x),
  null_ok = FALSE,
  call = rlang::caller_env()
) {
  if (is.null(x) && null_ok) {
    return(invisible(x))
  }
  if (.stat_arg_bad(x, null_ok) || x <= 0) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single positive number.",
        x = .stat_arg_got(x)
      ),
      call = call
    )
  }
  invisible(x)
}

# A probability strictly between 0 and 1 (e.g. a confidence `level`).
.check_prob <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (.stat_arg_bad(x, FALSE) || x <= 0 || x >= 1) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single number between 0 and 1.",
        x = .stat_arg_got(x)
      ),
      call = call
    )
  }
  invisible(x)
}

# The `orientation` of a distribution mark: `NA` (infer from the data), `"x"`
# (categories along x, the default look) or `"y"` (categories along y).
.check_orientation <- function(orientation, call = rlang::caller_env()) {
  if (length(orientation) == 1L && is.na(orientation)) {
    return(NA_character_)
  }
  rlang::arg_match(
    orientation,
    c("x", "y"),
    error_arg = "orientation",
    error_call = call
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
  if (
    !is.null(window$k) &&
      (!is.numeric(window$k) ||
        length(window$k) != 1L ||
        !is.finite(window$k) ||
        window$k < 1)
  ) {
    cli::cli_abort(
      "{.arg window} {.field k} must be a single positive integer.",
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
  # `position` is never an aesthetic. A constructor without a `position` formal
  # would let it fall into `...` and become an inert constant aesthetic; take it
  # as the layer's position instead.
  if ("position" %in% names(quos)) {
    position <- rlang::eval_tidy(quos$position)
    quos <- quos[names(quos) != "position"]
  }
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
  .check_position(
    mark,
    pos$type,
    fn = tryCatch(
      rlang::call_name(rlang::caller_call()),
      error = function(e) NULL
    ),
    call = rlang::caller_env()
  )
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
#'   linerange) also take `linewidth` and `linetype`. On `mark_line()`,
#'   `mark_step()` and `mark_segment()`, `linewidth` may be **mapped** as well as
#'   set: see [scale_linewidth()] for the tapered stroke that gives, and when
#'   it falls back to per-segment widths. Everywhere else `linewidth` is a constant. Every mark also takes
#'   `group`, which says which rows form one series without styling them:
#'   `mark_line(x = t, y = y, group = id)` draws one line per `id`. Without it,
#'   a discrete `color` (else `fill`) groups; a continuous colour never does, so
#'   a line mapped to one stays connected and changes colour along its length.
#'   `group = 1` makes the whole layer one group.
#' @param size,shape Convenience arguments for the point size (in mm) / shape;
#'   may be a constant or a mapped expression. A shape is a built-in marker
#'   (`"circle"`, `"square"`, `"triangle"`, `"diamond"`, `"plus"`, `"cross"`,
#'   `"triangle_down"`, `"star"`) or an **SVG icon** — a path `d` string or a
#'   `.svg` file — drawn as a crisp vector marker. Use a literal string for a
#'   constant icon, or map `shape` and pass icons to [scale_shape()].
#' @param position Position adjustment: a string — `"identity"` (default),
#'   `"nudge"`, `"stack"`, `"fill"`, `"dodge"`, `"dodge2"`, `"jitter"`,
#'   `"jitterdodge"`, `"sina"` — or a parameterised [position] object such as
#'   [position_dodge()] or [position_stack()]. Every mark takes `"identity"` and
#'   `"nudge"`. `"stack"`/`"fill"` work on bars, areas, points, text and labels
#'   (so a label can sit on its stacked bar); `"dodge"`/`"dodge2"` on bars,
#'   points, text, labels, error bars, line/point ranges, cross-bars, box plots,
#'   violins and slab-intervals (so error bars dodge over dodged bars);
#'   `"jitter"` on points, text and labels; `"jitterdodge"` and `"sina"` on
#'   points. A position a mark does not support is an error naming the ones it
#'   does.
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
  position = "identity",
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
    data = data,
    position = position
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
#' @param merge Dissolve adjacent features that share a fill into one region, so
#'   the internal borders between same-valued features disappear and each region
#'   is a single crisp path (no seam artefacts, and exact in PDF). The union is
#'   real boolean geometry via [vellum::vl_path_op()]. Default `FALSE`. Ignored
#'   when the layer is interactive (merging would drop the per-feature keys).
#' @param blend Optional blend mode (see [mark_point()]).
#' @param data Optional layer data (an `sf` object); overrides the plot data.
#' @param position Position adjustment; see [mark_point()]. A mark errors
#'   on a position it does not support, naming the ones it does.
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
  position = "identity",
  fill = NULL,
  color = NULL,
  alpha = NULL,
  linewidth = NULL,
  size = NULL,
  na_value = "grey80",
  merge = FALSE,
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
    stat_params = list(na_value = na_value, merge = isTRUE(merge)),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' Label sf features at their interior point
#'
#' `mark_sf_label()` draws one text label per `sf` feature, placed at the
#' feature's **interior point** (`sf::st_point_on_surface()`, guaranteed to fall
#' inside each polygon --- unlike a centroid, which can land in a bay or a hole).
#' The label points are reprojected through the *same* `coord_sf` CRS as
#' [mark_sf()], so they always land where the geometry is drawn. Layer it over a
#' `mark_sf()` choropleth to name the regions; `repel = TRUE` (the default) pushes
#' crowded labels apart.
#'
#' @inheritParams mark_text
#' @param ... Encodings (tidy-eval): the `label` to draw at each feature, plus an
#'   optional `color` / `size`. The position comes from the geometry, so `x` / `y`
#'   are not mapped.
#' @param data An `sf` data frame (defaults to the plot's). Its geometry supplies
#'   the label positions.
#' @return The modified [PlotSpec].
#' @seealso [mark_sf()], [mark_text()]
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'   vplot(nc) |>
#'     mark_sf(fill = AREA) |>
#'     mark_sf_label(label = NAME, size = 5)
#' }
#' @export
mark_sf_label <- function(
  plot,
  ...,
  position = "identity",
  size = NULL,
  repel = TRUE,
  box_padding = 1,
  min_segment_length = 2,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  .need_pkg("sf", "mark_sf_label()")
  .add_layer(
    plot,
    "sf_label",
    rlang::enquos(...),
    rlang::enquos(size = size),
    stat_params = list(
      # avoid = "labels": a feature label belongs at its interior point, so repel
      # spreads labels off each other only, never off the (huge-bbox) sf polygons.
      repel = .repel_params(
        repel,
        box_padding,
        1,
        min_segment_length,
        NULL,
        avoid = "labels"
      )
    ),
    blend = blend,
    data = data,
    position = position
  )
}

#' @rdname mark_point
#' @export
mark_rule <- function(
  plot,
  ...,
  position = "identity",
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
    data = data,
    position = position
  )
}

#' Reference lines and function curves
#'
#' `mark_abline()` draws a sloped reference line `y = slope * x + intercept`
#' across the panel (the diagonal cousin of `mark_rule()`'s horizontal/vertical
#' lines). `mark_function()` draws a curve `y = fun(x)` sampled over the panel's
#' x range — e.g. a theoretical density over a histogram, or a reference curve
#' over a scatter. Both take no data encodings; they reuse the panel's existing
#' x/y scales, so add them on top of a data layer. Both assume linear axes.
#'
#' @param plot A [PlotSpec].
#' @param ... For `mark_abline()`/`mark_function()`, styling passed through as
#'   constants (e.g. `color`, `linewidth`, `linetype`).
#' @param slope,intercept For `mark_abline()`, the line's slope and intercept
#'   (each may be a vector to draw several lines). Defaults `1` and `0`.
#' @param fun For `mark_function()`, a function (or its name) mapping a numeric
#'   `x` vector to `y`, e.g. `dnorm` or `\(x) x^2`.
#' @param n For `mark_function()`, the number of points sampled across the x
#'   range. Default `101`.
#' @param args For `mark_function()`, extra arguments passed to `fun` (e.g.
#'   `args = list(mean = 5, sd = 2)`).
#' @param blend,sketch,data See [mark_point()].
#' @param position Position adjustment; see [mark_point()]. A mark errors
#'   on a position it does not support, naming the ones it does.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> mark_abline(slope = -5, intercept = 37)
#' vplot(data.frame(x = rnorm(200))) |>
#'   mark_histogram(x = x, y = after_stat(density)) |>
#'   mark_function(fun = dnorm)
#' @export
mark_abline <- function(
  plot,
  ...,
  position = "identity",
  slope = 1,
  intercept = 0,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "abline",
    rlang::enquos(...),
    const_params = list(slope = slope, intercept = intercept),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_abline
#' @export
mark_function <- function(
  plot,
  ...,
  position = "identity",
  fun,
  n = 101,
  args = list(),
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  if (missing(fun)) {
    cli::cli_abort("{.fn mark_function} needs a {.arg fun}.")
  }
  .add_layer(
    plot,
    "fun",
    rlang::enquos(...),
    const_params = list(fun = match.fun(fun), n = n, args = args),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_point
#' @details
#' `mark_bar()` draws bars from a zero baseline. With an explicit `y` it uses the
#' `y` values as heights; with no `y` it counts rows per category (the `"count"`
#' stat). When `color`/`fill` is mapped, grouped bars are stacked by default; use
#' `position = "dodge"` for side-by-side bars or `"fill"` to normalise to 1.
#' `fill` fills the bars and a `color` given alongside it outlines them
#' (`fill = "steelblue", color = "white"`); a `color` alone fills them.
#' The count stat computes `count` and `prop`, the share of its group (or of the
#' layer, ungrouped), for use with [after_stat()], e.g.
#' `y = after_stat(prop)`. `density` is an older name for `prop`.
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
#' bins a continuous `x` and draws the per-bin counts as bars. The bin stat
#' computes `count`, `density` (each group integrates to 1), `ncount` and
#' `ndensity` (scaled to a maximum of 1), and `width`, for [after_stat()].
#' `mark_smooth()` fits a model of `y` on `x` (per group) and draws the fitted
#' line, with a confidence ribbon when `se = TRUE`.
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
#' @param bins Number of histogram bins (default 30), used when neither
#'   `binwidth` nor `breaks` is given. As in ggplot2, the outer bins are centred
#'   on the smallest and largest values unless `boundary` or `center` is set.
#' @param binwidth Width of each bin, overriding `bins`.
#' @param breaks A numeric vector of bin edges, overriding `bins` and
#'   `binwidth`. Bins may have different widths; values outside the edges are
#'   not counted.
#' @param boundary,center Where the bins are aligned: `boundary` is a position
#'   that falls on an edge between two bins, `center` one that falls on a bin's
#'   centre (give at most one). Either may lie outside the data. For example
#'   `binwidth = 1, boundary = 0` makes bins `(0, 1]`, `(1, 2]`, .... With
#'   neither, bins of a given `binwidth` are aligned on `binwidth / 2`.
#' @param closed Which side of each bin is closed: `"right"` (default, `(a, b]`)
#'   or `"left"` (`[a, b)`). The outermost edges always count.
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
#' # bins of width 2 with an edge at 10: (10, 12], (12, 14], ...
#' vplot(mtcars) |> mark_histogram(x = mpg, binwidth = 2, boundary = 10)
#' vplot(mtcars) |> mark_freqpoly(x = mpg, color = factor(cyl), binwidth = 2)
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
  binwidth = NULL,
  breaks = NULL,
  boundary = NULL,
  center = NULL,
  closed = c("right", "left"),
  position = "stack",
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  sp <- .bin_params(bins, binwidth, breaks, boundary, center, closed)
  .add_layer(
    plot,
    "bar",
    rlang::enquos(...),
    stat = "bin",
    stat_params = sp,
    position = position,
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_histogram
#' @details
#' `mark_freqpoly()` draws the same bin counts as a line through the bin
#' centres (a frequency polygon), one line per group. Like ggplot2's
#' `geom_freqpoly()` it adds an empty bin at each end, so each line starts and
#' ends at zero. It takes the same binning arguments as `mark_histogram()`, and
#' `y = after_stat(density)` draws densities instead of counts.
#' @export
mark_freqpoly <- function(
  plot,
  ...,
  bins = 30,
  binwidth = NULL,
  breaks = NULL,
  boundary = NULL,
  center = NULL,
  closed = c("right", "left"),
  position = "identity",
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  sp <- .bin_params(bins, binwidth, breaks, boundary, center, closed)
  sp$pad <- TRUE
  .add_layer(
    plot,
    "line",
    rlang::enquos(...),
    stat = "bin",
    stat_params = sp,
    position = position,
    blend = blend,
    sketch = sketch,
    data = data
  )
}

# Validate the binning arguments shared by `mark_histogram()` and
# `mark_freqpoly()` and return them as stat params (ggplot2's rules: `breaks`
# overrides `binwidth`, which overrides `bins`; `boundary` and `center` are
# alternatives).
.bin_params <- function(
  bins,
  binwidth,
  breaks,
  boundary,
  center,
  closed,
  call = rlang::caller_env()
) {
  .check_count(bins, call = call)
  .check_positive(binwidth, null_ok = TRUE, call = call)
  if (!is.null(breaks)) {
    if (!is.numeric(breaks) || sum(is.finite(unique(breaks))) < 2L) {
      cli::cli_abort(
        c(
          "{.arg breaks} must be a numeric vector of at least two bin edges.",
          x = .stat_arg_got(breaks)
        ),
        call = call
      )
    }
    breaks <- breaks[is.finite(breaks)]
  }
  for (nm in c("boundary", "center")) {
    v <- list(boundary = boundary, center = center)[[nm]]
    if (!is.null(v) && (!is.numeric(v) || length(v) != 1L || !is.finite(v))) {
      cli::cli_abort(
        c("{.arg {nm}} must be a single number.", x = .stat_arg_got(v)),
        call = call
      )
    }
  }
  if (!is.null(boundary) && !is.null(center)) {
    cli::cli_abort(
      "Give only one of {.arg boundary} and {.arg center}.",
      call = call
    )
  }
  closed <- rlang::arg_match(closed, c("right", "left"), error_call = call)
  list(
    bins = bins,
    binwidth = binwidth,
    breaks = breaks,
    boundary = boundary,
    center = center,
    closed = closed
  )
}

#' @rdname mark_histogram
#' @export
mark_smooth <- function(
  plot,
  ...,
  position = "identity",
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
  .check_positive(span)
  .check_prob(level)
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
    data = data,
    position = position
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
  position = "identity",
  type = "t",
  level = 0.95,
  segments = 51L,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .check_count(segments, min = 3L)
  if (identical(type, "euclid")) {
    .check_positive(level)
  } else {
    .check_prob(level)
  }
  .add_layer(
    plot,
    "ellipse",
    rlang::enquos(...),
    stat = "ellipse",
    stat_params = list(type = type, level = level, segments = segments),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_ellipse
#' @param expand For `mark_hull()`, a margin to grow each hull outward by, as a
#'   fraction of the layer's data range on each axis (e.g. `0.05` = 5% of the
#'   width and height), so it encloses the markers it wraps rather than passing
#'   through their centres. Every hull grows by the same margin on every side,
#'   with rounded corners, whatever its size or shape. `0` (default) is the
#'   tight hull.
#' @export
mark_hull <- function(
  plot,
  ...,
  position = "identity",
  expand = 0,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  if (!isTRUE(all.equal(expand, 0))) {
    .check_positive(expand)
  }
  .add_layer(
    plot,
    "hull",
    rlang::enquos(...),
    stat = "hull",
    stat_params = list(expand = as.numeric(expand)),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
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
  position = "identity",
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
  how <- rlang::arg_match0(how, c("eq_hist", "log", "cbrt", "linear"))
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
    data = data,
    position = position
  )
}

#' Area, ribbon, and step marks
#'
#' `mark_area()` fills the region between a `y` line and the zero baseline;
#' `mark_ribbon()` fills between `ymin` and `ymax`; `mark_step()` draws a
#' staircase line. All connect points in `x` order.
#'
#' An area or ribbon is drawn as one polygon per series (per `group`, or per
#' level of a discrete `fill`/`color`), and a polygon has a single fill. A
#' continuous `fill`, `color` or `alpha` must therefore be constant within each
#' series; one that varies along a series is an error. A `color` given
#' alongside a `fill` outlines the band: an area along its top, a ribbon along
#' both edges. A `color` alone fills it.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x` and `y` for area/step; `x`, `ymin`,
#'   `ymax` for ribbon; plus `color`/`fill`/`alpha`. `mark_step()` also takes
#'   `linewidth` and `linetype`; a mapped `linewidth` varies the staircase's
#'   width per tread (see [scale_linewidth()]).
#' @param position For `mark_area()`, how areas sharing an `x` combine when
#'   `fill`/`color` is mapped: `"stack"` (default) stacks them into a band,
#'   `"fill"` normalises each `x` to 1, `"identity"` overlays them from the zero
#'   baseline. With no fill mapping all three are equivalent (a single area).
#' @param direction For `mark_step()`, `"hv"` (horizontal then vertical, default)
#'   or `"vh"`.
#' @return The modified [PlotSpec].
#' @seealso [scale_linewidth()]
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
mark_ribbon <- function(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "ribbon",
    rlang::enquos(...),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_area
#' @export
mark_step <- function(
  plot,
  ...,
  position = "identity",
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
    data = data,
    position = position
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
#' @param box_padding Extra space (mm) kept around each label box during
#'   repulsion.
#' @param point_padding Retained for back-compatibility and currently ignored:
#'   the repulsion solver is deterministic and pads uniformly.
#' @param min_segment_length Shortest leader line (mm) worth drawing; a label
#'   that barely moved gets none.
#' @param seed Retained for back-compatibility and currently a no-op: the repel
#'   layout is deterministic, so there is nothing for a seed to vary.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_text(x = wt, y = mpg, label = rownames(mtcars))
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |>
#'   mark_text(x = wt, y = mpg, label = rownames(mtcars), nudge_y = 2)
#' @export
mark_text <- function(
  plot,
  ...,
  position = "identity",
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
  seed = NULL,
  effects = list(),
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
        seed
      )
    ),
    effects = effects,
    blend = blend,
    data = data,
    position = position
  )
}

#' @rdname mark_text
#' @export
mark_label <- function(
  plot,
  ...,
  position = "identity",
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
  seed = NULL,
  effects = list(),
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
        seed
      )
    ),
    effects = effects,
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' Direct series labels
#'
#' `mark_series_label()` labels each colour/fill **series at its end** (the point
#' with the largest `x`) with the series name — a legend-free alternative that
#' reads faster on a multi-line chart. Labels are pushed apart with the same
#' repel solver as [mark_text()]. Map the same `x`/`y`/`color` as the lines; the
#' label text and colour come from the series automatically.
#'
#' @inheritParams mark_text
#' @param ... Encodings (tidy-eval): the same `x`, `y`, and `color`/`fill` as the
#'   series being labelled.
#' @param nudge_x Rightward offset (mm) so a label clears its line end (default
#'   `2`). Widen the x range (or `coord_cartesian(clip = FALSE)`) if labels crowd
#'   the panel edge.
#' @return The modified [PlotSpec].
#' @seealso [mark_text()]
#' @examples
#' d <- data.frame(
#'   t = rep(1:10, 3), v = c(1:10, (1:10) * 1.5, 10:1),
#'   s = rep(c("a", "b", "c"), each = 10)
#' )
#' # Give the panel a little x-room and drop the now-redundant colour legend.
#' vplot(d) |>
#'   mark_line(x = t, y = v, color = s) |>
#'   mark_series_label(x = t, y = v, color = s) |>
#'   xlim(1, 12) |>
#'   guides(color = "none")
#' @export
mark_series_label <- function(
  plot,
  ...,
  position = "identity",
  size = NULL,
  nudge_x = 2,
  repel = TRUE,
  box_padding = 1,
  min_segment_length = 2,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  dots <- rlang::enquos(...)
  dots <- .default_channel(dots, "label", rlang::quo(after_stat(lab)))
  .add_layer(
    plot,
    "text",
    dots,
    rlang::enquos(size = size),
    # left-justify so the label starts at the (nudged) line end and reads rightward
    const_params = list(nudge_x = as.numeric(nudge_x), hjust = "left"),
    stat = "series_label",
    stat_params = list(
      # avoid = "labels": a line-end label belongs at its anchor, so it may be
      # pushed off neighbouring labels but never off the (diagonal-bbox) lines.
      repel = .repel_params(
        repel,
        box_padding,
        1,
        min_segment_length,
        NULL,
        avoid = "labels"
      )
    ),
    blend = blend,
    data = data,
    position = position
  )
}

#' Label the outliers
#'
#' `mark_outlier_label()` labels only the **outlying** points — the extremes,
#' where the story usually is — instead of every datum. It keeps the rows whose
#' `y` is an outlier (per colour/fill group) and labels just those, repelled
#' apart with the same solver as [mark_text()]. Layer it over the raw points.
#'
#' The default `method = "iqr"` flags `y` outside `[Q1 - k*IQR, Q3 + k*IQR]` (the
#' boxplot whisker rule); `method = "sd"` flags `|y - mean| > k * sd`. Map a
#' `label` to name each outlier; with none mapped, the outlier's `y` value is the
#' label.
#'
#' @inheritParams mark_text
#' @param ... Encodings (tidy-eval): `x`, `y`, an optional `label` (what to write
#'   on each outlier), and optionally `color` / `fill` to detect outliers within
#'   each group.
#' @param method Outlier rule: `"iqr"` (Tukey's, the default) or `"sd"`.
#' @param k Threshold multiplier — IQR whisker length (default `1.5`) or number
#'   of standard deviations.
#' @return The modified [PlotSpec].
#' @seealso [mark_text()], [mark_boxplot()]
#' @examples
#' vplot(mtcars) |>
#'   mark_point(x = wt, y = mpg) |>
#'   mark_outlier_label(x = wt, y = mpg, label = rownames(mtcars))
#' @export
mark_outlier_label <- function(
  plot,
  ...,
  position = "identity",
  method = c("iqr", "sd"),
  k = 1.5,
  size = NULL,
  repel = TRUE,
  box_padding = 1,
  min_segment_length = 2,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  method <- match.arg(method)
  dots <- rlang::enquos(...)
  # No `label` mapped? fall back to the outlier's y value (the stat's `.olab`).
  dots <- .default_channel(dots, "label", rlang::quo(after_stat(.olab)))
  .add_layer(
    plot,
    "text",
    dots,
    rlang::enquos(size = size),
    stat = "outlier",
    stat_params = list(
      method = method,
      k = as.numeric(k),
      repel = .repel_params(repel, box_padding, 1, min_segment_length, NULL)
    ),
    blend = blend,
    data = data,
    position = position
  )
}

#' Text set along a path
#'
#' `mark_text_path()` draws a label that *follows a curve* — one string per
#' group, its glyphs placed along the group's `x`/`y` points (in data order) and
#' rotated to the local tangent. Use it for a label riding a contour, an arc, or
#' a trend line (a direct-labelling alternative to a legend), where
#' [mark_text()]'s point-anchored, axis-aligned labels do not fit.
#'
#' One label is drawn per group. Split groups the usual way — a discrete `color`
#' (or a distinct `label`) starts a new run — and the label is taken from the
#' group's first row (it is constant along the path). The points are used in the
#' order they appear in the data: pre-sort if the curve should be traversed in a
#' particular direction. Because glyphs follow the tangent (like SVG `textPath`),
#' a label on the underside of a closed curve reads upside-down; reverse the path
#' to flip it.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y` (the path), `label` (the string),
#'   and optionally `color` / `group` to split into separate runs.
#' @param size Font size in points.
#' @param family,fontface Font family and face (`"plain"`, `"bold"`, ...).
#' @param hjust Where the run sits along the path: `"left"` starts it at the
#'   first point, `"centre"` centres it, `"right"` ends it at the last point.
#' @param vjust Vertical placement of the glyphs against the baseline.
#' @param offset Perpendicular standoff from the path, in points (positive is to
#'   the left of travel), for a label riding just above or below its curve.
#' @return The modified [PlotSpec].
#' @examples
#' # Traverse the arc left-to-right so the glyphs read upright (a path walked
#' # right-to-left would set the label upside-down).
#' t <- seq(pi, 0, length.out = 40)
#' d <- data.frame(x = cos(t), y = sin(t), lab = "along the arc")
#' vplot(d) |> mark_text_path(x = x, y = y, label = lab)
#' @export
mark_text_path <- function(
  plot,
  ...,
  position = "identity",
  size = NULL,
  family = NULL,
  fontface = NULL,
  hjust = "centre",
  vjust = "centre",
  offset = 0,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "text_path",
    rlang::enquos(...),
    rlang::enquos(
      size = size,
      family = family,
      fontface = fontface,
      hjust = hjust,
      vjust = vjust,
      offset = offset
    ),
    blend = blend,
    data = data,
    position = position
  )
}

#' Heatmap marks
#'
#' `mark_tile()` draws a rectangular tile at each `(x, y)` coloured by `fill`;
#' `mark_raster()` draws the same as one raster image (a fast path requiring a
#' complete regular grid). `mark_bin2d()` bins continuous `x`/`y` into a grid and
#' colours each cell by count. `mark_density()` draws a 1-D kernel density of `x`
#' as a filled curve. On all of them a `color` given alongside a `fill` (or the
#' default count fill of `mark_bin2d()` / `mark_hex()`) outlines the shapes; a
#' `color` alone fills them.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y`, `fill` for tile/raster; `x`, `y`
#'   for bin2d; `x` (+ `fill`/`color`) for density.
#' @param bins Number of bins per axis for `mark_bin2d()` / hex columns for
#'   `mark_hex()`.
#' @param adjust Bandwidth multiplier for `mark_density()`.
#' @param trim For `mark_density()`, `FALSE` (default) estimates every group's
#'   density over the range of the whole layer's `x`, as ggplot2 does; `TRUE`
#'   trims each group's curve to the range of its own data. The curve never runs
#'   past the data, so the axis is not padded by the kernel's tails.
#' @return The modified [PlotSpec].
#' @examples
#' d <- expand.grid(x = 1:5, y = 1:5)
#' d$z <- d$x * d$y
#' vplot(d) |> mark_tile(x = x, y = y, fill = z)
#' @export
mark_tile <- function(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "tile",
    rlang::enquos(...),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_tile
#' @export
mark_raster <- function(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "raster",
    rlang::enquos(...),
    blend = blend,
    data = data,
    position = position
  )
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
  position = "identity",
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
    data = data,
    position = position
  )
}

#' @rdname mark_tile
#' @export
mark_bin2d <- function(
  plot,
  ...,
  position = "identity",
  bins = 30,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  .check_count(bins)
  dots <- rlang::enquos(...)
  dots <- .default_channel(dots, "fill", rlang::quo(after_stat(count)))
  .add_layer(
    plot,
    "tile",
    dots,
    stat = "bin2d",
    stat_params = list(bins = bins),
    blend = blend,
    data = data,
    position = position
  )
}

#' @rdname mark_tile
#' @export
mark_density <- function(
  plot,
  ...,
  position = "identity",
  adjust = 1,
  trim = FALSE,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .check_positive(adjust)
  .add_layer(
    plot,
    "area",
    rlang::enquos(...),
    stat = "density",
    stat_params = list(adjust = adjust, trim = isTRUE(trim)),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' 2-D density contours
#'
#' `mark_contour()` draws iso-density contour **lines** of a 2-D point cloud
#' (`x`, `y`); `mark_contour_filled()` fills the bands between them. By default the
#' field is a kernel density estimate (needs the \pkg{MASS} package); map a `z`
#' aesthetic to instead contour a supplied surface over a regular `x`/`y` grid.
#' Contours are coloured by level automatically — `mark_contour()` maps
#' `color = after_stat(level)`, `mark_contour_filled()` maps `fill`. Contour
#' tracing is done by the engine (`vellum::vl_contour()`), so no extra package is
#' needed for the lines; a density field additionally needs \pkg{MASS}.
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
  position = "identity",
  bins = 10,
  binwidth = NULL,
  breaks = NULL,
  n = 100,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .check_count(bins)
  .check_positive(binwidth, null_ok = TRUE)
  .check_count(n, min = 2L)
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
    data = data,
    position = position
  )
}

#' @rdname mark_contour
#' @export
mark_contour_filled <- function(
  plot,
  ...,
  position = "identity",
  bins = 10,
  binwidth = NULL,
  breaks = NULL,
  n = 100,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .check_count(bins)
  .check_positive(binwidth, null_ok = TRUE)
  .check_count(n, min = 2L)
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
    data = data,
    position = position
  )
}

#' Quantile regression lines
#'
#' `mark_quantile()` fits a quantile regression of `y` on `x` per group for
#' each of `quantiles` and draws the fitted lines (ggplot2's
#' `geom_quantile()`). The fit is [quantreg::rq()] of `formula` (default
#' `y ~ x`), predicted on 100 points across the group's range of `x`. Map
#' `color = after_stat(factor(quantile))` (or `linetype`) to tell the lines
#' apart. Needs the \pkg{quantreg} package.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y` (+ `color`, `group`,
#'   `linewidth`, `linetype`, `alpha`).
#' @param quantiles The conditional quantiles to fit (default
#'   `c(0.25, 0.5, 0.75)`).
#' @param formula The model formula in `x` and `y` (default `y ~ x`), e.g.
#'   `y ~ splines::ns(x, 3)` for a curve.
#' @param method.args A list of further arguments to [quantreg::rq()].
#' @return The modified [PlotSpec].
#' @seealso [mark_smooth()] with `method = "rq"` for one quantile with the
#'   smoother's options.
#' @examples
#' \donttest{
#' if (requireNamespace("quantreg", quietly = TRUE)) {
#'   vplot(mtcars) |>
#'     mark_point(x = disp, y = mpg) |>
#'     mark_quantile(x = disp, y = mpg)
#' }
#' }
#' @export
mark_quantile <- function(
  plot,
  ...,
  quantiles = c(0.25, 0.5, 0.75),
  formula = NULL,
  method.args = list(),
  position = "identity",
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  if (
    !is.numeric(quantiles) ||
      !length(quantiles) ||
      any(!is.finite(quantiles) | quantiles <= 0 | quantiles >= 1)
  ) {
    cli::cli_abort("{.arg quantiles} must be probabilities between 0 and 1.")
  }
  if (!is.null(formula) && !inherits(formula, "formula")) {
    cli::cli_abort("{.arg formula} must be a formula, like {.code y ~ x}.")
  }
  if (!is.list(method.args)) {
    cli::cli_abort("{.arg method.args} must be a list.")
  }
  dots <- rlang::enquos(...)
  if (!any(c("color", "colour") %in% names(dots))) {
    dots$color <- rlang::quo("#3366CC")
  }
  .add_layer(
    plot,
    "line",
    dots,
    stat = "quantile",
    stat_params = list(
      quantiles = quantiles,
      formula = formula,
      method.args = method.args
    ),
    blend = blend,
    effects = effects,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' 2-D density estimate
#'
#' `mark_density_2d()` draws the contour lines of a 2-D kernel density estimate
#' of (`x`, `y`) and `mark_density_2d_filled()` fills the bands between them,
#' with the semantics of ggplot2's `geom_density_2d()` and
#' `geom_density_2d_filled()`: the density is [MASS::kde2d()] on an `n` x `n`
#' grid over the data range with bandwidths `h` (default
#' [MASS::bandwidth.nrd()] per axis) times `adjust`, and the levels are ~`bins`
#' pretty breaks of the density over every group. Unlike [mark_contour()], the
#' lines are not coloured by level unless you map `color = after_stat(level)`;
#' the filled bands map `fill = after_stat(level)`. A discrete `color`/`fill`
#' (or `group`) estimates one density per group. Needs the \pkg{MASS} package.
#'
#' @inheritParams mark_contour
#' @param ... Encodings (tidy-eval): `x`, `y` (+ `color`/`fill`, `group`,
#'   `linewidth`, `linetype`, `alpha`).
#' @param contour_var The variable to contour: `"density"` (default),
#'   `"ndensity"` (density scaled to a maximum of 1 per group) or `"count"`
#'   (density times the number of observations in the group).
#' @param n Number of grid points per axis.
#' @param h Bandwidth (the standard deviation of the kernel) per axis: a
#'   number or a pair; `NULL` (default) uses [MASS::bandwidth.nrd()].
#' @param adjust Multiplier(s) for the bandwidth, a number or a pair (default
#'   `c(1, 1)`).
#' @return The modified [PlotSpec].
#' @seealso [mark_contour()] to contour a supplied `z` surface.
#' @examples
#' vplot(faithful) |>
#'   mark_point(x = eruptions, y = waiting) |>
#'   mark_density_2d(x = eruptions, y = waiting)
#' vplot(faithful) |> mark_density_2d_filled(x = eruptions, y = waiting)
#' @export
mark_density_2d <- function(
  plot,
  ...,
  position = "identity",
  contour_var = "density",
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  n = 100,
  h = NULL,
  adjust = c(1, 1),
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .density_2d_layer(
    plot,
    rlang::enquos(...),
    "contour",
    position = position,
    contour_var = contour_var,
    bins = bins,
    binwidth = binwidth,
    breaks = breaks,
    n = n,
    h = h,
    adjust = adjust,
    blend = blend,
    sketch = sketch,
    data = data
  )
}

#' @rdname mark_density_2d
#' @export
mark_density_2d_filled <- function(
  plot,
  ...,
  position = "identity",
  contour_var = "density",
  bins = NULL,
  binwidth = NULL,
  breaks = NULL,
  n = 100,
  h = NULL,
  adjust = c(1, 1),
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  dots <- rlang::enquos(...)
  dots <- .default_channel(dots, "fill", rlang::quo(after_stat(level)))
  .density_2d_layer(
    plot,
    dots,
    "contour_filled",
    position = position,
    contour_var = contour_var,
    bins = bins,
    binwidth = binwidth,
    breaks = breaks,
    n = n,
    h = h,
    adjust = adjust,
    blend = blend,
    sketch = sketch,
    data = data
  )
}

# The shared constructor of the two 2-D density marks.
.density_2d_layer <- function(
  plot,
  dots,
  mark,
  position,
  contour_var,
  bins,
  binwidth,
  breaks,
  n,
  h,
  adjust,
  blend,
  sketch,
  data,
  call = rlang::caller_env()
) {
  .check_plot(plot, call = call)
  .check_count(bins, null_ok = TRUE, call = call)
  .check_positive(binwidth, null_ok = TRUE, call = call)
  .check_count(n, min = 2L, call = call)
  for (nm in c("h", "adjust")) {
    v <- if (nm == "h") h else adjust
    if (
      !is.null(v) &&
        (!is.numeric(v) || !length(v) %in% 1:2 || any(!is.finite(v) | v <= 0))
    ) {
      cli::cli_abort(
        "{.arg {nm}} must be one or two positive numbers.",
        call = call
      )
    }
  }
  contour_var <- rlang::arg_match0(
    contour_var,
    c("density", "ndensity", "count"),
    arg_nm = "contour_var",
    error_call = call
  )
  .add_layer(
    plot,
    mark,
    dots,
    stat = "density_2d",
    stat_params = list(
      bins = bins,
      binwidth = binwidth,
      breaks = breaks,
      n = n,
      h = h,
      adjust = adjust,
      contour_var = contour_var,
      filled = if (identical(mark, "contour_filled")) TRUE
    ),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
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
#' @param position Position adjustment; see [mark_point()]. A mark errors
#'   on a position it does not support, naming the ones it does.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_ecdf(x = mpg)
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> mark_rug()
#' vplot(mtcars) |> mark_qq(sample = mpg) |> mark_qq_line(sample = mpg)
#' @export
mark_ecdf <- function(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "step",
    rlang::enquos(...),
    stat = "ecdf",
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_ecdf
#' @export
mark_rug <- function(
  plot,
  ...,
  position = "identity",
  sides = "bl",
  length = 0.03,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  if (
    !is.character(sides) ||
      length(sides) != 1L ||
      !grepl("^[tblr]+$", sides)
  ) {
    cli::cli_abort(c(
      "{.arg sides} must be a string of the letters {.val t}, {.val b}, {.val l}, {.val r}.",
      i = 'For example {.code sides = "bl"} for the bottom and left edges.'
    ))
  }
  .add_layer(
    plot,
    "rug",
    rlang::enquos(...),
    stat = "identity",
    stat_params = list(sides = sides, length = length),
    blend = blend,
    data = data,
    position = position
  )
}

#' @rdname mark_ecdf
#' @export
mark_qq <- function(
  plot,
  ...,
  position = "identity",
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
    data = data,
    position = position
  )
}

#' @rdname mark_ecdf
#' @export
mark_qq_line <- function(
  plot,
  ...,
  position = "identity",
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
    data = data,
    position = position
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
#' @param orientation For `mark_violin()`, `NA` (default) to infer it from the
#'   data (horizontal when `y` is discrete and `x` is not, as in ggplot2), `"x"`
#'   for vertical violins along a discrete `x`, or `"y"` for horizontal violins
#'   along `y`.
#' @param trim For `mark_violin()`, `TRUE` (default, as ggplot2) ends each violin
#'   at the smallest and largest value of its data; `FALSE` lets the kernel's
#'   tails run on past them.
#' @param height Ridge height as a multiple of the row band (ridgeline; default
#'   `1.4`, so adjacent ridges overlap slightly).
#' @param binwidth Dot-plot bin width, or `NULL` to use ~1/30 of the data range.
#' @param blend,sketch,data Standard layer arguments (see [mark_point()]).
#' @param position Position adjustment; see [mark_point()]. A mark errors
#'   on a position it does not support, naming the ones it does.
#' @return The modified [PlotSpec].
#' @examples
#' df <- data.frame(g = rep(letters[1:3], each = 50), v = rnorm(150))
#' vplot(df) |> mark_violin(x = g, y = v)
#' vplot(df) |> mark_violin(x = v, y = g) # horizontal
#' vplot(df) |> mark_ridgeline(x = v, y = g)
#' vplot(df) |> mark_dotplot(x = v)
#' @export
mark_violin <- function(
  plot,
  ...,
  position = "identity",
  adjust = 1,
  trim = TRUE,
  orientation = NA,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .check_positive(adjust)
  .add_layer(
    plot,
    "violin",
    rlang::enquos(...),
    stat = "identity",
    stat_params = list(
      adjust = adjust,
      trim = isTRUE(trim),
      orientation = .check_orientation(orientation)
    ),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_violin
#' @export
mark_ridgeline <- function(
  plot,
  ...,
  position = "identity",
  adjust = 1,
  height = 1.4,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .check_positive(adjust)
  .check_positive(height)
  .add_layer(
    plot,
    "ridgeline",
    rlang::enquos(...),
    stat = "identity",
    stat_params = list(adjust = adjust, height = height),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_violin
#' @export
mark_dotplot <- function(
  plot,
  ...,
  position = "identity",
  binwidth = NULL,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .check_positive(binwidth, null_ok = TRUE)
  .add_layer(
    plot,
    "point",
    rlang::enquos(...),
    stat = "dotplot",
    stat_params = list(binwidth = binwidth),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' Uncertainty marks (slab + interval)
#'
#' Marks for visualising a distribution's shape *and* its summary intervals
#' together, in the style of the \pkg{ggdist} package — a natural fit for
#' posterior draws or bootstrap samples (one row per draw, grouped by a
#' categorical `x`).
#'
#' * `mark_halfeye()` draws, per category, a one-sided density "slab" (a half
#'   violin) with a **point-interval** at its base: the median (or mean) as a
#'   point, a thick bar for the inner interval and a thin bar for the outer one.
#' * `mark_interval()` draws the point-interval alone (no slab).
#'
#' Intervals are equal-tailed quantile intervals at the `.width` probabilities
#' (widest drawn thinnest). Supply many draws per `x` (e.g. posterior samples).
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): a categorical `x` and the sample `y`
#'   (+ `color`/`fill`), or a categorical `y` and the sample `x` for horizontal
#'   intervals.
#' @param .width Interval probabilities, widest last (default
#'   `c(0.66, 0.95)`).
#' @param point Central summary: `"median"` (default) or `"mean"`.
#' @param adjust `mark_halfeye()` density bandwidth multiplier.
#' @param scale Slab width as a fraction of the category band (`mark_halfeye()`).
#' @param orientation `NA` (default) to infer it from the data (horizontal when
#'   `y` is discrete and `x` is not, as in ggplot2), `"x"` for vertical intervals
#'   along a discrete `x` (the slab to the right of each), or `"y"` for
#'   horizontal intervals along `y` (the slab above each).
#' @return The modified [PlotSpec].
#' @examples
#' set.seed(1)
#' draws <- data.frame(
#'   grp = rep(c("a", "b", "c"), each = 400),
#'   val = rnorm(1200, rep(c(0, 1, 2), each = 400))
#' )
#' vplot(draws) |> mark_halfeye(x = grp, y = val)
#' vplot(draws) |> mark_interval(x = grp, y = val)
#' # horizontal: the discrete variable on y
#' vplot(draws) |> mark_halfeye(x = val, y = grp)
#' @export
mark_halfeye <- function(
  plot,
  ...,
  position = "identity",
  .width = c(0.66, 0.95),
  point = "median",
  adjust = 1,
  scale = 0.9,
  orientation = NA,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .check_positive(adjust)
  .check_positive(scale)
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
      scale = scale,
      orientation = .check_orientation(orientation)
    ),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_halfeye
#' @export
mark_interval <- function(
  plot,
  ...,
  position = "identity",
  .width = c(0.66, 0.95),
  point = "median",
  orientation = NA,
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
    stat_params = list(
      width = .width,
      point = point,
      orientation = .check_orientation(orientation)
    ),
    blend = blend,
    data = data,
    position = position
  )
}

#' Raincloud plot
#'
#' `mark_raincloud()` is a convenience that composes a **raincloud**: a one-sided
#' density "cloud" ([mark_halfeye()]) with the raw observations as "rain" below
#' it ([mark_point()] with sina spread). It is exactly
#' `plot |> mark_halfeye(...) |> mark_point(..., position = position_sina())`,
#' so any `color`/`fill` mapping in `...` flows to both layers.
#'
#' @param plot A [PlotSpec].
#' @param x,y The categorical `x` and the sample `y` (tidy-eval).
#' @param ... Further shared encodings (e.g. `color`, `fill`) forwarded to both
#'   the slab and the points.
#' @param width Sina spread of the rain, as a fraction of the band (default
#'   `0.4`).
#' @param alpha Point opacity for the rain (default `0.5`).
#' @return The modified [PlotSpec].
#' @seealso [mark_halfeye()], [position_sina()]
#' @examples
#' set.seed(1)
#' d <- data.frame(g = rep(c("a", "b", "c"), each = 100),
#'                 y = rnorm(300, rep(c(0, 1, 2), each = 100)))
#' vplot(d) |> mark_raincloud(x = g, y = y)
#' @export
mark_raincloud <- function(plot, x, y, ..., width = 0.4, alpha = 0.5) {
  .check_plot(plot)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  dots <- rlang::enquos(...)
  plot |>
    mark_halfeye(x = !!x, y = !!y, !!!dots) |>
    mark_point(
      x = !!x,
      y = !!y,
      !!!dots,
      position = position_sina(width = width),
      # unquote the value so it is a constant param, not a mapped `alpha` symbol
      # (which would train a spurious alpha scale + legend)
      alpha = !!alpha
    )
}

#' @rdname mark_tile
#' @export
mark_hex <- function(
  plot,
  ...,
  position = "identity",
  bins = 30,
  blend = NULL,
  data = NULL
) {
  .check_plot(plot)
  .check_count(bins)
  dots <- rlang::enquos(...)
  dots <- .default_channel(dots, "fill", rlang::quo(after_stat(count)))
  .add_layer(
    plot,
    "hex",
    dots,
    stat = "hexbin",
    stat_params = list(bins = bins),
    blend = blend,
    data = data,
    position = position
  )
}

#' Boxplot, error bar, and summary marks
#'
#' `mark_boxplot()` draws a box-and-whisker per `x` category from the raw `y`
#' values (box = Q1-Q3, median line, 1.5*IQR whiskers, outlier points).
#' `mark_errorbar()` draws vertical bars from `ymin` to `ymax` with horizontal
#' caps; `mark_linerange()` omits the caps. `mark_summary()` aggregates `y` per
#' `x` with `fun` (default mean) and draws the result as points; give it
#' `fun.data` (or `fun.min` / `fun.max`) and it draws each summary as a point
#' with a range instead, like ggplot2's `stat_summary()`.
#'
#' A box plot is vertical when `x` is the discrete axis. When `y` is discrete
#' and `x` continuous (`mark_boxplot(x = mpg, y = factor(cyl))`) it is drawn
#' horizontally, as in ggplot2; `orientation` forces either.
#' Error bars and ranges are horizontal when they map `xmin`/`xmax` at each `y`
#' instead: `mark_errorbar(y = g, xmin = lo, xmax = hi)`.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y` for boxplot/summary; `x`, `ymin`,
#'   `ymax` (or `y`, `xmin`, `xmax`) for errorbar/linerange; plus `color`/`fill` (and `linetype` for
#'   errorbar/linerange).
#' @param width For `mark_errorbar()`, the cap width as a fraction of the band.
#' @param fun For `mark_summary()`, the aggregation function (default `mean`).
#'   With `fun.min` / `fun.max` it gives the point of the range.
#' @param fun.data For `mark_summary()`, a summary giving a point and a range
#'   per category: `"mean_se"` (mean +/- one standard error), `"mean_cl_normal"`
#'   (mean with a t-based confidence interval), `"mean_sdl"` (mean +/- two
#'   standard deviations), `"median_hilow"` (median with the central 95% of the
#'   data), or a function of the values that returns `y`, `ymin` and `ymax`
#'   (such as ggplot2's `mean_se()`). Overrides `fun`, `fun.min` and `fun.max`.
#' @param fun.min,fun.max For `mark_summary()`, functions giving the bottom and
#'   top of each category's range (e.g. `fun.min = min, fun.max = max`).
#' @param fun.args For `mark_summary()`, a list of extra arguments to the
#'   summary functions, e.g. `list(mult = 2)` for `"mean_se"` or
#'   `list(conf.int = 0.9)` for `"mean_cl_normal"`.
#' @param orientation For `mark_boxplot()`, `NA` (default) to infer it from the
#'   data (horizontal when `y` is discrete and `x` is not), `"x"` for vertical
#'   boxes along a discrete `x`, or `"y"` for horizontal boxes along `y`. For
#'   `mark_errorbar()`, `mark_linerange()` and `mark_pointrange()`, `NA`
#'   (default) draws horizontal ranges when the layer maps `xmin`/`xmax` and no
#'   `ymin`/`ymax` (ggplot2's `geom_errorbarh()`), vertical ones otherwise;
#'   `"x"` / `"y"` force either.
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg)
#' # horizontal: the discrete variable on y
#' vplot(mtcars) |> mark_boxplot(x = mpg, y = factor(cyl))
#' # mean and standard error per category
#' vplot(mtcars) |> mark_summary(x = factor(cyl), y = mpg, fun.data = "mean_se")
#' # horizontal error bars
#' est <- data.frame(term = c("a", "b", "c"), lo = c(-1, 0.2, 1), hi = c(1, 1.5, 2.4))
#' vplot(est) |> mark_errorbar(y = term, xmin = lo, xmax = hi, width = 0.3)
#' @export
mark_boxplot <- function(
  plot,
  ...,
  position = "identity",
  orientation = NA,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "boxplot",
    rlang::enquos(...),
    stat_params = list(orientation = .check_orientation(orientation)),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' Significance brackets
#'
#' `mark_signif()` runs a pairwise test between `x` groups and draws a
#' significance **bracket** over each comparison with its p-value (or stars) —
#' the ggsignif / ggpubr idiom. Add it on top of a `mark_boxplot()` /
#' `mark_violin()`; the brackets stack above the data and the y-axis expands to
#' fit them.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): a categorical `x` and the numeric `y`.
#' @param comparisons A list of length-2 character vectors naming the group
#'   pairs to test, e.g. `list(c("a", "b"), c("b", "c"))`. `NULL` (default) tests
#'   each adjacent pair of levels.
#' @param method The two-sample test: `"wilcox.test"` (default) or `"t.test"`.
#' @param label `"p"` (default, a formatted p-value) or `"stars"`
#'   (`*`/`**`/`***`/`****`/`ns`).
#' @param step Vertical gap between stacked brackets, as a fraction of the data's
#'   y range (default `0.12`).
#' @param tip_length Bracket down-tick length, as a fraction of the y range
#'   (default `0.03`).
#' @return The modified [PlotSpec].
#' @examples
#' set.seed(1)
#' d <- data.frame(g = rep(c("a", "b", "c"), each = 30),
#'                 y = c(rnorm(30), rnorm(30, 1), rnorm(30, 0.4)))
#' vplot(d) |>
#'   mark_boxplot(x = g, y = y) |>
#'   mark_signif(x = g, y = y, comparisons = list(c("a", "b"), c("a", "c")))
#' @export
mark_signif <- function(
  plot,
  ...,
  position = "identity",
  comparisons = NULL,
  method = c("wilcox.test", "t.test"),
  label = c("p", "stars"),
  step = 0.12,
  tip_length = 0.03,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  method <- match.arg(method)
  label <- match.arg(label)
  .add_layer(
    plot,
    "signif",
    rlang::enquos(...),
    stat = "signif",
    stat_params = list(
      comparisons = comparisons,
      method = method,
      label = label,
      step = step,
      tip_length = tip_length
    ),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_boxplot
#' @export
mark_errorbar <- function(
  plot,
  ...,
  position = "identity",
  orientation = NA,
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
    stat_params = list(orientation = .check_orientation(orientation)),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_boxplot
#' @export
mark_linerange <- function(
  plot,
  ...,
  position = "identity",
  orientation = NA,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "linerange",
    rlang::enquos(...),
    stat_params = list(orientation = .check_orientation(orientation)),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_boxplot
#' @export
mark_summary <- function(
  plot,
  ...,
  position = "identity",
  fun = mean,
  fun.data = NULL,
  fun.min = NULL,
  fun.max = NULL,
  fun.args = list(),
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  if (is.character(fun.data)) {
    fun.data <- rlang::arg_match(
      fun.data,
      names(.SUMMARY_FUNS),
      error_arg = "fun.data"
    )
  } else if (!is.null(fun.data) && !is.function(fun.data)) {
    cli::cli_abort(
      "{.arg fun.data} must be a function or one of {.or {.val {names(.SUMMARY_FUNS)}}}."
    )
  }
  if (!is.list(fun.args)) {
    cli::cli_abort("{.arg fun.args} must be a list.")
  }
  ranged <- !is.null(fun.data) || !is.null(fun.min) || !is.null(fun.max)
  .add_layer(
    plot,
    if (ranged) "pointrange" else "point",
    rlang::enquos(...),
    stat = if (ranged) "summary" else "aggregate",
    stat_params = list(
      fun = fun,
      fun.data = fun.data,
      fun.min = fun.min,
      fun.max = fun.max,
      fun.args = fun.args
    ),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_boxplot
#' @details
#' `mark_pointrange()` draws a point at `(x, y)` with a vertical line from `ymin`
#' to `ymax` (an identity mark — supply the summary values yourself, e.g. from a
#' model). `mark_crossbar()` draws a box spanning `ymin`..`ymax` with a
#' horizontal line at `y`. Both are the identity counterparts of the aggregating
#' `mark_interval()`.
#' @export
mark_pointrange <- function(
  plot,
  ...,
  position = "identity",
  orientation = NA,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "pointrange",
    rlang::enquos(...),
    stat_params = list(orientation = .check_orientation(orientation)),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_boxplot
#' @export
mark_crossbar <- function(
  plot,
  ...,
  position = "identity",
  width = 0.5,
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "crossbar",
    rlang::enquos(...),
    rlang::enquos(width = width),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' Count overlapping points
#'
#' `mark_count()` collapses coincident `(x, y)` observations to one point sized
#' by how many fall there (ggplot2's `geom_count()` / `stat_sum()`) — the honest
#' way to show overplotting on a discrete or rounded grid. `size` defaults to
#' `after_stat(n)` (the overlap count); map it to `after_stat(prop)` for the
#' proportion instead.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y` (+ `color`/`fill` to count within
#'   groups, and `size` to override the default `after_stat(n)`).
#' @return The modified [PlotSpec].
#' @examples
#' vplot(mpg <- data.frame(cyl = mtcars$cyl, gear = mtcars$gear)) |>
#'   mark_count(x = cyl, y = gear)
#' @export
mark_count <- function(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  dots <- rlang::enquos(...)
  dots <- .default_channel(dots, "size", rlang::quo(after_stat(n)))
  .add_layer(
    plot,
    "point",
    dots,
    stat = "sum",
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' Segment, curve and spoke marks
#'
#' `mark_segment()` draws a straight line from `(x, y)` to `(xend, yend)` per row.
#' `mark_curve()` bends that line into an arc (ggplot2's `geom_curve()`).
#' `mark_spoke()` draws a segment from `(x, y)` in the direction `angle`
#' (radians) with length `radius`, both in data units (ggplot2's
#' `geom_spoke()`): a vector field.
#'
#' A curve bends in the panel's own frame (its x and y spans scaled to the
#' same length), so on a panel that is not square the arc stretches with the
#' panel; ggplot2 bends in device space instead.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y`, `xend`, `yend` (+ `color`,
#'   `linewidth`, `linetype`, `alpha`); `mark_spoke()` takes `angle` and
#'   `radius` in place of `xend`, `yend`. A mapped `linewidth` gives each segment
#'   its own width -- see [scale_linewidth()].
#' @return The modified [PlotSpec].
#' @seealso [scale_linewidth()]
#' @examples
#' d <- data.frame(x = 1, y = 1, xend = 5, yend = 4)
#' vplot(d) |> mark_segment(x = x, y = y, xend = xend, yend = yend)
#' vplot(d) |> mark_curve(x = x, y = y, xend = xend, yend = yend)
#'
#' # a vector field
#' g <- expand.grid(x = 1:8, y = 1:8)
#' g$angle <- atan2(g$y - 4.5, g$x - 4.5)
#' vplot(g) |> mark_spoke(x = x, y = y, angle = angle, radius = 0.4)
#' @export
mark_segment <- function(
  plot,
  ...,
  position = "identity",
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
    data = data,
    position = position
  )
}

#' @rdname mark_segment
#' @param curvature For `mark_curve()`, the amount of bend: `0` is straight,
#'   `1` a half circle; negative values bend the other way (ggplot2's
#'   convention: positive bends to the right, travelling from start to end).
#' @param angle For `mark_curve()`, where the apex of the bend sits, in degrees
#'   between 0 and 180: `90` (default) is symmetric, smaller values move it
#'   towards the start and larger towards the end.
#' @param ncp For `mark_curve()`, the number of control points (more give a
#'   smoother curve; default `5`).
#' @export
mark_curve <- function(
  plot,
  ...,
  curvature = 0.5,
  angle = 90,
  ncp = 5,
  position = "identity",
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  if (
    !is.numeric(curvature) || length(curvature) != 1L || !is.finite(curvature)
  ) {
    cli::cli_abort("{.arg curvature} must be a single finite number.")
  }
  if (
    !is.numeric(angle) ||
      length(angle) != 1L ||
      !is.finite(angle) ||
      angle <= 0 ||
      angle >= 180
  ) {
    cli::cli_abort("{.arg angle} must be a single number between 0 and 180.")
  }
  .check_count(ncp)
  .add_layer(
    plot,
    "segment",
    rlang::enquos(...),
    stat_params = list(curvature = curvature, angle = angle, ncp = ncp),
    blend = blend,
    effects = effects,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_segment
#' @export
mark_spoke <- function(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  dots <- rlang::enquos(...)
  names(dots) <- sub("colour", "color", names(dots), fixed = TRUE)
  need <- c("x", "y", "angle", "radius")
  miss <- setdiff(need, names(dots))
  if (length(miss)) {
    cli::cli_abort("{.fn mark_spoke} needs {.arg {miss}}.")
  }
  # The end point is a per-row expression of the start, the angle and the
  # radius (ggplot2's `stat_identity` + `xend = x + cos(angle) * radius`), so
  # every row stays its own element (keys and provenance follow the data).
  ex <- lapply(dots[need], rlang::quo_get_expr)
  env <- rlang::quo_get_env(dots$x)
  dots$xend <- rlang::new_quosure(
    rlang::expr(!!ex$x + cos(!!ex$angle) * !!ex$radius),
    env
  )
  dots$yend <- rlang::new_quosure(
    rlang::expr(!!ex$y + sin(!!ex$angle) * !!ex$radius),
    env
  )
  dots <- dots[setdiff(names(dots), c("angle", "radius"))]
  .add_layer(
    plot,
    "segment",
    dots,
    blend = blend,
    effects = effects,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' Polygon and path marks
#'
#' `mark_polygon()` draws one filled polygon per `group`, its vertices in data
#' order (ggplot2's `geom_polygon()`). A `subgroup` cuts holes: within a group,
#' each subgroup is a ring, filled with the even-odd rule so an inner ring is a
#' hole. `mark_path()` connects the observations in the order they appear in the
#' data, unlike [mark_line()], which sorts them by `x` (ggplot2's
#' `geom_path()`).
#'
#' As in ggplot2, `fill` fills a polygon (default `"grey20"`) and `color`
#' outlines it (no outline by default); a `color` given without any `fill` fills
#' the polygon instead, as it does for the other filled marks.
#'
#' @inheritParams mark_point
#' @param ... Encodings (tidy-eval): `x`, `y`, `group` (one polygon / path per
#'   group), `subgroup` (`mark_polygon()` holes), `fill`, `color`, `alpha`,
#'   `linewidth`, `linetype`.
#' @return The modified [PlotSpec].
#' @examples
#' tri <- data.frame(
#'   id = rep(c("a", "b"), each = 3),
#'   x = c(0, 1, 0.5, 2, 3, 2.5),
#'   y = c(0, 0, 1, 0, 0, 1)
#' )
#' vplot(tri) |> mark_polygon(x = x, y = y, group = id, fill = id)
#'
#' # a square with a square hole
#' sq <- data.frame(
#'   x = c(0, 4, 4, 0, 1, 3, 3, 1),
#'   y = c(0, 0, 4, 4, 1, 1, 3, 3),
#'   ring = rep(1:2, each = 4)
#' )
#' vplot(sq) |> mark_polygon(x = x, y = y, subgroup = ring)
#'
#' # a path follows the data order
#' t <- seq(0, 4 * pi, length.out = 200)
#' vplot(data.frame(x = t * cos(t), y = t * sin(t))) |> mark_path(x = x, y = y)
#' @export
mark_polygon <- function(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "polygon",
    rlang::enquos(...),
    blend = blend,
    sketch = sketch,
    data = data,
    position = position
  )
}

#' @rdname mark_polygon
#' @export
mark_path <- function(
  plot,
  ...,
  position = "identity",
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "line",
    rlang::enquos(...),
    stat_params = list(sort = FALSE),
    blend = blend,
    effects = effects,
    sketch = sketch,
    data = data,
    position = position
  )
}

# Fill in default encoding channels the user did not supply (used by the graph
# marks, whose x/y/xend/yend/label columns are produced by vgraph()).
.with_default_aes <- function(dots, defaults) {
  c(dots, defaults[setdiff(names(defaults), names(dots))])
}

# Move a graph edge's colour / alpha / line-type encodings onto dedicated edge
# channels (`edge_color`/`edge_alpha`/`edge_linetype`) so they train and legend
# independently of the node scales. Position
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
#' and `effects = list(shadow())` drops a shadow behind each label so it stays
#' readable over edges. `mark_edge_text()` takes the same `effects`. (Only
#' [shadow()] applies to text; a glyph outline / glow is not available yet.)
#'
#' Two more decorations: `mark_node_hull()` shades communities with a convex hull
#' behind the graph (grouped by a mapped `fill`, drawn under the edges), and
#' `mark_node_pie()` replaces node markers with pie / donut glyphs whose wedges
#' come from a set of compositional columns.
#'
#' `mark_edge_bundle()` is a drop-in alternative to `mark_edges()` that routes the
#' edges as **bundled** curves instead of straight lines, so a hairball collapses
#' into a few legible trunks. It delegates the geometry to the \pkg{edgebundle}
#' package (install it) and draws the returned paths with the edge aesthetics --
#' `type` picks the algorithm (`"force"`, `"divided"`, `"stub"`, `"path"`,
#' `"hammer"`, `"mingle"`) and `params` passes tuning through. Bundled edges are
#' faint by default (`alpha = 0.3`) so overlapping trunks read as density.
#'
#' `mark_flow_map()` draws a **flow map**: a single `root` fans out to many
#' destinations along smooth, merging branches whose width tracks the flow volume
#' (the Minard / migration-map idiom). It expects a one-to-many (star) graph
#' rooted at `root`, laid out at fixed coordinates; edge `weight` drives the flow.
#' `type = "spiral"` (default, an angle-restricted spiral tree) needs only
#' \pkg{edgebundle}; `type = "steiner"` (an approximate Steiner tree) additionally
#' needs \pkg{interp}. Branch width is mapped from the computed flow into
#' `width_range`, and each branch is drawn as **one tapering ribbon**: where
#' tributaries merge and the flow changes along a branch, the width varies
#' continuously rather than stepping. (That only arises for `type = "steiner"`,
#' whose flow is per-vertex; a spiral tree's flow is constant along each branch.)
#' A `sketch =` flow map falls back to one uniform stroke per branch, since a
#' hand-drawn ribbon would jitter its outline rather than the pen.
#'
#' These are thin over the point / segment / text marks; `igraph` need not be
#' installed to use them (only [vgraph()] needs it). `mark_edge_bundle()` and
#' `mark_flow_map()` also need the \pkg{edgebundle} package.
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
#'   [scale_edge_width()]) a mapped expression such as `linewidth = weight`. For
#'   `mark_node_pie()`, the wedge border width.
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
#'   layouts (still straight segments, no curvature). Elbows keep node-boundary
#'   caps and arrowheads.
#' @param elbow_at,elbow_axis For `mark_edges(routing = "elbow")`, the corner
#'   placement and axis. `elbow_at` is `"mid"` (default, an S-bend at the
#'   midpoint), or `"start"` / `"end"` for the corner at the source / target --
#'   `"start"` gives the dendrogram *bracket* (siblings share a bar at the
#'   parent's level). `elbow_axis` is `"auto"` (default, the longer side),
#'   `"v"`, or `"h"` to force which axis the corner steps along.
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
#'   to each vertex. `box_padding`, `point_padding`, `min_segment_length`, and
#'   `seed` tune it exactly as in [mark_text()].
#' @param box_padding,point_padding,min_segment_length,seed Repel
#'   tuning for `mark_node_text(repel = TRUE)`; see [mark_text()].
#' @param cols For `mark_node_pie()`, the compositional columns (a character
#'   vector of node-table column names, at least two) whose values size each
#'   node's wedges.
#' @param inner For `mark_node_pie()`, the inner-radius fraction: `0` (default)
#'   draws a pie, `> 0` a donut (e.g. `0.5`).
#' @param expand For `mark_node_hull()`, the margin to grow each hull outward by
#'   so it encloses the node markers, as a fraction of the layout's range on each
#'   axis (default `0.04`). Every hull gets the same rounded margin.
#' @param type For `mark_edge_bundle()`, the bundling algorithm:
#'   `"force"` (default, force-directed / FDEB), `"divided"` (force-directed with
#'   direction-split bundles), `"stub"` (short stubs at each endpoint), `"path"`
#'   (shortest-path bundling), `"hammer"` (hammer bundling), or `"mingle"`
#'   (multilevel agglomerative edge bundling). Delegated to
#'   [edgebundle::edge_bundle()]. For `mark_flow_map()`, the flow-tree layout:
#'   `"spiral"` (default, angle-restricted spiral tree; \pkg{edgebundle} only) or
#'   `"steiner"` (approximate Steiner tree; also needs \pkg{interp}).
#' @param params For `mark_edge_bundle()`, a named list of extra arguments passed
#'   to [edgebundle::edge_bundle()] for the chosen `type` (e.g.
#'   `params = list(compatibility_threshold = 0.8)`). For `mark_flow_map()`,
#'   extra arguments for the flow-tree builder -- [edgebundle::flow_tree()] when
#'   `type = "spiral"` (e.g. `list(alpha = 30)`), [edgebundle::tnss_tree()] when
#'   `type = "steiner"` (e.g. `list(gamma = 0.9)`). Empty by default.
#' @param root For `mark_flow_map()`, the source vertex the flow fans out from:
#'   a vertex name (matched against the node table) or a vertex index.
#' @param weight For `mark_flow_map()`, the per-edge flow volume (a mapped edge
#'   column, e.g. `weight = migrants`). Defaults to the graph's `weight` edge
#'   attribute, or `1` if there is none.
#' @param width_range For `mark_flow_map()`, the drawn branch width range
#'   `c(min, max)` (in `linewidth` units) that the computed flow is mapped onto.
#'   Default `c(0.3, 3)`. The width is resolved at render inside the panel, so it
#'   is a physical width that holds at any figure size.
#' @param blend Optional blend mode (see [mark_point()]).
#' @param data Optional layer data; overrides the default table.
#' @param effects A list of layer render effects ([glow()], [outline()],
#'   [shadow()]) applied to the mark at draw time. On the text marks
#'   (`mark_node_text()` / `mark_edge_text()`) only [shadow()] applies.
#' @param sketch A [sketch()] spec giving the layer a hand-drawn look,
#'   `NA`/`FALSE` to force it crisp, or `NULL` (default) to inherit.
#' @param position Position adjustment; see [mark_point()]. A mark errors
#'   on a position it does not support, naming the ones it does.
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
  position = "identity",
  color = NULL,
  linewidth = NULL,
  alpha = NULL,
  linetype = NULL,
  arrow = FALSE,
  routing = c("straight", "elbow"),
  elbow_at = c("mid", "start", "end"),
  elbow_axis = c("auto", "v", "h"),
  gradient = FALSE,
  auto = FALSE,
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  routing <- match.arg(routing)
  elbow_at <- match.arg(elbow_at)
  elbow_axis <- match.arg(elbow_axis)
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
      elbow_at = elbow_at,
      elbow_axis = elbow_axis,
      gradient = gradient,
      auto = isTRUE(auto)
    ),
    blend = blend,
    effects = effects,
    sketch = sketch,
    data = data %||% plot@edge_data,
    z = 1L,
    position = position
  )
}

#' @rdname mark_graph
#' @export
mark_edge_bundle <- function(
  plot,
  ...,
  position = "identity",
  type = c("force", "divided", "stub", "path", "hammer", "mingle"),
  color = NULL,
  linewidth = NULL,
  alpha = NULL,
  linetype = NULL,
  params = list(),
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  type <- match.arg(type)
  if (!is.list(params)) {
    cli::cli_abort(
      "{.arg params} must be a list of arguments for {.fn edgebundle::edge_bundle}."
    )
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
    "edge_bundle",
    .rename_edge_aes(dots),
    .rename_edge_aes(extra),
    stat_params = list(type = type, params = params),
    blend = blend,
    effects = effects,
    sketch = sketch,
    data = data %||% plot@edge_data,
    z = 1L,
    position = position
  )
}

#' @rdname mark_graph
#' @export
mark_flow_map <- function(
  plot,
  ...,
  position = "identity",
  root,
  type = c("spiral", "steiner"),
  weight = NULL,
  width_range = c(0.3, 3),
  color = NULL,
  alpha = NULL,
  params = list(),
  blend = NULL,
  effects = list(),
  sketch = NULL,
  data = NULL
) {
  .check_plot(plot)
  type <- match.arg(type)
  if (missing(root) || is.null(root)) {
    cli::cli_abort(
      "{.arg root} is required: the source vertex the flow fans out from."
    )
  }
  if (!is.list(params)) {
    cli::cli_abort(
      "{.arg params} must be a list of arguments for the flow-tree builder."
    )
  }
  if (!is.numeric(width_range) || length(width_range) != 2L) {
    cli::cli_abort("{.arg width_range} must be a numeric {.code c(min, max)}.")
  }
  # Resolve the root vertex to a coordinate now, from the node table -- so the
  # emitter (which reconstructs the graph from edge endpoints, with no vertex
  # identity) can find it as the reconstructed point nearest that coordinate.
  nodes <- plot@data
  if (is.null(nodes) || !all(c("x", "y") %in% names(nodes))) {
    cli::cli_abort(c(
      "{.fn mark_flow_map} needs a {.fn vgraph} plot with node coordinates.",
      i = "Build the plot with {.fn vgraph} (its node table carries {.field x}/{.field y})."
    ))
  }
  ri <- if (is.character(root)) match(root, nodes$name) else as.integer(root)[1]
  if (is.na(ri) || ri < 1L || ri > nrow(nodes)) {
    cli::cli_abort("{.arg root} does not match a vertex in the graph.")
  }
  root_xy <- c(nodes$x[ri], nodes$y[ri])
  # Per-edge flow volume, resolved against the edge table (mapped column, the
  # `weight` attribute, or 1).
  edges <- data %||% plot@edge_data
  wv <- rlang::eval_tidy(rlang::enquo(weight), edges)
  if (is.null(wv)) {
    wv <- edges$weight %||% 1
  }
  dots <- .with_default_aes(
    rlang::enquos(...),
    rlang::quos(x = x, y = y, xend = xend, yend = yend)
  )
  .add_layer(
    plot,
    "flow_map",
    .rename_edge_aes(dots),
    stat_params = list(
      type = type,
      root_xy = root_xy,
      weight = wv,
      width_range = width_range,
      color = color,
      alpha = alpha,
      params = params
    ),
    blend = blend,
    effects = effects,
    sketch = sketch,
    data = edges,
    z = 1L,
    position = position
  )
}

#' @rdname mark_graph
#' @export
mark_nodes <- function(
  plot,
  ...,
  position = "identity",
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
    z = 3L,
    position = position
  )
}

#' @rdname mark_graph
#' @export
mark_node_text <- function(
  plot,
  ...,
  position = "identity",
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
  # `label` defaults to the vertex `name`, but an explicit `label =` must win.
  # Resolve it here (rather than as a `.with_default_aes` default) so it doesn't
  # collide with the explicit param under the same key.
  label_q <- rlang::enquo(label)
  if (rlang::quo_is_null(label_q)) {
    label_q <- rlang::quo(name)
  }
  dots <- .with_default_aes(
    rlang::enquos(...),
    rlang::quos(x = x, y = y)
  )
  extra <- rlang::enquos(
    color = color,
    size = size,
    alpha = alpha
  )
  extra$label <- label_q
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
        seed
      )
    ),
    effects = effects,
    blend = blend,
    data = lab,
    z = 4L,
    position = position
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
  position = "identity",
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
    z = 2L,
    position = position
  )
}

#' @rdname mark_graph
#' @export
mark_node_hull <- function(
  plot,
  ...,
  position = "identity",
  fill = NULL,
  color = NULL,
  alpha = 0.25,
  expand = 0.04,
  data = NULL
) {
  .check_plot(plot)
  if (!isTRUE(all.equal(expand, 0))) {
    .check_positive(expand)
  }
  # A convex hull per group (the mapped `fill`) drawn *behind* the graph (z = 0),
  # to shade communities. The outline defaults to the fill's group colour so it
  # matches the shading. Reuses the general hull mark/stat (with `expand`).
  fill_q <- rlang::enquo(fill)
  color_q <- rlang::enquo(color)
  if (rlang::quo_is_null(color_q)) {
    color_q <- fill_q
  }
  dots <- .with_default_aes(rlang::enquos(...), rlang::quos(x = x, y = y))
  extra <- rlang::quos(fill = !!fill_q, color = !!color_q, alpha = !!alpha)
  .add_layer(
    plot,
    "hull",
    dots,
    extra,
    stat = "hull",
    stat_params = list(expand = as.numeric(expand)),
    data = data %||% plot@data,
    z = 0L,
    position = position
  )
}

#' @rdname mark_graph
#' @export
mark_node_pie <- function(
  plot,
  cols,
  ...,
  position = "identity",
  size = 4,
  inner = 0,
  fill = NULL,
  color = "white",
  linewidth = 0.5,
  alpha = NA,
  data = NULL
) {
  .check_plot(plot)
  d <- data %||% plot@data
  cols <- as.character(cols)
  if (length(cols) < 2L) {
    cli::cli_abort(c(
      "{.fn mark_node_pie} needs at least two {.arg cols} to divide each node.",
      i = "Pass the compositional columns, e.g. {.code cols = c(\"a\", \"b\", \"c\")}."
    ))
  }
  miss <- setdiff(cols, names(d))
  if (length(miss)) {
    cli::cli_abort("Unknown pie {.arg cols}: {.val {miss}}.")
  }
  weights <- as.matrix(d[, cols, drop = FALSE])
  storage.mode(weights) <- "double"
  if (!is.null(fill) && length(fill) != length(cols)) {
    cli::cli_abort(
      "{.arg fill} must give one colour per column ({length(cols)})."
    )
  }
  dots <- .with_default_aes(rlang::enquos(...), rlang::quos(x = x, y = y))
  .add_layer(
    plot,
    "node_pie",
    dots,
    stat_params = list(
      weights = weights,
      categories = cols,
      fill = fill,
      size = as.numeric(size),
      inner = as.numeric(inner),
      linewidth = as.numeric(linewidth),
      color = color,
      alpha = as.numeric(alpha)
    ),
    data = d,
    z = 3L,
    position = position
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
