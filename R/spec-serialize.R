#' @include classes.R
NULL

# ---------------------------------------------------------------------------
# The spec serializer (GAPS-HORIZON Phase A).
#
# `as_spec()` walks a PlotSpec into a plain, nested, serializable R list (the
# intermediate representation, "IR"); `from_spec()` rebuilds a PlotSpec from it.
# The IR is what `spec_to_json()` / `spec_from_json()` (spec-json.R) marshal, and
# what the Vega-Lite bridge (spec-vegalite.R) and the MCP tools consume.
#
# DESIGN
#   * The IR is a *serializable subset* of the full spec. Encoding-level state
#     (layers, encodings, params, stats, scales, coord, facet, labels, page size)
#     round-trips exactly. Styling and opaque state that a JSON document cannot
#     faithfully carry (custom transform functions, paint/pattern objects, sketch
#     geometry, secondary-axis closures, sf CRS objects) is REFUSED with a classed
#     `vellumplot_unserializable` error naming the exact slot -- never silently
#     dropped. The one documented exception is theme element customisation, which
#     is styling orthogonal to the data spec: it is dropped with a warning and the
#     preset name is kept (see `.theme_to_ir`).
#   * Channels hold rlang quosures. The IR stores the *expression text* (via
#     `as_label()`), not the quosure -- the captured environment is dropped. This
#     is faithful for the common cases (a bare column symbol, or a simple
#     transform like `log(x)` / `factor(cyl)`); it is a documented limitation for
#     expressions that close over local variables.
#   * `from_spec()` rebuilds S7 objects *directly* (not via the enquos-based user
#     constructors), so it does not depend on those constructors' internals.
# ---------------------------------------------------------------------------

# The IR schema version. Bump on any breaking change to the IR shape; the JSON
# bridge writes it as the top-level `version`, and `from_spec()` checks it.
.SPEC_VERSION <- "1"

# Raise the classed refusal used throughout the serializer. `slot` names the
# offending piece so an agent / caller can act on it.
.unserializable <- function(slot, detail, call = rlang::caller_env()) {
  cli::cli_abort(
    c(
      "Cannot serialize {.field {slot}}.",
      "x" = detail,
      "i" = "The spec serializer supports a documented subset; see {.help vellumplot::as_spec}."
    ),
    class = "vellumplot_unserializable",
    call = call
  )
}

# TRUE for a plain, JSON-expressible atomic value (or NULL / a plain list of
# such). Functions, closures, environments, and tagged S3/S4 objects are not.
.is_plain <- function(x) {
  if (is.null(x)) {
    return(TRUE)
  }
  if (is.function(x) || is.environment(x)) {
    return(FALSE)
  }
  if (is.list(x)) {
    # a bare list is fine; a data.frame / transform object / S3 record is not.
    if (!is.null(attr(x, "class")) && !identical(class(x), "list")) {
      return(FALSE)
    }
    return(all(vapply(x, .is_plain, logical(1))))
  }
  # a plain atomic vector carries no S3 class (a factor / Date / units does).
  is.atomic(x) && is.null(attr(x, "class"))
}

# --- channels ---------------------------------------------------------------

# One `channel` -> a plain IR record. A bare symbol becomes `field`; any other
# expression becomes `expr` (its deparsed text). `after_stat` and a conditional
# encoding are captured structurally.
.channel_to_ir <- function(ch, aes) {
  expr <- rlang::quo_get_expr(ch@expr)
  label <- rlang::as_label(expr)
  out <- list()
  if (rlang::is_symbol(expr)) {
    out$field <- rlang::as_string(expr)
  } else {
    out$expr <- label
  }
  if (nzchar(ch@type)) {
    out$type <- ch@type
  }
  if (isTRUE(ch@after)) {
    out$after_stat <- TRUE
  }
  if (!is.null(ch@condition)) {
    cond <- ch@condition
    ir <- list(selection = cond$selection, empty = cond$empty %||% TRUE)
    if (!is.null(cond$if_false)) {
      # if_false is a quosure or a constant; capture its text/value.
      iff <- cond$if_false
      ir$if_false <- if (rlang::is_quosure(iff)) {
        rlang::as_label(rlang::quo_get_expr(iff))
      } else {
        iff
      }
    }
    out$condition <- ir
  }
  out
}

# The inverse: an IR channel record -> a `channel` S7 object. The expression is
# re-parsed into a quosure evaluated in `env` (the global env by default), so a
# bare field resolves against the data at compile time.
.channel_from_ir <- function(rec, env) {
  # A `field` is a plain column reference: use it as a symbol so a non-syntactic
  # name ("Sepal Width", routine from Vega-Lite import) resolves as a column
  # rather than failing to `parse_expr()` as code. Only `expr` is parsed.
  if (!is.null(rec$field)) {
    expr <- rlang::sym(rec$field)
  } else if (!is.null(rec$expr)) {
    expr <- rlang::parse_expr(rec$expr)
  } else {
    cli::cli_abort("A channel needs a {.field field} or {.field expr}.")
  }
  q <- rlang::new_quosure(expr, env = env)
  condition <- NULL
  if (!is.null(rec$condition)) {
    c_ir <- rec$condition
    if_false <- c_ir$if_false
    if (is.character(if_false) && length(if_false) == 1) {
      # A bare colour/number string stays a constant; only a genuine *call*
      # (e.g. `rgb(...)`, `x > 3`) is re-quoted. A bare symbol like "red" is a
      # colour name, not a variable reference -- `is_syntactic_literal()` would
      # (wrongly) treat it as an expression and re-quote it into a quosure that
      # evaluates the missing variable `red`.
      parsed <- tryCatch(rlang::parse_expr(if_false), error = function(e) NULL)
      if (!is.null(parsed) && rlang::is_call(parsed)) {
        if_false <- rlang::new_quosure(parsed, env = env)
      }
    }
    condition <- list(
      selection = c_ir$selection,
      if_false = if_false,
      empty = c_ir$empty %||% TRUE
    )
  }
  channel(
    expr = q,
    type = rec$type %||% "",
    after = isTRUE(rec$after_stat),
    condition = condition
  )
}

# --- effects ----------------------------------------------------------------

.EFFECT_IR <- list(
  GlowSpec = list(
    name = "glow",
    slots = c("size", "layers", "alpha", "blend", "color")
  ),
  OutlineSpec = list(name = "outline", slots = c("size", "color", "alpha")),
  ShadowSpec = list(
    name = "shadow",
    slots = c("x", "y", "color", "alpha", "spread", "layers")
  ),
  MotionSpec = list(
    name = "motion",
    slots = c("x", "y", "n", "alpha", "decay", "spread", "blend", "color")
  )
)

.effect_to_ir <- function(eff) {
  cls <- class(eff)[1]
  # S7 class names are namespaced ("vellumplot::GlowSpec"); take the leaf.
  leaf <- sub("^.*::", "", cls)
  spec <- .EFFECT_IR[[leaf]]
  if (is.null(spec)) {
    .unserializable("effect", paste0("unknown effect class ", cls, "."))
  }
  out <- list(effect = spec$name)
  for (s in spec$slots) {
    out[[s]] <- S7::prop(eff, s)
  }
  out
}

.effect_from_ir <- function(rec) {
  ctor <- switch(
    rec$effect,
    glow = GlowSpec,
    outline = OutlineSpec,
    shadow = ShadowSpec,
    motion = ,
    echo = MotionSpec,
    .unserializable("effect", paste0("unknown effect ", rec$effect, "."))
  )
  args <- rec[setdiff(names(rec), "effect")]
  # integer slots must survive JSON's number->double coercion.
  for (int_slot in c("layers", "n")) {
    if (!is.null(args[[int_slot]])) {
      args[[int_slot]] <- as.integer(args[[int_slot]])
    }
  }
  do.call(ctor, args)
}

# --- params / stat_params ---------------------------------------------------

# Constant aesthetics and stat params are eagerly-evaluated values. Plain values
# pass through; a formula is captured as text (round-trips via as.formula); an
# opaque object (paint/pattern/function) is refused.
.values_to_ir <- function(vals, slot) {
  out <- list()
  for (nm in names(vals)) {
    v <- vals[[nm]]
    if (inherits(v, "formula")) {
      out[[nm]] <- list(`.formula` = paste(deparse(v), collapse = " "))
    } else if (.is_plain(v)) {
      out[[nm]] <- v
    } else {
      .unserializable(
        paste0(slot, "$", nm),
        paste0(
          "a ",
          class(v)[1],
          " value is not serializable (paint/pattern/function/opaque object)."
        )
      )
    }
  }
  out
}

.values_from_ir <- function(ir) {
  out <- list()
  for (nm in names(ir)) {
    v <- ir[[nm]]
    if (is.list(v) && !is.null(v[[".formula"]])) {
      out[[nm]] <- stats::as.formula(v[[".formula"]], env = globalenv())
    } else {
      out[[nm]] <- v
    }
  }
  out
}

# --- layers -----------------------------------------------------------------

.layer_to_ir <- function(L) {
  if (!is.null(L@sketch)) {
    .unserializable(
      "layer$sketch",
      "a hand-drawn sketch is styling and cannot be serialized."
    )
  }
  if (length(L@interactivity)) {
    .unserializable(
      "layer$interactivity",
      "per-row interactivity expressions are not serializable yet."
    )
  }
  if (!is.null(L@clip)) {
    .unserializable(
      "layer$clip",
      "a per-layer clip geometry is not serializable."
    )
  }
  if (!is.null(L@data)) {
    .unserializable(
      "layer$data",
      "per-layer data is not serializable; put shared data on the plot."
    )
  }
  enc <- list()
  for (nm in names(L@encoding)) {
    enc[[nm]] <- .channel_to_ir(L@encoding[[nm]], nm)
  }
  out <- list(mark = L@mark)
  if (length(enc)) {
    out$encoding <- enc
  }
  params <- .values_to_ir(L@params, "params")
  if (length(params)) {
    out$params <- params
  }
  if (!identical(L@stat, "identity")) {
    out$stat <- L@stat
  }
  sp <- .values_to_ir(L@stat_params, "stat_params")
  if (length(sp)) {
    out$stat_params <- sp
  }
  if (!identical(L@position, "identity")) {
    out$position <- L@position
  }
  if (!identical(L@blend, "normal")) {
    out$blend <- L@blend
  }
  if (!identical(L@z, 0L)) {
    out$z <- L@z
  }
  if (length(L@effects)) {
    out$effects <- lapply(L@effects, .effect_to_ir)
  }
  out
}

.layer_from_ir <- function(rec, env) {
  enc <- list()
  for (nm in names(rec$encoding)) {
    enc[[nm]] <- .channel_from_ir(rec$encoding[[nm]], env)
  }
  LayerSpec(
    mark = rec[["mark"]],
    encoding = enc,
    params = .values_from_ir(rec[["params"]] %||% list()),
    stat = rec[["stat"]] %||% "identity",
    stat_params = .values_from_ir(rec[["stat_params"]] %||% list()),
    position = rec[["position"]] %||% "identity",
    blend = rec[["blend"]] %||% "normal",
    z = as.integer(rec[["z"]] %||% 0L),
    effects = lapply(rec[["effects"]] %||% list(), .effect_from_ir)
  )
}

# --- scales -----------------------------------------------------------------

# Slots whose values must be plain (a colour/shape vector or a palette-name
# string, numeric breaks, etc.). A function (labeller, custom transform) or an
# opaque object (pattern list, transform object) is refused per slot.
.SCALE_SLOTS <- c(
  "domain",
  "palette",
  "name",
  "trans",
  "range",
  "breaks",
  "labels",
  "style",
  "n",
  "na_value",
  "midpoint",
  "date_breaks",
  "date_labels",
  "guide"
)

.scale_to_ir <- function(s) {
  if (!is.null(s@sec_axis)) {
    .unserializable(
      "scale$sec_axis",
      "a secondary axis carries a transform closure and is not serializable."
    )
  }
  out <- list(aesthetic = s@aesthetic, type = s@type)
  for (slot in .SCALE_SLOTS) {
    v <- S7::prop(s, slot)
    if (is.null(v)) {
      next
    }
    if (!.is_plain(v)) {
      hint <- if (slot == "trans") {
        "use a transform *name* string like \"log10\", not a transform object."
      } else if (slot == "labels" || slot == "breaks") {
        "a labelling/break *function* is not serializable; pass explicit values."
      } else {
        "value is a function or opaque object."
      }
      .unserializable(paste0("scale(", s@aesthetic, ")$", slot), hint)
    }
    out[[slot]] <- v
  }
  out
}

.scale_from_ir <- function(rec) {
  args <- rec[intersect(names(rec), c("aesthetic", "type", .SCALE_SLOTS))]
  if (!is.null(args[["n"]])) {
    args[["n"]] <- as.integer(args[["n"]])
  }
  do.call(ScaleSpec, args)
}

# --- coord ------------------------------------------------------------------

# Every `CoordSpec` slot except `kind` (which is handled explicitly). Single
# source for both directions so a new coord slot round-trips without editing two
# hand-written lists.
.COORD_SLOTS <- c(
  "xlim",
  "ylim",
  "ratio",
  "theta",
  "start",
  "end",
  "direction",
  "rmin",
  "crs",
  "graticule",
  "xtrans",
  "ytrans"
)

# Every `MarginalSpec` slot. Single source for both serialize directions; the
# reader rebuilds via the constructor, so its defaults (not a second hand-written
# list) fill any absent field.
.MARGINAL_FIELDS <- c("type", "sides", "size", "adjust", "bins", "group")

.coord_to_ir <- function(co) {
  for (slot in c("xtrans", "ytrans")) {
    v <- S7::prop(co, slot)
    if (!is.null(v) && !is.character(v)) {
      .unserializable(
        paste0("coord$", slot),
        "a custom transform object is not serializable; use a transform name."
      )
    }
  }
  if (!is.null(co@crs) && !(is.numeric(co@crs) || is.character(co@crs))) {
    .unserializable(
      "coord$crs",
      "an sf CRS object is not serializable; pass an EPSG code or string."
    )
  }
  out <- list(kind = co@kind)
  for (slot in .COORD_SLOTS) {
    v <- S7::prop(co, slot)
    if (!is.null(v)) {
      out[[slot]] <- v
    }
  }
  out
}

.coord_from_ir <- function(rec) {
  args <- rec[intersect(names(rec), c("kind", .COORD_SLOTS))]
  do.call(CoordSpec, args)
}

# --- facet ------------------------------------------------------------------

.facet_side_to_ir <- function(qs) {
  vapply(qs, function(q) rlang::as_label(rlang::quo_get_expr(q)), character(1))
}

.facet_to_ir <- function(f) {
  out <- list(type = f@type)
  rows <- .facet_side_to_ir(f@rows)
  cols <- .facet_side_to_ir(f@cols)
  if (length(rows)) {
    out$rows <- as.list(rows)
  }
  if (length(cols)) {
    out$cols <- as.list(cols)
  }
  if (!is.null(f@ncol)) {
    out$ncol <- f@ncol
  }
  if (!is.null(f@nrow)) {
    out$nrow <- f@nrow
  }
  out
}

.facet_side_from_ir <- function(x, env) {
  if (is.null(x)) {
    return(list())
  }
  lapply(unlist(x, use.names = FALSE), function(t) {
    # A non-syntactic facet field (e.g. "Miles per Gallon", routine from a
    # Vega-Lite import) does not parse as an expression; fall back to treating it
    # as a bare column name, mirroring the channel field/expr split.
    expr <- tryCatch(rlang::parse_expr(t), error = function(e) rlang::sym(t))
    rlang::new_quosure(expr, env = env)
  })
}

.facet_from_ir <- function(rec, env) {
  FacetSpec(
    type = rec$type,
    rows = .facet_side_from_ir(rec$rows, env),
    cols = .facet_side_from_ir(rec$cols, env),
    ncol = if (!is.null(rec$ncol)) as.integer(rec$ncol) else NULL,
    nrow = if (!is.null(rec$nrow)) as.integer(rec$nrow) else NULL
  )
}

# --- theme ------------------------------------------------------------------

# Theme is styling, orthogonal to the data spec. It round-trips by *preset name*
# (tagged on each theme_*() output) plus the plain scalar settings. Element-level
# customisation (custom `element_*()` overrides, a sketch) is dropped with a
# warning -- it cannot be expressed in a portable spec, and is not what an LLM /
# Vega-Lite consumer needs.
.theme_to_ir <- function(theme, call = rlang::caller_env()) {
  if (is.null(theme)) {
    return(NULL)
  }
  if (!is.null(theme[["sketch"]])) {
    .unserializable(
      "theme$sketch",
      "a hand-drawn theme is styling and cannot be serialized.",
      call = call
    )
  }
  preset <- attr(theme, "vp_preset") %||% "gray"
  settings <- theme[intersect(names(theme), .theme_setting_names)]
  # keep only plain settings (all are, defensively).
  settings <- settings[vapply(settings, .is_plain, logical(1))]
  list(preset = preset, settings = settings)
}

.theme_from_ir <- function(rec) {
  if (is.null(rec)) {
    return(NULL)
  }
  # Rebuild the complete theme by invoking the real preset builder on a stub,
  # then merge the scalar settings back in.
  builder <- switch(
    rec$preset,
    gray = theme_gray,
    minimal = theme_minimal,
    bw = theme_bw,
    classic = theme_classic,
    void = theme_void,
    cyberpunk = theme_cyberpunk,
    theme_gray
  )
  stub <- builder(vplot(data.frame()))
  th <- stub@theme
  if (length(rec$settings)) {
    th <- .merge_theme(th, rec$settings)
  }
  th
}

# --- data -------------------------------------------------------------------

# Rows*cols at or below this are inlined as `values`; larger frames are stored by
# reference (name + hash + schema) and must be supplied to `from_spec(data=)`.
.INLINE_CELL_LIMIT <- 5000L

# A stable content hash of a data frame (order- and column-name-sensitive).
# `rlang::hash()` (already a dependency) is a fast xxHash of any R object; it is
# a change-detection fingerprint, not a cryptographic guarantee.
.data_hash <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return("empty")
  }
  rlang::hash(list(names(df), df))
}

.data_to_ir <- function(df, name = "data") {
  if (is.null(df)) {
    return(NULL)
  }
  ncell <- as.numeric(nrow(df)) * as.numeric(ncol(df))
  atomic_cols <- all(vapply(
    df,
    function(col) is.atomic(col) && !inherits(col, "sfc"),
    logical(1)
  ))
  out <- list(
    name = name,
    hash = .data_hash(df),
    nrow = nrow(df),
    columns = as.list(names(df))
  )
  if (atomic_cols && ncell <= .INLINE_CELL_LIMIT) {
    # inline column-wise; Date/POSIXct/factor become character with a type tag so
    # they rebuild faithfully. Factor levels ride a parallel list (not an
    # attribute, which JSON would drop).
    cols <- list()
    types <- list()
    levs <- list()
    for (nm in names(df)) {
      col <- df[[nm]]
      if (inherits(col, "Date")) {
        types[[nm]] <- "Date"
        cols[[nm]] <- as.character(col)
      } else if (inherits(col, "POSIXct")) {
        types[[nm]] <- "POSIXct"
        cols[[nm]] <- format(col, "%Y-%m-%d %H:%M:%OS6", tz = "UTC")
      } else if (is.factor(col)) {
        types[[nm]] <- "factor"
        cols[[nm]] <- as.character(col)
        levs[[nm]] <- levels(col)
      } else {
        types[[nm]] <- typeof(col)
        cols[[nm]] <- col
      }
    }
    out$values <- cols
    out$value_types <- types
    if (length(levs)) {
      out$value_levels <- levs
    }
  }
  out
}

.data_from_ir <- function(rec, supplied) {
  if (is.null(rec)) {
    return(NULL)
  }
  if (!is.null(supplied)) {
    return(supplied)
  }
  if (is.null(rec$values)) {
    cli::cli_abort(
      c(
        "This spec stores its data {.emph by reference} ({.val {rec$name}}, hash {.val {rec$hash}}).",
        "i" = "Supply the data frame: {.code from_spec(spec, data = your_df)}."
      ),
      class = "vellumplot_missing_data"
    )
  }
  types <- rec$value_types %||% list()
  levs <- rec$value_levels %||% list()
  cols <- lapply(names(rec$values), function(nm) {
    v <- unlist(rec$values[[nm]], use.names = FALSE)
    switch(
      types[[nm]] %||% "",
      Date = as.Date(v),
      POSIXct = as.POSIXct(v, tz = "UTC"),
      factor = factor(v, levels = levs[[nm]] %||% unique(v)),
      # JSON collapses integer/logical to double/character on the way out, so
      # restore the recorded `typeof()` (write path stores it) or the column
      # comes back a different type -- changing the data hash and any
      # is.integer()/is.logical()-dependent stat.
      integer = as.integer(v),
      logical = as.logical(v),
      double = as.double(v),
      character = as.character(v),
      v
    )
  })
  names(cols) <- names(rec$values)
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

# --- top-level --------------------------------------------------------------

#' Serialize a plot to a plain spec (and back)
#'
#' `as_spec()` turns a [PlotSpec] into a plain, nested, **serializable** R list
#' (data, layers, encodings, scales, coordinates, facets, labels, page size);
#' `from_spec()` rebuilds a `PlotSpec` from that list. Together with
#' [spec_to_json()] / [spec_from_json()] this makes a vellumplot plot a portable
#' *document* — the substrate for the LLM / agent tooling ([spec_fields()],
#' [vplot_from_spec()]) and the Vega-Lite bridge ([spec_to_vegalite()]).
#'
#' @details
#' The spec is a **subset** of the full grammar — the encoding-level state that a
#' portable document can carry faithfully. State that a JSON document cannot
#' represent (custom transform *functions*, paint/pattern fills, hand-drawn
#' [sketch()] geometry, secondary axes, `sf` CRS objects, per-layer data) is
#' **refused** with a classed `vellumplot_unserializable` error naming the exact
#' slot — it is never silently dropped. The single exception is theme element
#' customisation, which is styling orthogonal to the data spec: the theme
#' *preset* name and its scalar settings round-trip, but custom `element_*()`
#' overrides are dropped with a warning.
#'
#' Channel expressions are stored as text (a column name, or an expression like
#' `log(x)`); the quosure's captured environment is dropped, so an encoding that
#' closes over a local variable will not round-trip.
#'
#' Data is **inlined** when small (at most a few thousand cells and all-atomic
#' columns) and otherwise stored **by reference** (name + content hash + column
#' schema); a by-reference spec needs `from_spec(spec, data = )` to recompile.
#'
#' @param plot A [PlotSpec].
#' @param spec A spec list from `as_spec()`.
#' @param data Optional data frame to attach when `spec` stores its data by
#'   reference. Ignored when the spec inlines its data.
#' @param env The environment channel expressions are re-quoted in (default the
#'   global environment).
#' @return `as_spec()` returns a named list; `from_spec()` returns a [PlotSpec].
#' @seealso [spec_to_json()], [spec_to_vegalite()], [vplot_from_spec()]
#' @examples
#' p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = cyl)
#' spec <- as_spec(p)
#' spec$layers[[1]]$mark
#' p2 <- from_spec(spec)
#' @export
as_spec <- function(plot) {
  if (!S7::S7_inherits(plot, PlotSpec)) {
    cli::cli_abort("{.arg plot} must be a {.cls PlotSpec}.")
  }
  if (!is.null(plot@edge_data)) {
    .unserializable(
      "edge_data",
      "graph specs (vgraph) are not serializable yet."
    )
  }
  if (!is.null(plot@clip)) {
    .unserializable(
      "clip",
      "a plot-level clip/mask geometry is not serializable."
    )
  }
  out <- list(
    `$schema` = "https://r-vellum.github.io/vellumplot/spec.json",
    version = .SPEC_VERSION,
    width = plot@width,
    height = plot@height,
    dpi = plot@dpi
  )
  out$data <- .data_to_ir(plot@data)
  out$layers <- lapply(plot@layers, .layer_to_ir)
  if (length(plot@scales)) {
    out$scales <- lapply(plot@scales, .scale_to_ir)
  }
  if (!is.null(plot@coord)) {
    out$coord <- .coord_to_ir(plot@coord)
  }
  if (!is.null(plot@facet)) {
    out$facet <- .facet_to_ir(plot@facet)
  }
  if (length(plot@resolve)) {
    out$resolve <- plot@resolve
  }
  if (length(plot@labels)) {
    labs <- plot@labels
    plain <- vapply(labs, is.character, logical(1))
    if (!all(plain)) {
      .unserializable(
        "labels",
        "a rich md() label is not serializable; use a plain string."
      )
    }
    out$labels <- labs
  }
  if (!is.null(plot@marginal)) {
    m <- plot@marginal
    out$marginal <- stats::setNames(
      lapply(.MARGINAL_FIELDS, function(f) S7::prop(m, f)),
      .MARGINAL_FIELDS
    )
  }
  th <- .theme_to_ir(plot@theme)
  if (!is.null(th)) {
    out$theme <- th
    dropped <- setdiff(
      names(plot@theme %||% list()),
      c(.theme_setting_names, "palette", "palette.continuous")
    )
    if (length(dropped) && is.null(attr(plot@theme, "vp_preset"))) {
      cli::cli_warn(
        "Dropped custom theme elements ({.field {dropped}}) \u2014 theme styling does not round-trip; the preset name is kept."
      )
    }
  }
  out
}

#' @rdname as_spec
#' @export
from_spec <- function(spec, data = NULL, env = globalenv()) {
  if (!is.list(spec)) {
    cli::cli_abort("{.arg spec} must be a spec list from {.fn as_spec}.")
  }
  v <- spec$version %||% .SPEC_VERSION
  if (!identical(as.character(v), .SPEC_VERSION)) {
    cli::cli_warn(
      "Spec version {.val {v}} differs from this package's {.val {.SPEC_VERSION}}; reading best-effort."
    )
  }
  df <- .data_from_ir(spec$data, data)
  p <- PlotSpec(
    data = df,
    width = as.double(spec$width %||% 6),
    height = as.double(spec$height %||% 4),
    dpi = as.double(spec$dpi %||% 96)
  )
  p@layers <- lapply(spec$layers %||% list(), .layer_from_ir, env = env)
  p@scales <- lapply(spec$scales %||% list(), .scale_from_ir)
  if (!is.null(spec$coord)) {
    p@coord <- .coord_from_ir(spec$coord)
  }
  if (!is.null(spec$facet)) {
    p@facet <- .facet_from_ir(spec$facet, env = env)
  }
  if (length(spec$resolve)) {
    p@resolve <- spec$resolve
  }
  if (length(spec$labels)) {
    p@labels <- spec$labels
  }
  if (!is.null(spec$marginal)) {
    m <- spec$marginal
    args <- m[intersect(names(m), .MARGINAL_FIELDS)]
    # JSON widens these; restore the types MarginalSpec's props expect. Absent
    # fields are left out so the constructor supplies its own defaults.
    if (!is.null(args$size)) {
      args$size <- as.double(args$size)
    }
    if (!is.null(args$adjust)) {
      args$adjust <- as.double(args$adjust)
    }
    if (!is.null(args$bins)) {
      args$bins <- as.integer(args$bins)
    }
    if (!is.null(args$group)) {
      args$group <- isTRUE(args$group)
    }
    p@marginal <- do.call(MarginalSpec, args)
  }
  if (!is.null(spec$theme)) {
    p@theme <- .theme_from_ir(spec$theme)
  }
  p
}
