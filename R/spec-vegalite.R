#' @include spec-serialize.R
NULL

# ---------------------------------------------------------------------------
# Vega-Lite interop (GAPS-HORIZON Feature 2).
#
# `spec_to_vegalite()` / `spec_from_vegalite()` translate a vellumplot spec IR
# to and from a Vega-Lite specification. Both engines are layered grammars of
# graphics, so the encoding-level mapping is direct; the bridge covers a
# documented subset and *reports* what it cannot map (a warning listing dropped
# features) rather than silently diverging. See the "Vega-Lite interoperability"
# vignette for the coverage table.
# ---------------------------------------------------------------------------

# vellumplot mark string <-> Vega-Lite mark type (the reversible core).
.VL_MARK <- c(
  point = "point",
  line = "line",
  area = "area",
  bar = "bar",
  rule = "rule",
  segment = "rule",
  tile = "rect",
  raster = "rect",
  text = "text",
  label = "text",
  boxplot = "boxplot",
  rug = "tick",
  errorbar = "errorbar",
  pie = "arc",
  donut = "arc",
  step = "line"
)
.VL_MARK_INV <- c(
  point = "point",
  line = "line",
  area = "area",
  bar = "bar",
  rule = "rule",
  rect = "tile",
  text = "text",
  boxplot = "boxplot",
  tick = "rug",
  errorbar = "errorbar",
  arc = "pie"
)

# vellumplot aesthetic <-> Vega-Lite encoding channel.
.VL_CHANNEL <- c(
  x = "x",
  y = "y",
  color = "color",
  fill = "color",
  size = "size",
  shape = "shape",
  alpha = "opacity",
  label = "text",
  group = "detail",
  xmin = "x",
  xmax = "x2",
  ymin = "y",
  ymax = "y2"
)
.VL_CHANNEL_INV <- c(
  x = "x",
  y = "y",
  x2 = "xmax",
  y2 = "ymax",
  color = "color",
  size = "size",
  shape = "shape",
  opacity = "alpha",
  text = "label",
  detail = "group"
)

# transform name <-> Vega-Lite scale type.
.VL_SCALE_TYPE <- c(
  log10 = "log",
  log = "log",
  log2 = "log",
  sqrt = "sqrt",
  identity = "linear"
)

# vellumplot aesthetic <-> the Vega-Lite channel its scale resolves on (for the
# facet-level `resolve`). `fill` shares Vega-Lite's `color` channel, as in the
# encoding map.
.VL_RESOLVE <- c(
  x = "x",
  y = "y",
  color = "color",
  fill = "color",
  size = "size",
  shape = "shape",
  alpha = "opacity",
  linetype = "strokeDash",
  linewidth = "strokeWidth"
)
.VL_RESOLVE_INV <- c(
  x = "x",
  y = "y",
  color = "color",
  fill = "color",
  stroke = "color",
  size = "size",
  shape = "shape",
  opacity = "alpha",
  strokeDash = "linetype",
  strokeWidth = "linewidth"
)

# Look up `key` in a named lookup vector, returning NULL (not an error) when the
# key is absent. `map[[key]]` on a named ATOMIC vector throws "subscript out of
# bounds" for a missing name, which would defeat the is.null()/%||% fallbacks the
# translator relies on to degrade gracefully (report an unmapped mark / channel /
# transform, never crash).
.vl_get <- function(map, key) {
  if (length(key) == 1L && !is.na(key) && key %in% names(map)) {
    map[[key]]
  } else {
    NULL
  }
}

.vl_note <- function(env, what) {
  env$dropped <- c(env$dropped, what)
}

# One IR channel + its matching scale -> a Vega-Lite encoding definition.
.channel_to_vl <- function(ch, aes, scale, notes) {
  def <- list()
  def$field <- ch$field %||% ch$expr
  if (is.null(ch$field) && !is.null(ch$expr)) {
    # Vega-Lite has no inline expression channel; the raw R text is exported as
    # the field name (approximate), so flag it rather than emit silently-invalid
    # VL. A future pass could lower it to a VL `calculate` transform.
    .vl_note(notes, paste0("expression channel '", ch$expr, "'"))
  }
  if (!is.null(ch$type) && nzchar(ch$type)) {
    def$type <- ch$type
  }
  if (isTRUE(ch$after_stat) && identical(ch$field, "count")) {
    def <- list(aggregate = "count")
  }
  if (!is.null(scale)) {
    sc <- list()
    if (!is.null(scale$domain)) {
      sc$domain <- scale$domain
    }
    if (!is.null(scale$trans)) {
      t <- .vl_get(.VL_SCALE_TYPE, scale$trans) %||% NA_character_
      if (is.na(t)) {
        .vl_note(notes, paste0("scale transform '", scale$trans, "'"))
      } else if (t != "linear") {
        sc$type <- t
      }
    }
    if (
      !is.null(scale$palette) &&
        is.character(scale$palette) &&
        length(scale$palette) > 1
    ) {
      sc$range <- scale$palette
    }
    if (length(sc)) {
      def$scale <- sc
    }
    if (!is.null(scale$name)) {
      def$title <- scale$name
    }
  }
  def
}

# Column-wise inline values -> Vega-Lite row-wise `values`.
.values_to_vl <- function(data_ir) {
  if (is.null(data_ir) || is.null(data_ir$values)) {
    return(NULL)
  }
  cols <- data_ir$values
  n <- length(cols[[1]])
  rows <- lapply(seq_len(n), function(i) {
    lapply(cols, function(col) col[[i]])
  })
  rows
}

# A faceted spec's resolve lattice -> Vega-Lite's facet-level `resolve`: each
# independent scale becomes `resolve.scale.<channel> = "independent"`, with the
# matching guide made independent too -- `resolve.axis` for x / y (each panel
# draws its own axis, as free facet scales do) and `resolve.legend` for the
# rest (one legend per panel). Shared is Vega-Lite's facet default, so shared
# entries are left out. An aesthetic with no Vega-Lite channel is reported.
# NULL when nothing is independent.
.resolve_to_vl <- function(resolve, notes) {
  out <- list()
  for (aes in names(resolve)) {
    if (!identical(resolve[[aes]], "independent")) {
      next
    }
    ch <- .vl_get(.VL_RESOLVE, aes)
    if (is.null(ch)) {
      .vl_note(notes, paste0("resolve '", aes, "'"))
      next
    }
    out$scale[[ch]] <- "independent"
    guide <- if (ch %in% c("x", "y")) "axis" else "legend"
    out[[guide]][[ch]] <- "independent"
  }
  if (length(out)) out[intersect(c("scale", "axis", "legend"), names(out))]
}

# Vega-Lite's facet-level `resolve` -> the resolve lattice. An independent
# scale maps back to its aesthetic. A guide made independent over a shared
# scale (separate axes / legends showing one scale) has no vellumplot
# equivalent and is reported in `dropped`, as is a channel with no aesthetic.
.resolve_from_vl <- function(res) {
  resolve <- list()
  dropped <- character(0)
  sc <- res$scale %||% list()
  for (ch in names(sc)) {
    if (!identical(sc[[ch]], "independent")) {
      next
    }
    aes <- .vl_get(.VL_RESOLVE_INV, ch)
    if (is.null(aes)) {
      dropped <- c(dropped, paste0("resolve.scale '", ch, "'"))
      next
    }
    resolve[[aes]] <- "independent"
  }
  for (guide in c("axis", "legend")) {
    g <- res[[guide]] %||% list()
    for (ch in names(g)) {
      if (
        identical(g[[ch]], "independent") &&
          !identical(sc[[ch]], "independent")
      ) {
        dropped <- c(
          dropped,
          paste0("resolve.", guide, " '", ch, "' over a shared scale")
        )
      }
    }
  }
  list(resolve = resolve, dropped = dropped)
}

# Does an exported x / y encoding need `scale: {zero: false}` (VP-124)? Yes for
# a continuous (linear / sqrt) position scale whose trained domain leaves zero
# out, when the export sets no explicit domain (which Vega-Lite would honour
# anyway). A log scale never includes zero in Vega-Lite either.
.vl_zero_false <- function(sc, def) {
  if (is.null(sc) || !identical(sc$type, "continuous") || isTRUE(sc$discrete)) {
    return(FALSE)
  }
  if (!is.null(def$scale$domain) || identical(def$scale$type, "log")) {
    return(FALSE)
  }
  if (isTRUE(def$bin) || identical(def$aggregate, "count")) {
    return(FALSE)
  }
  if (!is.null(sc$time_unit)) {
    return(FALSE)
  }
  dom <- suppressWarnings(range(as.numeric(sc$domain), finite = TRUE))
  all(is.finite(dom)) && (dom[1] > 0 || dom[2] < 0)
}

# A layer's tooltip quosure -> a Vega-Lite tooltip definition: a bare column
# is one field; any other expression lists the columns it reads (approximate,
# reported), as Vega-Lite has no expression tooltip.
.tooltip_to_vl <- function(q, notes) {
  e <- rlang::quo_get_expr(q)
  if (is.symbol(e)) {
    return(list(field = rlang::as_string(e)))
  }
  if (is.character(e) && length(e) == 1L) {
    return(list(value = e))
  }
  # an imported tooltip goes back out as it came in
  orig <- attr(q, "vellumplot_vl_tooltip")
  if (!is.null(orig)) {
    return(orig)
  }
  vars <- setdiff(all.vars(e), c("T", "F"))
  if (!length(vars)) {
    .vl_note(notes, "tooltip expression")
    return(NULL)
  }
  .vl_note(notes, "tooltip expression (exported as its fields)")
  lapply(vars, function(v) list(field = v))
}

#' Convert between a vellumplot spec and a Vega-Lite specification
#'
#' `spec_to_vegalite()` translates a [PlotSpec] into a
#' [Vega-Lite](https://vega.github.io/vega-lite/) specification;
#' `spec_from_vegalite()` translates one back. Both are layered grammars of
#' graphics, so the mapping is direct for the common cases — marks, encodings,
#' scales (domain / log-sqrt type / categorical range / title), `bin` +
#' `count`, faceting, inline data, and the plot title.
#'
#' Free facet scales and [resolve_scale()] map to the facet's Vega-Lite
#' `resolve`: an independent scale becomes `resolve.scale`, with its axis
#' (`x`/`y`) or legend made independent too. On import, a facet-level
#' `resolve.scale` comes back as the resolve lattice; an independent axis or
#' legend over a shared scale, and a resolve between the layers of one view
#' (e.g. a dual axis), have no vellumplot equivalent and are reported.
#'
#' A continuous x or y scale whose data leaves zero out exports
#' `scale: {zero: false}`, as Vega-Lite otherwise stretches a quantitative
#' position axis to zero. A layer's `tooltip =` is Vega-Lite's `tooltip`
#' channel: a column exports as that field, any other expression as the fields
#' it reads (reported). On import, one tooltip field becomes that column, and
#' an array of fields (or `tooltip: true`) a "title: value" line per field.
#' Other interactive arguments (`data_id`, `onclick`, ...) are reported.
#'
#' The bridge covers a **documented subset** (see the "Specs, agents, and
#' interoperability" article). Anything it cannot map — polar/flipped
#' coordinates, layer effects, patterns, statistical marks without a Vega-Lite
#' equivalent — is **reported** via a warning listing the dropped features, never
#' silently diverged.
#'
#' @param plot A [PlotSpec].
#' @param vl A Vega-Lite spec as a parsed list or a JSON string.
#' @param json For `spec_to_vegalite()`, return a JSON string instead of a list.
#' @param data Optional data frame for a Vega-Lite spec whose data is external.
#' @param env Environment channel expressions are re-quoted in (for
#'   `spec_from_vegalite()`).
#' @return `spec_to_vegalite()` returns a Vega-Lite spec (a list, or JSON string
#'   if `json = TRUE`); `spec_from_vegalite()` returns a [PlotSpec].
#' @seealso [as_spec()], [spec_to_json()]
#' @examples
#' p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = cyl)
#' vl <- spec_to_vegalite(p)
#' vl$mark
#' @export
spec_to_vegalite <- function(plot, json = FALSE) {
  notes <- new.env(parent = emptyenv())
  notes$dropped <- character(0)
  # Per-row interactivity is not part of the spec IR. A layer's `tooltip`
  # becomes Vega-Lite's tooltip channel (VP-124); its other interactive
  # arguments have no Vega-Lite channel and are reported.
  tips <- list()
  if (S7::S7_inherits(plot, PlotSpec)) {
    for (i in seq_along(plot@layers)) {
      ia <- plot@layers[[i]]@interactivity
      if (!length(ia)) {
        next
      }
      tips[i] <- list(ia$tooltip)
      for (a in setdiff(names(ia), "tooltip")) {
        .vl_note(notes, paste0("interactivity '", a, "'"))
      }
      plot@layers[[i]]@interactivity <- list()
    }
  }
  spec <- as_spec(plot)
  # The trained position scales, for `scale.zero` (VP-124): vellumplot only
  # reaches zero when the data (or a bar / area) does, while Vega-Lite starts a
  # quantitative x / y at zero unless told not to.
  trained <- tryCatch(.build_panels(plot)$scales, error = function(e) NULL)

  scales_by_aes <- list()
  for (sc in spec$scales %||% list()) {
    scales_by_aes[[sc$aesthetic]] <- sc
  }

  layer_to_vl <- function(L, i) {
    mk <- .vl_get(.VL_MARK, L$mark)
    if (is.null(mk)) {
      .vl_note(notes, paste0("mark '", L$mark, "'"))
      mk <- "point"
    }
    mark_def <- list(type = mk)
    if (identical(L$mark, "step")) {
      mark_def$interpolate <- "step-after"
    }
    if (length(L$effects)) {
      .vl_note(notes, "layer effects")
    }
    enc <- list()
    for (aes in names(L$encoding)) {
      vlc <- .vl_get(.VL_CHANNEL, aes)
      if (is.null(vlc)) {
        .vl_note(notes, paste0("encoding '", aes, "'"))
        next
      }
      # fill has its own scale (VP-48), though both land on Vega-Lite's colour
      sc <- scales_by_aes[[aes]]
      # Several vellumplot aesthetics map onto one Vega-Lite channel (color + fill
      # -> "color"; y + ymin -> "y"). If two of them are set, the second would
      # silently overwrite the first; report the collision as a dropped encoding
      # rather than losing it invisibly, and keep the first-mapped channel.
      if (!is.null(enc[[vlc]])) {
        .vl_note(
          notes,
          paste0(
            "encoding '",
            aes,
            "' (Vega-Lite channel '",
            vlc,
            "' already used)"
          )
        )
        next
      }
      d <- .channel_to_vl(L$encoding[[aes]], aes, sc, notes)
      if (identical(L[["stat"]], "bin") && aes == "x") {
        d$bin <- TRUE
      }
      if (aes %in% c("x", "y") && .vl_zero_false(trained[[aes]], d)) {
        d$scale$zero <- FALSE
      }
      enc[[vlc]] <- d
    }
    tip <- if (i <= length(tips)) tips[[i]]
    if (!is.null(tip)) {
      enc$tooltip <- .tooltip_to_vl(tip, notes)
    }
    lstat <- L[["stat"]] %||% "identity"
    if (identical(lstat, "bin") && is.null(enc$y)) {
      enc$y <- list(aggregate = "count")
    } else if (!lstat %in% c("identity", "bin")) {
      .vl_note(notes, paste0("stat '", lstat, "'"))
    }
    for (p in names(L$params)) {
      vlc <- .vl_get(.VL_CHANNEL, p)
      if (!is.null(vlc) && is.null(enc[[vlc]])) {
        enc[[vlc]] <- list(value = L$params[[p]])
      }
    }
    list(mark = mark_def, encoding = enc)
  }

  layers_vl <- lapply(seq_along(spec$layers), function(i) {
    layer_to_vl(spec$layers[[i]], i)
  })
  out <- list(`$schema` = "https://vega.github.io/schema/vega-lite/v5.json")
  vals <- .values_to_vl(spec$data)
  if (!is.null(vals)) {
    out$data <- list(values = vals)
  } else if (!is.null(spec$data)) {
    out$data <- list(name = spec$data$name %||% "data")
  }
  if (!is.null(spec$labels$title)) {
    out$title <- spec$labels$title
  }
  if (length(layers_vl) == 1) {
    out$mark <- layers_vl[[1]]$mark
    out$encoding <- layers_vl[[1]]$encoding
  } else {
    out$layer <- layers_vl
  }
  # faceting -> Vega-Lite facet operator wrapping the view.
  if (!is.null(spec$facet)) {
    inner <- out[c("mark", "encoding", "layer")]
    inner <- inner[!vapply(inner, is.null, logical(1))]
    out[c("mark", "encoding", "layer")] <- NULL
    if (identical(spec$facet$type, "wrap")) {
      out$facet <- list(field = unlist(spec$facet$cols)[1], type = "nominal")
      if (!is.null(spec$facet$ncol)) {
        out$columns <- spec$facet$ncol
      }
    } else {
      if (length(spec$facet$rows)) {
        out$facet$row <- list(
          field = unlist(spec$facet$rows)[1],
          type = "nominal"
        )
      }
      if (length(spec$facet$cols)) {
        out$facet$column <- list(
          field = unlist(spec$facet$cols)[1],
          type = "nominal"
        )
      }
    }
    out$spec <- inner
    # free facet scales / resolve_scale() -> the facet-level resolve
    res <- .resolve_to_vl(spec$resolve %||% list(), notes)
    if (!is.null(res)) {
      out$resolve <- res
    }
  }
  if (!is.null(spec$coord) && !spec$coord$kind %in% c("cartesian")) {
    .vl_note(notes, paste0("coord '", spec$coord$kind, "'"))
  }

  if (length(notes$dropped)) {
    cli::cli_warn(c(
      "Vega-Lite export dropped {length(unique(notes$dropped))} unsupported feature{?s}:",
      "x" = "{unique(notes$dropped)}"
    ))
  }
  if (isTRUE(json)) {
    .need_pkg("jsonlite", "spec_to_vegalite(json = TRUE)")
    return(jsonlite::toJSON(
      out,
      auto_unbox = TRUE,
      null = "null",
      pretty = TRUE
    ))
  }
  out
}

# One Vega-Lite encoding def -> an IR channel + (optionally) a scale record.
.channel_from_vl <- function(def, aes) {
  ch <- list()
  if (!is.null(def$aggregate) && identical(def$aggregate, "count")) {
    ch$field <- "count"
    ch$after_stat <- TRUE
  } else {
    ch$field <- def$field
  }
  if (!is.null(def$type)) {
    ch$type <- def$type
  }
  ch
}

#' @rdname spec_to_vegalite
#' @export
spec_from_vegalite <- function(vl, data = NULL, env = globalenv()) {
  if (is.character(vl)) {
    # Also accepts a file path (matching spec_from_json), via the shared reader.
    vl <- .read_json_spec(vl, "spec_from_vegalite()")
  }
  dropped <- character(0)

  # unwrap a facet operator to reach the view spec.
  facet_ir <- NULL
  if (!is.null(vl$facet) && !is.null(vl$spec)) {
    if (!is.null(vl$facet$field)) {
      facet_ir <- list(type = "wrap", cols = list(vl$facet$field))
      if (!is.null(vl$columns)) {
        facet_ir$ncol <- vl$columns
      }
    } else {
      facet_ir <- list(type = "grid")
      if (!is.null(vl$facet$row)) {
        facet_ir$rows <- list(vl$facet$row$field)
      }
      if (!is.null(vl$facet$column)) {
        facet_ir$cols <- list(vl$facet$column$field)
      }
    }
    view <- vl$spec
  } else {
    view <- vl
  }
  # A facet-level resolve maps onto the resolve lattice. A resolve between
  # layers (a dual axis, separate legends of one view) has no vellumplot
  # equivalent: its panels are facets, never overlaid layers.
  resolve_ir <- NULL
  if (!is.null(vl$resolve)) {
    if (!is.null(facet_ir)) {
      r <- .resolve_from_vl(vl$resolve)
      if (length(r$resolve)) {
        resolve_ir <- r$resolve
      }
      dropped <- c(dropped, r$dropped)
    } else {
      dropped <- c(dropped, "resolve (between layers)")
    }
  }
  if (!is.null(view$resolve)) {
    dropped <- c(dropped, "resolve (between layers)")
  }

  views <- if (!is.null(view$layer)) view$layer else list(view)
  scales <- list()

  tooltips <- list()
  tooltip_defs <- list()
  view_to_layer <- function(v) {
    tooltips[length(tooltips) + 1L] <<- list(.tooltip_from_vl(v$encoding))
    tooltip_defs[length(tooltip_defs) + 1L] <<- list(v$encoding$tooltip)
    mk <- if (is.list(v$mark)) v$mark$type else v$mark
    mark <- .vl_get(.VL_MARK_INV, mk) %||%
      {
        dropped <<- c(dropped, paste0("mark '", mk, "'"))
        "point"
      }
    enc <- list()
    params <- list()
    stat <- "identity"
    for (vlc in names(v$encoding)) {
      def <- v$encoding[[vlc]]
      if (identical(vlc, "tooltip")) {
        next # read above, into the layer's tooltip (VP-124)
      }
      aes <- .vl_get(.VL_CHANNEL_INV, vlc) %||%
        {
          dropped <<- c(dropped, paste0("channel '", vlc, "'"))
          next
        }
      if (!is.null(def$value)) {
        params[[aes]] <- def$value
        next
      }
      if (isTRUE(def$bin)) {
        stat <- "bin"
      }
      if (!is.null(def$aggregate) && identical(def$aggregate, "count")) {
        stat <- "bin"
      }
      # count on y is implied by stat="bin"; skip an explicit y count channel.
      if (identical(def$aggregate, "count") && vlc == "y") {
        next
      }
      enc[[aes]] <- .channel_from_vl(def, aes)
      if (!is.null(def$scale) || !is.null(def$title)) {
        aesthetic <- if (aes %in% c("color", "fill")) "color" else aes
        srec <- list(
          aesthetic = aesthetic,
          type = if (identical(def$type, "nominal")) {
            "discrete"
          } else {
            "continuous"
          }
        )
        if (!is.null(def$scale$domain)) {
          srec$domain <- def$scale$domain
        }
        if (!is.null(def$scale$type)) {
          srec$trans <- switch(
            def$scale$type,
            log = "log10",
            sqrt = "sqrt",
            NULL
          )
        }
        if (!is.null(def$scale$range)) {
          srec$palette <- def$scale$range
        }
        if (!is.null(def$title)) {
          srec$name <- def$title
        }
        scales[[length(scales) + 1]] <<- srec
      }
    }
    L <- list(mark = mark, encoding = enc)
    if (!identical(stat, "identity")) {
      L$stat <- stat
    }
    if (length(params)) {
      L$params <- params
    }
    L
  }

  layers <- lapply(views, view_to_layer)

  # inline data.values (row-wise) -> a data frame for from_spec.
  if (is.null(data) && !is.null(vl$data$values)) {
    rows <- vl$data$values
    nm <- unique(unlist(lapply(rows, names)))
    cols <- lapply(nm, function(k) {
      # Type-tolerant column build: collect per-row values (NULL/missing -> NA)
      # then `unlist` so the common type is derived from *all* rows, not a fragile
      # `vapply` template keyed off the first row (which breaks when row 1 is
      # missing the key or holds NA).
      vals <- lapply(rows, function(r) r[[k]] %||% NA)
      unlist(vals, use.names = FALSE)
    })
    names(cols) <- nm
    data <- as.data.frame(cols, stringsAsFactors = FALSE)
  }

  ir <- list(version = .SPEC_VERSION, layers = layers)
  if (length(scales)) {
    ir$scales <- scales
  }
  if (!is.null(facet_ir)) {
    ir$facet <- facet_ir
  }
  if (!is.null(resolve_ir)) {
    ir$resolve <- resolve_ir
  }
  if (!is.null(vl$title) && is.character(vl$title)) {
    ir$labels <- list(title = vl$title)
  }
  if (!is.null(data)) {
    ir$data <- .data_to_ir(data)
  }
  if (length(dropped)) {
    cli::cli_warn(c(
      "Vega-Lite import dropped {length(unique(dropped))} unsupported feature{?s}:",
      "x" = "{unique(dropped)}"
    ))
  }
  out <- from_spec(ir, data = data, env = env)
  # The tooltip channel is per-row interactivity, which the spec IR does not
  # carry: set it on each layer once the plot is built (VP-124).
  for (i in seq_along(out@layers)) {
    e <- if (i <= length(tooltips)) tooltips[[i]]
    if (!is.null(e)) {
      q <- rlang::new_quosure(e, env)
      attr(q, "vellumplot_vl_tooltip") <- tooltip_defs[[i]]
      out@layers[[i]]@interactivity$tooltip <- q
    }
  }
  out
}

# A Vega-Lite view's tooltip encoding -> the expression vellumplot's `tooltip =`
# takes, or NULL: one field is that column; an array of fields (or
# `tooltip: true` / `{content: "data"}`, every encoded field) becomes a
# "title: value" line per field. A field's `title` labels its line.
.tooltip_from_vl <- function(enc) {
  tt <- enc$tooltip
  if (is.null(tt) || isFALSE(tt)) {
    return(NULL)
  }
  if (isTRUE(tt) || (is.list(tt) && identical(tt$content, "data"))) {
    tt <- unname(Filter(
      function(d) is.list(d) && !is.null(d$field),
      enc[setdiff(names(enc), "tooltip")]
    ))
  } else if (is.list(tt) && !is.null(names(tt))) {
    # a single definition
    if (!is.null(tt$value)) {
      return(as.character(tt$value))
    }
    if (!is.null(tt$field) && is.null(tt$title)) {
      return(rlang::sym(tt$field))
    }
    tt <- list(tt)
  }
  fields <- Filter(function(d) is.list(d) && !is.null(d$field), tt)
  if (!length(fields)) {
    return(NULL)
  }
  parts <- list()
  for (k in seq_along(fields)) {
    d <- fields[[k]]
    if (k > 1L) {
      parts <- c(parts, list("\n"))
    }
    parts <- c(
      parts,
      list(paste0(d$title %||% d$field, ": "), rlang::sym(d$field))
    )
  }
  as.call(c(list(as.name("paste0")), parts))
}
