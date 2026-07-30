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
  text = "label"
)

# transform name <-> Vega-Lite scale type.
.VL_SCALE_TYPE <- c(
  log10 = "log",
  log = "log",
  log2 = "log",
  sqrt = "sqrt",
  identity = "linear"
)

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
      t <- .VL_SCALE_TYPE[[scale$trans]] %||% NA_character_
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

#' Convert between a vellumplot spec and a Vega-Lite specification
#'
#' `spec_to_vegalite()` translates a [PlotSpec] into a
#' [Vega-Lite](https://vega.github.io/vega-lite/) specification;
#' `spec_from_vegalite()` translates one back. Both are layered grammars of
#' graphics, so the mapping is direct for the common cases — marks, encodings,
#' scales (domain / log-sqrt type / categorical range / title), `bin` +
#' `count`, faceting, inline data, and the plot title.
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
  spec <- as_spec(plot)
  notes <- new.env(parent = emptyenv())
  notes$dropped <- character(0)

  scales_by_aes <- list()
  for (sc in spec$scales %||% list()) {
    scales_by_aes[[sc$aesthetic]] <- sc
  }
  colour_scale <- scales_by_aes[["color"]] %||% scales_by_aes[["fill"]]

  layer_to_vl <- function(L) {
    mk <- .VL_MARK[[L$mark]]
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
      vlc <- .VL_CHANNEL[[aes]]
      if (is.null(vlc)) {
        .vl_note(notes, paste0("encoding '", aes, "'"))
        next
      }
      sc <- if (aes %in% c("color", "fill")) {
        colour_scale
      } else {
        scales_by_aes[[aes]]
      }
      d <- .channel_to_vl(L$encoding[[aes]], aes, sc, notes)
      if (identical(L[["stat"]], "bin") && aes == "x") {
        d$bin <- TRUE
      }
      enc[[vlc]] <- d
    }
    lstat <- L[["stat"]] %||% "identity"
    if (identical(lstat, "bin") && is.null(enc$y)) {
      enc$y <- list(aggregate = "count")
    } else if (!lstat %in% c("identity", "bin")) {
      .vl_note(notes, paste0("stat '", lstat, "'"))
    }
    for (p in names(L$params)) {
      vlc <- .VL_CHANNEL[[p]]
      if (!is.null(vlc) && is.null(enc[[vlc]])) {
        enc[[vlc]] <- list(value = L$params[[p]])
      }
    }
    list(mark = mark_def, encoding = enc)
  }

  layers_vl <- lapply(spec$layers, layer_to_vl)
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

  views <- if (!is.null(view$layer)) view$layer else list(view)
  scales <- list()

  view_to_layer <- function(v) {
    mk <- if (is.list(v$mark)) v$mark$type else v$mark
    mark <- .VL_MARK_INV[[mk]] %||%
      {
        dropped <<- c(dropped, paste0("mark '", mk, "'"))
        "point"
      }
    enc <- list()
    params <- list()
    stat <- "identity"
    for (vlc in names(v$encoding)) {
      def <- v$encoding[[vlc]]
      aes <- .VL_CHANNEL_INV[[vlc]] %||%
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
  from_spec(ir, data = data, env = env)
}
