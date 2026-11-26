#' @include spec-json.R
NULL

# ---------------------------------------------------------------------------
# LLM / agent tooling (GAPS-HORIZON Feature 1).
#
# `spec_fields()` grounds a model in the actual data (column names + inferred
# encoding types) before it generates. `spec_diagnose()` / `vplot_from_spec()`
# turn a generated spec into a plot, returning *structured* diagnostics (unknown
# field, compile failure) rather than a traceback -- the machine-readable repair
# loop. `vplot_ask()` is a thin, model-agnostic convenience over the two.
# ---------------------------------------------------------------------------

# Map an R column to a Vega-Lite-style encoding type.
.field_type <- function(col) {
  if (inherits(col, c("Date", "POSIXct", "POSIXlt"))) {
    "temporal"
  } else if (is.ordered(col)) {
    "ordinal"
  } else if (is.numeric(col)) {
    "quantitative"
  } else {
    "nominal"
  }
}

#' Summarise a data frame's fields for a model
#'
#' `spec_fields()` returns one row per column of `data` with its name, inferred
#' encoding `type` (`quantitative` / `nominal` / `ordinal` / `temporal`), R
#' class, distinct-value count, and a few example values. It is the *grounding*
#' step for LLM plot generation ([vplot_ask()], the MCP `list_fields` tool):
#' handing a model the real vocabulary up front is what prevents hallucinated
#' column names, and [vplot_from_spec()] then validates against exactly these.
#'
#' @param data A data frame.
#' @param n_examples How many example values to show per field (default 3).
#' @return A data frame with columns `name`, `type`, `class`, `n_unique`,
#'   `examples`.
#' @seealso [vplot_from_spec()], [spec_schema()]
#' @examples
#' spec_fields(mtcars)
#' @export
spec_fields <- function(data, n_examples = 3L) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  ex <- function(col) {
    vals <- utils::head(unique(col[!is.na(col)]), n_examples)
    paste(format(vals, trim = TRUE), collapse = ", ")
  }
  data.frame(
    name = names(data),
    type = vapply(data, .field_type, character(1)),
    class = vapply(data, function(col) class(col)[1], character(1)),
    n_unique = vapply(data, function(col) length(unique(col)), integer(1)),
    examples = vapply(data, ex, character(1)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

# All bare-field references in a spec's channels + facets (the names that must
# exist in the data). Expression / after_stat channels are skipped -- they are
# not simple field lookups.
.spec_referenced_fields <- function(spec) {
  fields <- character(0)
  for (L in spec$layers %||% list()) {
    for (ch in L$encoding %||% list()) {
      if (!is.null(ch$field)) {
        fields <- c(fields, ch$field)
      }
    }
  }
  if (!is.null(spec$facet)) {
    fields <- c(
      fields,
      unlist(spec$facet$rows, use.names = FALSE),
      unlist(spec$facet$cols, use.names = FALSE)
    )
  }
  unique(fields)
}

# ---------------------------------------------------------------------------
# Compile report + pre-compile checks for `spec_diagnose()`.
#
# A row missing a continuous position is removed with a warning (or, on a path,
# leaves a gap); a row a log axis cannot map reaches the emitter and simply
# produces no ink; a row outside explicit scale limits is drawn and clipped away
# (the trainer's own comment in `.train_position_continuous()`). The report is
# the place to see all of it: per layer, how many input rows will not show and
# why. It evaluates the layer's own encodings with the compiler's
# `.resolve_layer()`, tests missingness with the compiler's
# `.is_continuous_position()`, and asks the same `.scale_for()` /
# `.resolve_trans()` the trainer asks, so it counts exactly what the compile
# will do rather than re-deriving the rules. A missing discrete position or
# facet value is drawn (as a trailing "NA" level or panel), so it is not counted.
# Rows a point or text mark draws nothing for because their size is missing or
# censored are taken from the compile's own tally (`.size_dropped_rows()`).
#
# Rows a stat leaves out itself (a non-finite value, a group too small for a
# density or a fit) are read back from the stat: every stat that drops input
# rows records them (`.stat_dropped()` in compile-stat.R), and the report
# resolves each panel the way the compile does to collect them.
#
# Scale limits follow each scale's `oob` handler, as the compile does. A
# position outside its limits is clipped (the default) or censored, so it is not
# shown; a squished one is drawn on the panel edge. A non-position value outside
# its limits is still drawn (colour clamps or takes the NA colour, size, alpha
# and linewidth extrapolate), except a censored size, which draws no mark. Rows
# drawn at a changed value are reported in their own `oob` column, not in
# `dropped`.
# ---------------------------------------------------------------------------

# Position channels, by axis, whose value places a row. A missing continuous
# one, or one the axis transform cannot map, leaves the row undrawn.
.DIAG_POS_CHANNELS <- list(
  x = c("x", "xend", "xmin", "xmax"),
  y = c("y", "yend", "ymin", "ymax")
)

# The reasons a row is not shown, in the order a row is attributed to one (a row
# missing `x` AND outside the `y` limits counts once, as missing).
.DIAG_DROP_REASONS <- c(
  missing = "missing a position or size value",
  transform = "not mappable on a transformed axis (e.g. <= 0 on a log scale)",
  stat = "left out by the layer's statistic (a non-finite value, or a group too small to summarise)",
  limits = "outside the scale limits"
)

# Non-position channels whose scale can carry explicit numeric `limits`.
.DIAG_OOB_CHANNELS <- c("color", "fill", "size", "alpha", "linewidth")

# Variables the plot's facet reads.
.facet_vars <- function(facet) {
  if (is.null(facet)) {
    return(character(0))
  }
  unique(unlist(lapply(
    c(facet@cols, facet@rows),
    function(q) all.vars(rlang::quo_get_expr(q))
  )))
}

# One layer's per-row drop reason (NA = shown), on its pre-stat data `ld`.
# `stat_rows` are the rows of `ld` the layer's stat left out (see
# `.stat_dropped_rows()`).
.layer_drop_reasons <- function(plot, layer, ld, stat_rows = integer(0)) {
  n <- nrow(ld)
  why <- rep(NA_character_, n)
  flag <- function(mask, reason) {
    mask <- mask & is.na(why)
    why[which(mask)] <<- reason
  }
  L <- tryCatch(.resolve_layer(layer, ld), error = function(e) NULL)
  if (is.null(L)) {
    return(why)
  }
  # A channel only describes rows when it has one value per row: layout marks
  # (sankey, venn, sf ...) synthesise a 2-value extent instead.
  per_row <- function(ch) {
    v <- L$values[[ch]]
    if (length(v) == n) v
  }
  axes <- lapply(names(.DIAG_POS_CHANNELS), function(axis) {
    sc <- .scale_for(plot, axis)
    vals <- Filter(Negate(is.null), lapply(.DIAG_POS_CHANNELS[[axis]], per_row))
    list(sc = sc, vals = vals)
  })
  for (a in axes) {
    for (v in a$vals) {
      if (.is_continuous_position(v)) {
        flag(is.na(v), "missing")
      }
    }
  }
  is_num <- function(v) is.numeric(v) || inherits(v, c("Date", "POSIXct"))
  for (a in axes) {
    sc <- a$sc
    is_time_scale <- !is.null(sc) && sc@type %in% c("date", "datetime", "time")
    tr <- if (is_time_scale) NULL else .resolve_trans(sc)
    if (is.null(tr)) {
      next
    }
    for (v in a$vals) {
      if (is.numeric(v) && !inherits(v, c("Date", "POSIXct"))) {
        num <- as.numeric(v)
        tv <- suppressWarnings(tr$transform(num))
        flag(is.finite(num) & !is.finite(tv), "transform")
      }
    }
  }
  stat_rows <- stat_rows[stat_rows >= 1L & stat_rows <= n]
  flag(seq_len(n) %in% stat_rows, "stat")
  # Outside the limits: clipped (oob "keep", the default) or censored to NA,
  # either way not shown; a squished value is drawn on the edge, so it is only
  # reported in `oob` (the "shown_oob" attribute).
  shown_oob <- rep(FALSE, n)
  for (a in axes) {
    sc <- a$sc
    dom <- if (!is.null(sc)) sc@domain
    if (length(dom) != 2L || !is_num(dom)) {
      next
    }
    for (v in a$vals) {
      if (!is_num(v)) {
        next
      }
      o <- .diag_oob(sc, as.numeric(v), as.numeric(dom))
      flag(o$out & !o$shown, "limits")
      shown_oob <- shown_oob | (o$out & o$shown)
    }
  }
  attr(why, "shown_oob") <- shown_oob
  why
}

# Rows of `num` outside a scale's limits `lims` (`out`), and what the scale's
# `oob` handler (`.oob_fun()`: censor, squish, keep or a function; none means
# keep) makes of them: `shown` -- mapped to a finite value inside the limits
# (squished) -- or `gone` -- censored to `NA`. A partial limit (an `NA` end,
# `ylim(0, NA)`) bounds only the end it names.
.diag_oob <- function(sc, num, lims) {
  rng <- sort(c(
    if (is.na(lims[1])) -Inf else lims[1],
    if (is.na(lims[2])) Inf else lims[2]
  ))
  out <- is.finite(num) & (num < rng[1] | num > rng[2])
  oob <- .oob_fun(sc)
  ov <- if (is.null(oob) || !any(out)) {
    num
  } else {
    suppressWarnings(as.numeric(oob(num, range = rng)))
  }
  list(
    out = out,
    shown = out & is.finite(ov) & ov >= rng[1] & ov <= rng[2],
    gone = out & !is.finite(ov)
  )
}

# Per layer, the rows of its input data (`layer@data %||% plot@data`) that its
# stat left out, collected by resolving every facet panel as the compile does
# (`.resolve_panel()`: missing positions removed, then the stat) and mapping
# each panel's `L$stat_dropped` back through its source rows. A panel that
# cannot be resolved reports none here; the dry-run compile reports its error.
.stat_dropped_rows <- function(plot) {
  out <- rep(list(integer(0)), length(plot@layers))
  fa <- tryCatch(.facet_assign(plot), error = function(e) NULL)
  panels <- lapply(fa$panels, function(p) {
    tryCatch(
      suppressWarnings(.resolve_panel(plot, p)),
      error = function(e) NULL
    )
  })
  for (res in panels) {
    for (i in seq_along(res)) {
      L <- res[[i]]
      drop <- L$stat_dropped
      if (length(drop)) {
        out[[i]] <- c(out[[i]], L$source$rows[drop])
      }
    }
  }
  lapply(out, function(r) sort(unique(r[!is.na(r)])))
}

# Per row of `ld`, for the non-position channels (colour, fill, size, alpha,
# linewidth) whose scale has explicit numeric limits: `oob` -- a value outside
# the limits (colour clamps or takes the NA colour, size, alpha and linewidth
# extrapolate). A size censored to `NA` (`scale_size(oob = "censor")`) draws no
# mark instead; the report counts those from the compile's own tally
# (`.size_dropped_rows()`) and leaves them out of `oob`.
.layer_oob_rows <- function(plot, layer, ld) {
  n <- nrow(ld)
  oob <- rep(FALSE, n)
  L <- tryCatch(.resolve_layer(layer, ld), error = function(e) NULL)
  if (is.null(L)) {
    return(list(oob = oob))
  }
  for (ch in .DIAG_OOB_CHANNELS) {
    v <- L$values[[ch]]
    sc <- .scale_for(plot, ch)
    dom <- if (!is.null(sc)) sc@domain
    if (
      is.null(sc) ||
        length(v) != n ||
        !is.numeric(v) ||
        identical(sc@type, "identity") ||
        length(dom) != 2L ||
        !is.numeric(dom)
    ) {
      next
    }
    o <- .diag_oob(sc, as.numeric(v), as.numeric(dom))
    oob <- oob | o$out
  }
  list(oob = oob)
}

# Per layer, the rows of its input data drawn without a mark because their size
# is missing (`missing`) or censored by the size scale (`censored`): the
# compile's own tally (`.panel_size_drops()`, which feeds the "Removed n rows"
# warning), taken from the panels built as the compile builds them and mapped
# back through each resolved layer's source rows (VP-149). A layer whose rows do
# not map back one-to-one (a stat aggregated them) reports none here.
.size_dropped_rows <- function(plot) {
  none <- list(missing = integer(0), censored = integer(0))
  out <- rep(list(none), length(plot@layers))
  built <- tryCatch(
    suppressMessages(suppressWarnings(.build_panels(plot))),
    error = function(e) NULL
  )
  if (is.null(built) || is.null(built$scales)) {
    return(out)
  }
  for (p in built$panels) {
    for (i in seq_along(p$resolved)) {
      sz <- .panel_size_drops(p, i, built$scales)
      if (!any(sz$missing) && !any(sz$censored)) {
        next
      }
      src <- p$resolved[[i]]$source
      to_input <- function(k) {
        if (!is.null(src$members)) {
          src$rows[unlist(src$members[k])]
        } else if (isTRUE(src$per_row)) {
          src$rows[k]
        } else {
          integer(0)
        }
      }
      out[[i]]$missing <- c(out[[i]]$missing, to_input(which(sz$missing)))
      out[[i]]$censored <- c(out[[i]]$censored, to_input(which(sz$censored)))
    }
  }
  out
}

# The per-layer compile report: one row per layer with its input row count, how
# many rows will not be shown for each reason, the total, and how many shown
# rows have a non-position value outside its scale limits (`oob`).
.compile_report <- function(plot) {
  stat_rows <- .stat_dropped_rows(plot)
  size_rows <- .size_dropped_rows(plot)
  rows <- lapply(seq_along(plot@layers), function(i) {
    layer <- plot@layers[[i]]
    ld <- layer@data %||% plot@data
    n <- if (is.data.frame(ld)) nrow(ld) else NA_integer_
    why <- if (is.data.frame(ld)) {
      .layer_drop_reasons(plot, layer, ld, stat_rows[[i]])
    } else {
      character(0)
    }
    oob <- 0L
    if (is.data.frame(ld)) {
      # A row drawn without a mark for a missing or censored size, counted as
      # the compile warning counts it (VP-149). A missing size is a missing
      # value, the first reason; a censored one is outside the limits, the last.
      shown_oob <- attr(why, "shown_oob") %||% FALSE
      sz <- size_rows[[i]]
      why[seq_len(n) %in% sz$missing] <- "missing"
      why[is.na(why) & seq_len(n) %in% sz$censored] <- "limits"
      o <- .layer_oob_rows(plot, layer, ld)
      oob <- sum((o$oob | shown_oob) & is.na(why))
    }
    counts <- vapply(
      names(.DIAG_DROP_REASONS),
      function(r) sum(why == r, na.rm = TRUE),
      integer(1)
    )
    data.frame(
      layer = i,
      mark = layer@mark,
      rows = as.integer(n),
      missing = counts[["missing"]],
      transform = counts[["transform"]],
      stat = counts[["stat"]],
      limits = counts[["limits"]],
      dropped = sum(counts),
      oob = as.integer(oob),
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) {
    return(data.frame(
      layer = integer(0),
      mark = character(0),
      rows = integer(0),
      missing = integer(0),
      transform = integer(0),
      stat = integer(0),
      limits = integer(0),
      dropped = integer(0),
      oob = integer(0)
    ))
  }
  do.call(rbind, rows)
}

# One "rows not shown" diagnostic per layer that loses any.
.report_diagnostics <- function(report) {
  out <- list()
  for (k in seq_len(nrow(report))) {
    r <- report[k, ]
    if (isTRUE(r$oob > 0L)) {
      out[[length(out) + 1L]] <- list(
        severity = "warning",
        field = NA_character_,
        message = sprintf(
          "Layer %d (%s): %d of %d rows have a value outside a scale's limits but are still drawn: squished to the edge, clamped to the end of the colour palette or given its NA colour, or at an extrapolated size, alpha or linewidth.",
          r$layer,
          r$mark,
          r$oob,
          r$rows
        ),
        hint = "Widen the scale `limits`, or filter the data. See `$report` (column `oob`)."
      )
    }
    if (r$dropped == 0L) {
      next
    }
    parts <- character(0)
    for (reason in names(.DIAG_DROP_REASONS)) {
      if (r[[reason]] > 0L) {
        parts <- c(
          parts,
          sprintf("%d %s", r[[reason]], .DIAG_DROP_REASONS[[reason]])
        )
      }
    }
    out[[length(out) + 1L]] <- list(
      severity = "warning",
      field = NA_character_,
      message = sprintf(
        "Layer %d (%s): %d of %d rows will not be shown: %s.",
        r$layer,
        r$mark,
        r$dropped,
        r$rows,
        paste(parts, collapse = ", ")
      ),
      hint = "See `$report` for the per-layer counts."
    )
  }
  out
}

# Pre-compile checks on a built plot: mistakes that compile to a wrong plot (or
# an opaque compile error) but can be read off the spec and its data.
.static_diagnostics <- function(plot) {
  out <- list()
  add <- function(
    severity,
    message,
    field = NA_character_,
    hint = NA_character_
  ) {
    out[[length(out) + 1L]] <<- list(
      severity = severity,
      field = field,
      message = message,
      hint = hint
    )
  }
  # 1. A facet variable missing from a layer's data. From the plot data that is
  # an error (the facet cannot be built); from a layer's own data the layer is
  # split on the facet variables it has and repeated across the values of the
  # one it lacks (ggplot2's rule, meant for annotation layers), which is worth a
  # warning because it is as often a forgotten column.
  fv <- .facet_vars(plot@facet)
  if (length(fv)) {
    miss_plot <- setdiff(fv, names(plot@data))
    for (f in miss_plot) {
      add(
        "error",
        sprintf("Facet variable '%s' is not a column of the plot data.", f),
        field = f,
        hint = paste0(
          "Available fields: ",
          paste(names(plot@data), collapse = ", "),
          "."
        )
      )
    }
    for (i in seq_along(plot@layers)) {
      ld <- plot@layers[[i]]@data
      if (is.null(ld)) {
        next
      }
      for (f in setdiff(fv, names(ld))) {
        add(
          "warning",
          sprintf(
            "Layer %d's data has no facet variable '%s', so its rows are repeated in every '%s' panel.",
            i,
            f,
            f
          ),
          field = f,
          hint = "Add the column to the layer's data to draw each row in its own panel; leave it out only for an annotation meant for every panel."
        )
      }
    }
  }

  # The layers' own encodings (pre-stat), for the colour checks below. A layer
  # that cannot even be evaluated is left to the compile step to report.
  resolved <- lapply(plot@layers, function(layer) {
    ld <- layer@data %||% plot@data
    tryCatch(.resolve_layer(layer, ld), error = function(e) NULL)
  })
  resolved <- Filter(Negate(is.null), resolved)
  # `color` and `fill` are separate scales; each gets the two checks.
  for (channel in c("color", "fill")) {
    votes <- .colour_channel_votes(resolved, channel)$votes
    scalespec <- .scale_for(plot, channel)
    explicit_kind <- !is.null(scalespec) &&
      length(scalespec@type) &&
      nzchar(scalespec@type)

    # 2. One channel (one scale, one legend) mapped to a continuous variable
    # in one layer and a discrete one in another. Without an explicit scale
    # kind the compile aborts; with one, one of the two variables is forced
    # onto a scale of the wrong kind.
    clash <- .colour_type_clash(votes)
    if (!is.null(clash)) {
      q <- clash$quantitative
      d <- clash$discrete
      add(
        if (explicit_kind) "warning" else "error",
        sprintf(
          "Layers share one %s scale and legend, but layer %d maps %s to a continuous variable and layer %d maps it to a %s one.",
          channel,
          q$layer,
          channel,
          d$layer,
          d$type
        ),
        field = channel,
        hint = "Map both to the same kind of variable (convert one with factor() or as.numeric())."
      )
    }

    # 3. A manual (discrete) palette on a continuous colour channel. The
    # compile refuses it, as ggplot2 does ("Continuous values supplied to a
    # discrete scale"), so it is an error here too, caught before compiling.
    quant <- Filter(function(v) identical(v$type, "quantitative"), votes)
    if (
      length(quant) &&
        !is.null(scalespec) &&
        identical(scalespec@type, "discrete") &&
        !is.null(scalespec@palette)
    ) {
      v <- quant[[1]]
      add(
        "error",
        sprintf(
          "A manual/discrete palette is set on the %s scale, but layer %d maps %s to a continuous variable: continuous values cannot go on a discrete colour scale.",
          channel,
          v$layer,
          v$channel
        ),
        field = v$channel,
        hint = sprintf(
          "Use a continuous colour scale (scale_%1$s_gradient(), scale_%1$s_viridis_c(), scale_%1$s_binned()), or convert the variable with factor().",
          channel
        )
      )
    }
  }
  out
}

#' Diagnose a spec against its data
#'
#' `spec_diagnose()` checks a spec (a list, JSON string, or path) or a built
#' [PlotSpec] without throwing: it parses it, validates every referenced field
#' against the data, runs a few pre-compile checks, dry-run compiles it, and
#' reports per layer how many rows will not be shown. It returns a structured
#' verdict. `vplot_from_spec()` is the strict form — it returns the [PlotSpec] on
#' success and raises a classed `vellumplot_spec_invalid` error (carrying the
#' same diagnostics) on failure.
#'
#' This is the agent repair loop: a failure comes back as data an agent can act
#' on (which field is unknown, the nearest real field) rather than a traceback.
#'
#' @section Pre-compile checks:
#' Before compiling, the plot is checked for mistakes that would compile to a
#' wrong plot or fail with an opaque error:
#' * a **facet variable missing** from the plot data (an error), or from a
#'   layer's own data (a warning: that layer is then repeated across that
#'   variable's panels);
#' * **`color` (or `fill`) mapped to variables of different type** in two
#'   layers (one continuous, one discrete), although the layers share one scale
#'   and legend (an error, or a warning when the scale kind is set explicitly);
#' * a **manual or discrete palette** (`scale_color_manual()`, ...) on a
#'   continuous colour channel, which the compile refuses (an error).
#'
#' @section Compile report:
#' Some rows produce no visible mark. The `report` element counts them per
#' layer, on the layer's input rows, attributing each row to the first reason
#' that applies:
#' * `missing`: a continuous position value (`x`, `y`, `xend`, `ymin`, ...) is
#'   `NA`, or the size of a point or text mark is. Such a row is removed from
#'   the plot with a warning (the rows that warning counts are counted here
#'   too), or leaves a gap in a line, step or area;
#' * `transform`: the axis transform cannot map the value, e.g. a value `<= 0`
#'   on a log axis;
#' * `stat`: the layer's statistic leaves the row out, e.g. an infinite value
#'   in a histogram or density, or a group with too few points for a density,
#'   a smooth or an ellipse;
#' * `limits`: the value is outside explicit scale limits ([lims()],
#'   `scale_*(limits = )`), so it is clipped away. Coordinate limits
#'   (`coord_cartesian(xlim = )`) zoom instead and are not counted.
#'
#' Their total is `dropped`. Limits follow each scale's `oob` handler: a
#' position outside its limits counts under `limits` when it is clipped (the
#' default) or censored, and so does a censored size. A separate column, `oob`,
#' counts rows that are outside a scale's `limits` but still drawn: a squished
#' position, or a colour, fill, size, alpha or linewidth value (clamped to the
#' end of the palette, given the NA colour, or extrapolated). They are not part
#' of `dropped`.
#'
#' A missing *non-position* value other than such a size (colour, fill, ...) is
#' drawn with the scale's `NA` value, a missing discrete position as a trailing
#' `"NA"` category, and a missing facet value in a trailing `"NA"` panel, so
#' none of them is counted.
#' Each layer that loses rows also adds a `"warning"` diagnostic, so it shows in
#' the printed summary.
#'
#' @param spec A spec list (from [as_spec()]), a JSON string, a `.json` path, or
#'   a [PlotSpec].
#' @param data The data frame the spec is drawn from. Required when the spec
#'   stores data by reference; optional (for field validation) when inlined.
#'   Ignored for a [PlotSpec], which carries its own data.
#' @param env Environment channel expressions are re-quoted in.
#' @return `spec_diagnose()` returns a list of class `vellumplot_diagnosis` with
#'   `ok` (logical), `plot` (the [PlotSpec] or `NULL`), `diagnostics` (a list of
#'   records with `severity`, `field`, `message`, `hint`), and `report` (a data
#'   frame with one row per layer: `layer`, `mark`, `rows`, the counts
#'   `missing`, `transform`, `stat`, `limits`, their total `dropped`, and
#'   `oob`, the rows outside a scale's limits that are still drawn; `NULL`
#'   when the plot could not be built). Printing it shows a summary.
#'   `vplot_from_spec()` returns a [PlotSpec].
#' @seealso [spec_fields()], [spec_schema()], [vplot_ask()]
#' @examples
#' spec <- as_spec(vplot(mtcars) |> mark_point(x = wt, y = mpg))
#' d <- spec_diagnose(spec, data = mtcars)
#' d$ok
#'
#' # a PlotSpec works too; the report shows rows a log axis cannot draw
#' df <- data.frame(x = 1:4, y = c(-1, 1, 10, NA))
#' p <- vplot(df) |>
#'   mark_point(x = x, y = y) |>
#'   scale_y_continuous(trans = "log10")
#' spec_diagnose(p)
#' @export
spec_diagnose <- function(spec, data = NULL, env = globalenv()) {
  diags <- list()
  add <- function(
    severity,
    message,
    field = NA_character_,
    hint = NA_character_
  ) {
    diags[[length(diags) + 1]] <<- list(
      severity = severity,
      field = field,
      message = message,
      hint = hint
    )
  }
  # 1. parse the spec (JSON string / path / list) into an IR list.
  if (is.character(spec)) {
    spec <- tryCatch(
      .read_json_spec(spec, "spec_diagnose()"),
      error = function(e) {
        add("error", paste0("Invalid JSON: ", conditionMessage(e)))
        NULL
      }
    )
    if (is.null(spec)) {
      return(.diagnosis(FALSE, NULL, diags, NULL))
    }
  }
  given_plot <- S7::S7_inherits(spec, PlotSpec)
  # 2. field validation against the data (the common hallucination). A built
  # PlotSpec has already evaluated its fields, so this is for serialized specs.
  known <- if (given_plot) NULL else names(data)
  if (!is.null(known)) {
    for (f in .spec_referenced_fields(spec)) {
      if (!f %in% known) {
        near <- known[utils::adist(f, known)[1, ] <= 2]
        hint <- if (length(near)) {
          paste0("Did you mean: ", paste(near, collapse = ", "), "?")
        } else {
          paste0("Available fields: ", paste(known, collapse = ", "), ".")
        }
        add("error", paste0("Unknown field '", f, "'."), field = f, hint = hint)
      }
    }
  }
  # 3. build.
  plot <- if (given_plot) {
    spec
  } else {
    tryCatch(
      from_spec(spec, data = data, env = env),
      error = function(e) {
        cls <- setdiff(class(e), c("rlang_error", "error", "condition"))[1]
        add(
          "error",
          conditionMessage(e),
          hint = if (!is.na(cls)) paste0("(", cls, ")")
        )
        NULL
      }
    )
  }
  # 4. pre-compile checks, skipping a finding the field validation above
  # already reported as an unknown field.
  report <- NULL
  if (S7::S7_inherits(plot, PlotSpec)) {
    seen <- vapply(diags, function(d) d$field, character(1))
    for (d in .static_diagnostics(plot)) {
      if (identical(d$severity, "error") && d$field %in% seen) {
        next
      }
      diags[[length(diags) + 1L]] <- d
    }
  }
  # 5. dry-run compile, then the per-layer report of rows not shown.
  if (
    !is.null(plot) &&
      !any(vapply(diags, function(d) d$severity == "error", logical(1)))
  ) {
    ok_compile <- tryCatch(
      {
        # Rows removed for a missing position or size are counted in the report
        # below, so the compile's own warning about them would only repeat it.
        withCallingHandlers(
          vellum::as_vellum_scene(plot),
          vellumplot_missing_rows = function(cnd) {
            invokeRestart("muffleWarning")
          }
        )
        TRUE
      },
      error = function(e) {
        add("error", paste0("Compile failed: ", conditionMessage(e)))
        FALSE
      }
    )
  } else {
    ok_compile <- FALSE
  }
  if (S7::S7_inherits(plot, PlotSpec)) {
    report <- tryCatch(.compile_report(plot), error = function(e) NULL)
    if (!is.null(report)) {
      diags <- c(diags, .report_diagnostics(report))
    }
  }
  ok <- ok_compile &&
    !any(vapply(diags, function(d) d$severity == "error", logical(1)))
  .diagnosis(ok, if (ok) plot else NULL, diags, report)
}

# The `spec_diagnose()` result object.
.diagnosis <- function(ok, plot, diagnostics, report) {
  structure(
    list(ok = ok, plot = plot, diagnostics = diagnostics, report = report),
    class = "vellumplot_diagnosis"
  )
}

# The print method for a `spec_diagnose()` result. Registered on base's `print`
# from `.onLoad()`, not via `S3method()` in NAMESPACE: the package's
# `S7::method(print, ...) <-` calls bind a copy of `print` in this namespace, so
# a NAMESPACE registration lands in this package's own S3 table, where base's
# `print()` never looks.
.print_diagnosis <- function(x, ...) {
  d <- x$diagnostics
  sev <- vapply(d, function(r) r$severity, character(1))
  cat(sprintf(
    "<vellumplot diagnosis> %s: %d error%s, %d warning%s\n",
    if (isTRUE(x$ok)) "ok" else "not ok",
    sum(sev == "error"),
    if (sum(sev == "error") == 1L) "" else "s",
    sum(sev == "warning"),
    if (sum(sev == "warning") == 1L) "" else "s"
  ))
  for (r in d) {
    cat(sprintf("* [%s] %s\n", r$severity, r$message))
    if (!is.na(r$hint)) {
      cat(sprintf("    %s\n", r$hint))
    }
  }
  rep <- x$report
  if (!is.null(rep) && nrow(rep)) {
    cat(
      "\nRows not shown, per layer (`oob`: drawn, outside a scale's limits):\n"
    )
    print(rep, row.names = FALSE)
  }
  invisible(x)
}

#' @rdname spec_diagnose
#' @export
vplot_from_spec <- function(spec, data = NULL, env = globalenv()) {
  res <- spec_diagnose(spec, data = data, env = env)
  if (!res$ok) {
    msgs <- vapply(
      res$diagnostics,
      function(d) {
        m <- d$message
        if (!is.na(d$hint)) {
          m <- paste0(m, " ", d$hint)
        }
        m
      },
      character(1)
    )
    cli::cli_abort(
      c("Invalid spec:", stats::setNames(msgs, rep("x", length(msgs)))),
      class = "vellumplot_spec_invalid"
    )
  }
  res$plot
}

#' Generate a plot from a natural-language request
#'
#' `vplot_ask()` is a thin, **model-agnostic** convenience: it assembles the
#' grounding payload (the JSON [spec_schema()] plus [spec_fields()] for `data`
#' and the user's `prompt`), hands it to a `responder` function you supply — any
#' function that takes the payload and returns a JSON spec string (wrapping the
#' Claude API, an MCP client, a local model, or a canned fixture) — and validates
#' the result with [vplot_from_spec()].
#'
#' vellumplot deliberately ships no built-in model client: the value is the
#' schema, the grounding, and the validated repair loop, not a hard dependency on
#' one provider. For a ready-to-run agent integration, point an MCP client at the
#' bundled server (`system.file("mcp/server.R", package = "vellumplot")`).
#'
#' @param prompt A natural-language description of the desired plot.
#' @param data The data frame to plot.
#' @param responder A function `function(payload) -> json_string`. `payload` is a
#'   list with `prompt`, `fields` (from [spec_fields()]), and `schema`.
#' @param env Environment channel expressions are re-quoted in.
#' @return A [PlotSpec].
#' @seealso [spec_fields()], [vplot_from_spec()], [spec_schema()]
#' @examplesIf requireNamespace("jsonlite", quietly = TRUE)
#' # a canned responder standing in for a model call:
#' responder <- function(payload) {
#'   spec_to_json(vplot(mtcars) |> mark_point(x = wt, y = mpg))
#' }
#' p <- vplot_ask("scatter of weight vs mpg", mtcars, responder)
#' @export
vplot_ask <- function(prompt, data, responder, env = globalenv()) {
  if (!is.function(responder)) {
    cli::cli_abort(c(
      "{.arg responder} must be a function {.code function(payload) -> json}.",
      "i" = "vellumplot ships no model client; supply one, or use the bundled MCP server."
    ))
  }
  payload <- list(
    prompt = prompt,
    fields = spec_fields(data),
    schema = spec_schema(as = "string")
  )
  json <- responder(payload)
  if (!is.character(json) || length(json) != 1) {
    cli::cli_abort("{.arg responder} must return a length-1 JSON string.")
  }
  vplot_from_spec(json, data = data, env = env)
}
