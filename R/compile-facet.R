#' @include classes.R facet.R coord.R compile-resolve.R compile-train.R
NULL

# A factor key from one or more faceting quosures, preserving factor level order
# for a single factor and ordering by each variable's natural type otherwise
# (numeric numerically, factor by level, character lexicographically) -- so a
# numeric facet reads 1, 2, 10 rather than the lexicographic 1, 10, 2.
#
# A missing facet value is a level of its own, labelled "NA" and placed last,
# as in ggplot2: its rows get their own trailing panel rather than vanishing.
# With several variables each one's NA sorts last within that variable.
# `na_level = FALSE` gives such a row an NA key instead (no level), for callers
# that drop it, such as the animation states.
.facet_key <- function(quos, data, na_level = TRUE) {
  vals <- lapply(quos, function(q) rlang::eval_tidy(q, data))
  .facet_key_vals(vals, na_level)
}

# `.facet_key()` on already evaluated facet values (one vector per quosure).
.facet_key_vals <- function(vals, na_level = TRUE) {
  if (length(vals) == 1 && is.factor(vals[[1]])) {
    f <- droplevels(vals[[1]])
    if (!anyNA(f) || !na_level) {
      return(f)
    }
    chr <- as.character(f)
    chr[is.na(chr)] <- "NA"
    return(factor(chr, levels = unique(c(levels(f), "NA"))))
  }
  parts <- lapply(vals, .facet_chr)
  combined <- do.call(paste, c(parts, sep = ", "))
  if (!na_level) {
    combined[Reduce(`|`, lapply(vals, is.na))] <- NA_character_
  }
  # Order rows by each variable in turn (factors by level code, everything else
  # by its own type; `order()` puts NA last), then take the level order from it.
  ord <- do.call(
    order,
    lapply(vals, function(v) if (is.factor(v)) as.integer(v) else v)
  )
  lv <- unique(combined[ord])
  factor(combined, levels = lv[!is.na(lv)])
}

# One facet variable's values as the labels panels are matched on ("NA" for a
# missing value).
.facet_chr <- function(v) {
  v <- as.character(v)
  v[is.na(v)] <- "NA"
  v
}

# Variables one facet quosure reads.
.quo_vars <- function(q) all.vars(rlang::quo_get_expr(q))

# Pool one facet variable's values from several tables. Values of one type
# combine as that type (factors union their levels, numbers stay numeric), so
# the pooled levels sort the way a single table's would; mixed types fall back
# to character.
.facet_pool <- function(vs) {
  if (length(vs) == 1L) {
    return(vs[[1]])
  }
  if (all(vapply(vs, is.factor, logical(1)))) {
    return(do.call(c, vs))
  }
  cls <- unique(lapply(vs, class))
  if (length(cls) == 1L && !is.factor(vs[[1]])) {
    return(do.call(c, vs))
  }
  unlist(lapply(vs, as.character), use.names = FALSE)
}

# The panel levels along one facet dimension (`quos`), unioned over every table
# in `datas` that carries all of the dimension's variables -- the plot data and
# any layer's own data, as ggplot2 does -- so a level that only a layer has
# still gets its panel. Returns the levels and, per level, each quosure's own
# value (`parts`), which `.layer_panel_idx()` matches a layer on when the layer
# carries only some of the facet variables.
.facet_dim <- function(quos, datas) {
  if (!length(quos)) {
    return(list(levels = "", parts = list(character(0))))
  }
  vars <- unique(unlist(lapply(quos, .quo_vars)))
  datas <- Filter(function(d) all(vars %in% names(d)), datas)
  vals <- lapply(seq_along(quos), function(k) {
    .facet_pool(lapply(datas, function(d) rlang::eval_tidy(quos[[k]], d)))
  })
  fkey <- .facet_key_vals(vals)
  levs <- levels(fkey)
  first <- match(levs, as.character(fkey))
  list(
    levels = levs,
    parts = lapply(first, function(i) {
      vapply(vals, function(v) .facet_chr(v[i]), character(1))
    })
  )
}

# Facet key(s) for `data`: the single wrap key, or the row and column keys for a
# grid (an empty-string factor for an unfaceted dimension). The one place that
# builds panel-membership keys from a facet + data, shared by `.facet_assign`
# (enumerate levels, slice each panel) and `.layer_panel_idx` (match a panel's
# stored level). Returns factors; `.layer_panel_idx` compares them as characters.
.facet_keys <- function(facet, data) {
  empty <- factor(rep("", nrow(data)))
  if (facet@type == "wrap") {
    list(wrap = .facet_key(facet@cols, data))
  } else {
    list(
      r = if (length(facet@rows)) .facet_key(facet@rows, data) else empty,
      c = if (length(facet@cols)) .facet_key(facet@cols, data) else empty
    )
  }
}

# Assign every data row to a panel and lay panels out on a grid. Returns the grid
# dimensions, the per-panel row indices and grid position, and strip labels.
.facet_assign <- function(spec) {
  data <- spec@data
  facet <- spec@facet
  if (is.null(facet)) {
    return(list(
      type = "none",
      R = 1L,
      C = 1L,
      panels = list(list(
        r = 1L,
        c = 1L,
        idx = seq_len(nrow(data)),
        lvl = NULL
      )),
      col_labels = NULL,
      row_labels = NULL,
      wrap_labels = NULL
    ))
  }

  # Levels come from the plot data and every layer's own data; rows of the
  # plot data are assigned here, a layer's own rows in `.layer_panel_idx()`.
  datas <- c(
    list(data),
    Filter(Negate(is.null), lapply(spec@layers, function(l) l@data))
  )
  if (facet@type == "wrap") {
    key <- as.character(.facet_keys(facet, data)$wrap)
    dim <- .facet_dim(facet@cols, datas)
    levs <- dim$levels
    n <- length(levs)
    ncol <- facet@ncol %||%
      (if (!is.null(facet@nrow)) ceiling(n / facet@nrow) else ceiling(sqrt(n)))
    ncol <- max(1L, as.integer(ncol))
    nrow <- as.integer(ceiling(n / ncol))
    panels <- lapply(seq_len(n), function(i) {
      list(
        r = (i - 1L) %/% ncol + 1L,
        c = (i - 1L) %% ncol + 1L,
        idx = which(key == levs[i]),
        lvl = list(wrap = levs[i]),
        match = .panel_match(facet@cols, dim$parts[[i]])
      )
    })
    list(
      type = "wrap",
      R = nrow,
      C = ncol,
      panels = panels,
      col_labels = NULL,
      row_labels = NULL,
      wrap_labels = .strip_labels(facet@labeller, facet@cols, dim$parts, levs)
    )
  } else {
    keys <- .facet_keys(facet, data)
    rkey <- as.character(keys$r)
    ckey <- as.character(keys$c)
    rdim <- .facet_dim(facet@rows, datas)
    cdim <- .facet_dim(facet@cols, datas)
    rlevs <- rdim$levels
    clevs <- cdim$levels
    R <- length(rlevs)
    C <- length(clevs)
    panels <- vector("list", R * C)
    k <- 0L
    for (r in seq_len(R)) {
      for (cc in seq_len(C)) {
        k <- k + 1L
        panels[[k]] <- list(
          r = r,
          c = cc,
          idx = which(rkey == rlevs[r] & ckey == clevs[cc]),
          lvl = list(r = rlevs[r], c = clevs[cc]),
          match = c(
            .panel_match(facet@rows, rdim$parts[[r]]),
            .panel_match(facet@cols, cdim$parts[[cc]])
          )
        )
      }
    }
    list(
      type = "grid",
      R = R,
      C = C,
      panels = panels,
      col_labels = if (length(facet@cols)) {
        .strip_labels(facet@labeller, facet@cols, cdim$parts, clevs)
      },
      row_labels = if (length(facet@rows)) {
        .strip_labels(facet@labeller, facet@rows, rdim$parts, rlevs)
      },
      wrap_labels = NULL
    )
  }
}

# A panel's membership test, one entry per facet quosure: the quosure and the
# value (as `.facet_chr()` labels it) a row needs to belong to the panel.
.panel_match <- function(quos, parts) {
  lapply(seq_along(quos), function(k) list(quo = quos[[k]], value = parts[[k]]))
}

# Row indices of `data` belonging to `panel`, for a layer with its own data.
# The layer is split on whichever facet variables it carries, as in ggplot2: a
# row must match the panel on each facet quosure whose variables are all in
# `data`, and a quosure the data lacks does not constrain it. So in
# `facet_grid(am ~ cyl)` a layer with only `am` draws in its `am` row, repeated
# across the `cyl` columns, and a layer with no facet variable draws in every
# panel. Guarding on the variables being present (not a catch-all `tryCatch`)
# lets a real evaluation error propagate.
.layer_panel_idx <- function(facet, data, panel) {
  if (is.null(facet) || is.null(panel$lvl)) {
    return(seq_len(nrow(data)))
  }
  keep <- rep(TRUE, nrow(data))
  for (m in panel$match %||% .panel_match_from_lvl(facet, panel$lvl)) {
    if (all(.quo_vars(m$quo) %in% names(data))) {
      keep <- keep & .facet_chr(rlang::eval_tidy(m$quo, data)) == m$value
    }
  }
  which(keep)
}

# The membership test for a panel given only its level labels (no `match`
# recorded): a single-quosure dimension's label is its value.
.panel_match_from_lvl <- function(facet, lvl) {
  one <- function(quos, label) {
    if (length(quos) == 1L) .panel_match(quos, label) else list()
  }
  if (facet@type == "wrap") {
    one(facet@cols, lvl$wrap)
  } else {
    c(one(facet@rows, lvl$r), one(facet@cols, lvl$c))
  }
}

# Resolve every layer for one panel. Main-data layers use the panel's precomputed
# row indices; a layer with its own `data` is subset by the facet variable(s)
# found in that data (or drawn whole when it lacks them).
.resolve_panel <- function(spec, panel) {
  # group_by/fields point selections -> per-row hover_group (see `.resolve_on`).
  fsel <- Filter(
    function(s) S7::S7_inherits(s, SelectionSpec) && length(s@fields) > 0L,
    spec@selections
  )
  lapply(seq_along(spec@layers), function(i) {
    layer <- spec@layers[[i]]
    if (is.null(layer@data)) {
      idx <- panel$idx
      d <- spec@data[idx, , drop = FALSE]
    } else {
      idx <- .layer_panel_idx(spec@facet, layer@data, panel)
      d <- layer@data[idx, , drop = FALSE]
    }
    # Rows missing a continuous position are removed before the stat, so they
    # are neither drawn nor counted as drawn by provenance. `.build_panels()`
    # sums the per-panel counts into one warning per layer.
    miss <- .missing_position_rows(layer, d)
    if (any(miss)) {
      d <- d[!miss, , drop = FALSE]
      idx <- idx[!miss]
    }
    res <- .apply_position(.apply_stat(.resolve_layer(layer, d)))
    res$selgroup <- .realign_row_value(
      .selection_group_values(fsel, d),
      res$stat_map
    )
    res$source <- .layer_source(res, layer, i, idx)
    res$n_missing <- sum(miss)
    res
  })
}

# Marks that draw one element per row and simply leave out a row whose
# position is missing. Their missing rows are removed (with a warning) rather
# than reaching the emitter at NaN. Paths (line, step, area, ribbon) keep theirs:
# an interior NA breaks the path into a gap, as in ggplot2. Layout and graph
# marks compute their own positions.
.NA_REMOVE_MARKS <- c(
  "point",
  "bar",
  "text",
  "label",
  "tile",
  "rect",
  "segment",
  "errorbar",
  "linerange",
  "pointrange",
  "crossbar",
  "rug",
  "boxplot",
  "violin",
  "ridgeline",
  "halfeye",
  "interval",
  "hex",
  "smooth"
)

# Is `v` a continuous position value (numeric, date or date-time)? A discrete
# one keeps its NA, which the trained scale draws as a trailing "NA" level.
.is_continuous_position <- function(v) {
  is.numeric(v) || inherits(v, c("Date", "POSIXt"))
}

# Logical per row of `data`: is a continuous position channel (`x`, `y`, `xend`,
# `ymin`, ...; see `.DIAG_POS_CHANNELS`) of `layer` missing? Only per-row data
# channels count: a constant, or an `after_stat()` channel, describes no row.
.missing_position_rows <- function(layer, data) {
  n <- nrow(data)
  miss <- rep(FALSE, n)
  if (!n || !layer@mark %in% .NA_REMOVE_MARKS) {
    return(miss)
  }
  chans <- intersect(
    unlist(.DIAG_POS_CHANNELS, use.names = FALSE),
    names(layer@encoding)
  )
  for (nm in chans) {
    ch <- layer@encoding[[nm]]
    if (isTRUE(ch@after)) {
      next
    }
    v <- rlang::eval_tidy(ch@expr, data = data)
    if (length(v) == n && .is_continuous_position(v)) {
      miss <- miss | is.na(v)
    }
  }
  miss
}

# One warning per layer for the rows `.resolve_panel()` removed, summed over
# every panel, in ggplot2's wording.
.warn_missing_positions <- function(spec, panels) {
  for (i in seq_along(spec@layers)) {
    n <- sum(vapply(
      panels,
      function(p) p$resolved[[i]]$n_missing %||% 0L,
      integer(1)
    ))
    if (n > 0L) {
      mark <- spec@layers[[i]]@mark
      cli::cli_warn(
        "Removed {n} row{?s} containing missing values (layer {i}, {.fn mark_{mark}}).",
        class = "vellumplot_missing_rows"
      )
    }
  }
  invisible()
}

# Which table a resolved layer's rows come from, and where they sit in it -- the
# provenance row-key mapping (see `R/provenance.R`). `idx` is the panel subset's
# row indices into that table (the plot data, or the layer's own `data =`).
# Emitters refine provenance with indices *into the resolved layer*; `per_row`
# says whether those map one-to-one onto `idx` (the layer kept its input rows, in
# order: identity stat and one element per row) or not (a stat aggregated them,
# or a layout mark made its own rows), in which case every element keys to the
# whole panel subset. A row-preserving stat (dot plot, outlier callout, window,
# ...) reorders or filters the rows but records which input row(s) each output
# row stands for (`L$stat_map$members`); `members` carries that through, so each
# element still names its own source rows.
.layer_source <- function(L, layer, i, idx) {
  members <- L$stat_map$members
  if (!is.null(members) && (.is_layout_layer(L) || length(members) != L$n)) {
    members <- NULL
  }
  list(
    name = if (is.null(layer@data)) {
      "plot"
    } else {
      paste0("layer:", attr(layer, "vp_spec_index") %||% i)
    },
    rows = as.integer(idx),
    members = members,
    per_row = !is.null(members) ||
      (!isTRUE(L$aggregated) &&
        !.is_layout_layer(L) &&
        isTRUE(L$n == length(idx)))
  )
}

# A layout mark (sankey, chord, hierarchy, venn, waffle) computes its own
# elements -- nodes, sectors, cells -- from the data, so its rows never map
# one-to-one onto the input rows, even when the counts happen to agree.
.is_layout_layer <- function(L) {
  !is.null(L$sankey) ||
    !is.null(L$chord) ||
    !is.null(L$hierarchy) ||
    L$mark %in% c("venn", "waffle")
}

# Build the panels with their resolved layers (per-panel data subset), then
# train scales honouring the resolve lattice: colour/size always shared;
# position shared by default, or independent per panel (wrap) / per column-row
# (grid) when free.
.build_panels <- function(spec, frozen_scales = NULL) {
  fa <- .facet_assign(spec)
  panels <- lapply(fa$panels, function(p) {
    p$resolved <- .resolve_panel(spec, p)
    p
  })
  .warn_missing_positions(spec, panels)

  all_res <- unlist(lapply(panels, function(p) p$resolved), recursive = FALSE)
  has_bar <- .needs_zero(all_res)
  # Every axis/legend title, resolved once (scale name > labs > mapping), and
  # read by the trainers, the alt text and the lint summary. See .plot_titles().
  titles <- .plot_titles(spec, all_res)

  free_x <- .resolve_for(spec, "x") == "independent"
  free_y <- .resolve_for(spec, "y") == "independent"

  # Shared scales (x, y, colour, size) trained on the pooled data. Colour/size
  # are always taken from here; x/y are used unless the aesthetic is free.
  # For a frozen-scale compile (a keyframe of an animation), the scales were
  # trained once over every state and are injected here instead of retrained, and
  # every aesthetic is shared (a frozen animation never has free/independent
  # scales) so axis breaks don't move between frames -- the non-reactive contract.
  if (is.null(frozen_scales)) {
    shared <- .train_scales(spec, all_res, titles = titles)
  } else {
    shared <- frozen_scales
    free_x <- FALSE
    free_y <- FALSE
  }
  shared_x <- shared$x
  shared_y <- shared$y

  # Group resolved layers for free training: per panel (wrap) or per column
  # (free x, grid) / per row (free y, grid).
  group_res <- function(sel) {
    unlist(lapply(panels[sel], function(p) p$resolved), recursive = FALSE)
  }
  co <- .coord_of(spec)
  train_free <- function(aes, intercept, title, inc0, lim) {
    function(res) {
      .train_axis(
        spec,
        res,
        aes,
        title,
        include_zero = inc0,
        lim = lim,
        values = .axis_pool(res, aes, intercept)
      )
    }
  }
  tx <- train_free("x", "xintercept", titles$title[["x"]], FALSE, co@xlim)
  ty <- train_free("y", "yintercept", titles$title[["y"]], has_bar, co@ylim)

  # A grid free scale is shared down a column (x) / across a row (y), so train it
  # once per column/row and reuse it rather than retraining for every panel.
  col_x <- list()
  row_y <- list()
  for (i in seq_along(panels)) {
    p <- panels[[i]]
    if (!free_x) {
      p$x_sc <- shared_x
    } else if (fa$type == "grid") {
      key <- as.character(p$c)
      if (is.null(col_x[[key]])) {
        col_x[[key]] <- tx(group_res(vapply(
          panels,
          function(q) q$c == p$c,
          logical(1)
        )))
      }
      p$x_sc <- col_x[[key]]
    } else {
      p$x_sc <- tx(p$resolved)
    }
    if (!free_y) {
      p$y_sc <- shared_y
    } else if (fa$type == "grid") {
      key <- as.character(p$r)
      if (is.null(row_y[[key]])) {
        row_y[[key]] <- ty(group_res(vapply(
          panels,
          function(q) q$r == p$r,
          logical(1)
        )))
      }
      p$y_sc <- row_y[[key]]
    } else {
      p$y_sc <- ty(p$resolved)
    }
    panels[[i]] <- p
  }

  list(
    fa = fa,
    panels = panels,
    scales = shared,
    titles = titles,
    free_x = free_x,
    free_y = free_y
  )
}
