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
#
# The level order is that of the rows sorted by each variable in turn, but the
# sort and the labels are worked out once per distinct combination of values
# (VP-69): each combination's first row stands for it, the representatives are
# sorted (`order()` breaks ties by position, i.e. by first row, as it would
# for all rows), and every row takes its combination's level.
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
  combo <- .combo_id(vals)
  first <- which(!duplicated(combo))
  rep_vals <- lapply(vals, function(v) v[first])
  label <- do.call(paste, c(lapply(rep_vals, .facet_chr), sep = ", "))
  if (!na_level) {
    label[Reduce(`|`, lapply(rep_vals, is.na))] <- NA_character_
  }
  # Order the combinations by each variable in turn (factors by level code,
  # everything else by its own type; `order()` puts NA last), then take the
  # level order from it.
  ord <- do.call(
    order,
    lapply(rep_vals, function(v) if (is.factor(v)) as.integer(v) else v)
  )
  lv <- unique(label[ord])
  lv <- lv[!is.na(lv)]
  codes <- match(label, lv)[combo]
  structure(codes, levels = lv, class = "factor")
}

# One integer id per distinct combination of values across the vectors in
# `vals` (all of one length), numbered in order of first appearance.
.combo_id <- function(vals) {
  id <- NULL
  for (v in vals) {
    code <- if (is.factor(v)) as.integer(v) else match(v, unique(v))
    code[is.na(code)] <- 0L
    id <- if (is.null(id)) {
      code
    } else {
      # Pair the ids so far with this variable's codes; doubles hold the
      # product exactly, and renumbering keeps the ids small.
      pair <- as.numeric(id) * (max(code, 0L) + 1) + code
      match(pair, unique(pair))
    }
  }
  match(id, unique(id))
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
  first <- match(seq_along(levs), as.integer(fkey))
  list(
    levels = levs,
    parts = lapply(first, function(i) {
      vapply(vals, function(v) .facet_chr(v[i]), character(1))
    })
  )
}

# The rows of `key` (a factor) at each of `levs`, as `which(as.character(key)
# == levs[i])` for every `i`, in one pass.
.rows_by_level <- function(key, levs) {
  code <- match(levels(key), levs)[as.integer(key)]
  .split_rows(code, length(levs))
}

# Row indices grouped by `code` (integers 1..k, NA for none): element `i` holds
# `which(code == i)`.
.split_rows <- function(code, k) {
  f <- structure(code, levels = as.character(seq_len(k)), class = "factor")
  unname(split(seq_along(code), f))
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
    Filter(Negate(is.null), lapply(spec@layers, function(l) l@data)),
    # An animation keyframe lays its panels out from the whole data, so every
    # frame has every panel whichever rows its state holds (VP-84).
    .mark_ctx$anim_facet_datas
  )
  if (facet@type == "wrap") {
    key <- .facet_keys(facet, data)$wrap
    dim <- .facet_dim(facet@cols, datas)
    levs <- dim$levels
    n <- length(levs)
    ncol <- facet@ncol %||%
      (if (!is.null(facet@nrow)) ceiling(n / facet@nrow) else ceiling(sqrt(n)))
    ncol <- max(1L, as.integer(ncol))
    nrow <- as.integer(ceiling(n / ncol))
    rows <- .rows_by_level(key, levs)
    panels <- lapply(seq_len(n), function(i) {
      list(
        r = (i - 1L) %/% ncol + 1L,
        c = (i - 1L) %% ncol + 1L,
        idx = rows[[i]],
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
    rdim <- .facet_dim(facet@rows, datas)
    cdim <- .facet_dim(facet@cols, datas)
    rlevs <- rdim$levels
    clevs <- cdim$levels
    R <- length(rlevs)
    C <- length(clevs)
    # Each row's cell, numbered row-major like the panels.
    rcode <- match(levels(keys$r), rlevs)[as.integer(keys$r)]
    ccode <- match(levels(keys$c), clevs)[as.integer(keys$c)]
    rows <- .split_rows((rcode - 1L) * C + ccode, R * C)
    panels <- vector("list", R * C)
    k <- 0L
    for (r in seq_len(R)) {
      for (cc in seq_len(C)) {
        k <- k + 1L
        panels[[k]] <- list(
          r = r,
          c = cc,
          idx = rows[[k]],
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

# Graph marks drawn from the edge table: in a faceted graph each panel keeps
# only these edges' rows whose two endpoints are both among the panel's nodes.
# A flow map's per-edge weights follow its kept rows (`.flow_map_weights()`).
.GRAPH_EDGE_MARKS <- c("edges", "edge_text", "edge_bundle", "flow_map")

# A faceted graph splits its nodes (the plot data) by a vertex attribute, which
# the edge table lacks, so an edge layer would otherwise draw in full in every
# panel, with edges whose endpoints are not there. Keep the edges whose two
# endpoints are both among the panel's nodes, as ggraph's `facet_nodes()` does
# (VP-64). Endpoints are matched by vertex name: the edge table's `from`/`to`
# and the node table's `name` come from the same igraph vertex ids.
.graph_panel_edges <- function(spec, layer, panel, idx) {
  d <- layer@data
  nm <- spec@data$name
  if (
    is.null(spec@facet) ||
      is.null(panel$lvl) ||
      is.null(spec@edge_data) ||
      !layer@mark %in% .GRAPH_EDGE_MARKS ||
      is.null(nm) ||
      is.null(d$from) ||
      is.null(d$to)
  ) {
    return(idx)
  }
  nodes <- as.character(nm[panel$idx])
  idx[
    as.character(d$from[idx]) %in% nodes & as.character(d$to[idx]) %in% nodes
  ]
}

# A graph edge's key ends in its row index. A panel that keeps only some edges
# (see `.graph_panel_edges()`) numbers them by their row in the whole edge
# table, so an edge's key, and its `join`, is the same in every panel and in
# the unfaceted plot.
.edge_keys_by_row <- function(gi, idx) {
  if (
    !isTRUE(gi$kind %in% c("edges", "flow")) || length(gi$source) != length(idx)
  ) {
    return(gi)
  }
  gi$key <- paste(gi$source, gi$target, idx, sep = "\001")
  gi
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
      idx <- .graph_panel_edges(spec, layer, panel, idx)
      d <- layer@data[idx, , drop = FALSE]
    }
    # Rows missing a continuous position are removed before the stat, so they
    # are neither drawn nor counted as drawn by provenance. `.build_panels()`
    # sums the per-panel counts into one warning per layer.
    miss <- .missing_position_rows(layer, d)
    # So are rows a position scale's `oob = "censor"` puts outside its limits
    # (before the stat, as in ggplot2), counted into the same warning.
    cens <- !miss & .censored_position_rows(spec, layer, d)
    if (any(miss | cens)) {
      d <- d[!(miss | cens), , drop = FALSE]
      idx <- idx[!(miss | cens)]
    }
    res <- .apply_position(.apply_stat(.resolve_layer(layer, d)))
    res$graph_identity <- .edge_keys_by_row(res$graph_identity, idx)
    res$selgroup <- .realign_row_value(
      .selection_group_values(fsel, d),
      res$stat_map
    )
    res$source <- .layer_source(res, layer, i, idx)
    res$n_missing <- sum(miss)
    res$n_censored <- sum(cens)
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

# One warning per layer for the rows not drawn, summed over every panel, in
# ggplot2's wording: the rows `.resolve_panel()` removed (a missing position, or
# one a position scale's `oob` censors) and, given the trained `shared` scales,
# the rows whose size is missing (`.missing_size_rows()`). A censored row makes
# the wording name values outside the scale range, as ggplot2's does.
.warn_missing_positions <- function(spec, panels, shared = NULL) {
  for (i in seq_along(spec@layers)) {
    count <- function(field) {
      sum(vapply(
        panels,
        function(p) as.integer(p$resolved[[i]][[field]] %||% 0L),
        integer(1)
      ))
    }
    n_cens <- count("n_censored")
    n_miss <- count("n_missing")
    if (!is.null(shared)) {
      for (p in panels) {
        sz <- .panel_size_drops(p, i, shared)
        n_miss <- n_miss + sum(sz$missing)
        n_cens <- n_cens + sum(sz$censored)
      }
    }
    .warn_removed_rows(n_miss + n_cens, n_cens, i, spec@layers[[i]]@mark)
  }
  invisible()
}

# The "Removed n rows" warning for layer `i` (mark `mark`): `n` rows not drawn,
# `n_cens` of them censored. The counts ride on the condition (`n_rows`,
# `n_censored`, `layer`, `mark`) so an animation can collect the warnings of its
# keyframes and give one per layer instead (`.removed_rows_tally()`, VP-150).
.warn_removed_rows <- function(n, n_cens, i, mark) {
  if (n <= 0L) {
    return(invisible())
  }
  what <- if (n_cens > 0L) {
    "missing values or values outside the scale range"
  } else {
    "missing values"
  }
  cli::cli_warn(
    "Removed {n} row{?s} containing {what} (layer {i}, {.fn mark_{mark}}).",
    class = "vellumplot_missing_rows",
    n_rows = as.integer(n),
    n_censored = as.integer(n_cens),
    layer = as.integer(i),
    mark = mark
  )
}

# The marks that draw nothing for a row whose size is missing: the point-like
# marks, the text marks (a font size), and an sf layer's point features.
.SIZE_DROP_MARKS <- c(
  "point",
  "nodes",
  "pointrange",
  "text",
  "label",
  "node_text",
  "edge_text",
  "sf_label",
  "text_path",
  "sf"
)

# `.missing_size_rows()` for layer `i` of built panel `p`, against the size
# scale the compile maps it with: the panel's own (an independent size scale)
# or the shared one. The one tally of size drops, used by the compile warning
# and by `spec_diagnose()`'s report (VP-149), so the two count the same rows.
.panel_size_drops <- function(p, i, shared) {
  .missing_size_rows(p$resolved[[i]], p$aes_sc$size %||% shared$size)
}

# Per row of resolved layer `L`, is its size missing, so that its emitter draws
# nothing for it (VR-38)? `missing`: the value is `NA` (or a constant
# `size = NA`); `censored`: a value the size scale `sc` maps to `NA`, i.e. one
# its `oob = "censor"` puts outside the limits. Mirrors `.aes_size()`.
.missing_size_rows <- function(L, sc) {
  none <- list(missing = logical(0), censored = logical(0))
  n <- L$n
  if (!isTRUE(n > 0L) || !isTRUE(L$mark %in% .SIZE_DROP_MARKS)) {
    return(none)
  }
  v <- L$values$size
  if (!is.null(sc) && !is.null(v)) {
    if (!length(v) %in% c(1L, n)) {
      return(none)
    }
    v <- rep_len(v, n)
    mapped <- rep_len(sc$map(v), n)
  } else {
    v <- L$params$size
    if (is.null(v) || !(is.numeric(v) || all(is.na(v)))) {
      return(none)
    }
    v <- rep_len(v, n)
    mapped <- as.numeric(v)
  }
  gone <- !is.finite(mapped)
  if (identical(L$mark, "sf")) {
    has_point <- vapply(
      L$sf %||% vector("list", n),
      function(prims) {
        any(vapply(prims, function(p) identical(p$kind, "point"), logical(1)))
      },
      logical(1)
    )
    gone <- gone & rep_len(has_point, n)
  }
  list(missing = gone & is.na(v), censored = gone & !is.na(v))
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
# train scales honouring the resolve lattice: every scale is shared by default;
# position is independent per panel (wrap) / per column-row (grid) when free,
# and a non-position scale set to "independent" (`resolve_scale(color = )`) is
# trained per panel, into `p$aes_sc` (F-8).
.build_panels <- function(spec, frozen_scales = NULL) {
  spec <- .settle_constants(spec)
  # Mark labels measured while resolving (a chord's label room) take this
  # plot's theme font (VP-135), whoever builds the panels: the draw, a
  # composition plan, an animation's frozen scales. Put the caller's back.
  old_font <- .mark_ctx$text_font
  on.exit(assign("text_font", old_font, envir = .mark_ctx), add = TRUE)
  .set_text_font(.theme_of(spec))
  fa <- .facet_assign(spec)
  # The panel grid, for the date-break label fit (`.axis_label_fit()`), which
  # shares the plot's width among the facet columns.
  old_grid <- .mark_ctx$panel_grid
  .mark_ctx$panel_grid <- c(R = fa$R %||% 1L, C = fa$C %||% 1L)
  on.exit(.mark_ctx$panel_grid <- old_grid, add = TRUE)
  panels <- lapply(fa$panels, function(p) {
    p$resolved <- .resolve_panel(spec, p)
    p
  })
  panels <- .share_dodge_n(panels)

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
  # the date-break fit of a free axis estimates its panel from the shared
  # legends and vertical axis (VP-101)
  fit_ctx <- function() {
    .axis_fit_ctx(spec, shared, titles, identical(co@kind, "flip"))
  }
  train_free <- function(aes, intercept, title, inc0, lim) {
    function(res) {
      .train_axis(
        spec,
        res,
        aes,
        title,
        include_zero = inc0,
        lim = lim,
        values = .axis_pool(res, aes, intercept),
        fit_ctx = fit_ctx
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

  # Independent non-position scales: each panel trains its own from its own
  # layers. Only with several panels, and never for a frozen (animation)
  # compile, whose frames must share every scale.
  independent <- if (is.null(frozen_scales) && length(panels) > 1L) {
    .independent_aes(spec)
  } else {
    character(0)
  }
  if (length(independent)) {
    for (i in seq_along(panels)) {
      panels[[i]]$aes_sc <- .train_panel_aes(
        spec,
        panels[[i]]$resolved,
        independent,
        titles
      )
    }
  }

  # One "Removed n rows" warning per layer, for the rows removed before the
  # stat (missing or censored positions) and those drawn without a mark
  # because their size is missing, which needs the trained scales (VR-38).
  .warn_missing_positions(spec, panels, shared)

  list(
    fa = fa,
    panels = panels,
    scales = shared,
    titles = titles,
    free_x = free_x,
    free_y = free_y,
    independent = independent
  )
}

# The non-position aesthetics (registry scale names, e.g. "color", "size")
# whose scale the spec resolves as independent across panels.
.independent_aes <- function(spec) {
  keys <- .aes_legend_keys()
  keys[vapply(
    keys,
    function(k) identical(.resolve_for(spec, k), "independent"),
    logical(1)
  )]
}

# Train the `keys` scales on one panel's resolved layers, as `.train_scales()`
# does for the pooled data (the registry trainer, then any guides() override).
# A scale the panel gives no value to stays out of the result, so the panel
# falls back to the shared scale (and shows no legend of its own for it).
.train_panel_aes <- function(spec, resolved, keys, titles) {
  out <- list()
  for (k in keys) {
    e <- .AES_REGISTRY[[k]]
    sc <- .aes_trainer(e)(spec, resolved, titles = titles)
    if (is.null(sc)) {
      next
    }
    ss <- .scale_for(spec, k)
    if (!is.null(ss) && !is.null(ss@guide)) {
      sc <- .apply_guide(sc, ss@guide)
    }
    out[[k]] <- sc
  }
  out
}

# The scale a panel's marks use for non-position aesthetic `key`: its own when
# the aesthetic is independent (see `.train_panel_aes()`), else the shared one.
.panel_scale <- function(built, panel, key) {
  panel$aes_sc[[key]] %||% built$scales[[key]]
}

# The strip text of panel `i` on one line, for titling its own legends: the
# wrap strip, or a grid's column and row strips joined by ", ".
.panel_label <- function(fa, i) {
  panel <- fa$panels[[i]]
  lab <- switch(
    fa$type,
    wrap = .label_text(fa$wrap_labels[[i]]),
    grid = paste(
      c(
        if (!is.null(fa$col_labels)) .label_text(fa$col_labels[[panel$c]]),
        if (!is.null(fa$row_labels)) .label_text(fa$row_labels[[panel$r]])
      ),
      collapse = ", "
    ),
    ""
  )
  gsub("\n", " ", lab %||% "")
}

.is_plain_title <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

# The legend guides of a faceted plot: the shared scales' guides (as
# `.legend_guides()` builds them), then each panel's guides for the
# aesthetics resolved as independent. A panel's own legend is drawn beside its
# panel by default (`position = "panel"`, tagged with the panel's index in
# `$panel`, see `.panel_legend_guides()`); one whose guide asks for another
# position (`guide_legend(position = "right")`) is collected with the shared
# legends, titled with the panel's strip text -- "title (panel)", or the panel
# alone for an untitled scale. Guides merge within one panel (a colour and a
# size legend of one variable), never across panels. With no independent
# aesthetic this is exactly `.legend_guides(built$scales)`. `collect = TRUE`
# moves every per-panel legend into the plot's legend (where the theme puts
# it), as if its guide had asked for that position: the fallback when legends
# beside each panel would leave the panels too narrow (VP-59).
.facet_legend_guides <- function(built, collect = FALSE) {
  indep <- built$independent %||% character(0)
  if (collect) {
    unpanel <- function(sc) {
      if (identical(sc$guide_position, "panel")) {
        sc$guide_position <- NULL
      }
      sc
    }
    built$scales <- lapply(built$scales, function(sc) {
      if (is.null(sc)) sc else unpanel(sc)
    })
  }
  if (!length(indep)) {
    return(.legend_guides(built$scales))
  }
  shared <- built$scales
  shared[indep] <- list(NULL)
  out <- .legend_guides(shared)
  for (i in seq_along(built$panels)) {
    p <- built$panels[[i]]
    own <- p$aes_sc[intersect(indep, names(p$aes_sc))]
    # a panel with no values of a discrete aesthetic has no keys to show
    own <- Filter(
      function(sc) !identical(sc$type, "discrete") || length(sc$levels) > 0L,
      own
    )
    if (!length(own)) {
      next
    }
    label <- .panel_label(built$fa, i)
    own <- lapply(own, function(sc) {
      pos <- sc$guide_position %||% "panel"
      if (collect && identical(pos, "panel")) {
        sc$guide_position <- NULL
        pos <- "collected"
      }
      if (identical(pos, "panel")) {
        # beside its own panel, the legend needs no panel label
        return(.scale_set(sc, guide_position = pos))
      }
      # A plain-text title gains the panel; a rich-text or empty one gives way
      # to the panel label, which is what tells the legends apart.
      sc$name <- if (.is_plain_title(sc$name)) {
        paste0(sc$name, " (", label, ")")
      } else {
        label
      }
      sc
    })
    gs <- lapply(.legend_guides(own), function(g) {
      if (identical(.guide_position(g), "panel")) {
        g$panel <- i
      }
      g
    })
    out <- c(out, gs)
  }
  out
}

# The legends drawn beside panel `i`, from the guides placed "panel": a shared
# scale's (repeated beside every panel) and the panel's own (`$panel == i`).
.panel_legend_guides <- function(guides, i) {
  Filter(function(g) is.null(g$panel) || identical(g$panel, i), guides)
}
