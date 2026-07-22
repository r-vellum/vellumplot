#' @include classes.R facet.R coord.R compile-resolve.R compile-train.R
NULL

# A factor key from one or more faceting quosures, preserving factor level order
# for a single factor and ordering by each variable's natural type otherwise
# (numeric numerically, factor by level, character lexicographically) -- so a
# numeric facet reads 1, 2, 10 rather than the lexicographic 1, 10, 2.
.facet_key <- function(quos, data) {
  vals <- lapply(quos, function(q) rlang::eval_tidy(q, data))
  if (length(vals) == 1 && is.factor(vals[[1]])) {
    return(droplevels(vals[[1]]))
  }
  parts <- lapply(vals, as.character)
  combined <- do.call(paste, c(parts, sep = ", "))
  # Order rows by each variable in turn (factors by level code, everything else
  # by its own type), then take the level order from that ordering.
  ord <- do.call(
    order,
    lapply(vals, function(v) if (is.factor(v)) as.integer(v) else v)
  )
  factor(combined, levels = unique(combined[ord]))
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

  if (facet@type == "wrap") {
    key <- .facet_keys(facet, data)$wrap
    levs <- levels(key)
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
        lvl = list(wrap = levs[i])
      )
    })
    list(
      type = "wrap",
      R = nrow,
      C = ncol,
      panels = panels,
      col_labels = NULL,
      row_labels = NULL,
      wrap_labels = levs
    )
  } else {
    keys <- .facet_keys(facet, data)
    rkey <- keys$r
    ckey <- keys$c
    rlevs <- levels(rkey)
    clevs <- levels(ckey)
    R <- length(rlevs)
    C <- length(clevs)
    panels <- list()
    for (r in seq_len(R)) {
      for (cc in seq_len(C)) {
        panels <- c(
          panels,
          list(list(
            r = r,
            c = cc,
            idx = which(rkey == rlevs[r] & ckey == clevs[cc]),
            lvl = list(r = rlevs[r], c = clevs[cc])
          ))
        )
      }
    }
    list(
      type = "grid",
      R = R,
      C = C,
      panels = panels,
      col_labels = if (length(facet@cols)) clevs else NULL,
      row_labels = if (length(facet@rows)) rlevs else NULL,
      wrap_labels = NULL
    )
  }
}

# Row indices of `data` belonging to `panel`, by matching the facet variable(s)
# evaluated on `data` against the panel's level. When `data` lacks the facet
# variable(s) (e.g. an annotation layer's own data), every row is returned so the
# layer draws on every panel.
.layer_panel_idx <- function(facet, data, panel) {
  if (is.null(facet) || is.null(panel$lvl)) {
    return(seq_len(nrow(data)))
  }
  # Fall back to "every panel" only when this data genuinely lacks the facet
  # variable(s) (an annotation layer's own data). A catch-all `tryCatch` here
  # would also swallow a real evaluation bug into a silently-wrong plot, so guard
  # on the specific condition and let any other error propagate.
  vars <- unique(unlist(lapply(
    c(facet@cols, facet@rows),
    function(q) all.vars(rlang::quo_get_expr(q))
  )))
  if (!all(vars %in% names(data))) {
    return(seq_len(nrow(data)))
  }
  keys <- .facet_keys(facet, data)
  if (facet@type == "wrap") {
    which(as.character(keys$wrap) == panel$lvl$wrap)
  } else {
    which(
      as.character(keys$r) == panel$lvl$r &
        as.character(keys$c) == panel$lvl$c
    )
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
  lapply(spec@layers, function(layer) {
    if (is.null(layer@data)) {
      d <- spec@data[panel$idx, , drop = FALSE]
    } else {
      idx <- .layer_panel_idx(spec@facet, layer@data, panel)
      d <- layer@data[idx, , drop = FALSE]
    }
    res <- .apply_position(.apply_stat(.resolve_layer(layer, d)))
    res$selgroup <- .selection_group_values(fsel, d)
    res
  })
}

# Build the panels with their resolved layers (per-panel data subset), then
# train scales honouring the resolve lattice: colour/size always shared;
# position shared by default, or independent per panel (wrap) / per column-row
# (grid) when free.
.build_panels <- function(spec) {
  fa <- .facet_assign(spec)
  panels <- lapply(fa$panels, function(p) {
    p$resolved <- .resolve_panel(spec, p)
    p
  })

  all_res <- unlist(lapply(panels, function(p) p$resolved), recursive = FALSE)
  has_bar <- .needs_zero(all_res)
  y_title <- .y_axis_title(spec, all_res)

  free_x <- .resolve_for(spec, "x") == "independent"
  free_y <- .resolve_for(spec, "y") == "independent"

  # Shared scales (x, y, colour, size) trained on the pooled data. Colour/size
  # are always taken from here; x/y are used unless the aesthetic is free.
  shared <- .train_scales(spec, all_res)
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
      .train_position(
        aes,
        .axis_pool(res, aes, intercept),
        .scale_for(spec, aes),
        title,
        include_zero = inc0,
        lim = lim
      )
    }
  }
  tx <- train_free("x", "xintercept", .default_title(spec, "x"), FALSE, co@xlim)
  ty <- train_free("y", "yintercept", y_title, has_bar, co@ylim)

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
    free_x = free_x,
    free_y = free_y
  )
}
