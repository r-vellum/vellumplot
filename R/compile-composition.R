#' @include classes.R concat.R compile-layout.R compile-guides.R seam.R
NULL

# ============================================================================
# Patchwork-style aligned composition.
#
# Instead of giving each sub-plot its own grid (the independent fallback), the
# aligned path builds ONE vellum grid_layout for the whole figure: every
# sub-plot's panel sits on a shared `null` track, and the decoration tracks
# (axis title / labels, legend) are sized to the max across the relevant
# composition row/column. Because the panel tracks are shared, panel edges line
# up across sub-plots even when their axis labels differ (patchwork's killer
# feature). Identical legends are collected into one (guides = "collect").
#
# Phase scope: single-panel, non-polar sub-plots on a regular grid. Faceted /
# polar sub-plots fall back to the independent layout (see .compile_composition).
# ============================================================================

# Can every sub-plot be placed on the shared aligned grid?
.comp_alignable <- function(comp) {
  if (!length(comp@plots)) {
    return(FALSE)
  }
  if (!is.null(comp@design) || length(comp@insets)) {
    return(FALSE)
  }
  ok <- function(p) {
    S7::S7_inherits(p, PlotSpec) &&
      length(p@layers) > 0L &&
      is.null(p@facet) &&
      !identical(.coord_of(p)@kind, "polar")
  }
  all(vapply(comp@plots, ok, logical(1)))
}

# Front half of .draw_plot(): everything needed to measure and place a plot,
# without drawing. Returns the resolved theme, built panels, guide list, coord
# flags and the per-plot layout (which carries the measured gutter sizes).
.plan_plot <- function(spec) {
  rt <- .resolve_theme(.theme_of(spec))
  built <- .build_panels(spec)
  guides <- .legend_guides(built$scales)
  co <- .coord_of(spec)
  flip <- identical(co@kind, "flip")
  lay <- .build_layout(built, guides, spec@labels, rt, flip, co)
  list(
    spec = spec,
    rt = rt,
    built = built,
    guides = guides,
    co = co,
    flip = flip,
    lay = lay
  )
}

# Max of vellum units (`max()` is unreliable on units; pairwise `>` works, even
# across mm / grobwidth kinds). Empty -> 0 mm.
.umax <- function(units) {
  units <- Filter(Negate(is.null), units)
  if (!length(units)) {
    return(vellum::vl_unit(0, "mm"))
  }
  Reduce(function(a, b) if (b > a) b else a, units)
}

# Read a measured track size off a per-plot layout by role index (NA -> 0 mm).
.lay_w <- function(lay, idx) {
  if (length(idx) && !is.na(idx[1])) {
    lay$widths[idx[1]]
  } else {
    vellum::vl_unit(0, "mm")
  }
}
.lay_h <- function(lay, idx) {
  if (length(idx) && !is.na(idx[1])) {
    lay$heights[idx[1]]
  } else {
    vellum::vl_unit(0, "mm")
  }
}

# Decompose a single-panel plan into canonical decoration bands + the panel.
# In the aligned path a sub-plot legend is normalised to a right-side band (or
# zero when guides are collected at the figure level).
.plot_bands <- function(plan, collect) {
  lay <- plan$lay
  z <- vellum::vl_unit(0, "mm")
  legw <- if (length(plan$guides) && !collect) {
    .legend_width(plan$guides, plan$rt)
  } else {
    z
  }
  list(
    L_ytitle = .lay_w(lay, lay$ytitle_col),
    L_ylabels = .lay_w(lay, lay$ylabels_col[1]),
    R_legend = legw,
    T_tag = .lay_h(lay, lay$tag_row),
    T_title = .lay_h(lay, lay$title_row),
    T_subtitle = .lay_h(lay, lay$subtitle_row),
    B_xlabels = .lay_h(lay, lay$xlabels_row[1]),
    B_xtitle = .lay_h(lay, lay$xtitle_row),
    B_caption = .lay_h(lay, lay$caption_row)
  )
}

# Recycle a relative-size vector to length n (NULL -> all 1).
.size_weights <- function(v, n) {
  if (is.null(v)) {
    return(rep(1, n))
  }
  rep_len(as.numeric(v), n)
}

# Build the shared figure grid: width/height track vectors plus, for each
# sub-plot, the global track indices its pieces draw into (`map`). Also returns
# the collected guide list and the figure-band row indices.
.composition_grid <- function(plans, comp, collect, rt0) {
  nr <- as.integer(comp@nrow)
  nc <- as.integer(comp@ncol)
  bands <- lapply(plans, .plot_bands, collect = collect)
  cell <- lapply(seq_along(plans), .comp_cell, comp = comp)
  in_col <- function(c) which(vapply(cell, function(e) e$c == c, logical(1)))
  in_row <- function(r) which(vapply(cell, function(e) e$r == r, logical(1)))

  # per-column shared left/right widths; per-row shared top/bottom heights
  Lyt <- Lyl <- Rleg <- vector("list", nc)
  for (c in seq_len(nc)) {
    ix <- in_col(c)
    Lyt[[c]] <- .umax(lapply(bands[ix], `[[`, "L_ytitle"))
    Lyl[[c]] <- .umax(lapply(bands[ix], `[[`, "L_ylabels"))
    Rleg[[c]] <- .umax(lapply(bands[ix], `[[`, "R_legend"))
  }
  Ttag <- Ttit <- Tsub <- Bxl <- Bxt <- Bcap <- vector("list", nr)
  for (r in seq_len(nr)) {
    ix <- in_row(r)
    Ttag[[r]] <- .umax(lapply(bands[ix], `[[`, "T_tag"))
    Ttit[[r]] <- .umax(lapply(bands[ix], `[[`, "T_title"))
    Tsub[[r]] <- .umax(lapply(bands[ix], `[[`, "T_subtitle"))
    Bxl[[r]] <- .umax(lapply(bands[ix], `[[`, "B_xlabels"))
    Bxt[[r]] <- .umax(lapply(bands[ix], `[[`, "B_xtitle"))
    Bcap[[r]] <- .umax(lapply(bands[ix], `[[`, "B_caption"))
  }

  gap <- vellum::vl_unit(rt0[["panel.spacing"]] %||% 5, "mm")
  wts_w <- .size_weights(comp@widths, nc)
  wts_h <- .size_weights(comp@heights, nr)

  # --- columns -------------------------------------------------------------
  W <- .tracks()
  ytitle_col <- ylabels_col <- panel_col <- rleg_col <- integer(nc)
  for (c in seq_len(nc)) {
    ytitle_col[c] <- .tk_add(W, Lyt[[c]])
    ylabels_col[c] <- .tk_add(W, Lyl[[c]])
    panel_col[c] <- .tk_add(W, vellum::vl_unit(wts_w[c], "null"))
    rleg_col[c] <- .tk_add(W, Rleg[[c]])
    if (c < nc) .tk_add(W, gap)
  }
  figguides <- if (collect) .pool_guides(plans) else list()
  figlegend_col <- NA_integer_
  if (collect && length(figguides)) {
    .tk_add(W, gap)
    figlegend_col <- .tk_add(W, .legend_width(figguides, rt0))
  }

  # --- rows ----------------------------------------------------------------
  H <- .tracks()
  figtitle_row <- if (!is.null(comp@labels$title)) {
    .tk_add(H, .track_h(rt0[["plot.title"]], comp@labels$title, 2 * .PAD_MM))
  } else {
    NA_integer_
  }
  figsubtitle_row <- if (!is.null(comp@labels$subtitle)) {
    .tk_add(H, .track_h(rt0[["plot.subtitle"]], comp@labels$subtitle, .PAD_MM))
  } else {
    NA_integer_
  }
  tag_row <- title_row <- subtitle_row <- integer(nr)
  panel_row <- xlabels_row <- xtitle_row <- caption_row <- integer(nr)
  for (r in seq_len(nr)) {
    tag_row[r] <- .tk_add(H, Ttag[[r]])
    title_row[r] <- .tk_add(H, Ttit[[r]])
    subtitle_row[r] <- .tk_add(H, Tsub[[r]])
    panel_row[r] <- .tk_add(H, vellum::vl_unit(wts_h[r], "null"))
    xlabels_row[r] <- .tk_add(H, Bxl[[r]])
    xtitle_row[r] <- .tk_add(H, Bxt[[r]])
    caption_row[r] <- .tk_add(H, Bcap[[r]])
    if (r < nr) .tk_add(H, gap)
  }
  figcaption_row <- if (!is.null(comp@labels$caption)) {
    .tk_add(H, .track_h(rt0[["plot.caption"]], comp@labels$caption, .PAD_MM))
  } else {
    NA_integer_
  }

  map <- lapply(seq_along(plans), function(i) {
    e <- cell[[i]]
    list(
      panel_row = panel_row[e$r],
      panel_col = panel_col[e$c],
      ytitle_col = ytitle_col[e$c],
      ylabels_col = ylabels_col[e$c],
      xlabels_row = xlabels_row[e$r],
      xtitle_row = xtitle_row[e$r],
      tag_row = tag_row[e$r],
      legend_col = rleg_col[e$c]
    )
  })

  list(
    widths = .tk_units(W),
    heights = .tk_units(H),
    ncol_total = length(W$u),
    map = map,
    figguides = figguides,
    figlegend_col = figlegend_col,
    panel_row_span = c(min(panel_row), max(panel_row)),
    figtitle_row = figtitle_row,
    figsubtitle_row = figsubtitle_row,
    figcaption_row = figcaption_row
  )
}

# A declarative key for a guide, so identical guides across sub-plots collapse to
# one (dedupe by spec, not by rendered grob — robust to position/theme diffs).
.guide_key <- function(g) {
  sc <- g$sc
  paste(
    g$kind,
    sc$name %||% "",
    paste(sc$levels %||% sc$legend_breaks %||% "", collapse = ""),
    sep = ""
  )
}

# Pool guides across sub-plots, keeping the first of each distinct key.
.pool_guides <- function(plans) {
  out <- list()
  seen <- character(0)
  for (p in plans) {
    for (g in p$guides) {
      k <- .guide_key(g)
      if (!k %in% seen) {
        seen <- c(seen, k)
        out <- c(out, list(g))
      }
    }
  }
  out
}

# Draw one sub-plot's panel + axes + titles into the shared grid cells.
.draw_subplot_aligned <- function(
  scene,
  plan,
  gm,
  collect,
  subplot = NA_integer_
) {
  built <- plan$built
  rt <- plan$rt
  panel <- built$panels[[1]]
  hv <- .hv_roles(panel$x_sc, panel$y_sc, plan$flip)
  hsc <- hv$h
  vsc <- hv$v
  psc <- list(
    x = panel$x_sc,
    y = panel$y_sc,
    color = built$scales$color,
    size = built$scales$size,
    shape = built$scales$shape,
    pattern = built$scales$pattern,
    edge_width = built$scales$edge_width,
    alpha = built$scales$alpha,
    linetype = built$scales$linetype,
    edge_color = built$scales$edge_color,
    edge_alpha = built$scales$edge_alpha,
    edge_linetype = built$scales$edge_linetype,
    flip = plan$flip,
    polar = NULL,
    sf_geographic = isTRUE(built$sf_geographic),
    sf_crs = built$sf_crs
  )
  # panel: background + marks
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      row = gm$panel_row,
      col = gm$panel_col,
      xscale = hsc$domain,
      yscale = vsc$domain,
      clip = TRUE
    )
  )
  scene <- .draw_panel_bg(scene, hsc, vsc, rt)
  co_g <- .coord_of(plan$spec)
  if (
    identical(co_g@kind, "sf") &&
      !is.null(co_g@graticule) &&
      !is.null(built$sf_crs)
  ) {
    scene <- .draw_graticule(
      scene,
      hsc,
      vsc,
      rt,
      built$sf_crs,
      isTRUE(built$sf_geographic),
      co_g@graticule
    )
  }
  pkey <- if (is.na(subplot)) NA_character_ else sprintf("subplot-%d", subplot)
  # Per-cell interaction context (the aligned path does not go through
  # `.draw_plot`): a cell declaring an interaction keys its marks, and a cell with
  # a `filter_by()` tags its elements so a host scopes the filter to this view.
  .set_interaction_ctx(plan$spec)
  scene <- .compile_marks(scene, panel$resolved, psc, panel = pkey)
  scene <- vellum::pop(scene)
  # axes + axis titles
  scene <- .draw_y_axis(scene, gm$panel_row, gm$ylabels_col, vsc, rt)
  scene <- .draw_x_axis(scene, gm$xlabels_row, gm$panel_col, hsc, rt)
  scene <- .draw_y_title(scene, gm$panel_row, gm$ytitle_col, vsc$name, rt)
  scene <- .draw_x_title(scene, gm$xtitle_row, gm$panel_col, hsc$name, rt)
  # per-sub-plot legend (only when not collecting)
  if (!collect && length(plan$guides)) {
    scene <- .draw_legends(
      scene,
      list(row = gm$panel_row, col = gm$legend_col),
      plan$guides,
      rt,
      orient = "vertical"
    )
  }
  scene
}

# Figure-level title / subtitle / caption bands spanning the whole grid.
.draw_figure_bands <- function(scene, comp, glo, rt0) {
  if (!is.na(glo$figtitle_row)) {
    scene <- .draw_title(
      scene,
      glo$figtitle_row,
      glo$ncol_total,
      comp@labels$title,
      rt0
    )
  }
  if (!is.na(glo$figsubtitle_row)) {
    scene <- .draw_subtitle(
      scene,
      glo$figsubtitle_row,
      glo$ncol_total,
      comp@labels$subtitle,
      rt0
    )
  }
  if (!is.na(glo$figcaption_row)) {
    scene <- .draw_caption(
      scene,
      glo$figcaption_row,
      glo$ncol_total,
      comp@labels$caption,
      rt0
    )
  }
  scene
}

# Draw a composition into the current viewport (so nesting works): push the
# shared grid, draw figure bands, the collected legend, then each sub-plot.
.draw_composition <- function(scene, comp) {
  collect <- identical(comp@guides, "collect")
  plans <- lapply(comp@plots, .plan_plot)
  # Figure-level chrome (title/subtitle/caption bands, the collected legend, panel
  # gap, tags) honours a theme set on the composition (compose_annotation(theme=)),
  # falling back to the default. Sub-plots resolve their own theme in `.plan_plot`.
  rt0 <- .resolve_theme(comp@theme %||% .theme_default())
  glo <- .composition_grid(plans, comp, collect, rt0)

  scene <- vellum::push(
    scene,
    vellum::vl_viewport(layout = vellum::grid_layout(glo$widths, glo$heights))
  )
  scene <- .draw_figure_bands(scene, comp, glo, rt0)
  if (collect && length(glo$figguides) && !is.na(glo$figlegend_col)) {
    span <- glo$panel_row_span
    scene <- .draw_legends(
      scene,
      list(
        row = span[1],
        col = glo$figlegend_col,
        rowspan = span[2] - span[1] + 1L
      ),
      glo$figguides,
      rt0,
      orient = "vertical"
    )
  }
  for (i in seq_along(plans)) {
    scene <- .draw_subplot_aligned(
      scene,
      plans[[i]],
      glo$map[[i]],
      collect,
      subplot = i
    )
  }
  if (!is.null(comp@tag)) {
    tags <- .format_tags(length(plans), comp@tag)
    for (i in seq_along(plans)) {
      gm <- glo$map[[i]]
      scene <- .draw_tag_corner(scene, gm$panel_row, gm$panel_col, tags[i], rt0)
    }
  }
  vellum::pop(scene)
}

# Spreadsheet-style alphabetic labels: A..Z, AA, AB, ... (bijective base-26), so
# a composition of more than 26 sub-plots keeps labelling instead of hitting the
# NA that `LETTERS[seq_len(n)]` returns past 26.
.alpha_labels <- function(n, upper = TRUE) {
  alph <- if (upper) LETTERS else letters
  vapply(
    seq_len(n),
    function(i) {
      out <- character(0)
      while (i > 0) {
        i <- i - 1L
        out <- c(alph[i %% 26L + 1L], out)
        i <- i %/% 26L
      }
      paste(out, collapse = "")
    },
    character(1)
  )
}

# Auto-tag labels for n sub-plots, e.g. "A","B",… / "1","2",… / "i","ii",…
.format_tags <- function(n, tag) {
  level <- tag$levels[1]
  body <- switch(
    level,
    "A" = .alpha_labels(n, upper = TRUE),
    "a" = .alpha_labels(n, upper = FALSE),
    "1" = as.character(seq_len(n)),
    "i" = tolower(utils::as.roman(seq_len(n))),
    "I" = as.character(utils::as.roman(seq_len(n))),
    as.character(seq_len(n))
  )
  paste0(tag$prefix %||% "", body, tag$suffix %||% "")
}

# Draw a sub-plot tag in the top-left corner of its panel cell.
.draw_tag_corner <- function(scene, row, col, text, rt) {
  el <- rt[["plot.tag"]]
  if (.is_blank(el)) {
    return(scene)
  }
  scene <- vellum::push(scene, vellum::vl_viewport(row = row, col = col))
  scene <- vellum::draw(
    scene,
    vellum::text_grob(
      text,
      x = vellum::vl_unit(0.04, "npc"),
      y = vellum::vl_unit(0.96, "npc"),
      just = c("left", "top"),
      gp = .el_gpar_text(el)
    )
  )
  vellum::pop(scene)
}
