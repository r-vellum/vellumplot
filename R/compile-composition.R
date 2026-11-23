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
# Phase scope: single-panel, non-polar, non-sf sub-plots on a regular grid.
# Faceted / polar / sf sub-plots fall back to the independent layout (see
# .compile_composition), where each is drawn through .draw_plot() (so an sf map
# is projected and its graticule drawn, which the aligned path does not do).
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
      !identical(.coord_of(p)@kind, "polar") &&
      # An sf map is aspect-locked (respect = TRUE), like polar, and needs
      # projection + graticule that the aligned path never runs (`.plan_plot`
      # skips `.dp_setup_coord`). Fall back to the independent layout, where
      # `.draw_plot` projects it correctly.
      !identical(.coord_of(p)@kind, "sf") &&
      !.has_sf_layer(p)
  }
  all(vapply(comp@plots, ok, logical(1)))
}

# Front half of .draw_plot(): everything needed to measure and place a plot,
# without drawing. Returns the resolved theme, built panels, guide list, coord
# flags and the per-plot layout (which carries the measured gutter sizes).
.plan_plot <- function(spec) {
  rt <- .resolve_theme(.theme_of(spec))
  built <- .build_panels(spec)
  guides <- .facet_legend_guides(built)
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

# Where a sub-plot's own legends go in the aligned path: its guides split by
# side as its layout placed them (each guide's `position =`, else the theme's
# `legend.position`; nothing at all under `legend.position = "none"`). A legend
# beside its (single) panel, `position = "panel"`, joins the side the theme
# names for those. Returns the guide lists `left`, `right`, `top`, `bottom`
# (bands around the sub-plot's cell, or pooled into the figure's legend when
# guides are collected) and `inside` (over its panel, at `inside_at`, even when
# collecting).
.subplot_legend_groups <- function(plan) {
  lay <- plan$lay
  sides <- c("left", "right", "top", "bottom", "inside")
  out <- stats::setNames(rep(list(list()), length(sides)), sides)
  groups <- lay$legend_groups
  for (sd in sides) {
    out[[sd]] <- groups[[sd]] %||% list()
  }
  pl <- lay$panel_legends
  if (!is.null(pl) && length(pl$guides) && length(pl$guides[[1]])) {
    out[[pl$side]] <- c(out[[pl$side]], pl$guides[[1]])
  }
  out$inside_at <- lay$legend_inside %||% pl$inside
  out
}

# The outside sides (`.subplot_legend_groups()`) holding any legend.
.LEGEND_SIDES <- c("left", "right", "top", "bottom")
.subplot_legend_sides <- function(lg) {
  .LEGEND_SIDES[vapply(.LEGEND_SIDES, function(sd) length(lg[[sd]]) > 0L, NA)]
}

# Decompose a single-panel plan into canonical decoration bands + the panel.
# A sub-plot's legends take a band on the side each was placed (zero when
# guides are collected at the figure level, or for a side with none); `legends`
# says which sides hold any, so a figure adds a left / top / bottom legend
# track only where some sub-plot uses it.
.plot_bands <- function(plan, collect) {
  lay <- plan$lay
  z <- vellum::vl_unit(0, "mm")
  lg <- .subplot_legend_groups(plan)
  sides <- if (collect) character(0) else .subplot_legend_sides(lg)
  legw <- function(sd) {
    if (sd %in% sides) .legend_width(lg[[sd]], plan$rt) else z
  }
  legh <- function(sd) {
    if (sd %in% sides) .legend_height(lg[[sd]], plan$rt) else z
  }
  list(
    legends = sides,
    L_legend = legw("left"),
    T_legend = legh("top"),
    B_legend = legh("bottom"),
    L_ytitle = .lay_w(lay, lay$ytitle_col),
    L_ylabels = .lay_w(lay, lay$ylabels_col[1]),
    R_legend = legw("right"),
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

  # per-column shared left/right widths; per-row shared top/bottom heights.
  # A left / top / bottom legend track exists only in a column / row where some
  # sub-plot puts a legend there (decided by presence, not measured size).
  uses <- function(ix, sd) {
    any(vapply(bands[ix], function(b) sd %in% b$legends, NA))
  }
  Lyt <- Lyl <- Rleg <- Lleg <- vector("list", nc)
  for (c in seq_len(nc)) {
    ix <- in_col(c)
    Lyt[[c]] <- .umax(lapply(bands[ix], `[[`, "L_ytitle"))
    Lyl[[c]] <- .umax(lapply(bands[ix], `[[`, "L_ylabels"))
    Rleg[[c]] <- .umax(lapply(bands[ix], `[[`, "R_legend"))
    if (uses(ix, "left")) {
      Lleg[[c]] <- .umax(lapply(bands[ix], `[[`, "L_legend"))
    }
  }
  Ttag <- Ttit <- Tsub <- Bxl <- Bxt <- Bcap <- vector("list", nr)
  Tleg <- Bleg <- vector("list", nr)
  for (r in seq_len(nr)) {
    ix <- in_row(r)
    if (uses(ix, "top")) {
      Tleg[[r]] <- .umax(lapply(bands[ix], `[[`, "T_legend"))
    }
    if (uses(ix, "bottom")) {
      Bleg[[r]] <- .umax(lapply(bands[ix], `[[`, "B_legend"))
    }
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

  # The collected legend goes where the composition theme's `legend.position`
  # puts it, as patchwork's does: a column left / right of the cells, or a row
  # above / below them; "none" drops it (VP-62).
  figguides <- if (collect) .pool_guides(plans) else list()
  figside <- .fig_legend_side(rt0)
  if (identical(figside, "none")) {
    figguides <- list()
  }
  has_fig <- length(figguides) > 0L

  # --- columns -------------------------------------------------------------
  W <- .tracks()
  ytitle_col <- ylabels_col <- panel_col <- rleg_col <- integer(nc)
  lleg_col <- rep(NA_integer_, nc)
  figlegend_col <- NA_integer_
  if (has_fig && identical(figside, "left")) {
    figlegend_col <- .tk_add(W, .legend_width(figguides, rt0))
    .tk_add(W, gap)
  }
  for (c in seq_len(nc)) {
    if (!is.null(Lleg[[c]])) {
      lleg_col[c] <- .tk_add(W, Lleg[[c]])
    }
    ytitle_col[c] <- .tk_add(W, Lyt[[c]])
    ylabels_col[c] <- .tk_add(W, Lyl[[c]])
    panel_col[c] <- .tk_add(W, vellum::vl_unit(wts_w[c], "null"))
    rleg_col[c] <- .tk_add(W, Rleg[[c]])
    if (c < nc) .tk_add(W, gap)
  }
  if (has_fig && identical(figside, "right")) {
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
  figlegend_row <- NA_integer_
  if (has_fig && identical(figside, "top")) {
    figlegend_row <- .tk_add(H, .legend_height(figguides, rt0))
    .tk_add(H, gap)
  }
  tag_row <- title_row <- subtitle_row <- integer(nr)
  panel_row <- xlabels_row <- xtitle_row <- caption_row <- integer(nr)
  tleg_row <- bleg_row <- rep(NA_integer_, nr)
  for (r in seq_len(nr)) {
    tag_row[r] <- .tk_add(H, Ttag[[r]])
    title_row[r] <- .tk_add(H, Ttit[[r]])
    subtitle_row[r] <- .tk_add(H, Tsub[[r]])
    if (!is.null(Tleg[[r]])) {
      tleg_row[r] <- .tk_add(H, Tleg[[r]])
    }
    panel_row[r] <- .tk_add(H, vellum::vl_unit(wts_h[r], "null"))
    xlabels_row[r] <- .tk_add(H, Bxl[[r]])
    xtitle_row[r] <- .tk_add(H, Bxt[[r]])
    if (!is.null(Bleg[[r]])) {
      bleg_row[r] <- .tk_add(H, Bleg[[r]])
    }
    caption_row[r] <- .tk_add(H, Bcap[[r]])
    if (r < nr) .tk_add(H, gap)
  }
  if (has_fig && identical(figside, "bottom")) {
    .tk_add(H, gap)
    figlegend_row <- .tk_add(H, .legend_height(figguides, rt0))
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
      legend_col = rleg_col[e$c],
      legend_cols = c(left = lleg_col[e$c], right = rleg_col[e$c]),
      legend_rows = c(top = tleg_row[e$r], bottom = bleg_row[e$r])
    )
  })

  list(
    widths = .tk_units(W),
    heights = .tk_units(H),
    ncol_total = length(W$u),
    map = map,
    figguides = figguides,
    figlegend_side = figside,
    figlegend_col = figlegend_col,
    figlegend_row = figlegend_row,
    panel_row_span = c(min(panel_row), max(panel_row)),
    panel_col_span = c(min(panel_col), max(panel_col)),
    figtitle_row = figtitle_row,
    figsubtitle_row = figsubtitle_row,
    figcaption_row = figcaption_row
  )
}

# Where a composition's collected legend goes: the composition theme's
# `legend.position` ("right", the default, "left", "top", "bottom" or "none").
# An inside position has no cell to sit in, so it falls back to the right.
.fig_legend_side <- function(rt) {
  pos <- .legend_default_position(rt)
  if (
    is.character(pos) && length(pos) == 1L && pos %in% c(.LEGEND_SIDES, "none")
  ) {
    pos
  } else {
    "right"
  }
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

# Pool guides across sub-plots, keeping the first of each distinct key. Only
# legends outside their panel contribute, from whichever side (see
# `.subplot_legend_groups()`); an inside one stays in its cell.
.pool_guides <- function(plans) {
  out <- list()
  seen <- character(0)
  for (p in plans) {
    lg <- .subplot_legend_groups(p)
    for (g in unlist(lg[.LEGEND_SIDES], recursive = FALSE)) {
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
    fill = built$scales$fill,
    size = built$scales$size,
    shape = built$scales$shape,
    pattern = built$scales$pattern,
    edge_width = built$scales$edge_width,
    linewidth = built$scales$linewidth,
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
  # panel: background + marks. Named per cell and, like a single plot's cartesian
  # panel, carrying the scale descriptors + a clip-stable pan group, so a host can
  # map its pixels to data and zoom it on its own (axis-aware zoom, bind_scale()).
  cell <- .comp_cell_take()
  co_g <- .coord_of(plan$spec)
  linear <- !identical(co_g@kind, "trans")
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      row = gm$panel_row,
      col = gm$panel_col,
      xscale = hsc$domain,
      yscale = vsc$domain,
      clip = TRUE,
      name = .cell_panel_name("panel-1-1", cell),
      meta = if (linear) .panel_meta(hsc, vsc, cell, rt),
      pannable = linear
    )
  )
  scene <- .draw_panel_bg(scene, hsc, vsc, rt)
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
  .set_interaction_ctx(plan$spec, cell)
  # ... and its theme's default mark colours, for its marks and own legend.
  .set_geom_ctx(.theme_of(plan$spec))
  scene <- .compile_marks(scene, panel$resolved, psc, panel = pkey)
  scene <- vellum::pop(scene)
  # axes + axis titles
  scene <- .draw_y_axis(scene, gm$panel_row, gm$ylabels_col, vsc, rt)
  scene <- .draw_x_axis(
    scene,
    gm$xlabels_row,
    gm$panel_col,
    hsc,
    rt,
    gm$xwrap_mm
  )
  scene <- .draw_y_title(scene, gm$panel_row, gm$ytitle_col, vsc$name, rt)
  scene <- .draw_x_title(scene, gm$xtitle_row, gm$panel_col, hsc$name, rt)
  # per-sub-plot legends: inside its own panel cell, and (only when not
  # collecting) in the band on each side a legend was placed -- a vertical
  # stack beside the panel (left / right), a horizontal row above / below it.
  # The legends at the theme's own position keep the viewport name "legend".
  lg <- .subplot_legend_groups(plan)
  vp_name <- function(pos) {
    if (identical(pos, plan$lay$legend_pos)) {
      "legend"
    } else {
      paste0("legend-", pos)
    }
  }
  if (length(lg$inside) && !is.null(lg$inside_at)) {
    scene <- .draw_legend_inside(
      scene,
      list(
        legend_inside = lg$inside_at,
        panel_row = gm$panel_row,
        panel_col = gm$panel_col,
        R = 1L,
        C = 1L
      ),
      lg$inside,
      rt,
      name = vp_name("inside")
    )
  }
  if (!collect) {
    for (sd in .subplot_legend_sides(lg)) {
      vertical <- sd %in% c("left", "right")
      cell <- if (vertical) {
        list(row = gm$panel_row, col = gm$legend_cols[[sd]])
      } else {
        list(row = gm$legend_rows[[sd]], col = gm$panel_col)
      }
      scene <- .draw_legends(
        scene,
        cell,
        lg[[sd]],
        rt,
        orient = if (vertical) "vertical" else "horizontal",
        name = vp_name(sd)
      )
    }
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
  # Default mark colours: each cell sets its own theme's; the collected legend
  # keys take the composition theme's, else the first sub-plot's. Put the
  # caller's back afterwards (a composition can be drawn inside a plot).
  old_geom <- .mark_ctx$geom
  old_ground <- .mark_ctx$geom_ground
  on.exit(assign("geom", old_geom, envir = .mark_ctx), add = TRUE)
  on.exit(assign("geom_ground", old_ground, envir = .mark_ctx), add = TRUE)
  .set_geom_ctx(
    if (!is.null(.theme_geom(comp@theme))) {
      comp@theme
    } else {
      .theme_of(plans[[1]]$spec)
    }
  )

  # The figure keeps the theme's `plot.margin` around its grid, as a single
  # plot does (`.dp_push_frame()`); before, the first row's panels touched the
  # top edge and the last column's the right edge (VP-50).
  m <- rep_len(rt0[["plot.margin"]] %||% 0, 4L) # (t, r, b, l) mm
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      layout = vellum::grid_layout(
        c(
          vellum::vl_unit(m[4], "mm"),
          vellum::vl_unit(1, "null"),
          vellum::vl_unit(m[2], "mm")
        ),
        c(
          vellum::vl_unit(m[1], "mm"),
          vellum::vl_unit(1, "null"),
          vellum::vl_unit(m[3], "mm")
        )
      )
    )
  )
  scene <- vellum::push(scene, vellum::vl_viewport(row = 2, col = 2))
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
  if (collect && length(glo$figguides) && !is.na(glo$figlegend_row)) {
    span <- glo$panel_col_span
    scene <- .draw_legends(
      scene,
      list(
        row = glo$figlegend_row,
        col = span[1],
        colspan = span[2] - span[1] + 1L
      ),
      glo$figguides,
      rt0,
      orient = "horizontal"
    )
  }
  for (i in seq_along(plans)) {
    .comp_cell_next()
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
  vellum::pop(vellum::pop(vellum::pop(scene)))
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
