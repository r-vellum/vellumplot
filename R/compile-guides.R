#' @include classes.R elements.R theme-tree.R compile-layout.R
NULL

# Minor gridline positions: midpoints between sorted finite breaks, plus one
# extrapolated half-step past each end so minor lines also appear outside the
# outer majors (the panel viewport clips any that fall beyond the scale). Empty
# when there are fewer than two breaks.
.minor_breaks <- function(b) {
  b <- sort(b[is.finite(b)])
  if (length(b) < 2) {
    return(numeric(0))
  }
  mids <- (utils::head(b, -1) + utils::tail(b, -1)) / 2
  step <- b[2] - b[1]
  c(b[1] - step / 2, mids, b[length(b)] + step / 2)
}

# Vertical gridlines at `xs` (native), spanning the panel height. Drawn as one
# batched segments_grob (vellum splits it per element only where gpar differs).
.vlines <- function(scene, xs, gp) {
  if (!length(xs)) {
    return(scene)
  }
  k <- length(xs)
  vellum::draw(
    scene,
    vellum::segments_grob(
      vellum::unit(xs, "native"),
      vellum::unit(rep(0, k), "npc"),
      vellum::unit(xs, "native"),
      vellum::unit(rep(1, k), "npc"),
      gp = gp
    )
  )
}

# Horizontal gridlines at `ys` (native), spanning the panel width.
.hlines <- function(scene, ys, gp) {
  if (!length(ys)) {
    return(scene)
  }
  k <- length(ys)
  vellum::draw(
    scene,
    vellum::segments_grob(
      vellum::unit(rep(0, k), "npc"),
      vellum::unit(ys, "native"),
      vellum::unit(rep(1, k), "npc"),
      vellum::unit(ys, "native"),
      gp = gp
    )
  )
}

# Panel background + gridlines + axis ticks, drawn while the scene is positioned
# inside the panel viewport (so `"native"` resolves against the panel's trained
# scales). `rt` is the resolved theme. Minor gridlines sit under major ones;
# ticks point inward from the panel edges (their length is in mm from the edge,
# which avoids mixed npc/mm unit arithmetic vellum disallows).
.draw_panel_bg <- function(scene, x_sc, y_sc, rt) {
  pb <- rt[["panel.background"]]
  if (!.is_blank(pb)) {
    scene <- vellum::draw(scene, vellum::rect_grob(gp = .el_gpar_rect(pb)))
  }
  mnx <- rt[["panel.grid.minor.x"]]
  if (!.is_blank(mnx)) {
    scene <- .vlines(scene, .minor_breaks(x_sc$breaks), .el_gpar_line(mnx))
  }
  mny <- rt[["panel.grid.minor.y"]]
  if (!.is_blank(mny)) {
    scene <- .hlines(scene, .minor_breaks(y_sc$breaks), .el_gpar_line(mny))
  }
  gx <- rt[["panel.grid.major.x"]]
  if (!.is_blank(gx)) {
    scene <- .vlines(scene, x_sc$breaks, .el_gpar_line(gx))
  }
  gy <- rt[["panel.grid.major.y"]]
  if (!.is_blank(gy)) {
    scene <- .hlines(scene, y_sc$breaks, .el_gpar_line(gy))
  }
  tlen <- rt[["axis.ticks.length"]]
  tx <- rt[["axis.ticks.x"]]
  if (!.is_blank(tx) && length(x_sc$breaks)) {
    b <- x_sc$breaks
    k <- length(b)
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::unit(b, "native"),
        vellum::unit(rep(0, k), "npc"),
        vellum::unit(b, "native"),
        vellum::unit(rep(tlen, k), "mm"),
        gp = .el_gpar_line(tx)
      )
    )
  }
  ty <- rt[["axis.ticks.y"]]
  if (!.is_blank(ty) && length(y_sc$breaks)) {
    b <- y_sc$breaks
    k <- length(b)
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::unit(rep(0, k), "npc"),
        vellum::unit(b, "native"),
        vellum::unit(rep(tlen, k), "mm"),
        vellum::unit(b, "native"),
        gp = .el_gpar_line(ty)
      )
    )
  }
  scene
}

# Points around a circle of radius `r` (closed; native units), for polar
# backgrounds and concentric gridlines.
.circle_pts <- function(r, n = 120L) {
  t <- seq(0, 2 * pi, length.out = n + 1L)
  list(x = r * cos(t), y = r * sin(t))
}

# Polar panel background + gridlines + axis labels, drawn inside the square
# [-1, 1] panel viewport (polar plots have no gutters). The theta aesthetic's
# breaks become radial spokes + rim labels; the radius aesthetic's breaks become
# concentric circles + labels up the top-centre. Theme elements follow the
# underlying aesthetic, so theming flows as in the cartesian case.
.draw_panel_polar <- function(scene, ctx, rt) {
  theta_aes <- ctx$theta_aes
  r_aes <- if (theta_aes == "x") "y" else "x"

  # background disk (an annulus for a donut, so the hole stays empty rather than
  # filled with panel colour and crossed by spokes)
  pb <- rt[["panel.background"]]
  if (!.is_blank(pb)) {
    co <- .circle_pts(ctx$rmax)
    if (ctx$rmin > 0) {
      ci <- .circle_pts(ctx$rmin)
      bx <- c(co$x, NA, rev(ci$x))
      by <- c(co$y, NA, rev(ci$y))
    } else {
      bx <- co$x
      by <- co$y
    }
    scene <- vellum::draw(
      scene,
      vellum::polygon_grob(
        vellum::unit(bx, "native"),
        vellum::unit(by, "native"),
        gp = .el_gpar_rect(pb)
      )
    )
  }

  # concentric circles at the radius breaks
  r_grid <- rt[[paste0("panel.grid.major.", r_aes)]]
  rbreaks <- ctx$r_sc$breaks
  if (!.is_blank(r_grid) && length(rbreaks)) {
    gp <- .el_gpar_line(r_grid)
    for (b in rbreaks) {
      rr <- ctx$r_map(b)
      if (rr <= 0) {
        next
      }
      cp <- .circle_pts(rr)
      scene <- vellum::draw(
        scene,
        vellum::lines_grob(
          vellum::unit(cp$x, "native"),
          vellum::unit(cp$y, "native"),
          gp = gp
        )
      )
    }
  }

  # radial spokes at the theta breaks
  theta_grid <- rt[[paste0("panel.grid.major.", theta_aes)]]
  tbreaks <- ctx$theta_sc$breaks
  if (!.is_blank(theta_grid) && length(tbreaks)) {
    ang <- ctx$theta_map(tbreaks)
    # Spokes run from the inner radius to the rim, so a donut's hole is not
    # crossed by lines converging at the centre.
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::unit(ctx$rmin * cos(ang), "native"),
        vellum::unit(ctx$rmin * sin(ang), "native"),
        vellum::unit(ctx$rmax * cos(ang), "native"),
        vellum::unit(ctx$rmax * sin(ang), "native"),
        gp = .el_gpar_line(theta_grid)
      )
    )
  }

  # angular tick labels just outside the rim
  tel <- rt[[paste0("axis.text.", theta_aes)]]
  if (!.is_blank(tel) && length(tbreaks)) {
    gp <- .el_gpar_text(tel)
    ang <- ctx$theta_map(tbreaks)
    rl <- ctx$rmax + 0.12
    labs <- ctx$theta_sc$labels
    for (i in seq_along(ang)) {
      scene <- vellum::draw(
        scene,
        vellum::text_grob(
          labs[i],
          x = vellum::unit(rl * cos(ang[i]), "native"),
          y = vellum::unit(rl * sin(ang[i]), "native"),
          just = c("centre", "centre"),
          gp = gp
        )
      )
    }
  }

  # radial tick labels up the top-centre spoke
  rel <- rt[[paste0("axis.text.", r_aes)]]
  if (!.is_blank(rel) && length(rbreaks)) {
    gp <- .el_gpar_text(rel)
    labs <- ctx$r_sc$labels
    for (i in seq_along(rbreaks)) {
      rr <- ctx$r_map(rbreaks[i])
      scene <- vellum::draw(
        scene,
        vellum::text_grob(
          labs[i],
          x = vellum::unit(0.02, "native"),
          y = vellum::unit(rr, "native"),
          just = c("left", "centre"),
          gp = gp
        )
      )
    }
  }
  scene
}

# y-axis labels for `y_sc`, right-aligned in the gutter cell and aligned to the
# gridlines (the gutter viewport shares the panel's y native scale).
.draw_y_axis <- function(scene, row, col, y_sc, rt) {
  vp_name <- sprintf("axis-y-%d", row)
  el <- rt[["axis.text.y"]]
  aline <- rt[["axis.line.y"]]
  if (.is_blank(el) && .is_blank(aline)) {
    return(scene)
  }
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = col, yscale = y_sc$domain, name = vp_name)
  )
  # axis line along the panel-adjacent (right) edge of the gutter
  if (!.is_blank(aline)) {
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::unit(1, "npc"),
        vellum::unit(0, "npc"),
        vellum::unit(1, "npc"),
        vellum::unit(1, "npc"),
        gp = .el_gpar_line(aline)
      )
    )
  }
  if (!.is_blank(el)) {
    gp <- .el_gpar_text(el)
    for (i in seq_along(y_sc$breaks)) {
      scene <- vellum::draw(
        scene,
        vellum::text_grob(
          y_sc$labels[i],
          x = vellum::unit(0.96, "npc"),
          y = vellum::unit(y_sc$breaks[i], "native"),
          just = c("right", "centre"),
          gp = gp
        )
      )
    }
  }
  vellum::pop(scene)
}

# x-axis labels for `x_sc`, centred under each gridline.
.draw_x_axis <- function(scene, row, col, x_sc, rt) {
  el <- rt[["axis.text.x"]]
  aline <- rt[["axis.line.x"]]
  if (.is_blank(el) && .is_blank(aline)) {
    return(scene)
  }
  scene <- vellum::push(
    scene,
    vellum::viewport(
      row = row,
      col = col,
      xscale = x_sc$domain,
      name = sprintf("axis-x-%d", col)
    )
  )
  # axis line along the panel-adjacent (top) edge of the gutter
  if (!.is_blank(aline)) {
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::unit(0, "npc"),
        vellum::unit(1, "npc"),
        vellum::unit(1, "npc"),
        vellum::unit(1, "npc"),
        gp = .el_gpar_line(aline)
      )
    )
  }
  if (!.is_blank(el)) {
    gp <- .el_gpar_text(el)
    for (i in seq_along(x_sc$breaks)) {
      scene <- vellum::draw(
        scene,
        vellum::text_grob(
          x_sc$labels[i],
          x = vellum::unit(x_sc$breaks[i], "native"),
          y = vellum::unit(0.82, "npc"),
          just = c("centre", "top"),
          gp = gp
        )
      )
    }
  }
  vellum::pop(scene)
}

# A facet strip: an optional filled background plus a centred label. `rot = 90`
# draws a right-side (row) strip.
.draw_strip <- function(
  scene,
  row,
  col,
  label,
  rt,
  rot = 0,
  rowspan = 1,
  colspan = 1,
  name = sprintf("strip-%d-%d", row, col)
) {
  scene <- vellum::push(
    scene,
    vellum::viewport(
      row = row,
      col = col,
      rowspan = rowspan,
      colspan = colspan,
      name = name
    )
  )
  sb <- rt[["strip.background"]]
  if (!.is_blank(sb)) {
    scene <- vellum::draw(scene, vellum::rect_grob(gp = .el_gpar_rect(sb)))
  }
  st <- rt[["strip.text"]]
  if (!.is_blank(st)) {
    scene <- vellum::draw(
      scene,
      vellum::text_grob(label, rot = rot, gp = .el_gpar_text(st))
    )
  }
  vellum::pop(scene)
}

.draw_y_title <- function(scene, row, col, name, rt, rowspan = 1) {
  el <- rt[["axis.title.y"]]
  if (.is_blank(el)) {
    return(scene)
  }
  scene <- vellum::push(
    scene,
    vellum::viewport(
      row = row,
      col = col,
      rowspan = rowspan,
      name = "axis-title-y"
    )
  )
  scene <- vellum::draw(
    scene,
    vellum::text_grob(name, rot = 90, gp = .el_gpar_text(el))
  )
  vellum::pop(scene)
}

.draw_x_title <- function(scene, row, col, name, rt, colspan = 1) {
  el <- rt[["axis.title.x"]]
  if (.is_blank(el)) {
    return(scene)
  }
  scene <- vellum::push(
    scene,
    vellum::viewport(
      row = row,
      col = col,
      colspan = colspan,
      name = "axis-title-x"
    )
  )
  scene <- vellum::draw(scene, vellum::text_grob(name, gp = .el_gpar_text(el)))
  vellum::pop(scene)
}

# --- plot title / subtitle / caption / tag bands ----------------------------
# Each spans the full page width (col = 1, colspan = ncol). `default_hjust`
# reproduces the ggplot2 default placement (title/subtitle/tag flush left,
# caption flush right); a theme `hjust` overrides it.
.draw_band <- function(scene, row, ncol, text, el, default_hjust) {
  if (.is_blank(el)) {
    return(scene)
  }
  hj <- el@hjust %||% default_hjust
  x_npc <- 0.01 + hj * 0.98
  just_h <- if (hj < 0.25) {
    "left"
  } else if (hj > 0.75) {
    "right"
  } else {
    "centre"
  }
  scene <- vellum::push(
    scene,
    vellum::viewport(row = row, col = 1, colspan = ncol)
  )
  scene <- vellum::draw(
    scene,
    vellum::text_grob(
      text,
      x = vellum::unit(x_npc, "npc"),
      just = c(just_h, "centre"),
      rot = .el_rot(el),
      gp = .el_gpar_text(el)
    )
  )
  vellum::pop(scene)
}

.draw_title <- function(scene, row, ncol, text, rt) {
  .draw_band(scene, row, ncol, text, rt[["plot.title"]], 0)
}

.draw_subtitle <- function(scene, row, ncol, text, rt) {
  .draw_band(scene, row, ncol, text, rt[["plot.subtitle"]], 0)
}

.draw_caption <- function(scene, row, ncol, text, rt) {
  .draw_band(scene, row, ncol, text, rt[["plot.caption"]], 1)
}

.draw_tag <- function(scene, row, ncol, text, rt) {
  .draw_band(scene, row, ncol, text, rt[["plot.tag"]], 0)
}

# --- legend -----------------------------------------------------------------

# The list of guides a plot needs, in draw order: colour first, then size, then
# shape, then edge width. Each guide is `list(kind, sc)` where `sc` is the
# trained scale (or, for a merged guide, a synthesized pseudo-scale).
#
# When one variable drives two aesthetics, the colour guide merges with its
# partner into a single guide whose keys carry both encodings (ggplot2's rule):
# a discrete colour scale merges with `shape`; a continuous colour scale merges
# with `size`. They merge only when the two scales share a title *and* a
# compatible break/level set -- give one scale a different `name=` to keep them
# apart. `merged` marks which partner (if any) was folded into the colour guide.
.legend_guides <- function(scales) {
  out <- list()
  merged <- NULL
  if (!is.null(scales$color)) {
    gk <- scales$color$kind
    # A binned (classed) colour scale draws as discrete swatches with interval
    # labels, so it reuses the discrete guide.
    discrete <- gk %in% c("discrete", "binned")
    if (discrete && .can_merge_shape(scales$color, scales$shape)) {
      out <- c(
        out,
        list(list(
          kind = "merged",
          sc = .merged_color_shape(scales$color, scales$shape)
        ))
      )
      merged <- "shape"
    } else if (
      identical(gk, "continuous") && .can_merge_size(scales$color, scales$size)
    ) {
      out <- c(
        out,
        list(list(
          kind = "merged",
          sc = .merged_color_size(scales$color, scales$size)
        ))
      )
      merged <- "size"
    } else {
      k <- if (discrete) "discrete" else gk
      out <- c(out, list(list(kind = paste0("color_", k), sc = scales$color)))
    }
  }
  if (!is.null(scales$size) && !identical(merged, "size")) {
    out <- c(out, list(list(kind = "size", sc = scales$size)))
  }
  if (!is.null(scales$shape) && !identical(merged, "shape")) {
    out <- c(out, list(list(kind = "shape", sc = scales$shape)))
  }
  if (!is.null(scales$edge_width)) {
    out <- c(out, list(list(kind = "edge_width", sc = scales$edge_width)))
  }
  out
}

# A discrete colour scale and a shape scale merge when they carry the same title
# and the identical set of levels (so the same variable, encoded the same way).
.can_merge_shape <- function(color, shape) {
  !is.null(color) &&
    !is.null(shape) &&
    identical(color$name, shape$name) &&
    identical(color$levels, shape$levels)
}

# A continuous colour scale and a size scale merge when they carry the same
# title (they then share the variable, hence its data range and breaks).
.can_merge_size <- function(color, size) {
  !is.null(color) && !is.null(size) && identical(color$name, size$name)
}

# Pseudo-scale for a merged colour+shape guide: one row per shared level, each a
# point drawn in the level's colour with the level's shape. `fill` and `col` are
# both set so stroke-only shapes (plus/cross) still take the colour.
.merged_color_shape <- function(color, shape) {
  cols <- color$colors
  list(
    name = color$name,
    labels = color$labels %||% color$levels,
    fills = cols,
    cols = cols,
    shapes = shape$shapes,
    sizes_mm = NULL
  )
}

# Pseudo-scale for a merged colour+size guide: the size scale drives the rows
# (breaks -> radii), and each key is coloured by mapping its break through the
# continuous colour scale. Same variable, so the size breaks lie in the colour
# scale's data range.
.merged_color_size <- function(color, size) {
  cols <- color$map(size$legend_breaks)
  # Sized keys are drawn as circles (the colour scale's own key glyph may be a
  # line/square, which are not point shapes).
  list(
    name = color$name,
    labels = size$legend_labels,
    fills = cols,
    cols = cols,
    shapes = rep_len("circle", length(cols)),
    sizes_mm = size$legend_sizes
  )
}

# --- legend geometry --------------------------------------------------------
# Legends are laid out in absolute millimetres, never in fractions of the
# (variable-height) legend cell. Each guide is measured to its content height
# (a title line + one row per key, the row pitch driven by the key's drawn
# size), guides are stacked with a fixed inter-guide gap, and the whole block is
# centred in the legend track. Everything is placed through nested
# `grid_layout()` cells so a key and its label land in their own mm-sized boxes
# and no npc/mm unit arithmetic (which vellum disallows) is ever needed.

# Millimetre text metrics at a given point size (device-space, resize-stable).
.mm_th <- function(fs) vellum::vl_strheight("Ag", "", "plain", fs, unit = "mm")
.mm_tw <- function(s, fs) {
  s <- as.character(s)
  s <- s[!is.na(s)]
  if (!length(s)) {
    return(0)
  }
  s[!nzchar(s)] <- " "
  max(vellum::vl_strwidth(s, "", "plain", fs, unit = "mm"))
}

# Resolved legend geometry (all mm, except the two font sizes). Phase 3 lets the
# theme override the key size / spacings; the defaults live in `.SETTINGS_DEFAULTS`.
.legend_metrics <- function(rt) {
  list(
    text_fs = rt[["legend.text"]]@size,
    title_fs = rt[["legend.title"]]@size,
    text_h = .mm_th(rt[["legend.text"]]@size),
    title_h = .mm_th(rt[["legend.title"]]@size),
    key = rt[["legend.key.size"]] %||% .LEGEND_SWATCH_MM,
    bar_w = .LEGEND_BAR_MM,
    row_gap = .LEGEND_ROW_GAP_MM,
    title_gap = .LEGEND_TITLE_GAP_MM,
    spacing = rt[["legend.spacing"]] %||% .LEGEND_SPACING_MM,
    lab_gap = .LEGEND_KEY_LABEL_GAP_MM,
    pad = .LEGEND_INNER_PAD_MM,
    # inset (t, r, b, l) around the whole legend block
    margin = rep_len(rt[["legend.margin"]] %||% .LEGEND_MARGIN_MM, 4L),
    show_title = !.is_blank(rt[["legend.title"]])
  )
}

# The row labels a guide shows. A discrete/binned colour guide appends an "NA"
# row where the data has missing values (its swatch is drawn in the NA colour);
# size/shape NA keys are not yet supported (see DESIGN.md).
.guide_labels <- function(g) {
  sc <- g$sc
  switch(
    g$kind,
    color_continuous = sc$legend_labels,
    color_discrete = {
      l <- sc$labels %||% sc$levels
      if (isTRUE(sc$na)) c(l, "NA") else l
    },
    size = sc$legend_labels,
    shape = sc$levels,
    edge_width = sc$legend_labels,
    merged = sc$labels
  )
}

# The drawn diameter (mm) of a guide's key, used to size the row pitch / key
# column so the largest key never collides with its neighbours. `points_grob`
# sizes are radii, so a size key of radius r is 2r across.
.guide_key_d <- function(g, m) {
  sizes <- if (g$kind == "size") {
    g$sc$legend_sizes
  } else if (g$kind == "merged") {
    g$sc$sizes_mm
  } else {
    NULL
  }
  if (length(sizes)) max(2 * max(sizes), m$key) else m$key
}

# The continuous colour-bar length (mm) for a vertical guide: long enough that
# consecutive break labels never touch.
.bar_len_mm <- function(g, m) {
  k <- length(g$sc$legend_labels)
  max(.LEGEND_MIN_BAR_MM, k * (m$text_h + m$lab_gap))
}

# Measured height (mm) of a vertical guide: title line + key rows (uniform pitch
# = max(key diameter, text height) + gap), or title + colour bar (+ NA row).
.guide_height_mm <- function(g, m) {
  th <- if (m$show_title) m$title_h + m$title_gap else 0
  if (g$kind == "color_continuous") {
    na <- if (isTRUE(g$sc$na)) max(m$key, m$text_h) + m$row_gap else 0
    th + .bar_len_mm(g, m) + na
  } else {
    k <- length(.guide_labels(g))
    pitch <- max(.guide_key_d(g, m), m$text_h) + m$row_gap
    th + k * pitch
  }
}

# Measured width (mm) of a horizontal guide: the wider of its title and its key
# row (keys packed left-to-right, each followed by its own label).
.guide_width_mm <- function(g, m) {
  labs <- .guide_labels(g)
  tw <- if (m$show_title && !is.null(g$sc$name)) {
    .mm_tw_any(g$sc$name, m$title_fs)
  } else {
    0
  }
  if (g$kind == "color_continuous") {
    body <- max(
      .LEGEND_MIN_BAR_MM,
      sum(vapply(labs, .mm_tw, 0, fs = m$text_fs) + m$lab_gap)
    )
  } else {
    key_d <- .guide_key_d(g, m)
    body <- sum(
      key_d +
        m$lab_gap +
        vapply(labs, .mm_tw, 0, fs = m$text_fs) +
        m$spacing
    )
  }
  max(tw, body)
}

# Title width in mm; a rich md() title is measured via grobwidth (falling back to
# a generous estimate) since vl_strwidth needs a plain string.
.mm_tw_any <- function(name, fs) {
  if (is.character(name)) {
    return(.mm_tw(name, fs))
  }
  # md()/expression title: estimate from its label text if available, else 0.
  txt <- tryCatch(as.character(name), error = function(e) "")
  .mm_tw(txt, fs)
}

# A unit vector of `sizes` (mm) separated by `gap` (mm) spacers, for a stacking
# grid_layout: c(size1, gap, size2, gap, ..., sizeN). Guides live in the odd
# tracks (2*i - 1).
.interleave_mm <- function(sizes, gap) {
  n <- length(sizes)
  if (!n) {
    return(vellum::unit(numeric(0), "mm"))
  }
  parts <- vector("list", 2L * n - 1L)
  for (i in seq_len(n)) {
    parts[[2L * i - 1L]] <- vellum::unit(sizes[i], "mm")
    if (i < n) {
      parts[[2L * i]] <- vellum::unit(gap, "mm")
    }
  }
  do.call(c, parts)
}

# Concatenate a list of units, dropping NULL entries (conditional tracks).
.c_units <- function(...) {
  do.call(c, Filter(Negate(is.null), list(...)))
}

# --- key glyphs -------------------------------------------------------------
# A single key glyph, drawn centred in the current (cell) viewport. Phase 2
# swaps the discrete-colour swatch for a mark-matched glyph via `sc$key_glyph`.

.key_grob <- function(g, i, m) {
  sc <- g$sc
  switch(
    g$kind,
    color_discrete = {
      cols <- sc$colors
      if (isTRUE(sc$na)) {
        cols <- c(cols, sc$na_value)
      }
      .colour_key_grob(sc$key_glyph, cols[i], m)
    },
    size = vellum::points_grob(
      vellum::unit(0.5, "npc"),
      vellum::unit(0.5, "npc"),
      size = vellum::unit(sc$legend_sizes[i], "mm"),
      shape = sc$key_glyph %||% "circle",
      gp = vellum::gpar(fill = "grey35", col = "grey35")
    ),
    shape = vellum::points_grob(
      vellum::unit(0.5, "npc"),
      vellum::unit(0.5, "npc"),
      size = vellum::unit(m$key / 2, "mm"),
      shape = sc$shapes[i],
      gp = vellum::gpar(fill = "grey35", col = "grey35")
    ),
    edge_width = vellum::segments_grob(
      vellum::unit(0.12, "npc"),
      vellum::unit(0.5, "npc"),
      vellum::unit(0.88, "npc"),
      vellum::unit(0.5, "npc"),
      gp = vellum::gpar(col = "grey35", lwd = sc$legend_widths[i])
    ),
    # A merged guide's key carries both encodings in one point: the shared
    # variable's colour (fill + stroke) and shape, sized when size is merged in.
    merged = vellum::points_grob(
      vellum::unit(0.5, "npc"),
      vellum::unit(0.5, "npc"),
      size = vellum::unit(sc$sizes_mm[i] %||% (m$key / 2), "mm"),
      shape = sc$shapes[i] %||% "circle",
      gp = vellum::gpar(fill = sc$fills[i], col = sc$cols[i])
    )
  )
}

# A discrete-colour key drawn as the mark's glyph: a filled point for point/glyph
# marks, a short line for line marks, else the default filled square swatch.
.colour_key_grob <- function(glyph, col, m) {
  switch(
    glyph %||% "square",
    point = ,
    circle = vellum::points_grob(
      vellum::unit(0.5, "npc"),
      vellum::unit(0.5, "npc"),
      size = vellum::unit(m$key / 2, "mm"),
      shape = "circle",
      gp = vellum::gpar(fill = col, col = NA)
    ),
    line = vellum::segments_grob(
      vellum::unit(0.12, "npc"),
      vellum::unit(0.5, "npc"),
      vellum::unit(0.88, "npc"),
      vellum::unit(0.5, "npc"),
      gp = vellum::gpar(col = col, lwd = 2)
    ),
    vellum::rect_grob(
      width = vellum::unit(m$key, "mm"),
      height = vellum::unit(m$key, "mm"),
      gp = vellum::gpar(fill = col, col = NA)
    )
  )
}

# --- guide drawers ----------------------------------------------------------

.draw_guide_title <- function(scene, name, rt) {
  el <- rt[["legend.title"]]
  if (.is_blank(el) || is.null(name)) {
    return(scene)
  }
  vellum::draw(
    scene,
    vellum::text_grob(
      name,
      x = vellum::unit(0, "npc"),
      y = vellum::unit(0.4, "npc"),
      just = c("left", "centre"),
      gp = .el_gpar_text(el)
    )
  )
}

# Draw a vertical guide into the current viewport (its exact measured mm height).
.draw_guide_v <- function(scene, g, m, rt) {
  txt <- .el_gpar_text(rt[["legend.text"]])
  th <- if (m$show_title) m$title_h + m$title_gap else 0
  if (g$kind == "color_continuous") {
    return(.draw_guide_continuous_v(scene, g, m, rt, txt, th))
  }
  labs <- .guide_labels(g)
  k <- length(labs)
  key_d <- .guide_key_d(g, m)
  pitch <- max(key_d, m$text_h) + m$row_gap
  key_w <- m$pad + key_d + m$lab_gap
  heights <- .c_units(
    if (m$show_title) vellum::unit(th, "mm"),
    vellum::unit(rep(pitch, k), "mm")
  )
  scene <- vellum::push(
    scene,
    vellum::viewport(
      layout = vellum::grid_layout(
        heights = heights,
        widths = c(vellum::unit(key_w, "mm"), vellum::unit(1, "null"))
      )
    )
  )
  off <- if (m$show_title) 1L else 0L
  if (m$show_title) {
    scene <- vellum::push(
      scene,
      vellum::viewport(row = 1, col = 1, colspan = 2)
    )
    scene <- .draw_guide_title(scene, g$sc$name, rt)
    scene <- vellum::pop(scene)
  }
  for (i in seq_len(k)) {
    scene <- vellum::push(scene, vellum::viewport(row = off + i, col = 1))
    scene <- vellum::draw(scene, .key_grob(g, i, m))
    scene <- vellum::pop(scene)
    scene <- vellum::push(scene, vellum::viewport(row = off + i, col = 2))
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        labs[i],
        x = vellum::unit(0, "npc"),
        y = vellum::unit(0.5, "npc"),
        just = c("left", "centre"),
        gp = txt
      )
    )
    scene <- vellum::pop(scene)
  }
  vellum::pop(scene)
}

.draw_guide_continuous_v <- function(scene, g, m, rt, txt, th) {
  cl <- g$sc
  has_na <- isTRUE(cl$na)
  na_h <- max(m$key, m$text_h) + m$row_gap
  heights <- .c_units(
    if (m$show_title) vellum::unit(th, "mm"),
    vellum::unit(1, "null"),
    if (has_na) vellum::unit(na_h, "mm")
  )
  scene <- vellum::push(
    scene,
    vellum::viewport(
      layout = vellum::grid_layout(
        heights = heights,
        widths = vellum::unit(1, "null")
      )
    )
  )
  row_bar <- if (m$show_title) 2L else 1L
  if (m$show_title) {
    scene <- vellum::push(scene, vellum::viewport(row = 1, col = 1))
    scene <- .draw_guide_title(scene, cl$name, rt)
    scene <- vellum::pop(scene)
  }
  scene <- vellum::push(scene, vellum::viewport(row = row_bar, col = 1))
  grad <- vellum::linear_gradient(cl$pal256, x1 = 0, y1 = 0, x2 = 0, y2 = 1)
  scene <- vellum::draw(
    scene,
    vellum::rect_grob(
      x = vellum::unit(m$pad + m$bar_w / 2, "mm"),
      y = vellum::unit(0.5, "npc"),
      width = vellum::unit(m$bar_w, "mm"),
      height = vellum::unit(1, "npc"),
      gp = vellum::gpar(fill = grad, col = "grey50", lwd = 0.5)
    )
  )
  for (i in seq_along(cl$legend_breaks)) {
    frac <- scales::rescale(cl$legend_breaks[i], from = cl$range)
    # A white break tick reaching in from the bar's right (label-side) edge.
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::unit(m$pad + m$bar_w - .LEGEND_TICK_MM, "mm"),
        vellum::unit(frac, "npc"),
        vellum::unit(m$pad + m$bar_w, "mm"),
        vellum::unit(frac, "npc"),
        gp = vellum::gpar(col = "white", lwd = 0.8)
      )
    )
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        cl$legend_labels[i],
        x = vellum::unit(m$pad + m$bar_w + m$lab_gap, "mm"),
        y = vellum::unit(frac, "npc"),
        just = c("left", "centre"),
        gp = txt
      )
    )
  }
  scene <- vellum::pop(scene)
  if (has_na) {
    scene <- vellum::push(
      scene,
      vellum::viewport(row = row_bar + 1L, col = 1)
    )
    scene <- vellum::draw(
      scene,
      vellum::rect_grob(
        x = vellum::unit(m$pad + m$bar_w / 2, "mm"),
        y = vellum::unit(0.5, "npc"),
        width = vellum::unit(m$key, "mm"),
        height = vellum::unit(m$key, "mm"),
        gp = vellum::gpar(fill = cl$na_value, col = NA)
      )
    )
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        "NA",
        x = vellum::unit(m$pad + m$bar_w + m$lab_gap, "mm"),
        y = vellum::unit(0.5, "npc"),
        just = c("left", "centre"),
        gp = txt
      )
    )
    scene <- vellum::pop(scene)
  }
  vellum::pop(scene)
}

# --- horizontal guide drawers (top/bottom legend) ---------------------------

# Draw a horizontal guide into the current viewport (its exact measured mm width).
.draw_guide_h <- function(scene, g, m, rt) {
  txt <- .el_gpar_text(rt[["legend.text"]])
  th <- if (m$show_title) m$title_h + m$title_gap else 0
  if (g$kind == "color_continuous") {
    return(.draw_guide_continuous_h(scene, g, m, rt, txt, th))
  }
  labs <- .guide_labels(g)
  k <- length(labs)
  key_d <- .guide_key_d(g, m)
  # per-key cell width: key + gap + label + trailing spacer
  cellw <- key_d +
    m$lab_gap +
    vapply(labs, .mm_tw, 0, fs = m$text_fs) +
    m$spacing
  heights <- .c_units(
    if (m$show_title) vellum::unit(th, "mm"),
    vellum::unit(1, "null")
  )
  scene <- vellum::push(
    scene,
    vellum::viewport(
      layout = vellum::grid_layout(
        heights = heights,
        widths = vellum::unit(1, "null")
      )
    )
  )
  key_row <- if (m$show_title) 2L else 1L
  if (m$show_title) {
    scene <- vellum::push(scene, vellum::viewport(row = 1, col = 1))
    scene <- .draw_guide_title(scene, g$sc$name, rt)
    scene <- vellum::pop(scene)
  }
  scene <- vellum::push(
    scene,
    vellum::viewport(
      row = key_row,
      col = 1,
      layout = vellum::grid_layout(
        widths = vellum::unit(cellw, "mm"),
        heights = vellum::unit(1, "null")
      )
    )
  )
  for (i in seq_len(k)) {
    scene <- vellum::push(scene, vellum::viewport(row = 1, col = i))
    scene <- vellum::push(
      scene,
      vellum::viewport(
        x = vellum::unit(key_d / 2, "mm"),
        width = vellum::unit(key_d, "mm")
      )
    )
    scene <- vellum::draw(scene, .key_grob(g, i, m))
    scene <- vellum::pop(scene)
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        labs[i],
        x = vellum::unit(key_d + m$lab_gap, "mm"),
        y = vellum::unit(0.5, "npc"),
        just = c("left", "centre"),
        gp = txt
      )
    )
    scene <- vellum::pop(scene)
  }
  scene <- vellum::pop(scene)
  vellum::pop(scene)
}

.draw_guide_continuous_h <- function(scene, g, m, rt, txt, th) {
  cl <- g$sc
  heights <- .c_units(
    if (m$show_title) vellum::unit(th, "mm"),
    vellum::unit(m$bar_w, "mm"),
    vellum::unit(m$text_h + m$lab_gap, "mm")
  )
  scene <- vellum::push(
    scene,
    vellum::viewport(
      layout = vellum::grid_layout(
        heights = heights,
        widths = vellum::unit(1, "null")
      )
    )
  )
  off <- if (m$show_title) 1L else 0L
  if (m$show_title) {
    scene <- vellum::push(scene, vellum::viewport(row = 1, col = 1))
    scene <- .draw_guide_title(scene, cl$name, rt)
    scene <- vellum::pop(scene)
  }
  scene <- vellum::push(scene, vellum::viewport(row = off + 1L, col = 1))
  grad <- vellum::linear_gradient(cl$pal256, x1 = 0, y1 = 0, x2 = 1, y2 = 0)
  scene <- vellum::draw(
    scene,
    vellum::rect_grob(
      x = vellum::unit(0.5, "npc"),
      y = vellum::unit(0.5, "npc"),
      width = vellum::unit(1, "npc"),
      height = vellum::unit(1, "npc"),
      gp = vellum::gpar(fill = grad, col = "grey50", lwd = 0.5)
    )
  )
  # White break ticks reaching up from the bar's bottom (label-side) edge.
  for (i in seq_along(cl$legend_breaks)) {
    frac <- scales::rescale(cl$legend_breaks[i], from = cl$range)
    scene <- vellum::draw(
      scene,
      vellum::segments_grob(
        vellum::unit(frac, "npc"),
        vellum::unit(0, "npc"),
        vellum::unit(frac, "npc"),
        vellum::unit(.LEGEND_TICK_MM, "mm"),
        gp = vellum::gpar(col = "white", lwd = 0.8)
      )
    )
  }
  scene <- vellum::pop(scene)
  scene <- vellum::push(scene, vellum::viewport(row = off + 2L, col = 1))
  for (i in seq_along(cl$legend_breaks)) {
    frac <- scales::rescale(cl$legend_breaks[i], from = cl$range)
    # Justify the end labels inward so they never spill past the bar ends.
    hjust <- if (frac <= 0.01) {
      "left"
    } else if (frac >= 0.99) {
      "right"
    } else {
      "centre"
    }
    scene <- vellum::draw(
      scene,
      vellum::text_grob(
        cl$legend_labels[i],
        x = vellum::unit(frac, "npc"),
        y = vellum::unit(0.5, "npc"),
        just = c(hjust, "centre"),
        gp = txt
      )
    )
  }
  scene <- vellum::pop(scene)
  vellum::pop(scene)
}

# --- legend assembly --------------------------------------------------------
# Lay the guides out in the legend cell. A vertical (left/right) legend stacks
# the guides top-to-bottom in a mm-height block centred in the cell; a horizontal
# (top/bottom) legend packs them left-to-right in a mm-width block centred in the
# cell. Each guide is drawn to its own measured extent, so spacing is uniform and
# resize-stable regardless of how many guides share the legend.
.draw_legends <- function(scene, cell, guides, rt, orient = "vertical") {
  scene <- vellum::push(
    scene,
    vellum::viewport(
      row = cell$row,
      col = cell$col,
      rowspan = cell$rowspan %||% 1,
      colspan = cell$colspan %||% 1,
      name = "legend"
    )
  )
  lb <- rt[["legend.background"]]
  if (!.is_blank(lb)) {
    scene <- vellum::draw(scene, vellum::rect_grob(gp = .el_gpar_rect(lb)))
  }
  m <- .legend_metrics(rt)
  n <- length(guides)
  if (!n) {
    return(vellum::pop(scene))
  }
  # Inset the content block by `legend.margin` (t, r, b, l) via a margin grid, so
  # everything below draws inside the middle (2, 2) cell.
  scene <- vellum::push(
    scene,
    vellum::viewport(
      layout = vellum::grid_layout(
        widths = c(
          vellum::unit(m$margin[4], "mm"),
          vellum::unit(1, "null"),
          vellum::unit(m$margin[2], "mm")
        ),
        heights = c(
          vellum::unit(m$margin[1], "mm"),
          vellum::unit(1, "null"),
          vellum::unit(m$margin[3], "mm")
        )
      )
    )
  )
  scene <- vellum::push(scene, vellum::viewport(row = 2, col = 2))
  if (orient == "horizontal") {
    ws <- vapply(guides, .guide_width_mm, 0, m = m)
    total <- sum(ws) + (n - 1) * m$spacing
    scene <- vellum::push(
      scene,
      vellum::viewport(
        x = vellum::unit(0.5, "npc"),
        width = vellum::unit(total, "mm"),
        height = vellum::unit(1, "npc"),
        layout = vellum::grid_layout(
          widths = .interleave_mm(ws, m$spacing),
          heights = vellum::unit(1, "null")
        )
      )
    )
    for (i in seq_len(n)) {
      scene <- vellum::push(
        scene,
        vellum::viewport(row = 1, col = 2L * i - 1L)
      )
      scene <- .draw_guide_h(scene, guides[[i]], m, rt)
      scene <- vellum::pop(scene)
    }
    scene <- vellum::pop(scene)
  } else {
    hs <- vapply(guides, .guide_height_mm, 0, m = m)
    total <- sum(hs) + (n - 1) * m$spacing
    scene <- vellum::push(
      scene,
      vellum::viewport(
        y = vellum::unit(0.5, "npc"),
        width = vellum::unit(1, "npc"),
        height = vellum::unit(total, "mm"),
        layout = vellum::grid_layout(
          heights = .interleave_mm(hs, m$spacing),
          widths = vellum::unit(1, "null")
        )
      )
    )
    for (i in seq_len(n)) {
      scene <- vellum::push(
        scene,
        vellum::viewport(row = 2L * i - 1L, col = 1)
      )
      scene <- .draw_guide_v(scene, guides[[i]], m, rt)
      scene <- vellum::pop(scene)
    }
    scene <- vellum::pop(scene)
  }
  scene <- vellum::pop(scene) # content (2, 2) cell
  scene <- vellum::pop(scene) # margin grid
  vellum::pop(scene) # legend cell
}
