#' @include classes.R elements.R theme-tree.R theme.R
NULL

# Layout geometry constants (mm). Font sizes, tick length, and panel spacing now
# come from the resolved theme; only the legend geometry and the generic gutter
# padding remain fixed here.
.PAD_MM <- 1.4
.LEGEND_BAR_MM <- 6 # colour-bar thickness
.LEGEND_SWATCH_MM <- 4 # default key / swatch side
.LEGEND_ROW_GAP_MM <- 1.4 # vertical gap between key rows
.LEGEND_TITLE_GAP_MM <- 1.2 # gap below a guide title
.LEGEND_SPACING_MM <- 3.5 # gap between stacked guides
.LEGEND_KEY_LABEL_GAP_MM <- 1.6 # gap between a key and its label
.LEGEND_INNER_PAD_MM <- 1.4 # legend content inset
.LEGEND_MIN_BAR_MM <- 22 # minimum colour-bar length
.LEGEND_MARGIN_MM <- c(2, 2, 2, 2) # default legend block inset (t, r, b, l)

# The longest string in a vector (by character count); "" for an empty vector.
.longest <- function(x) {
  x <- x[nzchar(x)]
  if (!length(x)) {
    return("0")
  }
  x[which.max(nchar(x))]
}

.txt <- function(label, fontsize, rot = 0) {
  vellum::text_grob(label, rot = rot, gp = vellum::gpar(fontsize = fontsize))
}

# Size of a text gutter/band track from a resolved theme element: the measured
# text extent at the element's font size plus `pad_mm`, or zero when the element
# is blank (so the track collapses).
.track_h <- function(el, label, pad_mm, rot = 0) {
  if (.is_blank(el)) {
    return(vellum::unit(0, "mm"))
  }
  vellum::grobheight(.txt(label, el@size, rot)) + vellum::unit(pad_mm, "mm")
}
.track_w <- function(el, label, pad_mm, rot = 0) {
  if (.is_blank(el)) {
    return(vellum::unit(0, "mm"))
  }
  vellum::grobwidth(.txt(label, el@size, rot)) + vellum::unit(pad_mm, "mm")
}

# Build the panel + gutter layout. In the simplest single-panel case the columns
# are [ y-title | y-labels | panel(null) ] and the rows [ panel(null) | x-labels
# | x-title ]; faceting, strips, a legend track, and the title/subtitle/tag/
# caption bands add further tracks (see the column/row builders below for the
# full track order). Gutter tracks are absolute (mm) from grobwidth/grobheight
# measurement; the panel track is `null` so it absorbs the remaining space.
# Width of the legend column: room for the widest swatch/bar/key needed by any
# guide, plus the widest label or title across all guides. Level/break labels are
# always plain strings; a guide title may be a rich `md()` object, so it is
# measured directly (never folded into the character vector, which would coerce
# it to a mangled string).
.legend_width <- function(guides, rt) {
  m <- .legend_metrics(rt)
  w <- 0
  for (g in guides) {
    labs <- .guide_labels(g)
    lw <- .mm_tw(labs, m$text_fs)
    body <- if (g$kind == "color_continuous") {
      m$pad + m$bar_w + m$lab_gap + lw
    } else {
      m$pad + .guide_key_d(g, m) + m$lab_gap + lw
    }
    tw <- if (m$show_title && !is.null(g$sc$name)) {
      .mm_tw_any(g$sc$name, m$title_fs)
    } else {
      0
    }
    w <- max(w, body, tw)
  }
  vellum::unit(w + m$margin[2] + m$margin[4], "mm")
}

# Height of a horizontal legend row (top/bottom): the tallest guide, each a title
# line above its keys (or above bar + labels for a colour bar). Independent of the
# guide count, which spreads across the row's width.
.legend_height <- function(guides, rt) {
  m <- .legend_metrics(rt)
  th <- if (m$show_title) m$title_h + m$title_gap else 0
  h <- 0
  for (g in guides) {
    gh <- if (g$kind == "color_continuous") {
      th + m$bar_w + m$text_h + m$lab_gap
    } else {
      th + max(.guide_key_d(g, m), m$text_h) + m$row_gap
    }
    h <- max(h, gh)
  }
  vellum::unit(h + m$margin[1] + m$margin[3], "mm")
}

# A tiny ordered track builder: `add(unit)` appends a track and returns its
# 1-based index; `units()` returns the concatenated unit vector.
.tracks <- function() {
  e <- new.env(parent = emptyenv())
  e$u <- list()
  e
}
.tk_add <- function(t, u) {
  t$u[[length(t$u) + 1L]] <- u
  length(t$u)
}
.tk_units <- function(t) do.call(c, t$u)

# Build the panel-grid layout for `built` (panels + scales + free flags). Returns
# the width/height track vectors plus the grid indices the seam needs to place
# panels, axes, strips, titles and the legend. The 1x1, no-facet, fixed-scale
# case reduces to a single-panel layout.
.build_layout <- function(
  built,
  guides = list(),
  labels = list(),
  rt = .resolve_theme(.theme_default()),
  flip = FALSE,
  coord = NULL
) {
  fa <- built$fa
  R <- fa$R
  C <- fa$C
  # Under coord_flip the bottom (x) axis shows the y-scale and the left (y) axis
  # shows the x-scale, so gutter sizing follows the horizontal / vertical roles.
  hv <- .hv_roles(built$scales$x, built$scales$y, flip)
  hsc <- hv$h
  vsc <- hv$v
  free_x <- built$free_x
  free_y <- built$free_y

  # Aspect lock. The panel `null` tracks carry weights that, with the layout's
  # `respect = TRUE`, force the device cell aspect (vellum makes 1 null-width =
  # 1 null-height in device units). coord_fixed(ratio) -> width:height of
  # x-range : (ratio * y-range); a bare theme aspect.ratio -> 1 : aspect.ratio.
  panel_w <- vellum::unit(1, "null")
  panel_h <- vellum::unit(1, "null")
  respect <- FALSE
  asp <- rt[["aspect.ratio"]]
  polar <- !is.null(coord) && identical(coord@kind, "polar")
  if (polar) {
    # Polar panels are square (1 null x 1 null, respect = TRUE).
    respect <- TRUE
  } else if (!is.null(coord) && identical(coord@kind, "fixed")) {
    ratio <- coord@ratio %||% 1
    xr <- abs(diff(range(built$scales$x$domain)))
    yr <- abs(diff(range(built$scales$y$domain)))
    panel_w <- vellum::unit(xr, "null")
    panel_h <- vellum::unit(ratio * yr, "null")
    respect <- TRUE
  } else if (!is.null(coord) && identical(coord@kind, "sf")) {
    # Map aspect: 1 for a projected CRS; the equirectangular correction
    # 1/cos(mean_latitude) for unprojected lon/lat (one degree N == one degree E
    # at the map centre). Same null-track mechanism as coord_fixed.
    xr <- abs(diff(range(built$scales$x$domain)))
    yr <- abs(diff(range(built$scales$y$domain)))
    ratio <- if (isTRUE(built$sf_geographic)) {
      mean_lat <- mean(built$scales$y$domain)
      1 / cos(mean_lat * pi / 180)
    } else {
      1
    }
    panel_w <- vellum::unit(xr, "null")
    panel_h <- vellum::unit(ratio * yr, "null")
    respect <- TRUE
  } else if (!is.null(asp)) {
    panel_h <- vellum::unit(asp, "null")
    respect <- TRUE
  }

  # Gutter sizes (measured from the widest labels across all panels) using the
  # resolved theme's font sizes; blank elements collapse their track.
  v_labs <- unique(unlist(lapply(
    built$panels,
    function(p) (if (flip) p$x_sc else p$y_sc)$labels
  )))
  h_labs <- unique(unlist(lapply(
    built$panels,
    function(p) (if (flip) p$y_sc else p$x_sc)$labels
  )))
  tick <- rt[["axis.ticks.length"]]
  yt <- .track_w(rt[["axis.title.y"]], vsc$name, .PAD_MM, rot = 90)
  yl <- .track_w(rt[["axis.text.y"]], .longest(v_labs), tick + .PAD_MM)
  xl <- .track_h(rt[["axis.text.x"]], .longest(h_labs), tick + .PAD_MM)
  xt <- .track_h(rt[["axis.title.x"]], hsc$name, .PAD_MM)
  # Polar panels carry their axis labels/titles inside the square panel, so the
  # four cartesian gutter tracks collapse to zero.
  if (polar) {
    yt <- yl <- xl <- xt <- vellum::unit(0, "mm")
  }
  strip <- .track_h(rt[["strip.text"]], "Ag", 2 * .PAD_MM)
  gap <- vellum::unit(rt[["panel.spacing"]], "mm")

  has_col_strip <- fa$type == "wrap" ||
    (fa$type == "grid" && !is.null(fa$col_labels))
  has_row_strip <- fa$type == "grid" && !is.null(fa$row_labels)

  # Legend placement. "right"/"left" take a column beside the panels (vertical
  # guide stack); "top"/"bottom" take a row spanning the panel columns
  # (horizontal guide flow); "none" suppresses it.
  pos <- rt[["legend.position"]]
  show_legend <- length(guides) && !identical(pos, "none")
  legend_vert <- show_legend && pos %in% c("left", "right")
  legend_horiz <- show_legend && pos %in% c("top", "bottom")

  # --- columns: [ legend(left)? | ytitle | (ylabels panel gap)xC |
  #               row-strip? | legend(right)? ] ---
  W <- .tracks()
  legend_col <- NA_integer_
  if (legend_vert && pos == "left") {
    legend_col <- .tk_add(W, .legend_width(guides, rt))
  }
  ytitle_col <- .tk_add(W, yt)
  panel_col <- integer(C)
  ylabels_col <- integer(C)
  shared_yl <- if (!free_y) .tk_add(W, yl) else NA_integer_
  for (c in seq_len(C)) {
    ylabels_col[c] <- if (free_y) .tk_add(W, yl) else shared_yl
    panel_col[c] <- .tk_add(W, panel_w)
    if (c < C) .tk_add(W, gap)
  }
  rowstrip_col <- if (has_row_strip) .tk_add(W, strip) else NA_integer_
  if (legend_vert && pos == "right") {
    legend_col <- .tk_add(W, .legend_width(guides, rt))
  }

  # --- rows: [ tag? | title? | subtitle? | col-strip? |
  #             (strip? panel xlabels?)xR | xlabels? | xtitle | caption? ] ---
  # The tag, title, and subtitle bands stack above the panels (the tag in the
  # top-left corner, like ggplot2's plot.tag.position = "topleft"); the caption
  # band sits below. Each band only occupies a track when its label is present,
  # so an unlabelled plot's layout is unchanged.
  H <- .tracks()
  tag_row <- if (!is.null(labels$tag)) {
    .tk_add(H, .track_h(rt[["plot.tag"]], labels$tag, .PAD_MM))
  } else {
    NA_integer_
  }
  title_row <- if (!is.null(labels$title)) {
    .tk_add(H, .track_h(rt[["plot.title"]], labels$title, 2 * .PAD_MM))
  } else {
    NA_integer_
  }
  subtitle_row <- if (!is.null(labels$subtitle)) {
    .tk_add(H, .track_h(rt[["plot.subtitle"]], labels$subtitle, .PAD_MM))
  } else {
    NA_integer_
  }
  legend_row <- NA_integer_
  if (legend_horiz && pos == "top") {
    legend_row <- .tk_add(H, .legend_height(guides, rt))
  }
  colstrip_row <- if (fa$type == "grid" && has_col_strip) {
    .tk_add(H, strip)
  } else {
    NA_integer_
  }
  panel_row <- integer(R)
  xlabels_row <- integer(R)
  wrapstrip_row <- integer(R)
  for (r in seq_len(R)) {
    if (fa$type == "wrap") {
      wrapstrip_row[r] <- .tk_add(H, strip)
    }
    panel_row[r] <- .tk_add(H, panel_h)
    if (free_x) {
      xlabels_row[r] <- .tk_add(H, xl)
    }
    if (r < R) .tk_add(H, gap)
  }
  shared_xl <- if (!free_x) .tk_add(H, xl) else NA_integer_
  if (!free_x) {
    for (r in seq_len(R)) {
      xlabels_row[r] <- shared_xl
    }
  }
  xtitle_row <- .tk_add(H, xt)
  if (legend_horiz && pos == "bottom") {
    legend_row <- .tk_add(H, .legend_height(guides, rt))
  }
  caption_row <- if (!is.null(labels$caption)) {
    .tk_add(H, .track_h(rt[["plot.caption"]], labels$caption, .PAD_MM))
  } else {
    NA_integer_
  }

  list(
    widths = .tk_units(W),
    heights = .tk_units(H),
    R = R,
    C = C,
    panel_row = panel_row,
    panel_col = panel_col,
    ylabels_col = ylabels_col,
    xlabels_row = xlabels_row,
    ytitle_col = ytitle_col,
    xtitle_row = xtitle_row,
    wrapstrip_row = wrapstrip_row,
    colstrip_row = colstrip_row,
    rowstrip_col = rowstrip_col,
    legend_col = legend_col,
    legend_row = legend_row,
    legend_pos = pos,
    title_row = title_row,
    subtitle_row = subtitle_row,
    tag_row = tag_row,
    caption_row = caption_row,
    ncol_total = length(W$u),
    nrow_total = length(H$u),
    respect = respect
  )
}
