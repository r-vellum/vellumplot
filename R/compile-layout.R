#' @include classes.R elements.R theme-tree.R theme.R
NULL

# Layout geometry constants (mm). Font sizes, tick length, and panel spacing now
# come from the resolved theme; only the legend geometry and the generic gutter
# padding remain fixed here.
.PAD_MM <- 1.4
.LEGEND_BAR_MM <- 6 # colour-bar thickness
.LEGEND_ROW_GAP_MM <- 1.4 # vertical gap between key rows
.LEGEND_TITLE_GAP_MM <- 1.2 # gap below a guide title
.LEGEND_KEY_LABEL_GAP_MM <- 1.6 # gap between a key and its label
.LEGEND_INNER_PAD_MM <- 1.4 # legend content inset
.LEGEND_MIN_BAR_MM <- 22 # minimum colour-bar length
.LEGEND_TICK_MM <- 1.6 # colour-bar break tick length

# A trained position range can be a single point (min == max) -- e.g. one
# distinct value under coord_fixed()/coord_sf(). Its width is 0, which as a `null`
# track weight collapses the panel to nothing under respect = TRUE. Floor a
# degenerate (zero / non-finite) span to 1 so the panel keeps a finite extent.
.nonzero_span <- function(x) if (!is.finite(x) || x == 0) 1 else x

# The longest string in a vector (by character count). For an empty vector it
# returns "0" -- a one-character width floor so an axis with no labels still
# reserves a sane gutter.
.longest <- function(x) {
  x <- x[nzchar(x)]
  if (!length(x)) {
    return("0")
  }
  x[which.max(nchar(x))]
}

.txt <- function(label, fontsize, rot = 0) {
  vellum::text_grob(label, rot = rot, gp = vellum::vl_gpar(fontsize = fontsize))
}

# Size of a text gutter/band track from a resolved theme element: the measured
# text extent at the element's font size plus `pad_mm`, or zero when the element
# is blank (so the track collapses).
.track_h <- function(el, label, pad_mm, rot = 0) {
  if (.is_blank(el)) {
    return(vellum::vl_unit(0, "mm"))
  }
  vellum::grobheight(.txt(label, el@size, rot)) + vellum::vl_unit(pad_mm, "mm")
}
.track_w <- function(el, label, pad_mm, rot = 0) {
  if (.is_blank(el)) {
    return(vellum::vl_unit(0, "mm"))
  }
  vellum::grobwidth(.txt(label, el@size, rot)) + vellum::vl_unit(pad_mm, "mm")
}

# Height (mm) of a full-width title/subtitle/caption band. `band_w` is the mm
# width the band spans; when finite the label wraps to it, so a long band grows
# to as many lines as it needs. Measured through the *same* grob the drawer uses
# (`.band_text_grob`), so the reserved height and the drawn text never disagree.
.track_h_band <- function(el, label, pad_mm, band_w) {
  if (.is_blank(el)) {
    return(vellum::vl_unit(0, "mm"))
  }
  vellum::grobheight(.band_text_grob(label, el, band_w)) +
    vellum::vl_unit(pad_mm, "mm")
}

# Height (mm) of the x tick-label row. Measured through the *same* grob the axis
# drawer builds (`.axis_text_grob`), so a rotated or wrapped label reserves the
# height it actually draws at. `wrap_mm` is the per-tick mm budget (NA = no wrap).
.track_h_axis <- function(el, label, pad_mm, rot = 0, wrap_mm = NA_real_) {
  if (.is_blank(el)) {
    return(vellum::vl_unit(0, "mm"))
  }
  g <- .axis_text_grob(
    label,
    el,
    c("centre", "top"),
    rot = rot,
    wrap_mm = wrap_mm
  )
  vellum::grobheight(g) + vellum::vl_unit(pad_mm, "mm")
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
# The width (mm) one guide needs: the wider of its key+label body and its title.
.guide_col_width <- function(g, m) {
  labs <- .guide_labels(g)
  lw <- .mm_tw(labs, m$text_fs)
  body <- if (g$kind %in% c("color_continuous", "color_steps")) {
    m$pad + .bar_w_mm(g, m) + m$lab_gap + lw
  } else {
    m$pad + .guide_key_d(g, m) + m$lab_gap + lw
  }
  tw <- if (m$show_title && !is.null(g$sc$name)) {
    .mm_tw_any(g$sc$name, m$title_fs)
  } else {
    0
  }
  max(body, tw)
}

# Pack a vertical legend's guides into columns that each fit `avail_h` (mm) --
# the height the legend column has to work with (roughly the figure height). A
# non-finite `avail_h` (the default, and every horizontal / composition path)
# keeps everything in one column, so behaviour is unchanged unless the caller
# opts in with a real height. Greedy top-to-bottom fill; a lone guide taller than
# `avail_h` still gets its own column (nothing else it can do). Returns a list of
# integer index vectors, one per column.
.legend_columns <- function(guides, m, avail_h) {
  n <- length(guides)
  if (!n) {
    return(list())
  }
  if (!is.finite(avail_h) || n == 1L) {
    return(list(seq_len(n)))
  }
  hs <- vapply(guides, .guide_height_mm, 0, m = m)
  cols <- list()
  cur <- integer(0)
  cur_h <- 0
  for (i in seq_len(n)) {
    inc <- if (length(cur)) m$spacing + hs[i] else hs[i]
    if (length(cur) && (cur_h + inc) > avail_h) {
      cols <- c(cols, list(cur))
      cur <- i
      cur_h <- hs[i]
    } else {
      cur <- c(cur, i)
      cur_h <- cur_h + inc
    }
  }
  c(cols, list(cur))
}

# Available height (mm) for a vertical legend that spans the whole figure height,
# less its top/bottom margins. `NULL` page height -> Inf (single column).
.legend_avail_h <- function(page_height, rt) {
  if (is.null(page_height)) {
    return(Inf)
  }
  m <- .legend_metrics(rt)
  page_height * 25.4 - m$margin[1] - m$margin[3]
}

# Width (mm) a vertical legend reserves: guides pack into as many columns as it
# takes to fit `avail_h`, and the reserved width is the sum of the per-column
# widths (+ inter-column spacing). `.draw_legends()` repeats the same packing so
# the drawn columns match the reserved width exactly.
.legend_width <- function(guides, rt, avail_h = Inf) {
  m <- .legend_metrics(rt)
  cols <- .legend_columns(guides, m, avail_h)
  if (!length(cols)) {
    return(vellum::vl_unit(m$margin[2] + m$margin[4], "mm"))
  }
  colw <- vapply(
    cols,
    function(idx) max(vapply(guides[idx], .guide_col_width, 0, m = m)),
    0
  )
  total <- sum(colw) + (length(cols) - 1L) * m$spacing
  vellum::vl_unit(total + m$margin[2] + m$margin[4], "mm")
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
  vellum::vl_unit(h + m$margin[1] + m$margin[3], "mm")
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
  coord = NULL,
  marginal = NULL,
  page_height = NULL,
  page_width = NULL
) {
  # A vertical legend spans the whole figure height; tell the width/draw code how
  # much height it has so a tall guide stack wraps into columns instead of
  # spilling off the top and bottom of the page (#80).
  legend_avail_h <- .legend_avail_h(page_height, rt)
  # The mm width a full-width title/subtitle/caption band spans: the page content
  # box, i.e. the page less its left/right plot margins (the band viewport spans
  # every column of that box). Long bands wrap to it; NA when the page width is
  # unknown (the composition path), where bands stay single-line.
  band_w <- if (is.null(page_width)) {
    NA_real_
  } else {
    pm <- rep_len(rt[["plot.margin"]] %||% 0, 4L) # (t, r, b, l) mm
    page_width * 25.4 - pm[2] - pm[4]
  }
  fa <- built$fa
  R <- fa$R
  C <- fa$C

  # Marginal-plot tracks (single panel only; guarded in the seam). A top row and/
  # or a right column, each a `null` track weighted to `size` relative to the
  # panel's `1`, so the marginal takes `size/(1+size)` of the combined extent.
  has_top <- !is.null(marginal) && grepl("t", marginal@sides, fixed = TRUE)
  has_right <- !is.null(marginal) && grepl("r", marginal@sides, fixed = TRUE)
  marg <- if (is.null(marginal)) {
    NULL
  } else {
    vellum::vl_unit(marginal@size, "null")
  }
  marg_top_row <- NA_integer_
  marg_right_col <- NA_integer_
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
  panel_w <- vellum::vl_unit(1, "null")
  panel_h <- vellum::vl_unit(1, "null")
  respect <- FALSE
  asp <- rt[["aspect.ratio"]]
  polar <- !is.null(coord) && identical(coord@kind, "polar")
  if (polar) {
    # Polar panels are square (1 null x 1 null, respect = TRUE).
    respect <- TRUE
  } else if (!is.null(coord) && identical(coord@kind, "fixed")) {
    ratio <- coord@ratio %||% 1
    xr <- .nonzero_span(abs(diff(range(built$scales$x$domain))))
    yr <- .nonzero_span(abs(diff(range(built$scales$y$domain))))
    panel_w <- vellum::vl_unit(xr, "null")
    panel_h <- vellum::vl_unit(ratio * yr, "null")
    respect <- TRUE
  } else if (!is.null(coord) && identical(coord@kind, "sf")) {
    # Map aspect: 1 for a projected CRS; the equirectangular correction
    # 1/cos(mean_latitude) for unprojected lon/lat (one degree N == one degree E
    # at the map centre). Same null-track mechanism as coord_fixed.
    xr <- .nonzero_span(abs(diff(range(built$scales$x$domain))))
    yr <- .nonzero_span(abs(diff(range(built$scales$y$domain))))
    ratio <- if (isTRUE(built$sf_geographic)) {
      # Clamp away from the poles: cos(+-90deg) == 0 would make the aspect
      # infinite for pole-centred lon/lat.
      mean_lat <- max(min(mean(built$scales$y$domain), 89.9), -89.9)
      1 / cos(mean_lat * pi / 180)
    } else {
      1
    }
    panel_w <- vellum::vl_unit(xr, "null")
    panel_h <- vellum::vl_unit(ratio * yr, "null")
    respect <- TRUE
  } else if (!is.null(asp)) {
    panel_h <- vellum::vl_unit(asp, "null")
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
  # Tick labels honour the element's rotation (`axis.text.x/y` angle), so the
  # gutter/row is sized from the *rotated* extent. The x tick-label row height is
  # deferred to below (it also depends on the wrap budget, which needs the legend
  # placement resolved first).
  xrot <- .el_rot(rt[["axis.text.x"]])
  yrot <- .el_rot(rt[["axis.text.y"]])
  yt <- .track_w(rt[["axis.title.y"]], vsc$name, .PAD_MM, rot = 90)
  yl <- .track_w(
    rt[["axis.text.y"]],
    .longest(v_labs),
    tick + .PAD_MM,
    rot = yrot
  )
  xt <- .track_h(rt[["axis.title.x"]], hsc$name, .PAD_MM)
  # Secondary axes (opposite edge): a right-column pair for the vertical scale's
  # sec axis, a top-row pair for the horizontal scale's. Only sized when present,
  # so a plot without a secondary axis keeps its original track layout.
  y_sec <- vsc$sec
  x_sec <- hsc$sec
  has_y_sec <- !is.null(y_sec)
  has_x_sec <- !is.null(x_sec)
  y2l <- if (has_y_sec) {
    .track_w(
      rt[["axis.text.y"]],
      .longest(y_sec$labels),
      tick + .PAD_MM,
      rot = yrot
    )
  }
  y2t <- if (has_y_sec) {
    .track_w(rt[["axis.title.y"]], y_sec$name, .PAD_MM, rot = 90)
  }
  x2l <- if (has_x_sec) {
    .track_h(
      rt[["axis.text.x"]],
      .longest(x_sec$labels),
      tick + .PAD_MM,
      rot = xrot
    )
  }
  x2t <- if (has_x_sec) .track_h(rt[["axis.title.x"]], x_sec$name, .PAD_MM)
  # Polar panels carry their axis labels/titles inside the square panel, so the
  # four cartesian gutter tracks collapse to zero.
  if (polar) {
    yt <- yl <- xl <- xt <- vellum::vl_unit(0, "mm")
    has_y_sec <- has_x_sec <- FALSE
  }
  strip <- .track_h(rt[["strip.text"]], "Ag", 2 * .PAD_MM)
  gap <- vellum::vl_unit(rt[["panel.spacing"]], "mm")

  # Only grid facets get a dedicated column-strip row here (wrap strips flow
  # through the per-panel wrapstrip_row instead).
  has_col_strip <- fa$type == "grid" && !is.null(fa$col_labels)
  has_row_strip <- fa$type == "grid" && !is.null(fa$row_labels)

  # Legend placement. "right"/"left" take a column beside the panels (vertical
  # guide stack); "top"/"bottom" take a row spanning the panel columns
  # (horizontal guide flow); "none" suppresses it.
  pos <- rt[["legend.position"]]
  show_legend <- length(guides) && !identical(pos, "none")
  legend_vert <- show_legend && pos %in% c("left", "right")
  legend_horiz <- show_legend && pos %in% c("top", "bottom")

  # Per-x-tick wrap budget (mm): the panel's mm width shared among its x ticks, so
  # a long discrete label wraps to its own column instead of running into its
  # neighbour. Only knowable when the page width is set (`band_w`), the labels are
  # unrotated, the x scale is shared, and the panel is a plain (unweighted) null
  # track -- aspect/fixed/sf/polar/marginal give the panel a weighted null width
  # that does not resolve to mm here, so those fall back to single-line labels.
  # Panel mm = content box less the fixed flanking columns (y title/labels, any
  # secondary y pair, a row strip, a vertical legend) and the inter-panel gaps,
  # split across the C panel columns.
  xwrap_mm <- NA_real_
  can_wrap_x <- is.finite(band_w) &&
    xrot == 0 &&
    !respect &&
    !polar &&
    is.null(marginal) &&
    !free_x &&
    length(hsc$breaks) > 0L &&
    !.is_blank(rt[["axis.text.x"]])
  if (can_wrap_x) {
    leg_w <- if (legend_vert) {
      .legend_width(guides, rt, legend_avail_h)
    } else {
      vellum::vl_unit(0, "mm")
    }
    side <- c(
      list(yt, yl, leg_w),
      if (has_row_strip) list(strip),
      if (has_y_sec) list(y2l, y2t)
    )
    side_mm <- sum(vapply(
      side,
      function(u) vellum::vl_convert(u, "mm"),
      numeric(1)
    ))
    gaps_mm <- vellum::vl_convert(gap, "mm") * (C - 1L)
    per_panel_mm <- (band_w - side_mm - gaps_mm) / C
    # Any track that does not resolve to a plain mm (a null width slipping
    # through) leaves `per_panel_mm` non-finite -- fall back to single-line.
    if (isTRUE(per_panel_mm > 0)) {
      budget <- per_panel_mm / length(hsc$breaks)
      # Only engage wrapping when the widest label actually overruns its budget,
      # so every plot whose labels already fit keeps its exact single-line layout
      # (measured through the same grob the drawer builds).
      el_x <- rt[["axis.text.x"]]
      natural <- vellum::vl_convert(
        vellum::grobwidth(.axis_text_grob(
          .longest(h_labs),
          el_x,
          c("centre", "top")
        )),
        "mm"
      )
      if (isTRUE(natural > budget)) {
        xwrap_mm <- budget
      }
    }
  }
  # The x tick-label row height, now that the wrap budget is known (deferred from
  # the gutter block above). Polar keeps the collapsed zero set earlier.
  if (!polar) {
    xl <- .track_h_axis(
      rt[["axis.text.x"]],
      .longest(h_labs),
      tick + .PAD_MM,
      rot = xrot,
      wrap_mm = xwrap_mm
    )
  }

  # --- columns: [ legend(left)? | ytitle | (ylabels panel gap)xC |
  #               row-strip? | legend(right)? ] ---
  W <- .tracks()
  legend_col <- NA_integer_
  if (legend_vert && pos == "left") {
    legend_col <- .tk_add(W, .legend_width(guides, rt, legend_avail_h))
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
  if (has_right) {
    .tk_add(W, gap)
    marg_right_col <- .tk_add(W, marg)
  }
  # A secondary y-axis sits just right of the panels (labels then title),
  # inside any row-strip / right legend.
  y2labels_col <- NA_integer_
  y2title_col <- NA_integer_
  if (has_y_sec) {
    y2labels_col <- .tk_add(W, y2l)
    y2title_col <- .tk_add(W, y2t)
  }
  rowstrip_col <- if (has_row_strip) .tk_add(W, strip) else NA_integer_
  if (legend_vert && pos == "right") {
    legend_col <- .tk_add(W, .legend_width(guides, rt, legend_avail_h))
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
    .tk_add(
      H,
      .track_h_band(rt[["plot.title"]], labels$title, 2 * .PAD_MM, band_w)
    )
  } else {
    NA_integer_
  }
  subtitle_row <- if (!is.null(labels$subtitle)) {
    .tk_add(
      H,
      .track_h_band(rt[["plot.subtitle"]], labels$subtitle, .PAD_MM, band_w)
    )
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
  # A secondary x-axis sits just above the panels (title outermost, then labels),
  # inside any column-strip / top legend.
  x2title_row <- NA_integer_
  x2labels_row <- NA_integer_
  if (has_x_sec) {
    x2title_row <- .tk_add(H, x2t)
    x2labels_row <- .tk_add(H, x2l)
  }
  panel_row <- integer(R)
  xlabels_row <- integer(R)
  wrapstrip_row <- integer(R)
  for (r in seq_len(R)) {
    if (fa$type == "wrap") {
      wrapstrip_row[r] <- .tk_add(H, strip)
    }
    if (r == 1L && has_top) {
      marg_top_row <- .tk_add(H, marg)
      .tk_add(H, gap)
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
    .tk_add(
      H,
      .track_h_band(rt[["plot.caption"]], labels$caption, .PAD_MM, band_w)
    )
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
    y2labels_col = y2labels_col,
    y2title_col = y2title_col,
    x2labels_row = x2labels_row,
    x2title_row = x2title_row,
    marg_top_row = marg_top_row,
    marg_right_col = marg_right_col,
    legend_col = legend_col,
    legend_row = legend_row,
    legend_pos = pos,
    legend_avail_h = legend_avail_h,
    title_row = title_row,
    subtitle_row = subtitle_row,
    tag_row = tag_row,
    caption_row = caption_row,
    ncol_total = length(W$u),
    nrow_total = length(H$u),
    band_w = band_w,
    xwrap_mm = xwrap_mm,
    respect = respect
  )
}
