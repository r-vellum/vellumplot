#' @include classes.R
NULL

# Typographic constants (font sizes in pt; paddings in mm).
.AXIS_FS <- 9
.TITLE_FS <- 11
.LEGEND_FS <- 9
.LEGEND_TITLE_FS <- 10
.STRIP_FS <- 9
.TICK_MM <- 1.5
.PAD_MM <- 1.4
.PANEL_GAP_MM <- 1.6
.LEGEND_BAR_MM <- 6
.LEGEND_BAR_H_NPC <- 0.6 # gradient bar height as a fraction of the legend cell
.LEGEND_SWATCH_MM <- 4

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

# Build the panel + gutter layout. Columns are
#   [ y-title | y-labels | panel(null) | legend? ]
# and rows are
#   [ panel(null) | x-labels | x-title ].
# Gutter tracks are absolute (mm) from grobwidth/grobheight measurement; the
# panel track is `null` so it absorbs the remaining space.
# Width of the legend column: room for the widest swatch/bar/key needed by any
# guide, plus the widest label or title across all guides.
.legend_width <- function(guides) {
  strs <- character(0)
  swatch <- .LEGEND_SWATCH_MM
  for (g in guides) {
    sc <- g$sc
    if (g$kind == "color_continuous") {
      strs <- c(strs, sc$legend_labels, sc$name)
      swatch <- max(swatch, .LEGEND_BAR_MM)
    } else if (g$kind == "color_discrete") {
      strs <- c(strs, sc$levels, sc$name)
    } else if (g$kind == "size") {
      strs <- c(strs, sc$legend_labels, sc$name)
      swatch <- max(swatch, 2 * max(sc$legend_sizes))
    }
  }
  vellum::grobwidth(.txt(.longest(strs), .LEGEND_TITLE_FS)) +
    vellum::unit(swatch + 3 * .PAD_MM, "mm")
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
# case reduces to the v1 single-panel layout.
.build_layout <- function(built, guides = list()) {
  fa <- built$fa
  R <- fa$R
  C <- fa$C
  sx <- built$scales$x
  sy <- built$scales$y
  free_x <- built$free_x
  free_y <- built$free_y

  # Gutter sizes (measured from the widest labels across all panels).
  y_labs <- unique(unlist(lapply(built$panels, function(p) p$y_sc$labels)))
  x_labs <- unique(unlist(lapply(built$panels, function(p) p$x_sc$labels)))
  yt <- vellum::grobwidth(.txt(sy$name, .TITLE_FS, rot = 90)) +
    vellum::unit(.PAD_MM, "mm")
  yl <- vellum::grobwidth(.txt(.longest(y_labs), .AXIS_FS)) +
    vellum::unit(.TICK_MM + .PAD_MM, "mm")
  xl <- vellum::grobheight(.txt(.longest(x_labs), .AXIS_FS)) +
    vellum::unit(.TICK_MM + .PAD_MM, "mm")
  xt <- vellum::grobheight(.txt(sx$name, .TITLE_FS)) +
    vellum::unit(.PAD_MM, "mm")
  strip <- vellum::grobheight(.txt("Ag", .STRIP_FS)) +
    vellum::unit(2 * .PAD_MM, "mm")
  gap <- vellum::unit(.PANEL_GAP_MM, "mm")

  has_col_strip <- fa$type == "wrap" ||
    (fa$type == "grid" && !is.null(fa$col_labels))
  has_row_strip <- fa$type == "grid" && !is.null(fa$row_labels)

  # --- columns: [ ytitle | (ylabels panel gap)xC | row-strip? | legend? ] ---
  W <- .tracks()
  ytitle_col <- .tk_add(W, yt)
  panel_col <- integer(C)
  ylabels_col <- integer(C)
  shared_yl <- if (!free_y) .tk_add(W, yl) else NA_integer_
  for (c in seq_len(C)) {
    ylabels_col[c] <- if (free_y) .tk_add(W, yl) else shared_yl
    panel_col[c] <- .tk_add(W, vellum::unit(1, "null"))
    if (c < C) .tk_add(W, gap)
  }
  rowstrip_col <- if (has_row_strip) .tk_add(W, strip) else NA_integer_
  legend_col <- if (length(guides)) {
    .tk_add(W, .legend_width(guides))
  } else {
    NA_integer_
  }

  # --- rows: [ col-strip? | (strip? panel xlabels?)xR | xlabels? | xtitle ] ---
  H <- .tracks()
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
    panel_row[r] <- .tk_add(H, vellum::unit(1, "null"))
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
    legend_col = legend_col
  )
}
