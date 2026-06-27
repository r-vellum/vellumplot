#' @include classes.R
NULL

# Typographic constants (font sizes in pt; paddings in mm).
.AXIS_FS <- 9
.TITLE_FS <- 11
.LEGEND_FS <- 9
.LEGEND_TITLE_FS <- 10
.TICK_MM <- 1.5
.PAD_MM <- 1.4
.LEGEND_BAR_MM <- 6
.LEGEND_BAR_H_NPC <- 0.6 # gradient bar height as a fraction of the legend cell
.LEGEND_SWATCH_MM <- 4

# The longest string in a vector (by character count); "" for an empty vector.
.longest <- function(x) {
  x <- x[nzchar(x)]
  if (!length(x)) return("0")
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
.build_layout <- function(scales) {
  yt <- vellum::grobwidth(.txt(scales$y$name, .TITLE_FS, rot = 90)) +
    vellum::unit(.PAD_MM, "mm")
  yl <- vellum::grobwidth(.txt(.longest(scales$y$labels), .AXIS_FS)) +
    vellum::unit(.TICK_MM + .PAD_MM, "mm")
  xl <- vellum::grobheight(.txt(.longest(scales$x$labels), .AXIS_FS)) +
    vellum::unit(.TICK_MM + .PAD_MM, "mm")
  xt <- vellum::grobheight(.txt(scales$x$name, .TITLE_FS)) +
    vellum::unit(.PAD_MM, "mm")

  widths <- c(yt, yl, vellum::unit(1, "null"))
  heights <- c(vellum::unit(1, "null"), xl, xt)

  cells <- list(
    panel = list(row = 1, col = 3),
    ytitle = list(row = 1, col = 1),
    ylabel = list(row = 1, col = 2),
    xlabel = list(row = 2, col = 3),
    xtitle = list(row = 3, col = 3)
  )

  if (!is.null(scales$color)) {
    cl <- scales$color
    strs <- if (cl$kind == "continuous") c(cl$legend_labels, cl$name) else c(cl$levels, cl$name)
    swatch <- if (cl$kind == "continuous") .LEGEND_BAR_MM else .LEGEND_SWATCH_MM
    lw <- vellum::grobwidth(.txt(.longest(strs), .LEGEND_TITLE_FS)) +
      vellum::unit(swatch + 3 * .PAD_MM, "mm")
    widths <- c(widths, lw)
    cells$legend <- list(row = 1, col = 4)
  }

  list(widths = widths, heights = heights, cells = cells)
}
