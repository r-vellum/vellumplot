#' @include classes.R compile-marks.R
NULL

#' Waffle chart
#'
#' `vwaffle()` draws a **waffle** (square pie): a grid of cells coloured by
#' category, where each category takes a share of the cells proportional to its
#' count — a part-of-whole chart that is easier to read than a pie because the
#' eye counts squares. It is a self-contained chart (like [vsankey()] /
#' [vvenn()]): it returns a [PlotSpec] with no axes.
#'
#' @param data A data frame.
#' @param category The categorical column that colours the cells (tidy-eval).
#' @param value Optional per-row weight column; if omitted, each row counts once.
#' @param n_cells Total number of cells in the grid (default `100`). Each
#'   category gets `round(share * n_cells)` cells.
#' @param rows Number of rows in the grid (default `10`); cells fill column by
#'   column from the bottom-left.
#' @param flip Fill row by row (left to right) instead of column by column.
#' @param pad Gap between cells, as a fraction of a cell (default `0.12`).
#' @param width,height,dpi Page size (inches) and resolution for the standalone
#'   chart.
#' @return A [PlotSpec].
#' @seealso [vvenn()], [mark_pie()]
#' @examples
#' df <- data.frame(part = c("a", "b", "c"), n = c(50, 30, 20))
#' vwaffle(df, category = part, value = n)
#' @export
vwaffle <- function(
  data,
  category,
  value = NULL,
  n_cells = 100,
  rows = 10,
  flip = FALSE,
  pad = 0.12,
  width = 5,
  height = 5,
  dpi = 96
) {
  .check_dim(width, "width")
  .check_dim(height, "height")
  .check_dpi(dpi)
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame.")
  }
  category <- rlang::enquo(category)
  value <- rlang::enquo(value)
  cat_v <- as.character(rlang::eval_tidy(category, data))
  levs <- unique(cat_v[!is.na(cat_v)])
  if (!length(levs)) {
    cli::cli_abort("{.arg category} has no non-missing values.")
  }
  counts <- if (rlang::quo_is_null(value)) {
    as.numeric(table(factor(cat_v, levs))[levs])
  } else {
    v <- as.numeric(rlang::eval_tidy(value, data))
    as.numeric(tapply(v, factor(cat_v, levs), sum, na.rm = TRUE)[levs])
  }
  counts[!is.finite(counts)] <- 0

  # Allocate cells per category proportional to its share, largest-remainder so
  # the cells sum to exactly n_cells.
  n_cells <- as.integer(n_cells)
  share <- counts / sum(counts)
  base <- floor(share * n_cells)
  rem <- n_cells - sum(base)
  if (rem > 0) {
    ord <- order(share * n_cells - base, decreasing = TRUE)
    base[ord[seq_len(rem)]] <- base[ord[seq_len(rem)]] + 1L
  }
  cells <- factor(rep(levs, times = base), levels = levs)
  # Name the fill column after the category expression, so the legend title reads
  # naturally (e.g. "part") rather than an internal name.
  cat_name <- rlang::as_label(rlang::quo_get_expr(category))
  d <- data.frame(cells)
  names(d) <- cat_name

  p <- PlotSpec(
    data = d,
    coord = CoordSpec(kind = "fixed", ratio = 1),
    theme = .theme_vgraph(),
    width = as.double(width),
    height = as.double(height),
    dpi = as.double(dpi)
  )
  .add_layer(
    p,
    "waffle",
    rlang::quos(fill = .data[[!!cat_name]]),
    const_params = list(
      rows = as.integer(rows),
      flip = isTRUE(flip),
      pad = as.double(pad)
    )
  )
}

# Draw the waffle grid directly in npc (a self-contained chart). Cells are
# coloured by the trained fill scale, so a discrete legend of the categories is
# emitted automatically. Cells fill column by column from the bottom-left (or row
# by row with `flip`).
.emit_waffle <- function(scene, L, scales) {
  n <- L$n
  if (!n) {
    return(scene)
  }
  fill <- rep_len(.aes_colour(L, scales, "grey50"), n)
  rows <- L$params$rows %||% 10L
  flip <- isTRUE(L$params$flip)
  pad <- L$params$pad %||% 0.12
  cols <- ceiling(n / rows)
  cw <- 1 / cols
  ch <- 1 / rows
  i0 <- seq_len(n) - 1L
  if (flip) {
    ri <- rows - 1L - (i0 %/% cols)
    cxi <- i0 %% cols
  } else {
    cxi <- i0 %/% rows
    ri <- i0 %% rows
  }
  cx <- (cxi + 0.5) * cw
  cy <- (ri + 0.5) * ch
  npc <- function(u) vellum::vl_unit(u, "npc")
  sk <- .mark_sketch(L, scales)
  for (idx in .style_groups(n, list(fill = fill))) {
    scene <- .draw(
      scene,
      vellum::rect_grob(
        x = npc(cx[idx]),
        y = npc(cy[idx]),
        width = npc(rep(cw * (1 - pad), length(idx))),
        height = npc(rep(ch * (1 - pad), length(idx))),
        sketch = sk,
        gp = vellum::vl_gpar(fill = fill[idx[1]], col = NA)
      ),
      rows = idx
    )
  }
  scene
}
