#' @include classes.R compile-resolve.R
NULL

# Apply a layer's position adjustment that must run *before* training (because
# it changes the data extent). `stack`/`fill` give each bar/area a `ymin`/`ymax`
# span; `dodge` and `jitter` are deferred to draw time (they only shift native
# geometry within the trained scales). Identity is a no-op.
.apply_position <- function(L) {
  # Nudge shifts every element by a constant in data units (continuous axes),
  # before training so the shifted positions stay in view. Any mark.
  if (identical(L$position, "nudge")) {
    return(.position_nudge(L))
  }
  if (!L$mark %in% c("bar", "area")) {
    return(L)
  }
  if (L$position %in% c("stack", "fill") && !is.null(L$values$y)) {
    return(.position_stack(L, fill = identical(L$position, "fill")))
  }
  L
}

# Shift numeric x / y by the nudge amount (data units). A non-numeric (discrete)
# axis is left untouched, since a data-unit shift has no meaning there.
.position_nudge <- function(L) {
  nx <- L$stat_params$nudge_x %||% 0
  ny <- L$stat_params$nudge_y %||% 0
  if (nx != 0 && is.numeric(L$values$x)) {
    L$values$x <- L$values$x + nx
  }
  if (ny != 0 && is.numeric(L$values$y)) {
    L$values$y <- L$values$y + ny
  }
  L
}

# Stack bars sharing an x: assign each a [ymin, ymax] span by cumulating heights
# within the x group (ordered by colour/fill level). `fill = TRUE` normalises
# each x group's total to 1.
.position_stack <- function(L, fill = FALSE) {
  n <- L$n
  # Recycle to n: a constant x (e.g. a pie's `x = factor(1)`) resolves to length
  # one, but stacking groups by row, so every value must be n long.
  x <- as.character(rep_len(L$values$x, n))
  y <- as.numeric(rep_len(L$values$y, n))
  grp <- L$values$color %||% L$values$fill
  if (!is.null(grp)) {
    grp <- rep_len(grp, n)
  }
  ymin <- numeric(n)
  ymax <- numeric(n)
  if (is.null(grp)) {
    ymin[] <- 0
    ymax <- y
  } else {
    order_levels <- .cat_levels(grp)
    g <- as.character(grp)
    for (xi in unique(x)) {
      rows <- which(x == xi)
      rows <- rows[order(match(g[rows], order_levels))]
      total <- sum(y[rows])
      cum <- 0
      for (i in rows) {
        h <- if (fill && total != 0) y[i] / total else y[i]
        ymin[i] <- cum
        ymax[i] <- cum + h
        cum <- cum + h
      }
    }
  }
  L$values$ymin <- ymin
  L$values$ymax <- ymax
  L$values$y <- ymax # the top, for any y reference / training
  L
}
