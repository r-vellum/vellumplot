#' @include classes.R compile-resolve.R
NULL

# Apply a layer's position adjustment that must run *before* training (because
# it changes the data extent). `stack`/`fill` give each bar a `ymin`/`ymax`
# span; `dodge` and `jitter` are deferred to draw time (they only shift native
# geometry within the trained scales). Identity is a no-op.
.apply_position <- function(L) {
  if (!identical(L$mark, "bar")) {
    return(L)
  }
  if (L$position %in% c("stack", "fill") && !is.null(L$values$y)) {
    return(.position_stack(L, fill = identical(L$position, "fill")))
  }
  L
}

# Stack bars sharing an x: assign each a [ymin, ymax] span by cumulating heights
# within the x group (ordered by colour/fill level). `fill = TRUE` normalises
# each x group's total to 1.
.position_stack <- function(L, fill = FALSE) {
  n <- L$n
  x <- as.character(L$values$x)
  y <- as.numeric(L$values$y)
  grp <- L$values$color %||% L$values$fill
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
