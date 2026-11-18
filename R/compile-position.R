#' @include classes.R compile-resolve.R
NULL

# Which position adjustments each mark supports. `identity` and `nudge` work on
# every mark and are not listed. A mark asked for anything else errors when the
# layer is built (`.check_position()`), rather than silently drawing unadjusted.
#
# Where each one runs:
#   * stack / fill -- data space, before training (`.apply_position()`): a
#     bar/area gets a `[ymin, ymax]` span; a point/text/label moves to a height
#     inside that span (`position_stack(vjust =)`), so labels sit on the bars
#     they annotate.
#   * dodge / dodge2 / jitter -- native space, at draw time. Bars, points
#     (jitter/jitterdodge/sina) and the distribution marks lay these out
#     themselves; every other supporting mark goes through the generic
#     `.position_native()` step, which shifts its x before the emitter runs.
.POSITION_MARKS <- local({
  stackable <- c("bar", "area", "point", "text", "label")
  dodgeable <- c(
    "bar",
    "point",
    "text",
    "label",
    "errorbar",
    "linerange",
    "pointrange",
    "crossbar",
    "boxplot",
    "violin",
    "halfeye",
    "interval"
  )
  list(
    stack = stackable,
    fill = stackable,
    dodge = dodgeable,
    dodge2 = dodgeable,
    jitter = c("point", "text", "label"),
    jitterdodge = "point",
    sina = "point"
  )
})

# Marks that lay out a dodge / jitter themselves; the generic native step skips
# them.
.NATIVE_POSITION_MARKS <- c(
  "bar",
  "point",
  "boxplot",
  "violin",
  "ridgeline",
  "halfeye",
  "interval"
)

# The positions `mark` accepts, in a stable order for messages.
.positions_for <- function(mark) {
  ok <- names(.POSITION_MARKS)[vapply(
    .POSITION_MARKS,
    function(m) mark %in% m,
    logical(1)
  )]
  c("identity", "nudge", ok)
}

# Abort when a layer asks for a position its mark does not support. `fn` names
# the user-facing constructor (e.g. `mark_histogram`, whose layer mark is "bar").
.check_position <- function(mark, type, fn = NULL, call = rlang::caller_env()) {
  ok <- .positions_for(mark)
  if (type %in% ok) {
    return(invisible(type))
  }
  known <- unique(c(ok, names(.POSITION_MARKS)))
  what <- if (is.null(fn)) "The {.val {mark}} mark" else "{.fn {fn}}"
  cli::cli_abort(
    c(
      paste0(what, " does not support {.code position = \"{type}\"}."),
      i = "It supports: {.val {ok}}.",
      if (!type %in% known) {
        c(x = "{.val {type}} is not a position adjustment.")
      }
    ),
    call = call
  )
}

# Apply a layer's position adjustment that must run *before* training (because
# it changes the data extent). `stack`/`fill` give each bar/area a `ymin`/`ymax`
# span, and place a point/text/label inside the matching span; `dodge` and
# `jitter` are deferred to draw time (they only shift native geometry within the
# trained scales). Identity is a no-op.
.apply_position <- function(L) {
  # Nudge shifts every element by a constant in data units (continuous axes),
  # before training so the shifted positions stay in view. Any mark.
  if (identical(L$position, "nudge")) {
    return(.position_nudge(L))
  }
  if (!L$position %in% c("stack", "fill") || is.null(L$values$y)) {
    return(L)
  }
  if (L$mark %in% c("bar", "area")) {
    return(.position_stack(L, fill = identical(L$position, "fill")))
  }
  if (L$mark %in% .POSITION_MARKS$stack) {
    # A located mark takes a height inside its stacked span: `vjust = 1` (the
    # default) its top, `0.5` its middle -- where a label over a stacked bar
    # segment belongs. The span itself is not kept: these marks have no extent.
    S <- .position_stack(L, fill = identical(L$position, "fill"))
    vj <- L$stat_params$stack_vjust %||% 1
    L$values$y <- S$values$ymin + vj * (S$values$ymax - S$values$ymin)
    return(L)
  }
  L
}

# Dodge offsets: element i sits at category `x[i]` in group `grp[i]`; returns
# its `offset` from the category centre and its `share` of the dodged span (the
# fraction of `span` its slot gets). `span` (native units, scalar or per row) is
# the total width the groups at one category share. The one implementation of
# `(rank - (G + 1) / 2) / G * span` -- bars, jitterdodge, dodge2, the generic
# native step and the distribution marks all call it, so dodged elements of
# different marks land on exactly the same centres.
#
# `ragged = FALSE` (dodge) gives every category a slot for every group level, so
# a group sits at the same offset everywhere. `ragged = TRUE` (dodge2) ranks only
# the groups present at each category, so a category with fewer groups centres
# them and gives each a wider slot.
.dodge_offsets <- function(x, grp, span, ragged = FALSE) {
  n <- length(x)
  if (is.null(grp) || !n) {
    return(list(offset = rep(0, n), share = rep(1, n)))
  }
  span <- rep_len(span, n)
  levs <- .cat_levels(grp)
  rank <- match(as.character(rep_len(grp, n)), levs)
  if (!ragged) {
    G <- length(levs)
    return(list(
      offset = (rank - (G + 1) / 2) / G * span,
      share = rep(1 / G, n)
    ))
  }
  offset <- numeric(n)
  share <- numeric(n)
  key <- as.character(x)
  for (rows in split(seq_len(n), factor(key, levels = unique(key)))) {
    present <- sort(unique(rank[rows]))
    k <- length(present)
    r <- match(rank[rows], present)
    offset[rows] <- (r - (k + 1) / 2) / k * span[rows]
    share[rows] <- 1 / k
  }
  list(offset = offset, share = share)
}

# The x- / y-family aesthetics a position shift moves together.
.X_AES <- c("x", "xend", "xmin", "xmax")
.Y_AES <- c("y", "yend", "ymin", "ymax")

# The generic draw-time position step, for every mark that does not lay out its
# own dodge / jitter (`.NATIVE_POSITION_MARKS`). It maps the layer's x-family
# values to native coordinates, shifts them, and hands the emitter an x scale
# whose `map` is the identity -- so the emitter draws at the shifted positions
# without knowing a position ran. Under dodge, a mark with a cap / box width
# (errorbar, crossbar) also narrows it to its slot, as a dodged bar does.
.position_native <- function(L, scales) {
  type <- L$position %||% "identity"
  if (
    !type %in% c("dodge", "dodge2", "jitter") ||
      L$mark %in% .NATIVE_POSITION_MARKS ||
      is.null(L$values$x) ||
      !L$n
  ) {
    return(list(L = L, scales = scales))
  }
  n <- L$n
  sx <- scales$x
  xn <- rep_len(sx$map(L$values$x), n)
  band <- sx$band_width %||% .resolution(xn)
  if (identical(type, "jitter")) {
    # The same defaults as a jittered point: 40% of the resolution each way.
    sy <- scales$y
    yn <- rep_len(sy$map(L$values$y), n)
    ax <- L$stat_params$jitter_width %||% (0.4 * band)
    ay <- L$stat_params$jitter_height %||% (0.4 * .resolution(yn))
    jit <- .with_seed(
      L$stat_params$seed,
      list(x = stats::runif(n, -ax, ax), y = stats::runif(n, -ay, ay))
    )
    off <- jit$x
    for (a in intersect(.Y_AES, names(L$values))) {
      L$values[[a]] <- rep_len(sy$map(L$values[[a]]), n) + jit$y
    }
    sy$map <- function(v) v
    scales$y <- sy
  } else {
    d <- .dodge_offsets(
      xn,
      .group_key(L),
      L$stat_params$dodge_width %||% band,
      ragged = identical(type, "dodge2")
    )
    off <- d$offset
    if (L$mark %in% c("errorbar", "crossbar")) {
      w <- L$values$width %||% .aes_param(L, "width", 0.5)
      L$values$width <- rep_len(w, n) * d$share
    }
  }
  for (a in intersect(.X_AES, names(L$values))) {
    L$values[[a]] <- rep_len(sx$map(L$values[[a]]), n) + off
  }
  sx$map <- function(v) v
  # The band is kept (marks size caps / boxes from it); the trained domain is in
  # native units already.
  sx$band_width <- band
  scales$x <- sx
  list(L = L, scales = scales)
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
# within the x group (ordered by the `.group_key()` level). `fill = TRUE` normalises
# each x group's total to 1.
.position_stack <- function(L, fill = FALSE) {
  n <- L$n
  # Recycle to n: a constant x (e.g. a pie's `x = factor(1)`) resolves to length
  # one, but stacking groups by row, so every value must be n long.
  x <- as.character(rep_len(L$values$x, n))
  y <- as.numeric(rep_len(L$values$y, n))
  # A non-finite height would poison the whole x-group's cumulative sum (and, under
  # fill, every proportion in the stack). ggplot2 drops NA rows before stacking;
  # treat non-finite as a zero-height contribution so the rest of the stack is sound.
  y[!is.finite(y)] <- 0
  grp <- .group_key(L)
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
