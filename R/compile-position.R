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
    S <- .stack_spans(L, fill = identical(L$position, "fill"))
    vj <- L$stat_params$stack_vjust %||% 1
    L$values$y <- S$start + vj * (S$end - S$start)
    return(L)
  }
  L
}

# Dodge offsets: element i sits at category `x[i]` in group `grp[i]`; returns
# its `offset` from the category centre and its `share` of the dodged span (the
# fraction of `span` its slot gets). `span` (native units, scalar or per row) is
# the total width the groups at one category share. The one implementation of
# ggplot2's `pos_dodge()` -- bars, points, jitterdodge, dodge2, the generic
# native step and the distribution marks all call it, so dodged elements of
# different marks land on exactly the same centres.
#
# At each category only the groups present there are ranked (in group-level
# order), as ggplot2 does. `preserve = "total"` (ggplot2's default) splits the
# span between those `k` groups, so a category with fewer groups centres them
# and gives each a wider slot. `preserve = "single"` gives every slot the width
# of one group at the fullest category (`n` groups), packing a sparser
# category's groups from the left: `(rank - 0.5) / n - 0.5` of the span.
# ggplot2 takes that `n` over the whole layer, every facet panel included, not
# just the categories drawn here: pass it as `n` (`L$dodge_n`, see
# `.share_dodge_n()`) so every panel's slots are equally wide (VP-63).
.dodge_offsets <- function(x, grp, span, preserve = "total", n_single = NULL) {
  n <- length(x)
  if (is.null(grp) || !n) {
    return(list(offset = rep(0, n), share = rep(1, n)))
  }
  span <- rep_len(span, n)
  rank <- match(as.character(rep_len(grp, n)), .cat_levels(grp))
  key <- as.character(x)
  cells <- split(seq_len(n), factor(key, levels = unique(key)))
  r <- integer(n)
  k <- integer(n)
  for (rows in cells) {
    present <- sort(unique(rank[rows]))
    r[rows] <- match(rank[rows], present)
    k[rows] <- length(present)
  }
  if (identical(preserve, "single")) {
    k[] <- max(k, n_single)
  }
  list(offset = ((r - 0.5) / k - 0.5) * span, share = 1 / k)
}

# The default dodge span, as a fraction of the band: a bar's width, so error
# bars, text and points dodged without a `position_dodge(width =)` land on the
# centres of dodged bars.
.DODGE_WIDTH <- 0.9

# How a dodged layer sizes its slots: `position_dodge(preserve =)`, else
# "total". dodge2 always splits between the groups present.
.dodge_preserve <- function(L) {
  if (identical(L$position, "dodge")) {
    L$stat_params$dodge_preserve %||% "total"
  } else {
    "total"
  }
}

# The category axis a dodged layer is dodged along: y for a horizontal box
# plot / violin / slab-interval or range mark, else x.
.dodge_axis <- function(L) {
  if (L$mark %in% c("boxplot", "violin", "halfeye", "interval")) {
    .dist_axes(.dist_orient(L))[["cat"]]
  } else if (
    L$mark %in% c("errorbar", "linerange", "pointrange") &&
      identical(.range_orient(L), "y")
  ) {
    "y"
  } else {
    "x"
  }
}

# The most groups a layer has at any one category of its dodge axis (0 for an
# empty layer, 1 for an ungrouped one).
.dodge_cell_max <- function(L) {
  if (!L$n) {
    return(0L)
  }
  g <- .group_key(L)
  x <- L$values[[.dodge_axis(L)]]
  if (is.null(g) || is.null(x)) {
    return(1L)
  }
  x <- as.character(rep_len(x, L$n))
  pair <- !duplicated(data.frame(x = x, g = as.character(g)))
  max(tabulate(match(x[pair], unique(x[pair]))))
}

# ggplot2 sizes a `position_dodge(preserve = "single")` layer's slots, and a
# `position_jitterdodge()` layer's jitter, from the most groups at any (PANEL,
# x) of the *whole* layer. Facet panels are resolved one by one, so count that
# here, over every panel's copy of each such layer, and give each copy the
# shared count as `L$dodge_n` (VP-63).
.share_dodge_n <- function(panels) {
  if (!length(panels)) {
    return(panels)
  }
  nl <- length(panels[[1]]$resolved)
  same <- vapply(panels, function(p) length(p$resolved) == nl, logical(1))
  if (!nl || !all(same)) {
    return(panels)
  }
  for (j in seq_len(nl)) {
    L1 <- panels[[1]]$resolved[[j]]
    pos <- L1$position %||% "identity"
    if (
      !identical(pos, "jitterdodge") &&
        !(identical(pos, "dodge") && identical(.dodge_preserve(L1), "single"))
    ) {
      next
    }
    n <- max(vapply(
      panels,
      function(p) as.integer(.dodge_cell_max(p$resolved[[j]])),
      integer(1)
    ))
    for (i in seq_along(panels)) {
      panels[[i]]$resolved[[j]]$dodge_n <- n
    }
  }
  panels
}

# The x- / y-family aesthetics a position shift moves together.
.X_AES <- c("x", "xend", "xmin", "xmax")
.Y_AES <- c("y", "yend", "ymin", "ymax")

# The generic draw-time position step, for every mark that does not lay out its
# own dodge / jitter (`.NATIVE_POSITION_MARKS`). It maps the layer's x-family
# values to native coordinates, shifts them, and hands the emitter an x scale
# whose `map` is the identity -- so the emitter draws at the shifted positions
# without knowing a position ran. Under dodge, a mark with a cap / box width
# (errorbar, crossbar) also narrows it to its slot, as a dodged bar does. A
# horizontal range mark (`orientation = "y"`: errorbar, linerange, pointrange)
# dodges along y, its category axis, as a horizontal box plot does.
.position_native <- function(L, scales) {
  type <- L$position %||% "identity"
  if (
    !type %in% c("dodge", "dodge2", "jitter") ||
      L$mark %in% .NATIVE_POSITION_MARKS ||
      !L$n
  ) {
    return(list(L = L, scales = scales))
  }
  # the axis the shift runs along: x, or y for a horizontal range mark
  ca <- if (
    !identical(type, "jitter") &&
      L$mark %in% c("errorbar", "linerange", "pointrange") &&
      identical(.range_orient(L), "y")
  ) {
    "y"
  } else {
    "x"
  }
  if (is.null(L$values[[ca]])) {
    return(list(L = L, scales = scales))
  }
  n <- L$n
  sx <- scales[[ca]]
  xn <- rep_len(sx$map(L$values[[ca]]), n)
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
      L$stat_params$dodge_width %||% (.DODGE_WIDTH * band),
      preserve = .dodge_preserve(L),
      n_single = L$dodge_n
    )
    off <- d$offset
    if (L$mark %in% c("errorbar", "crossbar")) {
      w <- L$values$width %||% .aes_param(L, "width", 0.5)
      L$values$width <- rep_len(w, n) * d$share
    }
  }
  fam <- if (identical(ca, "y")) .Y_AES else .X_AES
  for (a in intersect(fam, names(L$values))) {
    L$values[[a]] <- rep_len(sx$map(L$values[[a]]), n) + off
  }
  sx$map <- function(v) v
  # The band is kept (marks size caps / boxes from it); the trained domain is in
  # native units already.
  sx$band_width <- band
  scales[[ca]] <- sx
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

# Stack the rows sharing an x: give each a `[ymin, ymax]` span, as ggplot2's
# `position_stack()` does.
#   * Every row at an x stacks -- grouped or not, so ungrouped duplicates pile up
#     rather than all starting at zero.
#   * Positive and negative heights stack separately, away from zero, so mixed
#     signs never overlap.
#   * The order is ggplot2's: the last `.group_key()` level sits at the bottom and
#     the first on top, matching the legend read top-down; rows of one group keep
#     data order, the first lowest.
# `fill = TRUE` normalises each x's positive stack to `[0, 1]` and its negative
# stack to `[-1, 0]`. Areas are first interpolated onto a common x grid
# (`.align_area()`), so groups sampled at different x still stack.
.position_stack <- function(L, fill = FALSE) {
  if (identical(L$mark, "area")) {
    L <- .align_area(L)
  }
  S <- .stack_spans(L, fill)
  L$values$ymin <- pmin(S$start, S$end)
  L$values$ymax <- pmax(S$start, S$end)
  L$values$y <- S$end # the far end, for any y reference / training
  L
}

# The stacked extent of every row: `start` is the end nearer zero, `end` the far
# end (below `start` for a negative height). See `.position_stack()`.
.stack_spans <- function(L, fill = FALSE) {
  n <- L$n
  # Recycle to n: a constant x (e.g. a pie's `x = factor(1)`) resolves to length
  # one, but stacking runs by row, so every value must be n long.
  x <- rep_len(L$values$x, n)
  if (is.factor(x)) {
    x <- as.character(x)
  }
  y <- as.numeric(rep_len(L$values$y, n))
  # A non-finite height would poison the whole stack's cumulative sum (and, under
  # fill, every proportion in it). ggplot2 drops NA rows before stacking; treat
  # non-finite as a zero-height contribution so the rest of the stack is sound.
  y[!is.finite(y)] <- 0
  grp <- .group_key(L)
  rank <- if (is.null(grp)) {
    rep(1L, n)
  } else {
    match(as.character(grp), .cat_levels(grp))
  }
  start <- numeric(n)
  end <- numeric(n)
  for (rows in split(seq_len(n), match(x, unique(x)))) {
    rows <- rows[order(-rank[rows], rows)]
    for (neg in c(FALSE, TRUE)) {
      r <- rows[(y[rows] < 0) == neg]
      if (!length(r)) {
        next
      }
      h <- y[r]
      cum <- cumsum(h)
      total <- abs(cum[length(cum)])
      if (fill && total > 0) {
        h <- h / total
        cum <- cum / total
      }
      end[r] <- cum
      start[r] <- cum - h
    }
  }
  list(start = start, end = end)
}

# Interpolate a grouped area onto the union of its groups' x values, as
# ggplot2's `stat_align()` does, so groups sampled at different x still stack:
# each group gains a row at every other group's x inside its own range (y
# linearly interpolated), and drops to zero just outside an edge that ends
# inside the layer's range. A layer whose groups already share their x values,
# or whose x is not continuous, is returned unchanged. Added rows copy the
# style of their group's nearest earlier row, and provenance keys them to the
# rows they were interpolated between.
.align_area <- function(L) {
  n <- L$n
  grp <- .group_key(L)
  x0 <- L$values$x
  if (is.null(grp) || n < 2L || length(x0) != n) {
    return(L)
  }
  if (!is.numeric(x0) && !inherits(x0, c("Date", "POSIXct"))) {
    return(L)
  }
  x <- as.numeric(x0)
  y <- as.numeric(rep_len(L$values$y, n))
  ok <- is.finite(x) & is.finite(y)
  g <- as.character(grp)
  series <- split(which(ok), factor(g[ok], levels = unique(g[ok])))
  if (length(series) < 2L) {
    return(L)
  }
  ux <- sort(unique(x[ok]))
  shared <- vapply(
    series,
    function(idx) identical(sort(unique(x[idx])), ux),
    logical(1)
  )
  if (all(shared)) {
    return(L)
  }
  lo <- ux[1L]
  hi <- ux[length(ux)]
  adj <- min(diff(range(ux)) * 0.001, min(diff(ux)) / 3)
  # An edge inside the layer's range drops to zero a hair outside it.
  pads <- unlist(lapply(series, function(idx) {
    r <- range(x[idx])
    c(if (r[1] > lo) r[1] - adj, if (r[2] < hi) r[2] + adj)
  }))
  grid <- sort(unique(c(ux, pads)))

  new_x <- numeric(0)
  new_y <- numeric(0)
  new_src <- integer(0)
  new_members <- list()
  for (idx in series) {
    idx <- idx[order(x[idx])]
    xs <- x[idx]
    r <- range(xs)
    pad <- c(if (r[1] > lo) r[1] - adj, if (r[2] < hi) r[2] + adj)
    inside <- grid[grid > r[1] & grid < r[2] & !grid %in% xs]
    yi <- if (length(inside)) {
      stats::approx(xs, y[idx], xout = inside, ties = mean)$y
    } else {
      numeric(0)
    }
    gx <- c(inside, pad)
    if (!length(gx)) {
      next
    }
    # The group's nearest row at or left of each added x (its first row for a
    # left pad); the right neighbour too, for provenance.
    left <- findInterval(gx, xs)
    right <- pmin(left + 1L, length(xs))
    left[left < 1L] <- 1L
    new_x <- c(new_x, gx)
    new_y <- c(new_y, yi, rep(0, length(pad)))
    new_src <- c(new_src, idx[left])
    new_members <- c(
      new_members,
      Map(function(a, b) unique(c(a, b)), idx[left], idx[right])
    )
  }
  if (!length(new_x)) {
    return(L)
  }
  rows <- c(seq_len(n), new_src)
  for (nm in setdiff(names(L$values), c("x", "y"))) {
    v <- L$values[[nm]]
    if (length(v) == n) {
      L$values[[nm]] <- v[rows]
    }
  }
  xx <- c(x, new_x)
  L$values$x <- if (inherits(x0, "Date")) {
    structure(xx, class = "Date")
  } else if (inherits(x0, "POSIXct")) {
    .POSIXct(xx, tz = attr(x0, "tzone"))
  } else {
    xx
  }
  L$values$y <- c(y, new_y)
  members <- c(as.list(seq_len(n)), new_members)
  # Realign the interactivity to the new rows, then compose the row map with any
  # an earlier stat left, so provenance and selection still reach input rows.
  local_map <- list(rows = rows, n_in = n, n_out = length(rows))
  L <- .realign_meta(L, local_map)
  old <- L$stat_map
  L$stat_map <- if (is.null(old)) {
    list(rows = rows, members = members, n_in = n, n_out = length(rows))
  } else {
    old$members <- if (!is.null(old$members)) {
      lapply(members, function(m) unique(unlist(old$members[m])))
    }
    old$rows <- old$rows[rows]
    old$sgroup <- old$sgroup[rows]
    old$out_group <- old$out_group[rows]
    old$n_out <- length(rows)
    old
  }
  L$n <- length(rows)
  L
}
