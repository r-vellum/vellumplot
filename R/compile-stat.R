#' @include classes.R compile-resolve.R
NULL

# The grouping key of a layer: which rows belong to the same series / stat group.
# This is the ONE place grouping is derived; stats, positions and the connected
# marks all ask it rather than open-coding `color %||% fill`.
#
# Semantics (ggplot2's `add_group()`): an explicit `group` wins outright --
# mapped (`group = id`) it is the key, set as a constant (`group = 1`) it makes
# the whole layer one group. Without one, the key is the interaction of every
# *discrete* mapped aesthetic in `.GROUP_AES`, so `mark_bar(fill = gear, color =
# am)` counts, dodges and stacks one bar per (gear, am) pair (VP-52), and a
# layer split only by a pattern or a linetype groups as one split by fill does
# (VP-38). A continuous colour never groups: it varies within a series (a line
# coloured along its length), it does not split it.
#
# A layer with a single discrete grouping aesthetic -- or several that split the
# rows the same way (`color = cyl, fill = cyl`) -- returns that aesthetic's own
# vector unchanged, so its level order, and everything keyed on it, stays as it
# was. Only a genuine interaction builds a new factor (`.interaction_key()`).
#
# Returns NULL (ungrouped) or a vector recycled to the layer's row count, so a
# grouping aesthetic that resolved shorter than the data (a constant) still
# aligns 1:1 when masked by `is.finite(x)` etc. `aes` narrows the aesthetics
# the key may come from, for a stat whose groups are a narrower notion than
# the layer's (the outlier rule, `.OUTLIER_GROUP_AES`).
.group_key <- function(L, aes = .GROUP_AES) {
  g <- L$values$group
  if (is.null(g) && !is.null(L$params$group)) {
    g <- L$params$group
  }
  n <- L$n
  if (is.null(g)) {
    # Mapping order (the order of `L$values`), as ggplot2 takes its data
    # columns: the first aesthetic varies slowest in the interaction.
    disc <- list()
    for (a in intersect(names(L$values), aes)) {
      v <- L$values[[a]]
      if (length(v) && !.is_continuous_group(v)) {
        disc[[a]] <- v
      }
    }
    g <- .interaction_key(disc, n)
  }
  if (is.null(g)) {
    return(NULL)
  }
  n <- n %||% length(g)
  if (length(g) >= 1L && length(g) != n) {
    g <- rep_len(g, n)
  }
  g
}

# The aesthetics a layer without `group` takes its grouping key from: the
# interaction of the discrete ones among them, as ggplot2 groups by every
# discrete aesthetic (VP-57 added alpha, size and linewidth).
.GROUP_AES <- c(
  "color",
  "fill",
  "pattern",
  "linetype",
  "shape",
  "alpha",
  "size",
  "linewidth"
)

# The aesthetics the outlier rule (`mark_outlier_label()`) splits its data by.
# A discrete alpha, size or linewidth styles the labels it draws -- a bigger
# label for the "big" rows -- rather than splitting the distribution an outlier
# is judged against, so they are carried by row instead.
.OUTLIER_GROUP_AES <- setdiff(.GROUP_AES, c("alpha", "size", "linewidth"))

# The interaction of the discrete vectors `disc` (in order, the first varying
# slowest), or NULL for none. A vector that does not split any group further
# than the ones before it (a constant, or a duplicate mapping) is skipped, and a
# leading constant gives way to the first vector that does split; if only one is
# left it is returned as is. Otherwise the key is a factor with one
# level per combination present, ordered by each component's own level order
# (an NA category last), as ggplot2's `id()` numbers its groups. The labels join
# the components' labels with ":" (falling back to the combination number in the
# unlikely case two combinations would share a label).
.interaction_key <- function(disc, n = NULL) {
  if (!length(disc)) {
    return(NULL)
  }
  n <- n %||% max(lengths(disc))
  codes <- list()
  comb <- NULL
  for (k in seq_along(disc)) {
    v <- disc[[k]]
    if (length(v) != n) {
      v <- rep_len(v, n)
    }
    levs <- .cat_levels(v)
    code <- match(as.character(v), levs)
    code[is.na(code)] <- length(levs) + 1L
    this <- list(code = code, v = v, k = k)
    if (is.null(comb)) {
      comb <- as.character(code)
      codes <- list(this)
      next
    }
    nxt <- paste(comb, code, sep = "\r")
    if (length(unique(nxt)) == length(unique(comb))) {
      next # splits no group further
    }
    if (length(unique(comb)) == 1L) {
      # everything so far was one constant group: this one replaces it
      comb <- as.character(code)
      codes <- list(this)
    } else {
      comb <- nxt
      codes[[length(codes) + 1L]] <- this
    }
  }
  if (length(codes) == 1L) {
    return(disc[[codes[[1L]]$k]])
  }
  ord <- do.call(order, lapply(codes, `[[`, "code"))
  id <- match(comb, unique(comb[ord]))
  labs <- do.call(
    paste,
    c(
      lapply(codes, function(cc) {
        lab <- as.character(cc$v)
        lab[is.na(lab)] <- "NA"
        lab
      }),
      sep = ":"
    )
  )
  first <- match(seq_len(max(id)), id)
  levs <- labs[first]
  if (anyDuplicated(levs)) {
    levs <- as.character(seq_along(levs))
  }
  factor(levs[id], levels = levs)
}

# A vector that varies continuously (numeric / date-time), which `.group_key()`
# never splits on. A factor is discrete whatever its levels look like.
.is_continuous_group <- function(v) {
  !is.factor(v) && (is.numeric(v) || inherits(v, c("Date", "POSIXt")))
}

# Apply a layer's statistical transform, producing new `values` (and evaluating
# any `after_stat()` channels against the stat output). The identity stat is a
# no-op; a bar with no `y` defaults to the count stat.
.apply_stat <- function(L) {
  stat <- L$stat
  if (identical(L$mark, "bar") && stat == "identity" && is.null(L$values$y)) {
    stat <- "count"
  }
  if (L$mark %in% c("violin", "ridgeline", "halfeye")) {
    L <- .drop_varying_shape_style(L)
  }
  if (stat == "identity") {
    if (length(L$after)) {
      cli::cli_abort(
        "{.fn after_stat} needs a statistical mark (e.g. {.fn mark_histogram})."
      )
    }
    return(L)
  }
  # A structurally-empty input (e.g. an unpopulated facet_grid cell) has no rows
  # to aggregate. The binning/density stats abort on empty input, which would
  # kill the whole render; short-circuit to an empty layer that draws nothing so
  # the panel renders blank (matching ggplot2).
  n_in <- if (length(L$values)) max(lengths(L$values)) else 0L
  if (n_in == 0L) {
    return(.merge_stat(L, data.frame()))
  }
  .warn_nonfinite(L, stat)
  sdf <- switch(
    stat,
    count = .stat_count(L),
    sum = .stat_sum(L),
    signif = .stat_signif(L),
    series_label = .stat_series_label(L),
    outlier = .stat_outlier(L),
    bin = .stat_dropped(
      .stat_bin(L),
      which(!is.finite(as.numeric(L$values$x)))
    ),
    bin2d = .stat_bin2d(L),
    density_2d = .stat_density_2d(L),
    hexbin = .stat_hexbin(L),
    density = .stat_density(L),
    aggregate = .stat_aggregate(L),
    summary = .stat_summary(L),
    smooth = .stat_smooth(L),
    quantile = .stat_quantile(L),
    ecdf = .stat_ecdf(L),
    qq = .stat_qq(L),
    qq_line = .stat_qq_line(L),
    dotplot = .stat_dotplot(L),
    ellipse = .stat_ellipse(L),
    hull = .stat_hull(L),
    window = .stat_window(L),
    cli::cli_abort("Unknown stat {.val {stat}}.")
  )
  .merge_stat(L, sdf, stat)
}

# The continuous inputs each numeric stat reads. These stats compute only from
# finite values, so a row whose input is infinite (or missing, for marks whose
# missing rows are not removed up front) is left out of the computation.
.STAT_FINITE_INPUTS <- list(
  bin = "x",
  bin2d = c("x", "y"),
  hexbin = c("x", "y"),
  density = "x",
  density_2d = c("x", "y"),
  smooth = c("x", "y"),
  quantile = c("x", "y"),
  ecdf = "x",
  qq = "sample",
  qq_line = "sample",
  dotplot = "x",
  ellipse = c("x", "y"),
  hull = c("x", "y")
)

# The constructor behind each of those stats, for messages.
.STAT_FN <- c(
  bin = "mark_histogram",
  bin2d = "mark_bin2d",
  hexbin = "mark_hex",
  density = "mark_density",
  density_2d = "mark_contour",
  smooth = "mark_smooth",
  quantile = "mark_quantile",
  ecdf = "mark_ecdf",
  qq = "mark_qq",
  qq_line = "mark_qq_line",
  dotplot = "mark_dotplot",
  ellipse = "mark_ellipse",
  hull = "mark_hull"
)

# The constructor to name in a stat's message: `.STAT_FN`, except where two
# constructors share a stat (the bin stat draws both histograms and frequency
# polygons).
.stat_fn <- function(L, stat) {
  if (identical(stat, "bin") && identical(L$mark, "line")) {
    return("mark_freqpoly")
  }
  .STAT_FN[[stat]]
}

# Say how many rows a stat leaves out for a non-finite input, in ggplot2's
# wording ("Removed n rows containing non-finite values"), rather than dropping
# them silently. An `Inf` cannot be binned, smoothed or estimated, so ggplot2's
# stats remove such rows with this warning too.
.warn_nonfinite <- function(L, stat) {
  chans <- .STAT_FINITE_INPUTS[[stat]]
  if (is.null(chans)) {
    return(invisible())
  }
  if (stat %in% c("qq", "qq_line") && is.null(L$values$sample)) {
    chans <- if (!is.null(L$values$y)) "y" else "x"
  }
  n <- L$n %||% max(lengths(L$values))
  bad <- rep(FALSE, n)
  for (ch in chans) {
    v <- L$values[[ch]]
    if (is.numeric(v) && length(v) == n) {
      bad <- bad | !is.finite(v)
    }
  }
  if (any(bad)) {
    fn <- .stat_fn(L, stat)
    cli::cli_warn(
      "Removed {sum(bad)} row{?s} containing non-finite values ({.fn {fn}}).",
      class = "vellumplot_nonfinite_rows"
    )
  }
  invisible()
}

# A violin, ridge or slab is one polygon per category (and group), so it has
# one fill, colour and alpha. A per-row aesthetic that varies within one shape
# -- typically a continuous `fill` mapped to the value itself -- has no single
# value to paint it with. ggplot2 computes these shapes with a stat
# (`stat_ydensity()`), which drops such an aesthetic with a warning; so do we,
# before training, so no legend promises a gradient the shapes cannot show
# (VP-37). Before, each shape silently took the fill of its first row. An
# aesthetic constant within every shape (one value per category) is kept.
.drop_varying_shape_style <- function(L) {
  n <- L$n %||% max(lengths(L$values))
  cat_axis <- if (identical(L$mark, "ridgeline")) {
    "y"
  } else {
    .dist_axes(.dist_orient(L))[["cat"]]
  }
  cats <- L$values[[cat_axis]]
  if (!n || length(cats) != n) {
    return(L)
  }
  grp <- .group_key(L)
  key <- paste(
    as.character(cats),
    if (is.null(grp)) "" else as.character(grp),
    sep = "\r"
  )
  cells <- split(seq_len(n), key)
  dropped <- character(0)
  for (nm in intersect(.STAT_CARRIED, names(L$values))) {
    v <- L$values[[nm]]
    if (length(v) != n || nm == "group") {
      next
    }
    varies <- any(vapply(
      cells,
      function(i) length(unique(v[i])) > 1L,
      logical(1)
    ))
    if (varies) {
      L$values[[nm]] <- NULL
      L$types[[nm]] <- NULL
      dropped <- c(dropped, nm)
    }
  }
  if (length(dropped)) {
    fn <- paste0("mark_", L$mark)
    cli::cli_warn(
      c(
        "{.fn {fn}} dropped {.field {dropped}}: {?it varies/they vary} within one shape.",
        i = "Each {L$mark} is a single polygon with one {dropped[[1]]}. Map a variable that is constant within each category, or a discrete one."
      ),
      class = "vellumplot_dropped_aes"
    )
  }
  L
}

# The per-row styling aesthetics a stat carries through: it neither computes
# them nor consumes them as input (unlike `z` or `sample`), so after the stat
# they must be realigned to its output rows. `.STAT_OWNED` lists what the stat
# output itself supplies.
.STAT_CARRIED <- c(
  "color",
  "fill",
  "group",
  "alpha",
  "size",
  "shape",
  "linetype",
  "linewidth",
  "pattern"
)
.STAT_OWNED <- c(
  "x",
  "y",
  "xmin",
  "xmax",
  "ymin",
  "ymax",
  "width",
  "height",
  "label",
  ".piece",
  ".ring",
  ".x2",
  ".blabel"
)

# Realign every carried aesthetic to the stat's output rows, so a stat that
# changes the row count or order never leaves a pre-stat vector for `rep_len` to
# smear across its output (painting each output row from an arbitrary input row).
#
#   * A row-preserving stat (one output row per surviving input row -- dot plot,
#     outlier callout, count, window) returns `.rows`, the input row of each
#     output row: every carried aesthetic is indexed by it.
#   * A grouped stat returns `group`, the `.group_key()` level of each output
#     row. The aesthetic that *is* the key takes that factor (keeping its level
#     order for the scale); one constant within every group takes its group's
#     value, so `group = id, color = treatment` keeps each fit's own treatment.
#   * An ungrouped stat keeps an aesthetic only if it is constant.
#
# An aesthetic that varies within a group has no single value for a summary row,
# so it is dropped with a warning (ggplot2 does the same) rather than drawn wrong.
.realign_carried <- function(L, g_in, sdf, after_names, map = NULL) {
  carried <- setdiff(intersect(.STAT_CARRIED, names(L$values)), after_names)
  if (!length(carried)) {
    return(L)
  }
  map <- map %||% .stat_row_map(L, g_in, sdf)
  n_in <- map$n_in %||% max(lengths(L$values[carried]))
  rows <- map$rows
  sgroup <- map$sgroup
  out_group <- map$out_group
  dropped <- character(0)
  for (nm in carried) {
    v <- L$values[[nm]]
    if (length(v) <= 1L || length(v) != n_in) {
      next # a constant, or not a per-input-row vector (the guard checks)
    }
    if (!is.null(rows)) {
      L$values[[nm]] <- v[rows]
      next
    }
    if (!is.null(out_group) && !anyNA(out_group)) {
      if (identical(v, g_in)) {
        L$values[[nm]] <- sgroup
        next
      }
      per_group <- .per_group_value(v, map)
      if (!is.null(per_group)) {
        L$values[[nm]] <- per_group
        next
      }
    } else if (is.null(sgroup) && length(unique(v)) == 1L) {
      L$values[[nm]] <- v[1L]
      next
    } else if (!is.null(sgroup)) {
      # A stat whose groups are not the layer's key levels: tag colour/fill with
      # the stat's groups, so at least they stay length-consistent.
      if (nm %in% c("color", "fill")) {
        L$values[[nm]] <- sgroup
        next
      }
    }
    L$values[[nm]] <- NULL
    dropped <- c(dropped, nm)
  }
  if (length(dropped)) {
    cli::cli_warn(c(
      "Dropped {.field {dropped}}: {?it varies/they vary} within a group, so a summarised row has no single value.",
      i = "Map it to a variable that is constant within each group, or set {.arg group} to match."
    ))
  }
  L
}

# How a stat's output rows relate to its input rows, for realigning anything
# carried per input row (aesthetics, interactivity, provenance):
#   `rows`      the input row of each output row (a row-preserving stat's `.rows`);
#   `members`   the input rows each output row stands for -- `rows` as a list,
#               or a stat's own `members` attribute when one output row merges
#               several input rows (`.stat_sum()`'s coincident points);
#   `sgroup`, `out_group`, `in_group`, `glevs`  a grouped stat's output group
#               and the layer's `.group_key()` levels it indexes;
#   `n_in`, `n_out`  input and output row counts.
.stat_row_map <- function(L, g_in, sdf) {
  rows <- sdf$.rows
  sgroup <- sdf$group
  glevs <- if (!is.null(g_in)) .cat_levels(g_in)
  members <- attr(sdf, "members") %||% (if (!is.null(rows)) as.list(rows))
  list(
    rows = rows,
    members = members,
    sgroup = sgroup,
    glevs = glevs,
    in_group = if (!is.null(g_in)) match(as.character(g_in), glevs),
    out_group = if (!is.null(sgroup) && !is.null(g_in)) {
      match(as.character(sgroup), glevs)
    },
    n_in = L$n,
    n_out = nrow(sdf)
  )
}

# The value of a per-input-row vector `v` for each output row of a grouped stat,
# when `v` is constant within every group (one distinct value per group present);
# NULL when it varies within a group, so a summary row has no single value.
.per_group_value <- function(v, map) {
  in_group <- map$in_group
  if (
    length(unique(paste(in_group, v, sep = "\r"))) != length(unique(in_group))
  ) {
    return(NULL)
  }
  v[match(seq_along(map$glevs), in_group)[map$out_group]]
}

# Realign one per-input-row interactivity vector (a tooltip, data key, hover
# group, ...) to a stat's output rows. The rule, shared by every such value:
#   * a row-preserving stat (`.rows`): each output row takes its own input row's
#     value -- a dot's tooltip is its own observation's, whatever the stat's order;
#   * a grouped summarising stat: a value constant within each `.group_key()`
#     group is kept per group (a `tooltip = species` on a per-species density);
#   * an ungrouped summarising stat: a value constant across the layer is kept;
#   * otherwise NULL -- a summary row (a bin, a hexagon, a fitted line) stands
#     for many rows and has no single per-row value, so it is dropped rather than
#     drawn against the wrong element.
# A constant or a vector that is not one value per input row is returned as is.
.realign_row_value <- function(v, map) {
  if (is.null(v) || is.null(map) || length(v) != map$n_in) {
    return(v)
  }
  if (!is.null(map$rows)) {
    return(v[map$rows])
  }
  og <- map$out_group
  if (!is.null(og) && !anyNA(og)) {
    return(.per_group_value(v, map))
  }
  if (is.null(map$sgroup) && length(unique(v)) == 1L) {
    return(rep(v[1L], map$n_out))
  }
  NULL
}

# Realign a layer's interactivity (`L$meta`, from `.resolve_interactivity()`)
# and the per-row `if_false` of its conditional encodings to a stat's output rows
# (see `.realign_row_value()` for the rule). The data key decides: when it cannot
# be realigned the elements are unkeyed and the whole set is dropped -- except a
# default key (no `data_id` declared), which is rebuilt over the output rows so
# constant or per-group tooltips still reach the summary elements.
.realign_meta <- function(L, map) {
  if (is.null(map)) {
    return(L) # an empty stat output draws nothing
  }
  m <- L$meta
  if (!is.null(m)) {
    fields <- c("tooltip", "hover_group", "hover_color", "selected_color")
    for (nm in fields) {
      if (!is.null(m[[nm]])) {
        m[nm] <- list(.realign_row_value(m[[nm]], map))
      }
    }
    # A default key is a placeholder until `.default_keys()` builds the real
    # one at emit (panel, layer, row); it only has to align to the output rows.
    id <- if (isTRUE(m$default_id)) {
      as.character(seq_len(map$n_out))
    } else {
      .realign_row_value(m$data_id, map)
    }
    if (is.null(id)) {
      m <- NULL
    } else {
      m$data_id <- id
      m$n <- map$n_out
    }
    L$meta <- m
  }
  for (nm in names(L$conditions)) {
    f <- L$conditions[[nm]]$if_false
    if (length(f) > 1L) {
      L$conditions[[nm]]["if_false"] <- list(.realign_row_value(f, map))
    }
  }
  L
}

# The internal-consistency guard after a stat: every carried aesthetic is now a
# constant or one value per output row. Anything else would be recycled with
# `rep_len()` and silently paint output rows from unrelated input rows.
.check_stat_lengths <- function(L, nrow_out, after_names, stat) {
  for (nm in setdiff(intersect(.STAT_CARRIED, names(L$values)), after_names)) {
    len <- length(L$values[[nm]])
    if (len != 1L && len != nrow_out) {
      cli::cli_abort(
        c(
          "The {.val {stat}} stat left {.field {nm}} with {len} value{?s} for {nrow_out} output row{?s}.",
          i = "A stat that changes the rows must return {.field group} or {.field .rows} so its aesthetics can be realigned."
        ),
        .internal = TRUE
      )
    }
  }
  invisible(L)
}

# Fold a stat's output data frame back into the layer. After a stat, the layer's
# data IS the stat output: x (and y) come from it, raw stage-1 inputs are
# replaced. `after_stat()` channels (evaluated against the stat output) take
# precedence over the stat's default y.
.merge_stat <- function(L, sdf, stat = L$stat) {
  after_names <- names(L$after)
  # The pre-stat grouping, taken before any `after_stat()` channel replaces an
  # aesthetic with a stat-output-length vector.
  g_in <- .group_key(L)
  # An empty stat output (an unpopulated facet cell short-circuits `.apply_stat`
  # with a column-less `data.frame()`) has none of the columns the `after_stat()`
  # channels reference, so evaluating them would abort with "object '<col>' not
  # found". A zero-row layer draws nothing, so a zero-length value is correct --
  # skip the evaluation and let the empty layer render as a blank panel.
  empty <- nrow(sdf) == 0L
  for (nm in after_names) {
    L$values[[nm]] <- if (empty) {
      logical(0)
    } else {
      rlang::eval_tidy(L$after[[nm]], data = sdf)
    }
    L$types[[nm]] <- .infer_type(L$values[[nm]])
  }
  # An `after_stat()` bound to x takes precedence over the stat's default x, just
  # as it does for y below; only fall back to `sdf$x` when x was not overridden.
  if (!("x" %in% after_names) && !is.null(sdf$x)) {
    L$values$x <- sdf$x
  }
  map <- if (!empty) .stat_row_map(L, g_in, sdf)
  # Input rows the stat left out; a stat that dropped the whole layer (an
  # empty result from non-empty input) left out every row.
  L$stat_dropped <- attr(sdf, "dropped") %||%
    (if (empty && length(L$values)) seq_len(max(lengths(L$values))))
  if (!empty) {
    L <- .realign_carried(L, g_in, sdf, after_names, map)
  }
  L <- .realign_meta(L, map)
  if (!("y" %in% after_names) && !is.null(sdf$y)) {
    L$values$y <- sdf$y
    L$types$y <- "quantitative"
  }
  # An empty layer still has the y axis its stat would have produced (a count,
  # a density), so a plot of zero rows trains an empty panel instead of failing
  # for want of a y encoding.
  if (empty && is.null(L$values$y)) {
    L$values$y <- numeric(0)
    L$types$y <- "quantitative"
  }
  if (!is.null(sdf$ymin)) {
    L$values$ymin <- sdf$ymin
    L$values$ymax <- sdf$ymax
  }
  if (!is.null(sdf$xmin)) {
    L$values$xmin <- sdf$xmin
    L$values$xmax <- sdf$xmax
  }
  if (!is.null(sdf$width)) {
    L$values$width <- sdf$width
  }
  if (!is.null(sdf$height)) {
    L$values$height <- sdf$height
  }
  # Contour stats emit ordered vertices grouped into pieces (one polyline/band per
  # piece) and, for filled bands, sub-path rings; the contour emitters read these.
  if (!is.null(sdf$.piece)) {
    L$values$.piece <- sdf$.piece
    L$values$.ring <- sdf$.ring
  }
  # Significance brackets carry a second x endpoint (`.x2`) and a label (`.blabel`)
  # per row; the signif emitter reads these.
  if (!is.null(sdf$.x2)) {
    L$values$.x2 <- sdf$.x2
    L$values$.blabel <- sdf$.blabel
  }
  # A row-filtering stat (e.g. the outlier callout) that keeps a subset of the
  # input rows carries the surviving text labels forward, so a user-mapped
  # `label` aesthetic stays aligned with the reduced row set.
  if (!is.null(sdf$label) && !("label" %in% after_names)) {
    L$values$label <- sdf$label
    L$types$label <- .infer_type(sdf$label)
  }
  L$n <- nrow(sdf)
  if (!empty) {
    .check_stat_lengths(L, L$n, after_names, stat)
  }
  # The rows are now the stat's output, not the input data. A summarising stat's
  # element no longer maps to source rows, so provenance keys the layer whole; a
  # row-preserving stat's `stat_map` lets provenance and late per-row values
  # (`selgroup`) follow each output row back to its input row(s) (see
  # `.layer_source`, `.realign_row_value`).
  L$aggregated <- TRUE
  L$stat_map <- map
  L
}

# Count rows per x (per x and group, if a colour/fill is mapped). x (and the
# group) are kept as factors so a custom factor level order survives into the
# trained position/colour scales rather than being re-sorted alphabetically.
# A missing x (or group) is counted too, as an NA category: the trained scales
# draw it as their trailing "NA" level, as ggplot2 does.
.stat_count <- function(L) {
  xlevs <- .cat_levels(L$values$x)
  x <- factor(as.character(L$values$x), levels = xlevs)
  grp <- .group_key(L)
  if (is.null(grp)) {
    t <- table(x, useNA = "ifany")
    t <- t[t > 0]
    n <- as.numeric(t)
    data.frame(
      x = factor(names(t), levels = xlevs),
      count = n,
      y = n,
      prop = n / sum(n),
      # `density` is the old name of `prop`, kept as an alias.
      density = n / sum(n),
      stringsAsFactors = FALSE
    )
  } else {
    glevs <- .cat_levels(grp)
    g <- factor(as.character(grp), levels = glevs)
    agg <- as.data.frame(
      table(x = x, group = g, useNA = "ifany"),
      stringsAsFactors = FALSE
    )
    agg <- agg[agg$Freq > 0, , drop = FALSE]
    out <- data.frame(
      x = factor(agg$x, levels = xlevs),
      group = factor(agg$group, levels = glevs),
      count = agg$Freq,
      y = agg$Freq,
      # proportion within each group (each group sums to 1), matching ggplot2
      # (an NA group is a group of its own; `ave()` would skip it). ggplot2
      # calls it `prop`; `density` is kept as an alias.
      prop = agg$Freq /
        stats::ave(agg$Freq, addNA(factor(agg$group)), FUN = sum),
      stringsAsFactors = FALSE
    )
    out$density <- out$prop
    out
  }
}

# Count coincident (x, y) observations (per group), keeping one row per unique
# point with its count `n` and proportion `prop`. `mark_count()` maps `size` to
# `after_stat(n)`, so overlapping points become one bubble sized by overlap.
.stat_sum <- function(L) {
  x <- L$values$x
  y <- L$values$y
  grp <- .group_key(L)
  key <- if (is.null(grp)) {
    paste(as.character(x), as.character(y), sep = "\r")
  } else {
    paste(as.character(x), as.character(y), as.character(grp), sep = "\r")
  }
  first <- !duplicated(key)
  cnt <- as.integer(table(key)[key[first]])
  out <- data.frame(
    x = x[first],
    y = y[first],
    n = cnt,
    stringsAsFactors = FALSE
  )
  # `prop` is the share of the point's group (ggplot2's `stat_sum()`), so each
  # group's bubbles sum to 1; ungrouped, the group is the whole layer.
  if (is.null(grp)) {
    out$prop <- out$n / sum(out$n)
  } else {
    out$group <- grp[first]
    out$prop <- out$n /
      stats::ave(out$n, addNA(factor(as.character(out$group))), FUN = sum)
  }
  # Each bubble keeps its first row for per-row values (`.rows`) and names every
  # coincident row it merges for provenance (`members`).
  out$.rows <- which(first)
  attr(out, "members") <- unname(split(
    seq_along(key),
    factor(key, levels = key[first])
  ))
  out
}

# Format a p-value as text or significance stars.
.signif_label <- function(p, kind) {
  if (!is.finite(p)) {
    return("NA")
  }
  if (identical(kind, "stars")) {
    if (p < 1e-4) {
      "****"
    } else if (p < 1e-3) {
      "***"
    } else if (p < 1e-2) {
      "**"
    } else if (p < 0.05) {
      "*"
    } else {
      "ns"
    }
  } else {
    paste0("p = ", formatC(p, format = "g", digits = 2))
  }
}

# Pairwise significance brackets. For each `comparisons` pair of `x` groups, run
# `method` on the two groups' `y` and emit one bracket row: its two endpoints
# (`x` = left group, `.x2` = right group), a stacked height `y` (above the data
# so the y-scale expands to show it), and the p-value `.blabel`. The signif
# emitter draws the bracket + label; the y-scale training uses `y`.
.stat_signif <- function(L) {
  x <- as.character(L$values$x)
  y <- as.numeric(L$values$y)
  ok <- !is.na(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (!length(y)) {
    cli::cli_abort("{.fn mark_signif} needs finite {.field y} values.")
  }
  levs <- .cat_levels(x)
  sp <- L$stat_params
  comparisons <- sp$comparisons %||%
    lapply(seq_len(length(levs) - 1L), function(i) levs[c(i, i + 1L)])
  if (!length(comparisons)) {
    cli::cli_abort("{.fn mark_signif} needs at least two {.field x} groups.")
  }
  testfun <- match.fun(sp$method %||% "wilcox.test")
  span <- diff(range(y))
  if (!is.finite(span) || span == 0) {
    span <- 1
  }
  step <- (sp$step %||% 0.12) * span
  y0 <- max(y)
  rows <- lapply(seq_along(comparisons), function(i) {
    cc <- as.character(comparisons[[i]])
    a <- y[x == cc[1]]
    b <- y[x == cc[2]]
    if (length(a) < 2L || length(b) < 2L) {
      return(NULL)
    }
    p <- tryCatch(
      suppressWarnings(testfun(a, b)$p.value),
      error = function(e) NA_real_
    )
    data.frame(
      x = cc[1],
      .x2 = cc[2],
      y = y0 + step * i,
      .blabel = .signif_label(p, sp$label %||% "p"),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out) || !nrow(out)) {
    cli::cli_abort(
      "{.fn mark_signif}: no comparable groups (each needs >= 2 finite values)."
    )
  }
  out
}

# Direct labels: one label per colour/fill series at its line end (the row with
# the largest x). `lab` is the series name, `group` realigns the colour so the
# label inherits the series colour. `mark_series_label()` maps `label` to
# `after_stat(lab)` and repels the labels apart.
.stat_series_label <- function(L) {
  n <- if (length(L$values)) max(lengths(L$values)) else 0L
  xn <- suppressWarnings(as.numeric(rep_len(L$values$x, n)))
  yn <- suppressWarnings(as.numeric(rep_len(L$values$y, n)))
  # The line end is the largest-x point, so a discrete/categorical x has no
  # "end" to label -- coercing it to numeric would give all-NA and crash below.
  if (n > 0L && !any(is.finite(xn))) {
    cli::cli_abort(c(
      "{.fn mark_series_label} needs a continuous {.field x} (it labels each series at its largest x).",
      i = "The mapped {.field x} is discrete; use {.fn mark_text} to label a categorical axis."
    ))
  }
  grp <- .group_key(L)
  if (is.null(grp)) {
    i <- which.max(xn)
    return(data.frame(
      x = xn[i],
      y = yn[i],
      lab = "",
      .rows = i,
      stringsAsFactors = FALSE
    ))
  }
  g <- as.character(rep_len(grp, n))
  keep <- vapply(
    unique(g),
    function(lev) {
      sel <- which(g == lev)
      # `which.max` is empty for an all-NA group; fall back to its last row so a
      # single non-finite series never aborts the whole label set.
      m <- which.max(xn[sel])
      if (length(m)) sel[m] else sel[length(sel)]
    },
    integer(1)
  )
  data.frame(
    x = xn[keep],
    y = yn[keep],
    group = g[keep],
    lab = g[keep],
    .rows = keep,
    stringsAsFactors = FALSE
  )
}

# Outlier callout: keep only the rows whose y is an outlier (per colour/fill
# group), so the text/repel emitter labels just the extremes. `"iqr"` flags y
# outside [Q1 - k*IQR, Q3 + k*IQR] (the boxplot rule); `"sd"` flags |y - mean| >
# k*sd. A user-mapped `label` is subset to the survivors (`.merge_stat` carries
# it); when none is mapped, `.olab` (the formatted y value) is the default label.
.stat_outlier <- function(L) {
  n <- if (length(L$values)) max(lengths(L$values)) else 0L
  y <- as.numeric(rep_len(L$values$y, n))
  x <- as.numeric(rep_len(L$values$x, n))
  method <- L$stat_params$method %||% "iqr"
  k <- L$stat_params$k %||% 1.5
  grp <- .group_key(L, .OUTLIER_GROUP_AES)
  gvec <- if (is.null(grp)) rep_len("", n) else as.character(rep_len(grp, n))
  is_out <- logical(n)
  for (lev in unique(gvec)) {
    sel <- which(gvec == lev & is.finite(y))
    yy <- y[sel]
    if (length(yy) < 3L) {
      next # too few to judge an outlier
    }
    flag <- if (method == "sd") {
      s <- stats::sd(yy)
      if (s == 0) rep(FALSE, length(yy)) else abs(yy - mean(yy)) > k * s
    } else {
      q <- stats::quantile(yy, c(0.25, 0.75), names = FALSE)
      iqr <- q[2L] - q[1L]
      yy < q[1L] - k * iqr | yy > q[2L] + k * iqr
    }
    is_out[sel[flag]] <- TRUE
  }
  keep <- which(is_out)
  out <- data.frame(x = x[keep], y = y[keep], stringsAsFactors = FALSE)
  if (!is.null(grp)) {
    out$group <- gvec[keep]
  }
  lab <- L$values$label
  if (!is.null(lab)) {
    out$label <- rep_len(lab, n)[keep]
  }
  out$.olab <- format(y[keep], trim = TRUE)
  out$.rows <- keep
  out
}

# The bin edges of a histogram over the finite values `x`, with ggplot2's
# semantics (`bin_breaks_bins()` / `bin_breaks_width()`):
#   * `breaks` -- used as given (sorted);
#   * `binwidth` -- bins of that width, aligned so one edge falls on `boundary`
#     (or one centre on `center`); with neither, on `binwidth / 2`, which puts
#     the data's ends in the outer halves of their bins;
#   * else `bins` -- `bins` bins whose width is `range / (bins - 1)`, so the
#     outer bins are centred on the smallest and largest values, unless a
#     `boundary` or `center` is given to align them. One bin spans the range.
.bin_breaks <- function(x, sp) {
  if (!is.null(sp$breaks)) {
    return(sort(unique(as.numeric(sp$breaks))))
  }
  rng <- range(x)
  boundary <- sp$boundary
  center <- sp$center
  width <- sp$binwidth
  if (is.null(width)) {
    bins <- sp$bins %||% 30L
    if (diff(rng) == 0) {
      width <- 0.1
    } else if (bins == 1L) {
      return(rng)
    } else {
      width <- diff(rng) / (bins - 1)
      if (is.null(boundary) && is.null(center)) {
        center <- rng[1]
      }
    }
  }
  if (is.null(boundary)) {
    boundary <- if (is.null(center)) width / 2 else center - width / 2
  }
  origin <- boundary + floor((rng[1] - boundary) / width) * width
  # the small correction keeps a maximum that sits on an edge from opening an
  # empty extra bin (ggplot2's `1 - 1e-08`)
  max_x <- rng[2] + (1 - 1e-08) * width
  if ((max_x - origin) / width > 1e6) {
    cli::cli_abort(c(
      "{.fn mark_histogram} would make more than a million bins.",
      i = "Use a larger {.arg binwidth} or fewer {.arg bins}."
    ))
  }
  breaks <- seq(origin, max_x, by = width)
  if (length(breaks) == 1L) {
    breaks <- c(breaks, breaks + width)
  }
  breaks
}

# The bin index of each value of `x` for `breaks`, ggplot2's way: bins are
# closed on the right (`(a, b]`, the first also taking its left edge) or on the
# left, with the edges nudged by a tiny fuzz so a value that sits on an edge
# after floating-point error still lands in the bin it belongs to. A value
# outside the breaks gets NA.
.bin_index <- function(x, breaks, closed = "right") {
  fuzz <- 1e-08 * stats::median(diff(breaks))
  nb <- length(breaks)
  fuzzy <- if (identical(closed, "left")) {
    breaks + c(rep(-fuzz, nb - 1L), fuzz)
  } else {
    breaks + c(-fuzz, rep(fuzz, nb - 1L))
  }
  cut(
    x,
    fuzzy,
    right = !identical(closed, "left"),
    include.lowest = TRUE,
    labels = FALSE
  )
}

# Bin a continuous x (per group) and count per bin. The breaks (`.bin_breaks()`)
# are shared by every group. Per bin: its centre `x`, `width`, `count`,
# `density` (count / (total * width), so each group integrates to 1),
# `ncount` / `ndensity` (scaled to a maximum of 1). `pad = TRUE` (the frequency
# polygon) adds an empty bin at each end so the polygon starts and ends at 0.
.stat_bin <- function(L) {
  x0 <- as.numeric(L$values$x)
  ok <- is.finite(x0)
  x <- x0[ok]
  fn <- .stat_fn(L, "bin")
  if (!length(x)) {
    return(.stat_drop_layer("{.fn {fn}} needs at least one finite value."))
  }
  sp <- L$stat_params
  breaks <- .bin_breaks(x, sp)
  nb <- length(breaks) - 1L
  idx <- .bin_index(x, breaks, sp$closed %||% "right")
  centers <- (utils::head(breaks, -1L) + breaks[-1L]) / 2
  widths <- diff(breaks)
  grp <- .group_key(L)
  glevs <- if (is.null(grp)) "" else .cat_levels(grp)
  g <- if (is.null(grp)) {
    factor(rep("", length(x)), levels = "")
  } else {
    factor(as.character(grp)[ok], levels = glevs)
  }
  tab <- table(factor(idx, levels = seq_len(nb)), g)
  parts <- lapply(seq_along(glevs), function(k) {
    count <- as.numeric(tab[, k])
    xc <- centers
    w <- widths
    if (isTRUE(sp$pad)) {
      count <- c(0, count, 0)
      xc <- c(xc[1] - w[1], xc, xc[nb] + w[nb])
      w <- c(w[1], w, w[nb])
    }
    total <- sum(count)
    dens <- if (total > 0) count / (total * w) else count
    df <- data.frame(
      x = xc,
      count = count,
      y = count,
      width = w,
      density = dens,
      ncount = if (max(count) > 0) count / max(count) else count,
      ndensity = if (max(dens) > 0) dens / max(dens) else dens
    )
    if (!isTRUE(sp$pad)) {
      # the bar edges, so the x axis is trained to cover the outer bars
      df$xmin <- xc - w / 2
      df$xmax <- xc + w / 2
    }
    if (!is.null(grp)) {
      df$group <- factor(glevs[k], levels = glevs)
    }
    df
  })
  do.call(rbind, parts)
}

# Bin a continuous (x, y) into a `bins x bins` grid; one row per non-empty cell
# with its centre, count, and cell width/height (for tile rendering).
#
# Per group (`.group_key()`): every group is counted on the one grid laid over
# all the layer's points, so the groups' cells coincide and each cell row
# carries its group (and so its colour) instead of all groups being pooled.
.stat_bin2d <- function(L) {
  x0 <- as.numeric(L$values$x)
  y0 <- as.numeric(L$values$y)
  xy <- .finite_xy(x0, y0)
  if (!length(xy$x)) {
    return(.stat_drop_layer(
      "{.fn mark_bin2d} needs at least one finite (x, y) pair."
    ))
  }
  bins <- L$stat_params$bins %||% 30L
  edges <- function(v) {
    rng <- .pad_degenerate_range(range(v))
    seq(rng[1], rng[2], length.out = bins + 1L)
  }
  xb <- edges(xy$x)
  yb <- edges(xy$y)
  xc <- (utils::head(xb, -1L) + xb[-1L]) / 2
  yc <- (utils::head(yb, -1L) + yb[-1L]) / 2
  .per_group(
    L,
    length(x0),
    function(idx, gn, grouped, k) {
      g <- .finite_xy(x0[idx], y0[idx])
      if (!length(g$x)) {
        return(NULL)
      }
      xi <- findInterval(g$x, xb, rightmost.closed = TRUE, all.inside = TRUE)
      yi <- findInterval(g$y, yb, rightmost.closed = TRUE, all.inside = TRUE)
      tab <- table(
        factor(xi, levels = seq_len(bins)),
        factor(yi, levels = seq_len(bins))
      )
      cells <- which(tab > 0, arr.ind = TRUE)
      data.frame(
        x = xc[cells[, 1]],
        y = yc[cells[, 2]],
        count = as.numeric(tab[cells]),
        width = diff(xb)[1],
        height = diff(yb)[1]
      )
    },
    "{.fn mark_bin2d} needs at least one finite (x, y) pair.",
    usable = is.finite(x0) & is.finite(y0)
  )
}

# The 2-D scalar field to contour: a kernel density estimate of (x, y) (the
# default), or a supplied `z` surface reshaped from a regular (x, y) grid.
# Returns the grid coords `gx`/`gy` and a matrix `gz` with **rows = x, cols = y**
# -- the base-R convention `vl_contour()` uses (`dim(z) == c(length(x),
# length(y))`, as `image()`/`contour()`/`outer(xs, ys, f)` do).
#
# `lims` (`c(xmin, xmax, ymin, ymax)`) fixes the KDE grid; a grouped contour
# passes the whole layer's range so every group is estimated over the same
# window and its rings close inside it rather than at its own points' edge.
.contour_field <- function(L, lims = NULL) {
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  z <- L$values$z
  if (!is.null(z)) {
    ux <- sort(unique(x))
    uy <- sort(unique(y))
    # A complete regular grid needs the right row count AND one z per cell; a
    # count-only check passes a duplicated cell paired with a missing one, which
    # would leave a silent NA hole in the surface.
    if (
      length(ux) * length(uy) != length(z) ||
        anyDuplicated(cbind(match(y, uy), match(x, ux)))
    ) {
      cli::cli_abort(c(
        "Contouring a {.arg z} surface needs {.arg x}/{.arg y} on a complete regular grid.",
        i = "Got {length(x)} rows for a {length(ux)} x {length(uy)} grid; every (x, y) cell must appear exactly once."
      ))
    }
    m <- matrix(NA_real_, nrow = length(ux), ncol = length(uy))
    m[cbind(match(x, ux), match(y, uy))] <- as.numeric(z)
    list(gx = ux, gy = uy, gz = m)
  } else {
    .need_pkg("MASS", "2-D density contours (mark_contour())")
    xy <- .finite_xy(x, y)
    x <- xy$x
    y <- xy$y
    if (length(x) < 3L) {
      cli::cli_abort("A 2-D density contour needs at least 3 finite points.")
    }
    sp <- L$stat_params
    ng <- sp$n %||% 100L
    # Bandwidths as ggplot2's stat_density_2d(): `h` (default MASS's normal
    # reference `bandwidth.nrd()` per axis, also kde2d's own default) times
    # `adjust`.
    h <- rep_len(
      sp$h %||% c(MASS::bandwidth.nrd(x), MASS::bandwidth.nrd(y)),
      2L
    ) *
      rep_len(sp$adjust %||% 1, 2L)
    if (any(!is.finite(h) | h <= 0)) {
      cli::cli_abort(c(
        "A 2-D density needs a positive bandwidth on both axes.",
        i = "Do {.arg x} and {.arg y} both vary? Otherwise set {.arg h}."
      ))
    }
    kd <- MASS::kde2d(
      x,
      y,
      h = h,
      n = ng,
      lims = lims %||% c(range(x), range(y))
    )
    # kde2d's z is already [x, y] (rows = x) -- exactly what vl_contour() wants.
    z <- switch(
      sp$contour_var %||% "density",
      density = kd$z,
      ndensity = kd$z / max(kd$z, na.rm = TRUE),
      count = kd$z * length(x)
    )
    list(gx = kd$x, gy = kd$y, gz = z)
  }
}

# Contour break levels: explicit `breaks`, else a `binwidth` grid, else ~`bins`
# pretty cuts. Only levels strictly inside the field range are kept -- except,
# with `lowest = TRUE` (filled bands), the highest break at or below the field's
# minimum too: it bounds ggplot2's lowest band, `(b0, b1]`, which covers the
# whole field below the first inner level (VP-45).
.contour_breaks <- function(gz, sp, lowest = FALSE) {
  rng <- range(gz, na.rm = TRUE)
  br <- if (!is.null(sp$breaks)) {
    sort(unique(as.numeric(sp$breaks)))
  } else if (!is.null(sp$binwidth)) {
    seq(floor(rng[1] / sp$binwidth) * sp$binwidth, rng[2], by = sp$binwidth)
  } else {
    pretty(rng, n = sp$bins %||% 10L)
  }
  inner <- br[br > rng[1] & br < rng[2]]
  below <- br[br <= rng[1]]
  if (lowest && length(inner) && length(below)) {
    inner <- c(max(below), inner)
  }
  inner
}

# Pad a contour field with a border far below every level, so each filled
# band's contour closes on itself along the edge of the data grid. Closing an
# open contour along the grid boundary (`.close_contour_ring()`) has to guess
# which side holds the band, and for a band that spans two peaks (faithful's
# lowest level) it guessed the outside, flooding the panel. The padded rows sit
# a hair outside the grid: the iso-lines meet the original boundary to within
# 1e-6 of a cell, as the border value is ~1e6 ranges below the field.
.pad_field <- function(fld) {
  gz <- fld$gz
  rng <- range(gz, na.rm = TRUE)
  low <- rng[1] - 1e6 * max(diff(rng), abs(rng[1]), 1)
  step <- function(g) {
    if (length(g) > 1L) diff(range(g)) / (length(g) - 1L) else 1
  }
  pad <- function(g) c(g[1] - step(g), g, g[length(g)] + step(g))
  m <- matrix(low, nrow(gz) + 2L, ncol(gz) + 2L)
  m[2:(nrow(gz) + 1L), 2:(ncol(gz) + 1L)] <- gz
  m[is.na(m)] <- low
  list(gx = pad(fld$gx), gy = pad(fld$gy), gz = m)
}

# Empty contour vertex table (the shape .emit_contour / .emit_contour_filled read).
.empty_contour_df <- function() {
  data.frame(
    x = numeric(),
    y = numeric(),
    level = numeric(),
    .piece = integer(),
    .ring = integer()
  )
}

# `vl_contour()` output -> the ordered vertex table `.emit_contour()` reads: one
# `.piece` per traced contour (chained polyline). A closed contour comes back as
# an open sequence, so its first point is repeated to close the ring.
.vl_contour_lines_df <- function(cc) {
  if (!nrow(cc)) {
    return(.empty_contour_df())
  }
  ids <- unique(cc$id)
  parts <- lapply(seq_along(ids), function(k) {
    e <- cc[cc$id == ids[k], , drop = FALSE]
    if (isTRUE(e$closed[1])) {
      e <- rbind(e, e[1L, , drop = FALSE])
    }
    data.frame(
      x = e$x,
      y = e$y,
      level = e$level[1],
      .piece = k,
      .ring = NA_integer_
    )
  })
  do.call(rbind, parts)
}

# Close an *open* contour -- one that leaves the grid, so its endpoints sit on
# the domain boundary -- into a filled ring by walking the domain rectangle
# clockwise from the contour's end point back to its start, inserting the corners
# passed along the way. `vl_contour()` orients every traced line consistently
# (the field-above-level side on the right), so the clockwise return keeps that
# region enclosed instead of cutting a straight chord across the panel (which
# leaves triangular wedges). Endpoints not on the boundary -- which should not
# occur for an open marching-squares contour -- fall back to a straight close.
.close_contour_ring <- function(ex, ey, xlim, ylim) {
  x0 <- xlim[1]
  x1 <- xlim[2]
  y0 <- ylim[1]
  y1 <- ylim[2]
  eps <- 1e-9 * max(x1 - x0, y1 - y0, 1)
  # Clockwise perimeter parameter t in [0, 4): left edge up [0, 1), top edge
  # right [1, 2), right edge down [2, 3), bottom edge left [3, 4). Corners fall
  # at the integers t = 0, 1, 2, 3.
  perim_t <- function(px, py) {
    if (abs(px - x0) <= eps) {
      return((py - y0) / (y1 - y0))
    }
    if (abs(py - y1) <= eps) {
      return(1 + (px - x0) / (x1 - x0))
    }
    if (abs(px - x1) <= eps) {
      return(2 + (y1 - py) / (y1 - y0))
    }
    if (abs(py - y0) <= eps) {
      return(3 + (x1 - px) / (x1 - x0))
    }
    NA_real_
  }
  n <- length(ex)
  t_end <- perim_t(ex[n], ey[n])
  t_start <- perim_t(ex[1], ey[1])
  if (is.na(t_end) || is.na(t_start)) {
    return(list(x = ex, y = ey))
  }
  corner_x <- c(x0, x0, x1, x1) # t = 0, 1, 2, 3
  corner_y <- c(y0, y1, y1, y0)
  span <- (t_start - t_end) %% 4 # clockwise arc length from end back to start
  off <- (c(0, 1, 2, 3) - t_end) %% 4 # each corner's clockwise offset from end
  keep <- which(off > eps & off < span - eps)
  keep <- keep[order(off[keep])]
  list(x = c(ex, corner_x[keep]), y = c(ey, corner_y[keep]))
}

# `vl_contour()` output -> filled bands. Each contour becomes a filled ring, and
# the rings are ordered by level ascending so an inner (higher) level paints over
# the outer (lower) one -- painter's order, which for the nested closed loops of
# a density gives the layered filled look without an even-odd band construction.
# An open contour (one that exits the `xlim`/`ylim` grid) is closed along the
# domain boundary so it fills the region on the field-above-level side rather
# than closing with a chord that would slice a wedge across the panel.
.vl_contour_bands_df <- function(cc, xlim, ylim) {
  if (!nrow(cc)) {
    return(.empty_contour_df())
  }
  ids <- unique(cc$id)
  pieces <- lapply(ids, function(i) cc[cc$id == i, , drop = FALSE])
  # a sliver traced where the padded border meets a grid corner (fewer than
  # three vertices) encloses nothing
  pieces <- pieces[vapply(pieces, nrow, integer(1)) >= 3L]
  if (!length(pieces)) {
    return(.empty_contour_df())
  }
  pieces <- pieces[order(vapply(pieces, function(e) e$level[1], numeric(1)))]
  parts <- lapply(seq_along(pieces), function(k) {
    e <- pieces[[k]]
    ring <- if (isTRUE(e$closed[1])) {
      list(x = e$x, y = e$y)
    } else {
      .close_contour_ring(e$x, e$y, xlim, ylim)
    }
    data.frame(
      x = ring$x,
      y = ring$y,
      level = e$level[1],
      .piece = k,
      .ring = 1L
    )
  })
  do.call(rbind, parts)
}

# stat_density_2d / stat_contour: trace iso-lines (or, when `filled`, filled
# iso-rings) of the 2-D field with the engine's marching-squares
# `vellum::vl_contour()`. Output is an ordered vertex table consumed by
# .emit_contour / .emit_contour_filled. Needs no `isoband`.
#
# Per group (`.group_key()`): each group gets its own field -- its own KDE, so
# `mark_contour(color = Species)` draws one set of rings per species rather than
# one KDE of every point pooled. The break levels are shared, computed over all
# the groups' fields, so a level means the same density in every group.
.stat_density_2d <- function(L) {
  grp <- .group_key(L)
  g <- .stat_groups(grp, L$n %||% length(L$values$x))
  lims <- NULL
  if (!is.null(grp) && is.null(L$values$z)) {
    xy <- .finite_xy(as.numeric(L$values$x), as.numeric(L$values$y))
    if (length(xy$x)) {
      lims <- c(range(xy$x), range(xy$y))
    }
  }
  fields <- lapply(names(g$groups), function(gn) {
    idx <- g$groups[[gn]]
    Lg <- L
    Lg$values <- lapply(L$values, function(v) {
      if (length(v) > 1L) v[idx] else v
    })
    Lg$n <- length(idx)
    if (!is.null(grp) && is.null(L$values$z)) {
      # A group with too few points for a density (or none: a facet panel
      # holds only some groups) is left out, as ggplot2 drops it.
      nfin <- length(
        .finite_xy(as.numeric(Lg$values$x), as.numeric(Lg$values$y))$x
      )
      if (nfin < 3L) {
        if (nfin > 0L) {
          cli::cli_warn(
            "Skipping {.field {gn}}: a 2-D density needs at least 3 points."
          )
        }
        return(NULL)
      }
    }
    .contour_field(Lg, lims)
  })
  names(fields) <- names(g$groups)
  fields <- fields[!vapply(fields, is.null, logical(1))]
  if (!length(fields)) {
    return(.stat_drop_layer("A 2-D density needs at least 3 points per group."))
  }
  filled <- isTRUE(L$stat_params$filled)
  gz_all <- unlist(lapply(fields, `[[`, "gz"), use.names = FALSE)
  brks <- .contour_breaks(gz_all, L$stat_params, lowest = filled)
  # whether the first level bounds the lowest band (at or below every field)
  has_lowest <- filled && length(brks) && brks[1] <= min(gz_all, na.rm = TRUE)
  if (!length(brks)) {
    cli::cli_abort(c(
      "No contour levels fall inside the data range.",
      i = "Set {.arg breaks} or {.arg binwidth} explicitly."
    ))
  }
  pieces <- 0L
  parts <- lapply(names(fields), function(gn) {
    fld <- fields[[gn]]
    levels <- brks
    if (filled) {
      # The lowest band's level can sit at (or below) the field's minimum: trace
      # it a hair below, so cells exactly at the level are inside the band and
      # it covers the whole field, then label it with its break.
      zr <- range(fld$gz, na.rm = TRUE)
      if (has_lowest && levels[1] >= zr[1]) {
        levels[1] <- zr[1] - 1e-9 * max(diff(zr), abs(zr[1]), 1)
      }
      fld <- .pad_field(fld)
    }
    xlim <- range(fld$gx)
    ylim <- range(fld$gy)
    cc <- vellum::vl_contour(fld$gz, levels = levels, xlim = xlim, ylim = ylim)
    cc$level <- brks[match(cc$level, levels)]
    df <- if (filled) {
      .vl_contour_bands_df(cc, xlim, ylim)
    } else {
      .vl_contour_lines_df(cc)
    }
    # pieces numbered across groups, so no two groups share a polyline / ring
    df$.piece <- df$.piece + pieces
    pieces <<- pieces + length(unique(df$.piece))
    if (!is.null(grp)) {
      df$group <- factor(rep(gn, nrow(df)), levels = g$levels)
    }
    df
  })
  do.call(rbind, parts)
}

# Round axial hex coordinates (q, r) to the nearest hex centre via cube rounding
# (Red Blob Games). Vectorised; returns the integer axial coordinates.
.hex_round <- function(q, rax) {
  xc <- q
  zc <- rax
  yc <- -xc - zc
  rx <- round(xc)
  ry <- round(yc)
  rz <- round(zc)
  dx <- abs(rx - xc)
  dy <- abs(ry - yc)
  dz <- abs(rz - zc)
  cond_x <- dx > dy & dx > dz
  cond_z <- !cond_x & (dz > dy)
  rx[cond_x] <- -ry[cond_x] - rz[cond_x]
  rz[cond_z] <- -rx[cond_z] - ry[cond_z]
  list(q = rx, r = rz)
}

# Hexagonal 2-D binning (flat-top). Bins (x, y) into a hex lattice with ~`bins`
# columns across x; returns one row per occupied hex with its data-space centre,
# count, and circumradius `width` (in x-data units) used to size the hexagon.
# Binning is done in an isotropic space (y scaled to the x range) so hexes are
# regular there; they render geometrically exact under coord_fixed().
#
# Per group (`.group_key()`): the lattice is laid over all the layer's points,
# then each group is counted on it, so groups share hex centres and every hex
# row carries its own group's colour.
.stat_hexbin <- function(L) {
  x0 <- as.numeric(L$values$x)
  y0 <- as.numeric(L$values$y)
  xy <- .finite_xy(x0, y0)
  if (!length(xy$x)) {
    return(.stat_drop_layer(
      "{.fn mark_hex} needs at least one finite (x, y) pair."
    ))
  }
  bins <- L$stat_params$bins %||% 30L
  xr <- .pad_degenerate_range(range(xy$x))
  yr <- .pad_degenerate_range(range(xy$y))
  r <- diff(xr) / (bins * 1.5) # circumradius in x-units (flat-top columns at 1.5r)
  asp <- diff(xr) / diff(yr)
  .per_group(
    L,
    length(x0),
    function(idx, gn, grouped, k) {
      g <- .finite_xy(x0[idx], y0[idx])
      if (!length(g$x)) {
        return(NULL)
      }
      px <- g$x - xr[1]
      py <- (g$y - yr[1]) * asp # isotropic y in x-units
      ax <- .hex_round((2 / 3 * px) / r, (-1 / 3 * px + sqrt(3) / 3 * py) / r)
      key <- paste(ax$q, ax$r, sep = ",")
      tab <- table(key)
      qr <- do.call(rbind, lapply(strsplit(names(tab), ","), as.numeric))
      qi <- qr[, 1]
      ri <- qr[, 2]
      cx <- r * 1.5 * qi + xr[1]
      cy_iso <- r * sqrt(3) * (ri + qi / 2)
      cy <- cy_iso / asp + yr[1]
      # `width` is the x-data circumradius; `height` the full y-data extent of a
      # hex (the isotropic r*sqrt(3) un-scaled back to y-data units). The emitter
      # draws each hex from these so they tile data space at any panel aspect.
      data.frame(
        x = cx,
        y = cy,
        count = as.numeric(tab),
        width = r,
        height = r * sqrt(3) / asp
      )
    },
    "{.fn mark_hex} needs at least one finite (x, y) pair.",
    usable = is.finite(x0) & is.finite(y0)
  )
}

# Split `n` row indices into groups, preserving factor level order (so a custom
# factor order survives into the trained position/colour scales). Returns a named
# list keyed by level, or a single "all" group when ungrouped, plus the levels.
.stat_groups <- function(grp, n) {
  if (is.null(grp)) {
    return(list(groups = list(all = seq_len(n)), levels = NULL))
  }
  glevs <- .cat_levels(grp)
  gc <- as.character(grp)
  # One pass in level order, instead of a which() scan per level (O(n * groups)).
  groups <- split(seq_len(n), factor(gc, levels = glevs))
  list(groups = groups, levels = glevs)
}

# The shared per-group scaffold for the grouped stats (density/ecdf/qq/qq_line/
# smooth/ellipse/hull). `compute(idx, gn, grouped, k)` gets a group's row indices,
# its level name, whether the layer is grouped, and the group's 1-based index
# (for a `.piece`); it returns that group's stat rows (without the `group`
# column) or NULL to drop the group. The scaffold splits by group, drops the
# NULLs, attaches the `group` factor, warns with `empty_msg` and drops the layer
# when every group is dropped, and rbinds the survivors.
#
# `usable` (a logical per input row, e.g. `is.finite(x)`) marks the rows the stat
# can use. The input rows the stat leaves out -- unusable rows, and every row of
# a dropped group -- are recorded as the result's "dropped" attribute (see
# `.stat_dropped()`).
.per_group <- function(L, n, compute, empty_msg, usable = NULL) {
  grp <- .group_key(L)
  g <- .stat_groups(grp, n)
  glevs <- g$levels
  gnames <- names(g$groups)
  parts <- lapply(seq_along(gnames), function(k) {
    gn <- gnames[[k]]
    df <- compute(g$groups[[gn]], gn, !is.null(grp), k)
    if (is.null(df)) {
      return(NULL)
    }
    if (!is.null(grp)) {
      df$group <- factor(gn, levels = glevs)
    }
    df
  })
  kept <- !vapply(parts, is.null, logical(1))
  parts <- parts[kept]
  if (!length(parts)) {
    return(.stat_drop_layer(empty_msg))
  }
  dropped <- c(
    unlist(g$groups[!kept], use.names = FALSE),
    if (!is.null(usable)) which(!usable)
  )
  .stat_dropped(do.call(rbind, parts), dropped)
}

# Record which input rows (indices into the layer's rows) a stat left out of its
# computation -- a non-finite value, or a group too small to summarise -- as the
# output's "dropped" attribute. `.merge_stat()` keeps it as `L$stat_dropped`;
# `spec_diagnose()`'s report counts these rows.
.stat_dropped <- function(sdf, rows) {
  attr(sdf, "dropped") <- sort(unique(as.integer(rows)))
  sdf
}

# A stat with nothing to compute (too few points, no finite values) warns and
# returns an empty result, so its layer draws nothing while the rest of the plot
# renders -- ggplot2's behaviour, and the same whether or not the layer is
# grouped. `.merge_stat()` turns the zero-row result into an empty layer.
.stat_drop_layer <- function(msg, env = parent.frame()) {
  cli::cli_warn(
    c(msg, i = "The layer is dropped."),
    class = "vellumplot_stat_dropped",
    .envir = env
  )
  data.frame()
}

# Keep only the positions where both x and y are finite (a common pre-filter for
# the 2-D stats).
.finite_xy <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  list(x = x[ok], y = y[ok])
}

# Expand a degenerate (zero-width) range so downstream binning has a non-zero
# span to divide.
.pad_degenerate_range <- function(rng) {
  if (diff(rng) == 0) rng + c(-0.5, 0.5) else rng
}

# A 1-D kernel density of x (per group): a dense (x, density) curve. Groups with
# fewer than 2 finite points are skipped with a warning (a density needs >= 2).
#
# As in ggplot2, the curve is evaluated over the data's range only, never the
# ~3-bandwidth tails `stats::density()` pads by default: over the whole layer's
# x range (`trim = FALSE`, the default, so every group's curve spans the same
# x), or over each group's own range (`trim = TRUE`).
.stat_density <- function(L) {
  x <- as.numeric(L$values$x)
  adjust <- L$stat_params$adjust %||% 1
  trim <- isTRUE(L$stat_params$trim)
  layer_rng <- .finite_range(x)
  .per_group(
    L,
    length(x),
    function(idx, gn, grouped, k) {
      xi <- x[idx]
      xi <- xi[is.finite(xi)]
      if (length(xi) < 2) {
        if (grouped) {
          cli::cli_warn(
            "Skipping {.field {gn}}: {.fn mark_density} needs at least 2 points."
          )
        }
        return(NULL)
      }
      rng <- if (trim) range(xi) else layer_rng
      d <- stats::density(xi, adjust = adjust, from = rng[1], to = rng[2])
      data.frame(x = d$x, y = d$y, density = d$y)
    },
    "{.fn mark_density} needs at least 2 finite points.",
    usable = is.finite(x)
  )
}

# Warn about aggregated categories whose summary is `NA` (an empty group, or a
# summary of data containing `NA` when `fun` does not remove it -- e.g. the
# default `mean` without `na.rm = TRUE`), and return the keep mask. Both branches
# of `.stat_aggregate` use this so the grouped and ungrouped paths drop `NA`
# summaries on identical terms.
.drop_na_summary <- function(is_na) {
  if (any(is_na)) {
    cli::cli_warn(
      "Dropped {sum(is_na)} categor{?y/ies} with a missing summary value ({.code NA})."
    )
  }
  !is_na
}

# Summarise y per x category (per group) with `fun` (default mean). `fun` sees
# every value in a category (including NAs); a category whose summary is `NA` is
# dropped with a warning. The grouped path uses `na.action = na.pass` so it feeds
# `fun` the same NA-inclusive values the ungrouped `tapply` path does.
.stat_aggregate <- function(L) {
  x <- L$values$x
  y <- as.numeric(L$values$y)
  fun <- match.fun(L$stat_params$fun %||% "mean")
  xlevs <- .cat_levels(x)
  xf <- factor(as.character(x), levels = xlevs)
  grp <- .group_key(L)
  if (is.null(grp)) {
    yv <- tapply(y, xf, fun)
    keep <- .drop_na_summary(is.na(yv))
    data.frame(
      x = factor(xlevs[keep], levels = xlevs),
      y = as.numeric(yv[keep])
    )
  } else {
    glevs <- .cat_levels(grp)
    gf <- factor(as.character(grp), levels = glevs)
    agg <- stats::aggregate(y ~ xf + gf, FUN = fun, na.action = stats::na.pass)
    keep <- .drop_na_summary(is.na(agg$y))
    data.frame(
      x = factor(as.character(agg$xf[keep]), levels = xlevs),
      group = factor(as.character(agg$gf[keep]), levels = glevs),
      y = agg$y[keep]
    )
  }
}

# The built-in `fun.data` summaries of `mark_summary()`, by name: ggplot2's
# `mean_se()`, and base-R versions of its Hmisc wrappers `mean_cl_normal()`,
# `mean_sdl()` and `median_hilow()`. Each takes the finite values of one
# category and returns `c(y, ymin, ymax)`; `mult` / `conf.int` follow ggplot2.
.SUMMARY_FUNS <- list(
  mean_se = function(v, mult = 1) {
    m <- mean(v)
    se <- if (length(v) > 1L) stats::sd(v) / sqrt(length(v)) else NA_real_
    c(y = m, ymin = m - mult * se, ymax = m + mult * se)
  },
  mean_cl_normal = function(v, conf.int = 0.95) {
    m <- mean(v)
    n <- length(v)
    h <- if (n > 1L) {
      stats::qt((1 + conf.int) / 2, n - 1L) * stats::sd(v) / sqrt(n)
    } else {
      NA_real_
    }
    c(y = m, ymin = m - h, ymax = m + h)
  },
  mean_sdl = function(v, mult = 2) {
    m <- mean(v)
    s <- if (length(v) > 1L) stats::sd(v) else NA_real_
    c(y = m, ymin = m - mult * s, ymax = m + mult * s)
  },
  median_hilow = function(v, conf.int = 0.95) {
    q <- stats::quantile(v, c(0.5, (1 - conf.int) / 2, (1 + conf.int) / 2))
    c(y = q[[1]], ymin = q[[2]], ymax = q[[3]])
  }
)

# Turn `fun.data` (a name from `.SUMMARY_FUNS` or a function) into a function of
# one category's values returning `c(y, ymin, ymax)`. A user function may
# return a data frame or a named vector with those names (what ggplot2's
# `mean_se()` and the Hmisc wrappers return), so ggplot2's own helpers work.
.summary_fun_data <- function(fun.data, args) {
  f <- if (is.character(fun.data)) .SUMMARY_FUNS[[fun.data]] else fun.data
  function(v) {
    out <- do.call(f, c(list(v), args))
    out <- unlist(as.list(out)[c("y", "ymin", "ymax")])
    if (length(out) != 3L) {
      cli::cli_abort(c(
        "{.arg fun.data} must return {.field y}, {.field ymin} and {.field ymax}.",
        i = "For example {.code fun.data = \"mean_se\"}."
      ))
    }
    out
  }
}

# Summarise y per x category (per group) into a point and a range, for
# `mark_summary(fun.data = )` (or `fun.min` / `fun.max`), drawn as a point
# range: ggplot2's `stat_summary()`. Missing values are left out; a category
# with no finite value is dropped.
.stat_summary <- function(L) {
  x <- L$values$x
  y <- as.numeric(L$values$y)
  sp <- L$stat_params
  args <- sp$fun.args %||% list()
  summ <- if (!is.null(sp$fun.data)) {
    .summary_fun_data(sp$fun.data, args)
  } else {
    centre <- match.fun(sp$fun %||% "mean")
    lo <- if (!is.null(sp$fun.min)) match.fun(sp$fun.min)
    hi <- if (!is.null(sp$fun.max)) match.fun(sp$fun.max)
    function(v) {
      yc <- do.call(centre, c(list(v), args))
      c(
        y = yc,
        ymin = if (is.null(lo)) yc else do.call(lo, c(list(v), args)),
        ymax = if (is.null(hi)) yc else do.call(hi, c(list(v), args))
      )
    }
  }
  xlevs <- .cat_levels(x)
  xc <- as.character(x)
  grp <- .group_key(L)
  gc <- if (is.null(grp)) rep("", length(y)) else as.character(grp)
  key <- paste(xc, gc, sep = "\r")
  cells <- split(seq_along(y), factor(key, levels = unique(key)))
  rows <- lapply(cells, function(idx) {
    v <- y[idx]
    v <- v[is.finite(v)]
    if (!length(v)) {
      return(NULL)
    }
    s <- summ(v)
    data.frame(
      x = xc[idx[1]],
      g = gc[idx[1]],
      y = s[["y"]],
      ymin = s[["ymin"]],
      ymax = s[["ymax"]],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    return(.stat_drop_layer(
      "{.fn mark_summary} needs finite {.field y} values."
    ))
  }
  # a numeric x stays numeric (a continuous axis); a discrete x keeps its levels
  out$x <- if (is.numeric(x)) {
    as.numeric(out$x)
  } else {
    factor(out$x, levels = xlevs)
  }
  if (!is.null(grp)) {
    out$group <- factor(out$g, levels = .cat_levels(grp))
  }
  out$g <- NULL
  # one row per category in axis order (then group order), as ggplot2's
  o <- if (is.null(grp)) order(out$x) else order(out$x, out$group)
  out <- out[o, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Supported smoothing methods. "auto" resolves per group by point count
# (loess for small n, gam for large) -- ggplot2's rule.
.SMOOTH_METHODS <- c("auto", "lm", "loess", "glm", "gam", "rq")

# Resolve "auto" to a concrete method given a group's finite-point count.
.smooth_auto <- function(n) {
  if (n < 1000) "loess" else "gam"
}

# Fit one group with `method` and predict on the dense grid `xg`, returning a
# data frame of `(x, y[, ymin, ymax])`. The confidence band is a t-interval for
# lm/loess (residual df) and a normal interval for glm/gam (on the link scale,
# back-transformed); quantile regression (`rq`) draws the fitted line only.
.smooth_fit <- function(
  method,
  xi,
  yi,
  xg,
  se,
  level,
  span,
  formula,
  method.args
) {
  d <- data.frame(x = xi, y = yi)
  nd <- data.frame(x = xg)
  z <- stats::qnorm(1 - (1 - level) / 2)

  if (method == "lm") {
    fo <- formula %||% (y ~ x)
    fit <- do.call(stats::lm, c(list(formula = fo, data = d), method.args))
    pr <- stats::predict(fit, newdata = nd, se.fit = se)
    fitv <- if (se) pr$fit else pr
    out <- data.frame(x = xg, y = fitv)
    if (se) {
      t <- stats::qt(1 - (1 - level) / 2, fit$df.residual)
      out$ymin <- fitv - t * pr$se.fit
      out$ymax <- fitv + t * pr$se.fit
    }
    return(out)
  }

  if (method == "loess") {
    fo <- formula %||% (y ~ x)
    fit <- do.call(
      stats::loess,
      c(list(formula = fo, data = d, span = span), method.args)
    )
    pr <- stats::predict(fit, newdata = nd, se = se)
    fitv <- if (se) pr$fit else pr
    out <- data.frame(x = xg, y = fitv)
    if (se) {
      t <- stats::qt(1 - (1 - level) / 2, pr$df)
      out$ymin <- fitv - t * pr$se.fit
      out$ymax <- fitv + t * pr$se.fit
    }
    return(out)
  }

  if (method == "glm") {
    fo <- formula %||% (y ~ x)
    args <- method.args
    if (is.null(args$family)) {
      args$family <- stats::gaussian()
    }
    fit <- do.call(stats::glm, c(list(formula = fo, data = d), args))
    pr <- stats::predict(fit, newdata = nd, se.fit = se, type = "link")
    linkinv <- fit$family$linkinv
    fitv <- if (se) pr$fit else pr
    out <- data.frame(x = xg, y = linkinv(fitv))
    if (se) {
      out$ymin <- linkinv(fitv - z * pr$se.fit)
      out$ymax <- linkinv(fitv + z * pr$se.fit)
    }
    return(out)
  }

  if (method == "gam") {
    .need_pkg("mgcv", "{.code mark_smooth(method = \"gam\")}")
    fo <- formula %||% (y ~ s(x))
    # gam looks up smooth constructors (`s`, `te`, ...) in the formula's
    # environment; point it at mgcv's namespace so the default (and a bare
    # `y ~ s(x)` from the caller) resolves without mgcv being attached.
    environment(fo) <- asNamespace("mgcv")
    args <- method.args
    if (is.null(args$family)) {
      args$family <- stats::gaussian()
    }
    fit <- do.call(mgcv::gam, c(list(formula = fo, data = d), args))
    pr <- mgcv::predict.gam(fit, newdata = nd, se.fit = se, type = "link")
    linkinv <- fit$family$linkinv
    fitv <- if (se) pr$fit else pr
    out <- data.frame(x = xg, y = linkinv(fitv))
    if (se) {
      out$ymin <- linkinv(fitv - z * pr$se.fit)
      out$ymax <- linkinv(fitv + z * pr$se.fit)
    }
    return(out)
  }

  if (method == "rq") {
    .need_pkg("quantreg", "{.code mark_smooth(method = \"rq\")}")
    tau <- method.args$tau %||% 0.5
    if (length(tau) != 1) {
      cli::cli_abort(c(
        "{.fn mark_smooth} with {.val rq} takes a single {.arg tau}.",
        i = "For several quantiles, add one {.fn mark_smooth} layer per {.arg tau}."
      ))
    }
    fo <- formula %||% (y ~ x)
    fit <- quantreg::rq(fo, tau = tau, data = d)
    fitv <- as.numeric(stats::predict(fit, newdata = nd))
    return(data.frame(x = xg, y = fitv))
  }

  cli::cli_abort("Unknown smoothing method {.val {method}}.")
}

# Fit y ~ x (per group) with the requested method and predict on a dense grid,
# with a confidence ribbon where the method supports one.
.stat_smooth <- function(L) {
  method <- L$stat_params$method %||% "auto"
  ok_methods <- .SMOOTH_METHODS
  if (!method %in% ok_methods) {
    cli::cli_abort(c(
      "Unknown {.arg method} {.val {method}} for {.fn mark_smooth}.",
      i = "Use one of {.or {.val {ok_methods}}}."
    ))
  }
  se <- isTRUE(L$stat_params$se %||% TRUE)
  # Quantile regression has no symmetric confidence band; never draw a ribbon.
  if (identical(method, "rq")) {
    se <- FALSE
  }
  level <- L$stat_params$level %||% 0.95
  span <- L$stat_params$span %||% 0.75
  formula <- L$stat_params$formula
  method.args <- L$stat_params$method.args %||% list()
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  .per_group(
    L,
    length(x),
    function(idx, gn, grouped, k) {
      f <- .finite_xy(x[idx], y[idx])
      xi <- f$x
      yi <- f$y
      if (length(unique(xi)) < 2) {
        if (grouped) {
          cli::cli_warn(
            "Skipping {.field {gn}}: {.fn mark_smooth} needs at least 2 distinct x values."
          )
        }
        return(NULL)
      }
      m <- if (identical(method, "auto")) .smooth_auto(length(xi)) else method
      xg <- seq(min(xi), max(xi), length.out = 80)
      .smooth_fit(m, xi, yi, xg, se, level, span, formula, method.args)
    },
    "{.fn mark_smooth} needs at least 2 distinct x values to fit.",
    usable = is.finite(x) & is.finite(y)
  )
}

# Quantile regression lines (ggplot2's stat_quantile()): per group, one
# `quantreg::rq()` fit of `formula` (default `y ~ x`) per quantile, predicted on
# 100 points across the group's x range (or `xseq`). Each fitted line is its own
# `.piece`, numbered across groups, so the line emitter draws one polyline per
# (group, quantile); `quantile` is available to `after_stat()`.
.stat_quantile <- function(L) {
  .need_pkg("quantreg", "{.fn mark_quantile}")
  sp <- L$stat_params
  qs <- sp$quantiles %||% c(0.25, 0.5, 0.75)
  fo <- sp$formula %||% (y ~ x)
  margs <- sp$method.args %||% list()
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  .per_group(
    L,
    length(x),
    function(idx, gn, grouped, k) {
      f <- .finite_xy(x[idx], y[idx])
      if (length(unique(f$x)) < 2L) {
        if (grouped) {
          cli::cli_warn(
            "Skipping {.field {gn}}: {.fn mark_quantile} needs at least 2 distinct x values."
          )
        }
        return(NULL)
      }
      d <- data.frame(x = f$x, y = f$y)
      xg <- sp$xseq %||% seq(min(f$x), max(f$x), length.out = 100)
      nd <- data.frame(x = xg)
      parts <- lapply(seq_along(qs), function(j) {
        fit <- do.call(
          quantreg::rq,
          c(list(formula = fo, tau = qs[j], data = d), margs)
        )
        data.frame(
          x = xg,
          y = as.numeric(stats::predict(fit, newdata = nd)),
          quantile = qs[j],
          .piece = (k - 1L) * length(qs) + j
        )
      })
      do.call(rbind, parts)
    },
    "{.fn mark_quantile} needs at least 2 distinct x values to fit.",
    usable = is.finite(x) & is.finite(y)
  )
}

# The empirical cumulative distribution of x (per group): sorted x against the
# cumulative proportion, drawn as a right-continuous step.
.stat_ecdf <- function(L) {
  x <- as.numeric(L$values$x)
  .per_group(
    L,
    length(x),
    function(idx, gn, grouped, k) {
      xi <- sort(x[idx])
      xi <- xi[is.finite(xi)]
      if (!length(xi)) {
        return(NULL)
      }
      n <- length(xi)
      y <- seq_len(n) / n
      data.frame(x = xi, y = y, ecdf = y)
    },
    "{.fn mark_ecdf} needs at least one finite value.",
    usable = is.finite(x)
  )
}

# Sorted sample values against the theoretical quantiles of a reference
# distribution (per group): x = theoretical, y = sample.
.stat_qq <- function(L) {
  s0 <- L$values$sample %||% L$values$y %||% L$values$x
  s0 <- as.numeric(s0)
  dist <- match.fun(L$stat_params$distribution %||% "qnorm")
  .per_group(
    L,
    length(s0),
    function(idx, gn, grouped, k) {
      s <- sort(s0[idx])
      s <- s[is.finite(s)]
      if (!length(s)) {
        return(NULL)
      }
      theo <- dist(stats::ppoints(length(s)))
      data.frame(x = theo, y = s, sample = s, theoretical = theo)
    },
    "{.fn mark_qq} needs at least one finite sample value.",
    usable = is.finite(s0)
  )
}

# The Q-Q reference line (per group): the line through the 1st and 3rd sample
# quartiles vs the matching theoretical quartiles, drawn across the theoretical
# range.
.stat_qq_line <- function(L) {
  s0 <- L$values$sample %||% L$values$y %||% L$values$x
  s0 <- as.numeric(s0)
  dist <- match.fun(L$stat_params$distribution %||% "qnorm")
  .per_group(
    L,
    length(s0),
    function(idx, gn, grouped, k) {
      s <- sort(s0[idx])
      s <- s[is.finite(s)]
      if (length(s) < 2) {
        return(NULL)
      }
      theo <- dist(stats::ppoints(length(s)))
      sq <- stats::quantile(s, c(0.25, 0.75), names = FALSE)
      tq <- dist(c(0.25, 0.75))
      slope <- diff(sq) / diff(tq)
      intercept <- sq[1] - slope * tq[1]
      xr <- range(theo)
      data.frame(x = xr, y = intercept + slope * xr)
    },
    "{.fn mark_qq_line} needs at least 2 finite sample values.",
    usable = is.finite(s0)
  )
}

# A dot plot: bin x into equal-width bins and stack one dot per observation
# within its bin. Returns one row per observation with x = bin centre and
# y = stack height (0.5, 1.5, ...), for the point emitter.
.stat_dotplot <- function(L) {
  x0 <- as.numeric(L$values$x)
  # Each dot is one observation, so it keeps its input row (`.rows`): colour and
  # the other carried aesthetics follow their own row through the sort, instead
  # of being recycled in input order over the sorted dots.
  grp <- .group_key(L)
  ok <- which(is.finite(x0))
  # Within a bin the dots stack grouped (by `.group_key()` level, then x), so a
  # coloured stack reads as bands rather than an interleaved mix.
  gk <- if (is.null(grp)) {
    integer(length(x0))
  } else {
    match(as.character(grp), .cat_levels(grp))
  }
  rows <- ok[order(x0[ok], gk[ok])]
  x <- x0[rows]
  if (!length(x)) {
    return(.stat_drop_layer(
      "{.fn mark_dotplot} needs at least one finite value."
    ))
  }
  span <- diff(range(x))
  bw <- L$stat_params$binwidth %||% (if (span > 0) span / 30 else 1)
  if (!is.finite(bw) || bw <= 0) {
    bw <- 1
  }
  bin <- floor((x - min(x)) / bw)
  if (!is.null(grp)) {
    o <- order(bin, gk[rows])
    rows <- rows[o]
    x <- x[o]
    bin <- bin[o]
  }
  stack <- stats::ave(seq_along(bin), bin, FUN = seq_along)
  out <- data.frame(
    x = min(x) + (bin + 0.5) * bw,
    y = stack - 0.5,
    count = 1,
    .rows = rows
  )
  .stat_dropped(out, which(!is.finite(x0)))
}

# Window (rolling / cumulative / offset) operations on an x-ordered y series.
.WINDOW_OPS <- c(
  "mean",
  "sum",
  "median",
  "min",
  "max",
  "cumsum",
  "cummean",
  "cummax",
  "cummin",
  "lag",
  "lead",
  "rank"
)

# A sliding-window reduction of `y` with window size `k` and alignment
# ("right" = trailing, "left" = leading, "center"). `partial = TRUE` computes at
# the edges from the available (shorter) window; `FALSE` leaves them NA.
.roll <- function(y, k, fun, align, partial) {
  n <- length(y)
  out <- rep(NA_real_, n)
  lead_n <- switch(
    align,
    right = 0L,
    left = k - 1L,
    center = ceiling((k - 1) / 2)
  )
  lag_n <- k - 1L - lead_n
  for (i in seq_len(n)) {
    lo <- max(i - lag_n, 1L)
    hi <- min(i + lead_n, n)
    if (!partial && (hi - lo + 1L) < k) {
      next
    }
    out[i] <- fun(y[lo:hi])
  }
  out
}

# Apply one window op to an x-ordered `y`.
.window_apply <- function(y, op, k, align, partial) {
  n <- length(y)
  switch(
    op,
    cumsum = cumsum(y),
    cummean = cumsum(y) / seq_len(n),
    cummax = cummax(y),
    cummin = cummin(y),
    rank = as.numeric(rank(y, ties.method = "average")),
    lag = c(rep(NA_real_, min(k, n)), utils::head(y, -k)),
    lead = c(utils::tail(y, -k), rep(NA_real_, min(k, n))),
    mean = .roll(y, k, mean, align, partial),
    sum = .roll(y, k, sum, align, partial),
    median = .roll(y, k, stats::median, align, partial),
    min = .roll(y, k, min, align, partial),
    max = .roll(y, k, max, align, partial)
  )
}

# Window transform: a rolling / cumulative / offset statistic of `y` computed
# per group, over rows ordered by `x`. Output is the standard `(x, y[, group])`
# stat shape, so any line/point emitter draws it unchanged. Rows where the
# statistic is undefined (non-partial edges, lag/lead shifts) are dropped, so a
# line does not dip to a gap.
.stat_window <- function(L) {
  op <- L$stat_params$op
  ok_ops <- .WINDOW_OPS
  if (is.null(op) || !op %in% ok_ops) {
    cli::cli_abort(c(
      "{.fn mark_line} {.arg window} needs an {.field op}.",
      i = "Use one of {.or {.val {ok_ops}}}."
    ))
  }
  # Rolling windows default to 7; lag/lead shift by 1 unless told otherwise.
  k <- L$stat_params$k %||% (if (op %in% c("lag", "lead")) 1L else 7L)
  if (!is.numeric(k) || length(k) != 1L || is.na(k) || k < 1 || k != round(k)) {
    cli::cli_abort(c(
      "{.fn mark_line} {.arg window} needs a positive integer {.field k}.",
      i = "Got {.obj_type_friendly {k}}."
    ))
  }
  k <- as.integer(k)
  align <- L$stat_params$align %||% "right"
  partial <- L$stat_params$partial %||% TRUE
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  grp <- .group_key(L)
  g <- .stat_groups(grp, length(x))
  glevs <- g$levels
  gnames <- names(g$groups)
  parts <- lapply(gnames, function(gn) {
    i <- g$groups[[gn]]
    o <- order(x[i])
    xi <- x[i][o]
    yi <- y[i][o]
    yo <- .window_apply(yi, op, k, align, partial)
    df <- data.frame(x = xi, y = yo, .rows = i[o])
    if (!is.null(grp)) {
      df$group <- factor(gn, levels = glevs)
    }
    df[is.finite(df$y), , drop = FALSE]
  })
  parts <- parts[vapply(parts, nrow, integer(1)) > 0]
  if (!length(parts)) {
    cli::cli_abort("{.fn mark_line} {.arg window} produced no finite values.")
  }
  out <- do.call(rbind, parts)
  .stat_dropped(out, setdiff(seq_along(x), out$.rows))
}

# Require x and y for the region stats (ellipse/hull), with a clear message.
.need_xy <- function(L, what) {
  if (is.null(L$values$x) || is.null(L$values$y)) {
    cli::cli_abort("{what} needs both {.arg x} and {.arg y}.")
  }
}

# Boundary vertices of a data-space confidence ellipse for one group, following
# ggplot2's `stat_ellipse`: a unit circle transformed by the Cholesky factor of
# the (robust `t`, normal, or Euclidean) covariance, scaled by an F-quantile
# radius, and translated to the centre. Returns a `segments+1 x 2` matrix.
.ellipse_points <- function(x, y, type, level, segments) {
  n <- length(x)
  dfn <- 2
  dfd <- n - 1
  angles <- (0:segments) * 2 * pi / segments
  unit <- cbind(cos(angles), sin(angles))
  if (type == "euclid") {
    # A circle of radius `level` (data units) about the mean -- no covariance.
    centre <- c(mean(x), mean(y))
    pts <- sweep(level * unit, 2, centre, "+")
    return(pts)
  }
  if (type == "t") {
    .need_pkg("MASS", "{.code mark_ellipse(type = \"t\")}")
    fit <- MASS::cov.trob(cbind(x, y))
    cov <- fit$cov
    centre <- fit$center
  } else {
    cov <- stats::cov(cbind(x, y))
    centre <- c(mean(x), mean(y))
  }
  radius <- sqrt(dfn * stats::qf(level, dfn, dfd))
  chol_decomp <- chol(cov)
  t(centre + radius * t(unit %*% chol_decomp))
}

# Per-group confidence/data ellipse. Emits ordered boundary vertices with a
# per-group `.piece` (so each ellipse is a distinct closed polygon) and the
# `group` column so colour/fill flows back through `.merge_stat`.
.stat_ellipse <- function(L) {
  .need_xy(L, "{.fn mark_ellipse}")
  type <- L$stat_params$type %||% "t"
  if (!type %in% c("t", "norm", "euclid")) {
    cli::cli_abort(c(
      "Unknown ellipse {.arg type} {.val {type}}.",
      i = "Use one of {.or {.val {c('t', 'norm', 'euclid')}}}."
    ))
  }
  level <- L$stat_params$level %||% 0.95
  segments <- L$stat_params$segments %||% 51L
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  .per_group(
    L,
    length(x),
    function(idx, gn, grouped, k) {
      f <- .finite_xy(x[idx], y[idx])
      xi <- f$x
      yi <- f$y
      if (length(xi) < 3) {
        if (grouped) {
          cli::cli_warn(
            "Skipping {.field {gn}}: {.fn mark_ellipse} needs at least 3 points."
          )
        }
        return(NULL)
      }
      pts <- .ellipse_points(xi, yi, type, level, segments)
      data.frame(x = pts[, 1], y = pts[, 2], .piece = k)
    },
    "{.fn mark_ellipse} needs at least 3 points to fit.",
    usable = is.finite(x) & is.finite(y)
  )
}

# Per-group convex hull. Emits the hull vertices (counter-clockwise) with a
# per-group `.piece`; `polygon_grob` closes the ring.
.stat_hull <- function(L) {
  .need_xy(L, "{.fn mark_hull}")
  x <- as.numeric(L$values$x)
  y <- as.numeric(L$values$y)
  expand <- L$stat_params$expand %||% 0
  # The margin is measured against the whole layer's extent, so every group's
  # hull grows by the same amount.
  xspan <- diff(.finite_range(x) %||% c(0, 1))
  yspan <- diff(.finite_range(y) %||% c(0, 1))
  .per_group(
    L,
    length(x),
    function(idx, gn, grouped, k) {
      f <- .finite_xy(x[idx], y[idx])
      xi <- f$x
      yi <- f$y
      if (length(xi) < 3) {
        if (grouped) {
          cli::cli_warn(
            "Skipping {.field {gn}}: {.fn mark_hull} needs at least 3 points."
          )
        }
        return(NULL)
      }
      # chull() lists the hull clockwise; reverse to counter-clockwise.
      h <- rev(grDevices::chull(xi, yi))
      hx <- xi[h]
      hy <- yi[h]
      # `expand` grows the hull outward so it encloses the node markers (whose
      # centres the raw hull passes through). 0 = the tight hull.
      if (expand > 0) {
        off <- .offset_hull(hx, hy, expand, xspan, yspan)
        hx <- off$x
        hy <- off$y
      }
      data.frame(x = hx, y = hy, .piece = k)
    },
    "{.fn mark_hull} needs at least 3 points.",
    usable = is.finite(x) & is.finite(y)
  )
}

# Grow a convex polygon (counter-clockwise vertices `x`, `y`) outward by a
# uniform margin with rounded corners: its Minkowski sum with a disc, the
# outline ggforce's hull marks draw. The margin is `expand` of the layer's span
# on each axis (`xspan`, `yspan`), so the offset is done in coordinates where
# both spans are 1 and is the same on every side however long, thin or small
# the hull is. (Scaling about the centroid, as before, barely thickened a long
# thin hull and grew a large one more than a small one.) Each vertex becomes an
# arc between the outward normals of its two edges, with a point every
# `step` radians at most.
.offset_hull <- function(x, y, expand, xspan, yspan, step = pi / 18) {
  sx <- if (isTRUE(xspan > 0)) xspan else 1
  sy <- if (isTRUE(yspan > 0)) yspan else 1
  u <- x / sx
  v <- y / sy
  m <- length(u)
  nxt <- c(seq_len(m)[-1L], 1L)
  # outward normal angle of each edge i -> nxt[i] (counter-clockwise ring)
  ang <- atan2(-(u[nxt] - u), v[nxt] - v)
  ox <- list()
  oy <- list()
  for (i in seq_len(m)) {
    a0 <- ang[c(m, seq_len(m - 1L))][i] # the edge arriving at vertex i
    a1 <- ang[i] # the edge leaving it
    while (a1 < a0) {
      a1 <- a1 + 2 * pi
    }
    a <- seq(a0, a1, length.out = max(2L, ceiling((a1 - a0) / step) + 1L))
    ox[[i]] <- u[i] + expand * cos(a)
    oy[[i]] <- v[i] + expand * sin(a)
  }
  list(x = unlist(ox) * sx, y = unlist(oy) * sy)
}
