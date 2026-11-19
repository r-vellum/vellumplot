#' @include classes.R
NULL

# Bivariate choropleths: two variables on one fill.
#
# The grammar is one-aesthetic-one-variable, so the two variables are folded
# into one *before* they reach a channel: `bi_class()` bins each variable over
# the whole table and writes a single "i-j" class column (biscale's design).
# The fill then maps that one column, and `scale_fill_bivariate()` is an
# ordinary discrete colour scale whose palette is a dim x dim matrix and whose
# guide is the 2-D `guide_bivariate()` grid. Classing over the whole table (not
# inside the channel) is what keeps the classes identical across facet panels,
# which each resolve their channels on a subset. The scale stays serializable:
# it carries a palette name, the class count, and the axis titles -- no
# quosures.
#
# `bi_class()` records its axis variables on the class column (the
# `vellum_bivariate` attribute). The trainer reads it from the plot or layer
# data, not from the resolved values, because subsetting a factor for a facet
# panel drops custom attributes.

# Joshua Stevens' bivariate schemes, each a 3x3 matrix indexed [x class, y
# class] (column-major below: the first three entries are y class 1). Other
# class counts are interpolated from these in Lab space.
.BIVARIATE_PALETTES <- list(
  DkBlue = c(
    "#e8e8e8",
    "#b5c0da",
    "#6c83b5",
    "#b8d6be",
    "#90b2b3",
    "#567994",
    "#73ae80",
    "#5a9178",
    "#2a5a5b"
  ),
  GrPink = c(
    "#e8e8e8",
    "#e4acac",
    "#c85a5a",
    "#b0d5df",
    "#ad9ea5",
    "#985356",
    "#64acbe",
    "#627f8c",
    "#574249"
  ),
  DkViolet = c(
    "#e8e8e8",
    "#ace4e4",
    "#5ac8c8",
    "#dfb0d6",
    "#a5add3",
    "#5698b9",
    "#be64ac",
    "#8c62aa",
    "#3b4994"
  ),
  Brown = c(
    "#e8e8e8",
    "#e4d9ac",
    "#c8b35a",
    "#cbb8d7",
    "#c8ada0",
    "#af8e53",
    "#9972af",
    "#976b82",
    "#804d36"
  )
)

.BIVARIATE_DIMS <- 2:4

# The class labels "i-j" for a dim x dim matrix, x varying fastest (so the
# vector lines up with a column-major [x, y] matrix).
.bi_levels <- function(dim) {
  paste(rep(seq_len(dim), dim), rep(seq_len(dim), each = dim), sep = "-")
}

.check_bi_dim <- function(dim, call = rlang::caller_env()) {
  if (
    !is.numeric(dim) ||
      length(dim) != 1L ||
      !is.finite(dim) ||
      !dim %in% .BIVARIATE_DIMS
  ) {
    cli::cli_abort(
      "{.arg dim} must be one of {.val {(.BIVARIATE_DIMS)}}.",
      call = call
    )
  }
  as.integer(dim)
}

# Interpolate a 3x3 [x, y] colour matrix to dim x dim, bilinearly in Lab.
.bi_interp <- function(cols3, dim) {
  if (dim == 3L) {
    return(cols3)
  }
  lab <- farver::decode_colour(cols3, to = "lab")
  g <- 1 + 2 * (seq_len(dim) - 1) / (dim - 1)
  out <- character(dim * dim)
  k <- 0L
  for (yy in g) {
    for (xx in g) {
      x0 <- min(floor(xx), 2)
      y0 <- min(floor(yy), 2)
      fx <- xx - x0
      fy <- yy - y0
      at <- function(i, j) lab[(j - 1) * 3 + i, ]
      v <- (1 - fx) *
        (1 - fy) *
        at(x0, y0) +
        fx * (1 - fy) * at(x0 + 1, y0) +
        (1 - fx) * fy * at(x0, y0 + 1) +
        fx * fy * at(x0 + 1, y0 + 1)
      k <- k + 1L
      out[k] <- farver::encode_colour(matrix(v, nrow = 1), from = "lab")
    }
  }
  out
}

#' Bivariate palettes
#'
#' `bi_pal()` returns the colours of a bivariate palette: a `dim` x `dim` matrix
#' of colours, one per pair of classes, flattened to a vector named by class
#' (`"1-1"`, `"2-1"`, ..., `"3-3"`; the first number is the `x` class, the second
#' the `y` class). It is what [scale_fill_bivariate()] colours each class with.
#'
#' The built-in palettes are Joshua Stevens' four 3x3 schemes; for `dim = 2` and
#' `dim = 4` they are interpolated in Lab space from the 3x3 grid, so the corner
#' colours are the same at every size. The lightest corner (`"1-1"`) is low on
#' both variables, the darkest (`"3-3"` at `dim = 3`) high on both.
#'
#' @param palette A palette name: `"DkBlue"` (default), `"GrPink"`,
#'   `"DkViolet"` or `"Brown"`.
#' @param dim Classes per variable: `2`, `3` (default) or `4`.
#' @return A named character vector of `dim^2` colours.
#' @seealso [bi_class()], [scale_fill_bivariate()]
#' @family bivariate
#' @export
#' @examples
#' bi_pal("GrPink")
#' bi_pal("DkBlue", dim = 2)
bi_pal <- function(palette = "DkBlue", dim = 3) {
  dim <- .check_bi_dim(dim)
  if (
    !is.character(palette) ||
      length(palette) != 1L ||
      !palette %in% names(.BIVARIATE_PALETTES)
  ) {
    cli::cli_abort(
      "{.arg palette} must be one of {.val {names(.BIVARIATE_PALETTES)}}."
    )
  }
  stats::setNames(
    .bi_interp(.BIVARIATE_PALETTES[[palette]], dim),
    .bi_levels(dim)
  )
}

# The dim^2 colours for a scale's `palette`: a built-in name, or the user's own
# colours -- named by class ("1-1", ...) or unnamed in class order (x fastest).
.bi_colours <- function(palette, dim, call = rlang::caller_env()) {
  palette <- palette %||% "DkBlue"
  if (is.character(palette) && length(palette) == 1L) {
    return(bi_pal(palette, dim))
  }
  lv <- .bi_levels(dim)
  if (!is.character(palette) || length(palette) != length(lv)) {
    cli::cli_abort(
      c(
        "A custom bivariate {.arg palette} needs {length(lv)} colours for {.code dim = {dim}}.",
        i = "Name them by class ({.val {lv[1:2]}}, ...) or give them in that order."
      ),
      call = call
    )
  }
  if (!is.null(names(palette))) {
    if (!setequal(names(palette), lv)) {
      cli::cli_abort(
        "A named bivariate {.arg palette} must be named {.val {lv}}.",
        call = call
      )
    }
    palette <- palette[lv]
  }
  stats::setNames(unname(palette), lv)
}

#' Classify two variables for a bivariate map
#'
#' `bi_class()` bins two numeric columns into `dim` classes each and adds one
#' column holding the pair of classes as `"x-y"` (`"1-1"` is low on both, `"3-3"`
#' high on both at `dim = 3`). Map a fill to that column and add
#' [scale_fill_bivariate()] to colour it with a bivariate palette and draw the
#' 2-D legend. The design follows the `biscale` package's `bi_class()`.
#'
#' Classes are computed over the whole table, so a faceted map shares one set of
#' breaks across its panels. `bi_class()` works on a plain data frame or an `sf`
#' table (the geometry is kept). It records the two variable names on the new
#' column, which [scale_fill_bivariate()] uses as the legend's axis titles when
#' you don't give your own.
#'
#' @param data A data frame or `sf` object.
#' @param x,y The two numeric variables (bare column names). `x` runs along the
#'   legend's horizontal axis, `y` up its vertical one.
#' @param dim Classes per variable: `2`, `3` (default) or `4`.
#' @param style How to choose the class breaks: `"quantile"` (default),
#'   `"equal"`, `"pretty"`, or any `classInt::classIntervals()` style (needs the
#'   `classInt` package) --- the same styles as [scale_fill_binned()].
#' @param name Name of the new column (default `"bi_class"`).
#' @return `data` with a factor column `name` whose levels are all `dim^2`
#'   classes in order. Rows where `x` or `y` is missing get `NA`.
#' @seealso [scale_fill_bivariate()], [bi_pal()]
#' @family bivariate
#' @export
#' @examples
#' d <- bi_class(mtcars, hp, wt)
#' table(d$bi_class)
bi_class <- function(
  data,
  x,
  y,
  dim = 3,
  style = "quantile",
  name = "bi_class"
) {
  if (!is.data.frame(data)) {
    cli::cli_abort("{.arg data} must be a data frame or {.cls sf} object.")
  }
  dim <- .check_bi_dim(dim)
  if (!is.character(style) || length(style) != 1L) {
    cli::cli_abort("{.arg style} must be a single string.")
  }
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a single non-empty string.")
  }
  xq <- rlang::enquo(x)
  yq <- rlang::enquo(y)
  # An sf table evaluates like a data frame; drop the class so the geometry
  # column's own methods stay out of the data mask.
  mask <- as.data.frame(data)
  xv <- rlang::eval_tidy(xq, data = mask)
  yv <- rlang::eval_tidy(yq, data = mask)
  for (v in list(list(xv, "x"), list(yv, "y"))) {
    if (!is.numeric(v[[1]]) || length(v[[1]]) != nrow(data)) {
      cli::cli_abort(
        "{.arg {v[[2]]}} must be a numeric column of {.arg data}."
      )
    }
  }
  xb <- .binned_breaks(xv, dim, style)
  yb <- .binned_breaks(yv, dim, style)
  bin <- function(v, b) {
    i <- findInterval(v, b, rightmost.closed = TRUE, all.inside = TRUE)
    i[!is.finite(v)] <- NA_integer_
    i
  }
  xi <- bin(xv, xb)
  yi <- bin(yv, yb)
  cls <- ifelse(is.na(xi) | is.na(yi), NA_character_, paste(xi, yi, sep = "-"))
  out <- factor(cls, levels = .bi_levels(dim))
  attr(out, "vellum_bivariate") <- list(
    dim = dim,
    style = style,
    x = rlang::as_label(xq),
    y = rlang::as_label(yq),
    x_breaks = xb,
    y_breaks = yb
  )
  data[[name]] <- out
  data
}

#' Bivariate colour scales
#'
#' `scale_fill_bivariate()` colours a [bi_class()] column with a bivariate
#' palette and draws its legend as a `dim` x `dim` grid of swatches, the `x`
#' variable along the bottom and the `y` variable up the side (see
#' [guide_bivariate()]). It is the scale half of a bivariate choropleth; it works
#' on any mark with a fill (`mark_sf()` polygons, tiles, bars, points), and
#' `scale_color_bivariate()` does the same for `color`.
#'
#' A column made by [bi_class()] is recognised on its own, so the scale is only
#' needed to change the palette or the axis titles. The class count is read from
#' that column too. A plain character or factor column of `"x-y"` class labels
#' also works with an explicit `dim`.
#'
#' Hover and selection work as for any discrete colour scale: on an interactive
#' plot each legend swatch is keyed to the features in its class.
#'
#' @param plot A [PlotSpec].
#' @param palette A palette name for [bi_pal()] (`"DkBlue"`, `"GrPink"`,
#'   `"DkViolet"`, `"Brown"`), or your own `dim^2` colours, named by class
#'   (`"1-1"`, `"2-1"`, ...) or given in that order.
#' @param dim Classes per variable (`2`, `3` or `4`). `NULL` (default) takes it
#'   from the [bi_class()] column, falling back to `3`.
#' @param xlab,ylab The legend's axis titles. `NULL` uses the variable names
#'   recorded by [bi_class()].
#' @param name A title above the legend grid. `NULL` (default) draws none ---
#'   the axis titles already say what the colours mean. [labs()] also sets it.
#' @param na_value Fill for features with a missing class. Default `"grey80"`.
#' @return The modified [PlotSpec].
#' @seealso [bi_class()], [guide_bivariate()], [bi_pal()]
#' @family bivariate
#' @export
#' @examples
#' d <- bi_class(mtcars, hp, wt)
#' vplot(d) |>
#'   mark_point(x = hp, y = wt, color = bi_class, size = 2) |>
#'   scale_color_bivariate(palette = "GrPink", xlab = "Power", ylab = "Weight")
#'
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#'   nc <- bi_class(nc, BIR74, SID74)
#'   vplot(nc) |>
#'     mark_sf(fill = bi_class, color = "white", linewidth = 0.2) |>
#'     scale_fill_bivariate(xlab = "Births", ylab = "SIDS cases")
#' }
scale_fill_bivariate <- function(
  plot,
  palette = "DkBlue",
  dim = NULL,
  xlab = NULL,
  ylab = NULL,
  name = NULL,
  na_value = "grey80"
) {
  .bivariate_scale(plot, "fill", palette, dim, xlab, ylab, name, na_value)
}

#' @rdname scale_fill_bivariate
#' @export
scale_color_bivariate <- function(
  plot,
  palette = "DkBlue",
  dim = NULL,
  xlab = NULL,
  ylab = NULL,
  name = NULL,
  na_value = "grey80"
) {
  .bivariate_scale(plot, "color", palette, dim, xlab, ylab, name, na_value)
}

#' @rdname scale_fill_bivariate
#' @export
scale_colour_bivariate <- scale_color_bivariate

.bivariate_scale <- function(
  plot,
  aesthetic,
  palette,
  dim,
  xlab,
  ylab,
  name,
  na_value
) {
  .check_plot(plot)
  if (!is.null(dim)) {
    dim <- .check_bi_dim(dim)
    # Fail at the call, not at compile time.
    .bi_colours(palette, dim)
  } else if (!(is.character(palette) && length(palette) == 1L)) {
    cli::cli_abort(
      "A custom bivariate {.arg palette} needs an explicit {.arg dim}."
    )
  }
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = "bivariate",
      palette = palette,
      n = dim,
      name = name,
      na_value = na_value,
      guide = guide_bivariate(xlab = xlab, ylab = ylab)
    )
  )
}

#' A 2-D legend for a bivariate scale
#'
#' `guide_bivariate()` draws a bivariate colour scale as a square grid of
#' swatches: the `x` variable's classes run left to right along the bottom, the
#' `y` variable's classes bottom to top up the side, each axis with its title
#' and an arrow pointing towards "more". When the classes come from
#' [bi_class()], each axis is labelled with its class breaks at the swatch
#' edges. `rotate = TRUE` turns the grid 45 degrees into a diamond, low-low at
#' the bottom and high-high at the top. It is the default guide of
#' [scale_fill_bivariate()]; use it through [guides()] to retitle the axes or
#' resize the swatches without respelling the scale. Asking for it on a fill that
#' maps a [bi_class()]-style column makes that scale bivariate.
#'
#' @param title A title above the grid, or `NULL` (default) for none.
#' @param xlab,ylab The axis titles, or `NULL` to keep the scale's (or the
#'   variable names recorded by [bi_class()]).
#' @param key.size Side of one swatch, in millimetres. `NULL` (default) uses
#'   1.5 x the theme's `legend.key.size`, widened to fit the break labels.
#' @param labels Label the swatch edges with the class breaks [bi_class()]
#'   recorded (default `TRUE`). A scale without recorded breaks (a hand-made
#'   class column) has none to show.
#' @param rotate Turn the grid 45 degrees, so the classes read as a diamond
#'   with high-high at the top (default `FALSE`).
#' @return A guide specification for [guides()].
#' @seealso [scale_fill_bivariate()]
#' @family bivariate
#' @export
#' @examples
#' d <- bi_class(mtcars, hp, wt)
#' vplot(d) |>
#'   mark_point(x = hp, y = wt, color = bi_class, size = 2) |>
#'   guides(color = guide_bivariate(xlab = "More power", ylab = "More weight"))
#'
#' # the diamond form
#' vplot(d) |>
#'   mark_point(x = hp, y = wt, color = bi_class, size = 2) |>
#'   guides(color = guide_bivariate(rotate = TRUE))
guide_bivariate <- function(
  title = NULL,
  xlab = NULL,
  ylab = NULL,
  key.size = NULL,
  labels = TRUE,
  rotate = FALSE
) {
  for (a in list(list(labels, "labels"), list(rotate, "rotate"))) {
    if (!is.logical(a[[1]]) || length(a[[1]]) != 1L || is.na(a[[1]])) {
      cli::cli_abort("{.arg {a[[2]]}} must be {.code TRUE} or {.code FALSE}.")
    }
  }
  for (a in list(list(xlab, "xlab"), list(ylab, "ylab"))) {
    if (!is.null(a[[1]]) && !(is.character(a[[1]]) && length(a[[1]]) == 1L)) {
      cli::cli_abort("{.arg {a[[2]]}} must be a single string or {.code NULL}.")
    }
  }
  list(
    kind = "bivariate",
    title = title,
    xlab = xlab,
    ylab = ylab,
    key_size = .check_bar_dim(key.size, "key.size"),
    labels = labels,
    rotate = rotate
  )
}

# --- training ---------------------------------------------------------------

# The `bi_class()` record (dim, axis names, breaks) for the column a layer maps
# to color/fill, or NULL. Looked up on the plot/layer data by column name,
# since resolved values may be per-panel subsets that lost the attribute.
.bivariate_info <- function(spec) {
  for (layer in spec@layers) {
    for (a in c("fill", "color")) {
      ch <- layer@encoding[[a]]
      if (is.null(ch)) {
        next
      }
      e <- rlang::quo_get_expr(ch@expr)
      if (!is.symbol(e)) {
        next
      }
      d <- layer@data %||% spec@data
      if (!is.data.frame(d)) {
        next
      }
      col <- d[[as.character(e)]]
      info <- attr(col, "vellum_bivariate", exact = TRUE)
      if (!is.null(info)) {
        return(info)
      }
    }
  }
  NULL
}

# Train a bivariate colour scale. It comes out as an ordinary *discrete* scale
# (so mark colouring, legend membership for hover, and composition pooling all
# work unchanged) plus a `bivariate` record that routes its legend to the 2-D
# grid drawer. `title` is the titles record's `named` entry: only a title the
# user gave, since the grid's axis labels already say what is mapped.
.train_bivariate <- function(
  title,
  scalespec,
  values,
  info,
  na_value,
  has_na,
  key_glyph,
  aes
) {
  dim <- if (!is.null(scalespec) && !is.null(scalespec@n)) {
    as.integer(scalespec@n)
  } else {
    as.integer(info$dim %||% 3L)
  }
  pal <- if (!is.null(scalespec)) scalespec@palette else NULL
  cols <- .bi_colours(pal, dim)
  lv <- names(cols)
  seen <- unique(as.character(unlist(lapply(values, as.character))))
  seen <- seen[!is.na(seen)]
  bad <- setdiff(seen, lv)
  if (length(bad)) {
    cli::cli_abort(c(
      "A bivariate {.field {aes}} scale needs {.val x-y} class labels for {.code dim = {dim}}.",
      x = "Found {.val {utils::head(bad, 3)}}.",
      i = "Make the column with {.fn bi_class}, or set {.arg dim} to match it."
    ))
  }
  map <- function(x) {
    out <- unname(cols[as.character(x)])
    out[is.na(out)] <- na_value
    out
  }
  trained_scale(
    aes,
    "discrete",
    map = map,
    name = title,
    levels = lv,
    labels = lv,
    colors = unname(cols),
    na = has_na,
    na_value = na_value,
    key_glyph = key_glyph,
    bivariate = list(
      dim = dim,
      xlab = info$x %||% "x",
      ylab = info$y %||% "y",
      x_breaks = if (identical(info$dim, dim)) info$x_breaks,
      y_breaks = if (identical(info$dim, dim)) info$y_breaks,
      key_size = NULL,
      labels = TRUE,
      rotate = FALSE
    )
  )
}

# guide_bivariate() onto a trained scale: retitle the axes / resize the keys.
.apply_bivariate_guide <- function(trained, guide) {
  if (is.null(trained$bivariate)) {
    cli::cli_abort(c(
      "{.fn guide_bivariate} needs a bivariate colour scale.",
      i = "Map the fill to a {.fn bi_class} column, or add {.fn scale_fill_bivariate}."
    ))
  }
  for (f in c("xlab", "ylab", "key_size", "labels", "rotate")) {
    if (!is.null(guide[[f]])) {
      trained$bivariate[[f]] <- guide[[f]]
    }
  }
  trained
}

# --- the 2-D legend ---------------------------------------------------------

# Geometry (mm) of a bivariate guide, shared by the measurers and the drawer.
# The body is a block of four columns -- the rotated y title, the y arrow, the
# y break labels, the grid -- over four rows -- the grid, the x break labels,
# the x arrow, the x title. Both titles start at the grid's low-low corner, so
# a title longer than the grid side grows the guide instead of colliding. A
# rotated guide turns that block 45 degrees, inside a square cell as wide as
# its diagonal extent.
.bi_geom <- function(g, m) {
  bi <- g$sc$bivariate
  arrow <- 2.4
  lab_fs <- 0.8 * m$text_fs
  ticks <- .bi_tick_labels(bi)
  lab_gap <- 0.6
  s <- bi$key_size %||%
    max(
      1.5 * m$key,
      if (!is.null(ticks$x)) .mm_tw(ticks$x, lab_fs) + 1
    )
  side <- bi$dim * s
  xw <- .mm_tw(bi$xlab, m$text_fs)
  yw <- .mm_tw(bi$ylab, m$text_fs)
  xlab_h <- if (!is.null(ticks$x)) .mm_th(lab_fs) + lab_gap else 0
  ylab_w <- if (!is.null(ticks$y)) .mm_tw(ticks$y, lab_fs) + lab_gap else 0
  show_title <- isTRUE(m$show_title) && !is.null(g$sc$name)
  # The y arrowhead reaches the grid's top edge, so a title gets a row gap more
  # clearance than a keyed legend's.
  th <- if (show_title) m$title_h + m$title_gap + m$row_gap else 0
  body_h <- max(side, yw)
  body_w <- max(side, xw)
  block_w <- m$text_h + arrow + ylab_w + body_w
  block_h <- body_h + xlab_h + arrow + m$text_h
  rotate <- isTRUE(bi$rotate)
  cell <- if (rotate) {
    d <- (block_w + block_h) / sqrt(2)
    c(d, d)
  } else {
    c(block_w, block_h)
  }
  list(
    s = s,
    side = side,
    arrow = arrow,
    th = th,
    show_title = show_title,
    body_h = body_h,
    body_w = body_w,
    ticks = ticks,
    lab_fs = lab_fs,
    lab_gap = lab_gap,
    xlab_h = xlab_h,
    ylab_w = ylab_w,
    block_w = block_w,
    block_h = block_h,
    rotate = rotate,
    cell = cell,
    width = max(
      m$pad + cell[1],
      if (show_title) .mm_tw_any(g$sc$name, m$title_fs) else 0
    ),
    height = th + cell[2] + m$row_gap
  )
}

# The class-break labels of each axis (one per swatch edge, dim + 1), or NULL
# for an axis whose breaks bi_class() did not record or when the guide turns
# them off.
.bi_tick_labels <- function(bi) {
  lab <- function(b) {
    if (!isTRUE(bi$labels %||% TRUE) || length(b) != bi$dim + 1L) {
      return(NULL)
    }
    .steps_break_labels(list(breaks = b))
  }
  list(x = lab(bi$x_breaks), y = lab(bi$y_breaks))
}

# Hover text for the swatch of class (i, j): the class ranges when bi_class()
# recorded the breaks, else the class indices.
.bi_swatch_tooltip <- function(bi, i, j) {
  rng <- function(b, k) {
    if (is.null(b) || length(b) < k + 1L) {
      return(paste("class", k))
    }
    .interval_labels(b)[k]
  }
  paste0(
    bi$xlab,
    ": ",
    rng(bi$x_breaks, i),
    "\n",
    bi$ylab,
    ": ",
    rng(bi$y_breaks, j)
  )
}

.draw_guide_bivariate <- function(scene, g, m, rt) {
  sc <- g$sc
  bi <- sc$bivariate
  geo <- .bi_geom(g, m)
  txt <- .el_gpar_text(rt[["legend.text"]])
  mm <- function(v) vellum::vl_unit(v, "mm")
  # Trailing `null` tracks absorb whatever the legend cell has beyond the
  # guide's measured extent, so the guide is pinned to the top-left.
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      layout = vellum::grid_layout(
        heights = .c_units(
          if (geo$show_title) mm(geo$th),
          mm(geo$cell[2]),
          vellum::vl_unit(1, "null")
        ),
        widths = .c_units(
          mm(m$pad),
          mm(geo$cell[1]),
          vellum::vl_unit(1, "null")
        )
      )
    )
  )
  off <- if (geo$show_title) 1L else 0L
  if (geo$show_title) {
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(row = 1, col = 1, colspan = 3)
    )
    scene <- .draw_guide_title(scene, sc$name, rt)
    scene <- vellum::pop(scene)
  }
  scene <- vellum::push(scene, vellum::vl_viewport(row = off + 1L, col = 2))
  # The body block: the whole cell, or turned 45 degrees about the cell's
  # centre for the diamond.
  scene <- vellum::push(
    scene,
    vellum::vl_viewport(
      width = mm(geo$block_w),
      height = mm(geo$block_h),
      angle = if (geo$rotate) 45 else 0,
      layout = vellum::grid_layout(
        heights = .c_units(
          mm(geo$body_h),
          mm(geo$xlab_h),
          mm(geo$arrow),
          mm(m$text_h)
        ),
        widths = .c_units(
          mm(m$text_h),
          mm(geo$arrow),
          mm(geo$ylab_w),
          mm(geo$body_w)
        )
      )
    )
  )
  scene <- .draw_bi_body(scene, g, geo, m, rt, txt)
  scene <- vellum::pop(scene) # body block
  scene <- vellum::pop(scene) # body cell
  vellum::pop(scene)
}

# The body of a bivariate guide, in its 4 x 4 block layout (see `.bi_geom()`):
# the swatch grid, the break labels, the two arrows and the two titles.
.draw_bi_body <- function(scene, g, geo, m, rt, txt) {
  sc <- g$sc
  bi <- sc$bivariate
  mm <- function(v) vellum::vl_unit(v, "mm")
  # The swatch grid, bottom-left aligned in the body cell.
  scene <- vellum::push(scene, vellum::vl_viewport(row = 1, col = 4))
  cols <- matrix(sc$colors, nrow = bi$dim)
  interactive <- rt[[".interactive"]]
  for (j in seq_len(bi$dim)) {
    for (i in seq_len(bi$dim)) {
      sw <- vellum::rect_grob(
        x = mm((i - 0.5) * geo$s),
        y = mm((j - 0.5) * geo$s),
        width = mm(geo$s),
        height = mm(geo$s),
        gp = vellum::vl_gpar(fill = cols[i, j], col = NA)
      )
      idx <- (j - 1L) * bi$dim + i
      scene <- vellum::draw(
        scene,
        .tag_legend_swatch(
          sw,
          g,
          idx,
          .bi_swatch_tooltip(bi, i, j),
          interactive
        )
      )
    }
  }
  scene <- vellum::pop(scene)
  # The class breaks, one at each swatch edge: x under the grid, y beside it.
  lab_el <- rt[["legend.text"]]
  lab_el@size <- geo$lab_fs
  lab_gp <- .el_gpar_text(lab_el)
  # The two labels at the low-low corner would meet there, so the first x
  # label starts at its edge and the first y label sits on its edge.
  edges <- (seq_len(bi$dim + 1L) - 1) * geo$s
  first <- c(TRUE, rep(FALSE, bi$dim))
  if (!is.null(geo$ticks$x)) {
    scene <- vellum::push(scene, vellum::vl_viewport(row = 2, col = 4))
    for (k in c(TRUE, FALSE)) {
      scene <- vellum::draw(
        scene,
        vellum::text_grob(
          geo$ticks$x[first == k],
          x = mm(edges[first == k]),
          y = vellum::vl_unit(1, "npc"),
          just = c(if (k) "left" else "centre", "top"),
          gp = lab_gp
        )
      )
    }
    scene <- vellum::pop(scene)
  }
  if (!is.null(geo$ticks$y)) {
    scene <- vellum::push(scene, vellum::vl_viewport(row = 1, col = 3))
    for (k in c(TRUE, FALSE)) {
      scene <- vellum::draw(
        scene,
        vellum::text_grob(
          geo$ticks$y[first == k],
          x = mm(geo$ylab_w - geo$lab_gap),
          y = mm(edges[first == k]),
          just = c("right", if (k) "bottom" else "centre"),
          gp = lab_gp
        )
      )
    }
    scene <- vellum::pop(scene)
  }
  ink <- rt[["legend.text"]]@colour %||% "grey20"
  agp <- vellum::vl_gpar(col = ink, fill = ink, lwd = 0.8)
  arr <- vellum::vl_arrow(type = "closed", length = mm(1.3), angle = 25)
  # The two arrows start from a shared corner outside the grid's low-low
  # swatch (beyond the break labels) and run along its left and bottom edges
  # towards "more".
  # y arrow, up the grid's left edge.
  scene <- vellum::push(scene, vellum::vl_viewport(row = 1, col = 2))
  scene <- vellum::draw(
    scene,
    vellum::segments_grob(
      mm(geo$arrow / 2),
      mm(-geo$xlab_h - geo$arrow / 2),
      mm(geo$arrow / 2),
      mm(geo$side),
      arrow = arr,
      gp = agp
    )
  )
  scene <- vellum::pop(scene)
  # y title, rotated, reading upward from the grid's bottom edge. In the
  # diamond that would read upside down, so it reads downward to the corner.
  scene <- vellum::push(scene, vellum::vl_viewport(row = 1, col = 1))
  scene <- vellum::draw(
    scene,
    vellum::text_grob(
      bi$ylab,
      x = vellum::vl_unit(0.5, "npc"),
      y = mm(0),
      just = c(if (geo$rotate) "right" else "left", "centre"),
      rot = if (geo$rotate) -90 else 90,
      gp = txt
    )
  )
  scene <- vellum::pop(scene)
  # x arrow, along the grid's bottom edge.
  scene <- vellum::push(scene, vellum::vl_viewport(row = 3, col = 4))
  scene <- vellum::draw(
    scene,
    vellum::segments_grob(
      mm(-geo$ylab_w - geo$arrow / 2),
      mm(geo$arrow / 2),
      mm(geo$side),
      mm(geo$arrow / 2),
      arrow = arr,
      gp = agp
    )
  )
  scene <- vellum::pop(scene)
  # x title, starting at the grid's left edge.
  scene <- vellum::push(scene, vellum::vl_viewport(row = 4, col = 4))
  scene <- vellum::draw(
    scene,
    vellum::text_grob(
      bi$xlab,
      x = mm(0),
      y = vellum::vl_unit(0.5, "npc"),
      just = c("left", "centre"),
      gp = txt
    )
  )
  vellum::pop(scene)
}
