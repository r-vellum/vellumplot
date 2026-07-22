#' @include classes.R
NULL

# A marginal-plot specification: distributions drawn along the top and/or right
# edge of a single panel, sharing that panel's x / y scale (like
# ggExtra::ggMarginal). `type` picks the distribution mark; `sides` selects the
# edges ("t", "r", or "tr"); `size` is the marginal extent as a fraction of the
# panel; `group` splits each marginal by the main layer's colour mapping.
MarginalSpec <- S7::new_class(
  "MarginalSpec",
  package = "vellumplot",
  properties = list(
    type = S7::class_character, # "density" | "histogram"
    sides = S7::new_property(S7::class_character, default = "tr"),
    size = S7::new_property(S7::class_double, default = 0.15), # fraction of panel
    adjust = S7::new_property(S7::class_double, default = 1), # density bandwidth
    bins = S7::new_property(S7::class_integer, default = 30L), # histogram bins
    group = S7::new_property(S7::class_logical, default = FALSE) # split by colour?
  )
)

#' Add marginal distributions to a plot
#'
#' `add_marginal()` draws a distribution of the panel's `x` variable along the
#' top edge and/or of its `y` variable along the right edge, each sharing the
#' main panel's axis so it lines up with the scatter (the vellumplot analogue of
#' `ggExtra::ggMarginal()`). It is a plot-level modifier, like [facet_wrap()] or
#' [coord_flip()]: it takes no encoding of its own and instead reads `x`, `y`
#' (and, with `group = TRUE`, `color`) from the first layer that maps a numeric
#' `x` and `y` (typically your [mark_point()]).
#'
#' The marginals are drawn without their own axes or background. This version
#' supports a single panel only: combining it with [facet_wrap()] /
#' [facet_grid()], a non-Cartesian coordinate system ([coord_flip()],
#' [coord_polar()], [coord_fixed()], `coord_sf()`), or a locked aspect ratio is
#' an error.
#'
#' @param plot A [PlotSpec] (from [vplot()]) with at least one `x`/`y` layer.
#' @param type The marginal distribution: `"density"` (a kernel-density curve,
#'   the default) or `"histogram"` (binned counts).
#' @param sides Which edges to draw, as a string of `"t"` (top, the `x`
#'   distribution) and/or `"r"` (right, the `y` distribution). Default `"tr"`.
#' @param size The marginal size as a fraction of the panel, in `(0, 1)`.
#' @param adjust Bandwidth multiplier for `type = "density"` (see [mark_density()]).
#' @param bins Number of bins for `type = "histogram"` (see [mark_histogram()]).
#' @param group When `TRUE` and the plot maps `color`/`fill` to a discrete
#'   variable, draw one distribution per group in the matching colour (like
#'   `ggMarginal(groupColour = TRUE)`). The scatter's legend already covers them,
#'   so no extra legend is added.
#' @return The modified [PlotSpec].
#' @seealso [mark_density()], [mark_histogram()], [facet_wrap()]
#' @examples
#' vplot(faithful) |>
#'   mark_point(x = eruptions, y = waiting) |>
#'   add_marginal()
#'
#' vplot(faithful) |>
#'   mark_point(x = eruptions, y = waiting) |>
#'   add_marginal(type = "histogram", sides = "t")
#' @export
add_marginal <- function(
  plot,
  type = c("density", "histogram"),
  sides = "tr",
  size = 0.15,
  adjust = 1,
  bins = 30,
  group = FALSE
) {
  .check_plot(plot)
  type <- match.arg(type)
  if (!is.character(sides) || length(sides) != 1L || !nzchar(sides)) {
    cli::cli_abort(
      '{.arg sides} must be a string using "t" and/or "r", e.g. {.val tr}.'
    )
  }
  chars <- strsplit(sides, "", fixed = TRUE)[[1]]
  if (!all(chars %in% c("t", "r"))) {
    cli::cli_abort(c(
      '{.arg sides} must use only "t" (top) and "r" (right).',
      i = 'Got {.val {sides}}.'
    ))
  }
  if (!is.numeric(size) || length(size) != 1L || size <= 0 || size >= 1) {
    cli::cli_abort(
      '{.arg size} is the marginal size as a fraction of the panel, in (0, 1).'
    )
  }
  plot@marginal <- MarginalSpec(
    type = type,
    sides = paste(unique(chars), collapse = ""),
    size = as.double(size),
    adjust = as.double(adjust),
    bins = as.integer(bins),
    group = isTRUE(group)
  )
  plot
}

# --- compile helpers --------------------------------------------------------

# Pick the reference layer for the marginals: the first identity-stat layer
# mapping both a numeric x and y (a scatter / line). Returns its raw x / y and
# the mapped colour/fill grouping, or errors with a hint. Aggregating layers
# (histogram, density, ...) are skipped: their post-stat x/y are binned summaries,
# not the raw values a marginal distribution is computed from.
.marginal_source <- function(resolved) {
  for (L in resolved) {
    if (!identical(L$stat, "identity")) {
      next
    }
    vx <- L$values$x
    vy <- L$values$y
    if (is.null(vx) || is.null(vy)) {
      next
    }
    nx <- suppressWarnings(as.numeric(vx))
    ny <- suppressWarnings(as.numeric(vy))
    # Require cleanly numeric x and y: a partly-coercible column (e.g.
    # c("1","2","a")) would introduce silent NAs that skew the marginal. A value
    # that was present but became NA under coercion means the column isn't numeric.
    # A factor is explicitly rejected: `as.numeric(factor(...))` yields the integer
    # level codes with no NAs, so it would otherwise pass and the marginal would be
    # computed over the codes (1, 2, 3, ...) rather than the data.
    clean <- function(orig, num) {
      !is.factor(orig) && !any(is.na(num) & !is.na(orig))
    }
    if (!clean(vx, nx) || !clean(vy, ny)) {
      next
    }
    return(list(x = nx, y = ny, color = L$values$color %||% L$values$fill))
  }
  cli::cli_abort(c(
    "{.fn add_marginal} needs a layer mapping numeric {.field x} and {.field y}.",
    i = "Add e.g. {.code mark_point(x = ..., y = ...)} before {.fn add_marginal}."
  ))
}

# A minimal trained "scale" for a marginal's density/count axis: a continuous
# identity map over [0, maxd] with no ticks or labels (a blank axis). Mirrors the
# shape of a trained position scale (see .train_position in compile-train.R) so
# the mark emitters can read $map / $band_width / $domain.
.marg_density_scale <- function(maxd) {
  list(
    aesthetic = "y",
    type = "continuous",
    discrete = FALSE,
    band_width = NULL,
    data_range = c(0, maxd),
    domain = c(0, maxd),
    breaks = numeric(0),
    labels = character(0),
    map = function(v) as.numeric(v),
    name = NULL
  )
}

# Build a synthetic resolved layer for one marginal, run its stat, and return the
# post-stat layer plus the maximum density/count (the extent of the blank axis).
# `values` carries the distribution's `x` (the panel variable), and `color` when
# grouping. Reuses .apply_stat (compile-stat.R) so the density / bin computation
# is identical to mark_density() / mark_histogram().
.marg_layer <- function(values, ms) {
  hist <- identical(ms@type, "histogram")
  L <- list(
    mark = if (hist) "bar" else "area",
    values = values,
    types = list(),
    after = list(),
    params = list(
      fill = "grey35",
      alpha = if (ms@group) 0.5 else NA_real_
    ),
    n = length(values$x),
    stat = if (hist) "bin" else "density",
    stat_params = if (hist) list(bins = ms@bins) else list(adjust = ms@adjust),
    position = "identity",
    blend = "normal",
    sketch = NULL,
    meta = NULL
  )
  L2 <- .apply_stat(L)
  yv <- as.numeric(L2$values$y)
  maxd <- if (length(yv) && any(is.finite(yv))) max(yv[is.finite(yv)]) else 1
  if (!is.finite(maxd) || maxd <= 0) {
    maxd <- 1
  }
  list(L = L2, maxd = maxd)
}

# Draw the requested marginals into the layout's marginal cells. `hsc` / `vsc`
# are the panel's horizontal / vertical trained scales (already role-swapped for
# coord_flip upstream, though flip is disallowed with marginals). The top
# marginal shares `hsc` and draws upright; the right marginal shares `vsc` and
# draws sideways via the coord_flip emitter path (flip = TRUE) so a density of y
# grows horizontally.
.draw_marginals <- function(
  scene,
  ms,
  src,
  lay,
  hsc,
  vsc,
  colorscale,
  rt,
  plot_sketch
) {
  group_vals <- if (ms@group) src$color else NULL
  if (
    ms@group && (is.null(colorscale) || !identical(colorscale$kind, "discrete"))
  ) {
    cli::cli_abort(c(
      "{.code add_marginal(group = TRUE)} needs a discrete {.field color}/{.field fill} mapping.",
      i = "Map a discrete variable to {.arg color} in a layer, or use {.code group = FALSE}."
    ))
  }

  # top: distribution of x, upright
  if (!is.na(lay$marg_top_row)) {
    vals <- list(x = src$x)
    if (!is.null(group_vals)) {
      vals$color <- group_vals
    }
    ml <- .marg_layer(vals, ms)
    sc <- list(
      x = hsc,
      y = .marg_density_scale(ml$maxd),
      color = colorscale,
      flip = FALSE,
      polar = NULL,
      sketch = plot_sketch
    )
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(
        row = lay$marg_top_row,
        col = lay$panel_col[1],
        xscale = hsc$domain,
        yscale = c(0, ml$maxd),
        clip = TRUE,
        name = "marginal-top"
      )
    )
    scene <- .emit_layer(scene, ml$L, sc)
    scene <- vellum::pop(scene)
  }

  # right: distribution of y, drawn sideways (flip = TRUE)
  if (!is.na(lay$marg_right_col)) {
    vals <- list(x = src$y)
    if (!is.null(group_vals)) {
      vals$color <- group_vals
    }
    ml <- .marg_layer(vals, ms)
    sc <- list(
      x = vsc,
      y = .marg_density_scale(ml$maxd),
      color = colorscale,
      flip = TRUE,
      polar = NULL,
      sketch = plot_sketch
    )
    scene <- vellum::push(
      scene,
      vellum::vl_viewport(
        row = lay$panel_row[1],
        col = lay$marg_right_col,
        xscale = c(0, ml$maxd),
        yscale = vsc$domain,
        clip = TRUE,
        name = "marginal-right"
      )
    )
    scene <- .emit_layer(scene, ml$L, sc)
    scene <- vellum::pop(scene)
  }
  scene
}
