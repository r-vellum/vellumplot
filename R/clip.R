#' @include classes.R
NULL

#' Clip or mask a plot to a geometry
#'
#' Restrict where a plot's marks show, by a shape rather than the panel
#' rectangle. `clip_to()` is a **hard** clip: only marks inside `region` are
#' drawn (or, with `invert = TRUE`, only those outside) -- the way to fill a
#' raster / hexbin / tile heatmap into a country outline (a textured
#' choropleth). `set_mask()` is a **soft** mask: a radial luminance ramp that
#' fades the panel out towards its edges (a vignette / spotlight).
#'
#' Both attach to the plot and resolve at render into an isolated masked layer
#' ([vellum::as_mask()]), so the static output is unchanged when neither is set.
#' Cartesian coordinates only (not polar or a nonlinear `coord_trans()`).
#'
#' A smooth *feathered* edge on an arbitrary polygon needs a blur the renderer
#' does not provide yet, so `clip_to()` is hard-edged; use `set_mask()` for a
#' soft (radial) fade.
#'
#' @param plot A [PlotSpec].
#' @param region The clip geometry: an `sf` object (its polygons), or a
#'   `data.frame` / matrix with `x` / `y` columns (and an optional `group` column
#'   for several rings). For `set_mask()`, `NULL` (default) fades the whole panel.
#' @param invert For `clip_to()`, `TRUE` keeps the marks *outside* `region`
#'   (punching the shape out as a hole) instead of inside.
#' @param type For `set_mask()`, how the mask's pixels set coverage: `"luminance"`
#'   (default -- brightness, white shows / black hides) or `"alpha"`.
#' @param feather For `set_mask()`, the soft-edge fraction of the radial ramp in
#'   `[0, 0.9]`: `0` a hard disc, larger a gentler fade. Default `0.35`.
#' @return The modified [PlotSpec].
#' @examples
#' \dontrun{
#' # clip a raster heatmap to a country outline
#' vplot(grid) |> mark_raster(x = x, y = y, fill = z) |> clip_to(country_sf)
#'
#' # a vignette
#' vplot(mtcars) |> mark_point(x = wt, y = mpg) |> set_mask(feather = 0.5)
#' }
#' @name clip
NULL

#' @rdname clip
#' @export
clip_to <- function(plot, region, invert = FALSE) {
  .check_plot(plot)
  if (missing(region) || is.null(region)) {
    cli::cli_abort("{.arg region} is required: the geometry to clip to.")
  }
  plot@clip <- ClipSpec(
    region = .clip_region(region),
    kind = "clip",
    type = "alpha",
    feather = 0,
    invert = isTRUE(invert)
  )
  plot
}

#' @rdname clip
#' @export
set_mask <- function(
  plot,
  region = NULL,
  type = c("luminance", "alpha"),
  feather = 0.35
) {
  .check_plot(plot)
  type <- match.arg(type)
  if (!is.numeric(feather) || length(feather) != 1L || is.na(feather)) {
    cli::cli_abort(
      "{.arg feather} must be a single number in {.val {c(0, 0.9)}}."
    )
  }
  plot@clip <- ClipSpec(
    region = if (is.null(region)) NULL else .clip_region(region),
    kind = "mask",
    type = type,
    feather = max(0, min(0.9, feather)),
    invert = FALSE
  )
  plot
}

# Normalise a clip region to `list(rings = <list of 2-col x/y matrices>)` in data
# coordinates. Accepts an sf object (its polygon rings) or a data.frame / matrix
# of x/y vertices (split into rings by an optional `group` column).
.clip_region <- function(region) {
  if (inherits(region, c("sf", "sfc", "sfg"))) {
    .need_pkg("sf", "clip_to() with an sf region")
    geoms <- if (inherits(region, "sf")) sf::st_geometry(region) else region
    if (inherits(geoms, "sfg")) {
      geoms <- list(geoms)
    }
    rings <- list()
    for (g in geoms) {
      for (prim in .sf_decompose(g)) {
        if (identical(prim$kind, "poly")) {
          rings <- c(rings, prim$parts)
        }
      }
    }
    if (!length(rings)) {
      cli::cli_abort("{.arg region} has no polygon geometry to clip to.")
    }
    return(list(rings = rings))
  }
  if (is.matrix(region)) {
    region <- as.data.frame(region)
    if (ncol(region) >= 2L) {
      names(region)[1:2] <- c("x", "y")
    }
  }
  if (!is.data.frame(region) || !all(c("x", "y") %in% names(region))) {
    cli::cli_abort(c(
      "{.arg region} must be an {.pkg sf} object or a data frame with {.field x}/{.field y}.",
      i = "Add a {.field group} column to give several rings."
    ))
  }
  grp <- region$group %||% rep(1L, nrow(region))
  rings <- lapply(split(seq_len(nrow(region)), grp), function(i) {
    cbind(region$x[i], region$y[i])
  })
  list(rings = unname(rings))
}

# Build the vellum mask for a resolved clip spec against a panel's scales, drawn
# in the panel's native (data) coordinate system -- so it aligns with the marks.
# Returns a `vellum_mask` or NULL. Cartesian panels only (the caller guards).
.clip_mask <- function(cs, hsc, vsc) {
  if (is.null(cs)) {
    return(NULL)
  }
  if (identical(cs@kind, "mask")) {
    # Soft vignette: a full-panel rect with a radial white->black luminance ramp.
    # `feather` widens the white core; the ramp reaches ~the panel corners.
    f <- cs@feather
    grad <- vellum::radial_gradient(
      c("white", "white", "black"),
      stops = c(0, max(0, 1 - f), 1),
      cx = 0.5,
      cy = 0.5,
      r = 0.72,
      units = "npc"
    )
    g <- vellum::rect_grob(gp = vellum::vl_gpar(fill = grad, col = NA))
    return(vellum::as_mask(g, type = cs@type))
  }
  # Hard clip: region rings in native coords, filled opaque white; even-odd so
  # nested rings cut holes. `invert` prepends the panel-domain rectangle as ring 0
  # so the region is punched out (marks outside it survive).
  rings <- cs@region$rings
  xs <- ys <- id <- list()
  k <- 0L
  if (isTRUE(cs@invert)) {
    k <- k + 1L
    xs[[k]] <- hsc$domain[c(1, 2, 2, 1)]
    ys[[k]] <- vsc$domain[c(1, 1, 2, 2)]
    id[[k]] <- rep.int(k, 4L)
  }
  for (ring in rings) {
    if (!nrow(ring)) {
      next
    }
    k <- k + 1L
    xs[[k]] <- ring[, 1L]
    ys[[k]] <- ring[, 2L]
    id[[k]] <- rep.int(k, nrow(ring))
  }
  if (!k) {
    return(NULL)
  }
  g <- vellum::path_grob(
    x = vellum::vl_unit(unlist(xs, use.names = FALSE), "native"),
    y = vellum::vl_unit(unlist(ys, use.names = FALSE), "native"),
    id = unlist(id, use.names = FALSE),
    rule = "evenodd",
    gp = vellum::vl_gpar(fill = "white", col = NA)
  )
  vellum::as_mask(g, type = "alpha")
}
