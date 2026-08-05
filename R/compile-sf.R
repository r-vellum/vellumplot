#' @include classes.R coord.R
NULL

# --- sf (simple features) support -------------------------------------------
#
# sf objects are data frames with a list-column of geometries. An sf *layer*
# resolves its ordinary aesthetics (fill/colour/alpha/...) exactly like any other
# layer -- one value per feature -- while its coordinates come from the geometry
# column, never from x/y encodings. This file holds the geometry engine: a
# base-R traversal of the `sfc` list structure (so extraction needs no `sf`
# call), plus the CRS reprojection and bbox helpers (which do).
#
# `sf` (and `classInt`, for classed breaks) are Suggests: the whole package
# installs and runs without them; the map entry points error clearly if absent.

# Error unless an optional (Suggests) package is installed. `what` names the
# feature that needs it (for the cli message).
.need_pkg <- function(pkg, what, call = rlang::caller_env()) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(
      c(
        "{what} requires the {.pkg {pkg}} package.",
        i = "Install it with {.run install.packages(\"{pkg}\")}."
      ),
      call = call
    )
  }
  invisible(TRUE)
}

# Is `data` an sf object (data frame with a geometry list-column)?
.is_sf <- function(data) inherits(data, "sf")

# The active geometry column name of an sf object.
.sf_geom_col <- function(data) attr(data, "sf_column")

# Does any layer (or the plot itself) draw an sf mark?
.has_sf_layer <- function(spec) {
  any(vapply(
    spec@layers,
    function(L) L@mark %in% c("sf", "sf_label"),
    logical(1)
  ))
}

# Is an sfg empty? (POINT EMPTY -> no finite coords; others -> zero length /
# zero rows.) Empty geometries return NA extents and must be skipped.
.sfg_empty <- function(g) {
  if (is.matrix(g)) {
    return(nrow(g) == 0L)
  }
  if (is.numeric(g)) {
    return(length(g) == 0L || !any(is.finite(g)))
  }
  length(g) == 0L
}

# Keep the planar (XY) coordinates of a coordinate matrix, dropping any Z/M.
.xy_cols <- function(m) m[, 1:2, drop = FALSE]

# Decompose one geometry (sfg) into a list of drawing primitives, each
#   list(kind = "point" | "line" | "poly", parts = <list of 2-col matrices>).
# MULTI* fan out into multiple parts; a POLYGON/MULTIPOLYGON contributes one
# part per ring (exterior + holes), each an independent sub-path -- the
# even-odd fill rule then cuts the holes and leaves islands solid, regardless of
# ring winding (sf does not enforce winding). A GEOMETRYCOLLECTION recurses and
# may yield several primitives of mixed kind. EMPTY geometries yield none.
.sf_decompose <- function(g) {
  if (.sfg_empty(g)) {
    return(list())
  }
  cls <- class(g)[2]
  switch(
    cls,
    POINT = list(list(kind = "point", parts = list(matrix(g[1:2], nrow = 1L)))),
    MULTIPOINT = list(list(kind = "point", parts = list(.xy_cols(g)))),
    LINESTRING = list(list(kind = "line", parts = list(.xy_cols(g)))),
    MULTILINESTRING = list(list(
      kind = "line",
      parts = lapply(g, .xy_cols)
    )),
    POLYGON = list(list(kind = "poly", parts = lapply(g, .xy_cols))),
    MULTIPOLYGON = list(list(
      kind = "poly",
      # flatten every ring of every sub-polygon; each ring is its own sub-path
      parts = unlist(
        lapply(g, function(poly) lapply(poly, .xy_cols)),
        recursive = FALSE
      )
    )),
    GEOMETRYCOLLECTION = unlist(
      lapply(g, .sf_decompose),
      recursive = FALSE
    ),
    cli::cli_abort("Unsupported sf geometry type {.val {cls}}.")
  )
}

# Bounding box (c(xmin, xmax, ymin, ymax)) over a list of decomposed features
# (each a list of primitives). Ignores non-finite coords / empty features. A
# single linear pass over the coordinates, accumulating running scalar min/max
# per part -- never growing (and reallocating) coordinate vectors -- so it stays
# O(total coords) on huge geometry sets rather than the quadratic `c()` growth of
# the naive gather.
.sf_bbox <- function(features) {
  xmin <- ymin <- Inf
  xmax <- ymax <- -Inf
  for (prims in features) {
    for (p in prims) {
      for (m in p$parts) {
        if (!length(m) || !nrow(m)) {
          next
        }
        xc <- m[, 1L]
        xc <- xc[is.finite(xc)]
        yc <- m[, 2L]
        yc <- yc[is.finite(yc)]
        if (length(xc)) {
          xmin <- min(xmin, xc)
          xmax <- max(xmax, xc)
        }
        if (length(yc)) {
          ymin <- min(ymin, yc)
          ymax <- max(ymax, yc)
        }
      }
    }
  }
  if (!all(is.finite(c(xmin, xmax, ymin, ymax)))) {
    cli::cli_abort("The sf layer has no finite coordinates to plot.")
  }
  c(xmin, xmax, ymin, ymax)
}

# The target CRS for a plot's sf layers: the coord's explicit `crs`, else the
# CRS of the first sf data found (plot data or a layer's own data). NULL means
# "leave coordinates as-is" (no reprojection).
.sf_target_crs <- function(spec, co) {
  if (!is.null(co@crs)) {
    return(co@crs)
  }
  datas <- c(list(spec@data), lapply(spec@layers, function(L) L@data))
  for (d in datas) {
    if (.is_sf(d)) {
      cr <- sf::st_crs(d)
      if (!is.na(cr)) {
        return(cr)
      }
    }
  }
  NULL
}

# Reproject every sf data frame in the spec (plot data + per-layer data) to the
# target CRS, *before* scale training so vellum only ever sees projected
# cartesian coordinates. Returns list(spec = <reprojected>, geographic = <lgl>,
# crs = <target CRS or NULL>) where `geographic` reports whether the target CRS
# is lon/lat (drives the map aspect correction in the layout) and `crs` is the
# resolved target CRS, which a scale bar / graticule needs at emit time.
# Requires `sf`.
.project_sf_data <- function(spec) {
  .need_pkg("sf", "coord_sf()")
  co <- .coord_of(spec)
  crs <- .sf_target_crs(spec, co)

  reproject <- function(d) {
    if (.is_sf(d) && !is.null(crs) && !is.na(sf::st_crs(d))) {
      sf::st_transform(d, crs)
    } else {
      d
    }
  }
  spec@data <- reproject(spec@data)
  spec@layers <- lapply(spec@layers, function(L) {
    if (!is.null(L@data)) {
      L@data <- reproject(L@data)
    }
    L
  })

  # geographic flag: prefer the explicit target CRS, else the first sf data.
  geo <- FALSE
  ref <- if (!is.null(crs)) {
    crs
  } else {
    d0 <- if (.is_sf(spec@data)) {
      spec@data
    } else {
      Find(.is_sf, lapply(spec@layers, function(L) L@data))
    }
    if (is.null(d0)) NULL else sf::st_crs(d0)
  }
  if (!is.null(ref)) {
    ll <- sf::st_is_longlat(ref)
    geo <- isTRUE(ll)
  }
  list(spec = spec, geographic = geo, crs = crs)
}
