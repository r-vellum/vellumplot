#' @include classes.R marks.R compile-sf.R
NULL

# --- map decorations: scale bar, north arrow, graticule --------------------
#
# Cartographic furniture for coord_sf() maps. The scale bar and compass are
# fixed-position decorations (modelled on mark_rule(): no x/y encoding, all
# config in stat_params, drawn in relative npc + absolute mm units at a panel
# corner). The graticule is a coord feature drawn from the panel-background path
# (behind the marks), because it needs the target CRS to reproject a generated
# lon/lat grid into curved polylines. Scale bar / compass need no `sf` (they read
# only the trained panel domain); the graticule reprojection needs `sf`, already
# guaranteed inside a coord_sf() render.

#' Map decorations
#'
#' Cartographic furniture for [coord_sf()] maps. `mark_scalebar()` draws a
#' segmented distance bar; `mark_compass()` draws a north arrow. Both are
#' fixed-position decorations pinned to a panel corner (they take no data
#' aesthetics). Graticule lines (meridians and parallels) are added through
#' [coord_sf()]'s `graticule` argument, not a mark, so they render behind the
#' map.
#'
#' @param plot A [PlotSpec].
#' @param position Panel corner to anchor the decoration:
#'   `"bottomleft"` (scale bar default), `"topright"` (compass default),
#'   `"bottomright"`, or `"topleft"` (the abbreviations `"bl"`/`"br"`/`"tl"`/
#'   `"tr"` also work).
#' @param pad Inset from the panel edge, in millimetres.
#' @param color,fill Stroke and fill colours. For the scale bar the segments
#'   alternate `color` and `fill`; for the compass the arrow is `fill` with a
#'   `color` outline.
#' @param data Optional layer data (rarely needed -- decorations are not
#'   data-driven).
#' @return The modified [PlotSpec].
#' @seealso [coord_sf()], [mark_sf()]
#' @examples
#' \dontrun{
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' vplot(nc) |>
#'   mark_sf(fill = BIR74) |>
#'   coord_sf(crs = 3857, graticule = TRUE) |>
#'   mark_scalebar() |>
#'   mark_compass()
#' }
#' @name map_decorations
NULL

#' @rdname map_decorations
#'
#' @param distance Bar length in `unit`. `NULL` (default) picks a round number
#'   near a quarter of the panel width.
#' @param unit Distance unit for the label and `distance`: `"km"` (default),
#'   `"m"`, `"mi"`, or `"ft"`.
#' @param segments Number of alternating segments in the bar (default `4`).
#' @param height Bar thickness, in millimetres.
#' @param text_size Label font size (points).
#' @export
mark_scalebar <- function(
  plot,
  distance = NULL,
  unit = "km",
  position = "bottomleft",
  segments = 4L,
  height = 2.5,
  pad = 4,
  text_size = 7,
  color = "black",
  fill = "white",
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "scalebar",
    rlang::enquos(),
    stat_params = list(
      distance = distance,
      unit = unit,
      position = position,
      segments = as.integer(segments),
      height = height,
      pad = pad,
      text_size = text_size,
      color = color,
      fill = fill
    ),
    data = data
  )
}

#' @rdname map_decorations
#'
#' @param size Glyph height, in millimetres.
#' @param rotation Extra clockwise rotation of the arrow, in degrees (e.g. to
#'   point at grid north on a rotated layout).
#' @param text Draw the `"N"` label above the arrow (default `TRUE`).
#' @export
mark_compass <- function(
  plot,
  position = "topright",
  size = 10,
  pad = 4,
  rotation = 0,
  color = "black",
  fill = "black",
  text = TRUE,
  data = NULL
) {
  .check_plot(plot)
  .add_layer(
    plot,
    "compass",
    rlang::enquos(),
    stat_params = list(
      position = position,
      size = size,
      pad = pad,
      rotation = rotation,
      color = color,
      fill = fill,
      text = text
    ),
    data = data
  )
}

# Resolve a corner keyword to an anchor descriptor: the npc corner (x0, y0), the
# inward step directions (hdir/vdir), and the text justification that grows the
# label away from the edges. Shared by the scale bar and compass.
.corner_anchor <- function(position) {
  position <- tolower(position)
  position <- switch(
    position,
    bl = "bottomleft",
    br = "bottomright",
    tl = "topleft",
    tr = "topright",
    position
  )
  if (!position %in% c("bottomleft", "bottomright", "topleft", "topright")) {
    cli::cli_abort(c(
      "Unknown decoration {.arg position} {.val {position}}.",
      i = "Use one of {.val bottomleft}, {.val bottomright}, {.val topleft}, {.val topright}."
    ))
  }
  left <- grepl("left", position, fixed = TRUE)
  bottom <- grepl("bottom", position, fixed = TRUE)
  list(
    x0 = if (left) 0 else 1,
    y0 = if (bottom) 0 else 1,
    hdir = if (left) 1 else -1,
    vdir = if (bottom) 1 else -1,
    hjust = if (left) "left" else "right",
    vjust = if (bottom) "bottom" else "top"
  )
}

# Snap a raw distance to a "nice" 1/2/5 x 10^k value at or below it, so an
# auto-sized scale bar reads as a round number.
.nice_scalebar_len <- function(x) {
  if (!is.finite(x) || x <= 0) {
    return(1)
  }
  p <- 10^floor(log10(x))
  f <- x / p
  mult <- if (f >= 5) {
    5
  } else if (f >= 2) {
    2
  } else {
    1
  }
  mult * p
}

# Metres per one unit of a projected CRS (so the scale bar can convert a panel
# span in CRS units to metres). Handles the common metre and foot cases; assumes
# metres otherwise. The lon/lat case is handled separately (degrees, cos-lat).
.crs_metres_per_unit <- function(crs) {
  u <- tryCatch(sf::st_crs(crs)$units_gdal, error = function(e) NULL)
  if (is.null(u) || is.na(u)) {
    return(1)
  }
  if (grepl("met(er|re)", u, ignore.case = TRUE)) {
    return(1)
  }
  if (grepl("foot|feet", u, ignore.case = TRUE)) {
    # US survey foot vs international foot.
    return(if (grepl("US", u, ignore.case = TRUE)) 1200 / 3937 else 0.3048)
  }
  1
}

.emit_scalebar <- function(scene, L, scales) {
  sp <- L$stat_params
  crs <- scales$sf_crs
  if (is.null(crs)) {
    cli::cli_abort(c(
      "{.fn mark_scalebar} needs a map coordinate system.",
      i = "Add {.fn coord_sf} (or an {.fn mark_sf} layer, which adopts it) with a known CRS."
    ))
  }
  xdom <- range(scales$x$domain)
  ydom <- range(scales$y$domain)
  span_x <- abs(diff(xdom))
  if (!is.finite(span_x) || span_x <= 0) {
    return(scene)
  }
  unit <- sp$unit %||% "km"
  m_per_unit <- switch(
    unit,
    m = 1,
    km = 1000,
    mi = 1609.344,
    ft = 0.3048,
    cli::cli_abort("Unknown scalebar {.arg unit} {.val {unit}}.")
  )
  if (isTRUE(scales$sf_geographic)) {
    # x-domain is degrees of longitude; metres per degree at the panel centre.
    mean_lat <- max(min(mean(ydom), 89.9), -89.9)
    m_per_crsunit <- 111320 * cos(mean_lat * pi / 180)
  } else {
    m_per_crsunit <- .crs_metres_per_unit(crs)
  }
  panel_m <- span_x * m_per_crsunit
  dist <- sp$distance %||% .nice_scalebar_len(panel_m / m_per_unit / 4)
  bar_frac <- (dist * m_per_unit) / panel_m
  if (!is.finite(bar_frac) || bar_frac <= 0) {
    return(scene)
  }

  nseg <- max(1L, sp$segments %||% 4L)
  h <- sp$height %||% 2.5
  pad <- sp$pad %||% 4
  ts <- sp$text_size %||% 7
  color <- sp$color %||% "black"
  fill <- sp$fill %||% "white"
  a <- .corner_anchor(sp$position %||% "bottomleft")
  sk <- .mark_sketch(L, scales)

  # x at fraction `f` of the bar from the corner (npc), inset `pad` mm inward.
  xat <- function(f) {
    vellum::vl_unit(a$x0 + a$hdir * f * bar_frac, "npc") +
      vellum::vl_unit(a$hdir * pad, "mm")
  }
  ybase <- vellum::vl_unit(a$y0, "npc") + vellum::vl_unit(a$vdir * pad, "mm")
  ytop <- vellum::vl_unit(a$y0, "npc") +
    vellum::vl_unit(a$vdir * (pad + h), "mm")

  for (i in seq_len(nseg)) {
    f0 <- (i - 1L) / nseg
    f1 <- i / nseg
    xs <- c(xat(f0), xat(f1), xat(f1), xat(f0))
    ys <- c(ybase, ybase, ytop, ytop)
    fcol <- if (i %% 2L == 1L) color else fill
    scene <- .draw(
      scene,
      vellum::polygon_grob(
        xs,
        ys,
        sketch = .sketch_bump(sk, i),
        gp = vellum::vl_gpar(fill = fcol, col = color, lwd = 0.8)
      )
    )
  }

  # End labels, just outside the bar. A scale bar always reads left-to-right --
  # "0" at the low-x end, the distance at the high-x end -- regardless of which
  # corner it sits in. For a right-anchored corner the bar grows leftward
  # (`hdir < 0`), so `xat(0)` is the right end and `xat(1)` the left; swap the
  # label positions there so "0" stays on the left.
  ylab <- vellum::vl_unit(a$y0, "npc") +
    vellum::vl_unit(a$vdir * (pad + h + 1), "mm")
  lab_just <- c("centre", a$vjust)
  gp_txt <- vellum::vl_gpar(fontsize = ts, col = color)
  x_zero <- if (a$hdir > 0) xat(0) else xat(1)
  x_dist <- if (a$hdir > 0) xat(1) else xat(0)
  scene <- .draw(
    scene,
    vellum::text_grob("0", x_zero, ylab, just = lab_just, gp = gp_txt)
  )
  end_lab <- paste0(format(dist, trim = TRUE), " ", unit)
  scene <- .draw(
    scene,
    vellum::text_grob(end_lab, x_dist, ylab, just = lab_just, gp = gp_txt)
  )
  scene
}

.emit_compass <- function(scene, L, scales) {
  sp <- L$stat_params
  size <- sp$size %||% 10
  pad <- sp$pad %||% 4
  color <- sp$color %||% "black"
  fill <- sp$fill %||% "black"
  rot <- (sp$rotation %||% 0) * pi / 180
  show_text <- !isFALSE(sp$text)
  a <- .corner_anchor(sp$position %||% "topright")
  sk <- .mark_sketch(L, scales)

  # Glyph centre: the corner, inset (pad + half the glyph) millimetres inward.
  cx <- vellum::vl_unit(a$x0, "npc") +
    vellum::vl_unit(a$hdir * (pad + size / 2), "mm")
  cy <- vellum::vl_unit(a$y0, "npc") +
    vellum::vl_unit(a$vdir * (pad + size / 2), "mm")

  # A notched arrow pointing up (+y), in millimetre offsets from the centre.
  hw <- size / 4
  hh <- size / 2
  dx <- c(0, -hw, 0, hw)
  dy <- c(hh, -hh, -hh * 0.4, -hh)
  # Rotate clockwise by `rot`.
  rx <- dx * cos(rot) + dy * sin(rot)
  ry <- -dx * sin(rot) + dy * cos(rot)
  scene <- .draw(
    scene,
    vellum::polygon_grob(
      cx + vellum::vl_unit(rx, "mm"),
      cy + vellum::vl_unit(ry, "mm"),
      sketch = sk,
      gp = vellum::vl_gpar(fill = fill, col = color, lwd = 0.8)
    )
  )
  if (show_text) {
    off <- hh + 2
    scene <- .draw(
      scene,
      vellum::text_grob(
        "N",
        cx + vellum::vl_unit(off * sin(rot), "mm"),
        cy + vellum::vl_unit(off * cos(rot), "mm"),
        just = c("centre", "centre"),
        gp = vellum::vl_gpar(
          fontsize = size * 0.9,
          col = color,
          fontface = "bold"
        )
      )
    )
  }
  scene
}

# --- graticule --------------------------------------------------------------

# Nice degree breaks within a lon/lat range.
.grat_breaks <- function(rng) {
  b <- pretty(rng, n = 5)
  b <- b[b >= rng[1] & b <= rng[2]]
  # A range too narrow for any pretty cut still deserves a gridline: fall back to
  # the midpoint so a valid CRS never renders a blank graticule.
  if (!length(b)) {
    b <- mean(rng)
  }
  b
}

# Format a longitude / latitude break as a degree label (e.g. "80W", "40N" with
# a degree sign).
.fmt_lon <- function(d) {
  hemi <- if (d < 0) {
    "W"
  } else if (d > 0) {
    "E"
  } else {
    ""
  }
  paste0(formatC(abs(d), format = "g"), "\u00b0", hemi)
}
.fmt_lat <- function(d) {
  hemi <- if (d < 0) {
    "S"
  } else if (d > 0) {
    "N"
  } else {
    ""
  }
  paste0(formatC(abs(d), format = "g"), "\u00b0", hemi)
}

# Recover the lon/lat range covered by the projected panel extent: densify the
# panel-edge rectangle, inverse-project it to lon/lat, take the bounding box. The
# edge is densified because a projected rectangle maps to a curved lon/lat quad.
.grat_lonlat_range <- function(xr, yr, crs) {
  gx <- seq(xr[1], xr[2], length.out = 40)
  gy <- seq(yr[1], yr[2], length.out = 40)
  edge <- rbind(
    cbind(gx, yr[1]),
    cbind(gx, yr[2]),
    cbind(xr[1], gy),
    cbind(xr[2], gy)
  )
  pts <- sf::st_sfc(
    lapply(seq_len(nrow(edge)), function(i) sf::st_point(edge[i, ])),
    crs = crs
  )
  ll <- sf::st_coordinates(sf::st_transform(pts, "OGC:CRS84"))
  list(lon = range(ll[, 1]), lat = range(ll[, 2]))
}

# Project a lon/lat polyline into the target CRS, returning native x/y vectors.
.grat_project_line <- function(lon, lat, crs) {
  ls <- sf::st_sfc(sf::st_linestring(cbind(lon, lat)), crs = "OGC:CRS84")
  co <- sf::st_coordinates(sf::st_transform(ls, crs))
  list(x = co[, 1], y = co[, 2])
}

# Place one graticule label where its line meets the requested panel edge, only
# if the line actually reaches near that edge (else it would float mid-panel).
.grat_label <- function(scene, ln, xr, yr, edge, txt, gp) {
  inside <- ln$x >= xr[1] & ln$x <= xr[2] & ln$y >= yr[1] & ln$y <= yr[2]
  if (!any(inside)) {
    return(scene)
  }
  xi <- ln$x[inside]
  yi <- ln$y[inside]
  if (identical(edge, "bottom")) {
    j <- which.min(yi)
    if (yi[j] > yr[1] + 0.08 * diff(yr)) {
      return(scene)
    }
    just <- c("centre", "bottom")
  } else {
    j <- which.min(xi)
    if (xi[j] > xr[1] + 0.08 * diff(xr)) {
      return(scene)
    }
    just <- c("left", "centre")
  }
  .draw(
    scene,
    vellum::text_grob(
      txt,
      vellum::vl_unit(xi[j], "native"),
      vellum::vl_unit(yi[j], "native"),
      just = just,
      gp = gp
    )
  )
}

# Draw meridians and parallels behind the marks. For a projected CRS the lines
# are reprojected point-sequences (curved); for unprojected lon/lat they are
# straight and reuse the ordinary gridline drawers. `grat` is the resolved
# `coord_sf(graticule=)` spec (a list with optional `lon`/`lat` breaks + labels).
.draw_graticule <- function(scene, x_sc, y_sc, rt, crs, geographic, grat) {
  .need_pkg("sf", "coord_sf(graticule = )")
  if (is.null(crs)) {
    return(scene)
  }
  # Meridians follow the major-x gridline style, parallels the major-y; fall back
  # to whichever is not blank so a theme that hides one axis grid still draws.
  el_x <- rt[["panel.grid.major.x"]]
  el_y <- rt[["panel.grid.major.y"]]
  el_lon <- if (.is_blank(el_x)) el_y else el_x
  el_lat <- if (.is_blank(el_y)) el_x else el_y
  # A theme that blanks *both* grids (e.g. theme_classic()) leaves no line style
  # to inherit. `graticule = TRUE` is an explicit opt-in, so fall back to a plain
  # default rather than crash on the blank element.
  grat_default <- element_line(colour = "grey85", linewidth = 0.3)
  if (.is_blank(el_lon)) {
    el_lon <- grat_default
  }
  if (.is_blank(el_lat)) {
    el_lat <- grat_default
  }
  gp_lon <- .el_gpar_line(el_lon)
  gp_lat <- .el_gpar_line(el_lat)
  sk <- .el_sketch(el_lon, 8L)
  gp_txt <- vellum::vl_gpar(fontsize = 7, col = "grey30")
  do_labels <- !isFALSE(grat$labels)
  xr <- range(x_sc$domain)
  yr <- range(y_sc$domain)

  if (isTRUE(geographic)) {
    lon <- grat$lon %||% .grat_breaks(xr)
    lat <- grat$lat %||% .grat_breaks(yr)
    lon <- lon[lon >= xr[1] & lon <= xr[2]]
    lat <- lat[lat >= yr[1] & lat <= yr[2]]
    scene <- .vlines(scene, lon, gp_lon, sk)
    scene <- .hlines(scene, lat, gp_lat, sk)
    if (do_labels) {
      y_at <- vellum::vl_unit(0, "npc") + vellum::vl_unit(1, "mm")
      for (b in lon) {
        scene <- .draw(
          scene,
          vellum::text_grob(
            .fmt_lon(b),
            vellum::vl_unit(b, "native"),
            y_at,
            just = c("centre", "bottom"),
            gp = gp_txt
          )
        )
      }
      x_at <- vellum::vl_unit(0, "npc") + vellum::vl_unit(1, "mm")
      for (b in lat) {
        scene <- .draw(
          scene,
          vellum::text_grob(
            .fmt_lat(b),
            x_at,
            vellum::vl_unit(b, "native"),
            just = c("left", "centre"),
            gp = gp_txt
          )
        )
      }
    }
    return(scene)
  }

  ll <- .grat_lonlat_range(xr, yr, crs)
  lon <- grat$lon %||% .grat_breaks(ll$lon)
  lat <- grat$lat %||% .grat_breaks(ll$lat)
  lat_seq <- seq(ll$lat[1], ll$lat[2], length.out = 100)
  lon_seq <- seq(ll$lon[1], ll$lon[2], length.out = 100)
  for (b in lon) {
    ln <- .grat_project_line(rep(b, 100), lat_seq, crs)
    scene <- vellum::draw(
      scene,
      vellum::lines_grob(
        vellum::vl_unit(ln$x, "native"),
        vellum::vl_unit(ln$y, "native"),
        sketch = sk,
        gp = gp_lon
      )
    )
    if (do_labels) {
      scene <- .grat_label(scene, ln, xr, yr, "bottom", .fmt_lon(b), gp_txt)
    }
  }
  for (b in lat) {
    ln <- .grat_project_line(lon_seq, rep(b, 100), crs)
    scene <- vellum::draw(
      scene,
      vellum::lines_grob(
        vellum::vl_unit(ln$x, "native"),
        vellum::vl_unit(ln$y, "native"),
        sketch = sk,
        gp = gp_lat
      )
    )
    if (do_labels) {
      scene <- .grat_label(scene, ln, xr, yr, "left", .fmt_lat(b), gp_txt)
    }
  }
  scene
}
