# Spatial (sf) support: geometry decomposition, mark_sf / coord_sf, bbox-driven
# scale training, map aspect lock, binned choropleth scale, and NA handling.

# --- geometry decomposition (needs sf to build sfg objects) -----------------

test_that(".sf_decompose classifies each geometry type", {
  skip_if_not_installed("sf")
  pt <- sf::st_point(c(1, 2))
  expect_identical(.sf_decompose(pt)[[1]]$kind, "point")

  ls <- sf::st_linestring(matrix(c(0, 0, 1, 1, 2, 0), ncol = 2, byrow = TRUE))
  d <- .sf_decompose(ls)
  expect_identical(d[[1]]$kind, "line")
  expect_identical(nrow(d[[1]]$parts[[1]]), 3L)

  mpt <- sf::st_multipoint(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE))
  expect_identical(.sf_decompose(mpt)[[1]]$kind, "point")
})

test_that("polygons keep exterior + hole rings as separate sub-paths", {
  skip_if_not_installed("sf")
  outer <- matrix(c(0, 0, 10, 0, 10, 10, 0, 10, 0, 0), ncol = 2, byrow = TRUE)
  hole <- matrix(c(3, 3, 6, 3, 6, 6, 3, 6, 3, 3), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(outer, hole))
  d <- .sf_decompose(poly)
  expect_identical(d[[1]]$kind, "poly")
  expect_identical(length(d[[1]]$parts), 2L) # exterior + one hole

  # a MULTIPOLYGON flattens every ring of every part
  mp <- sf::st_multipolygon(list(list(outer), list(outer + 20)))
  d <- .sf_decompose(mp)
  expect_identical(d[[1]]$kind, "poly")
  expect_identical(length(d[[1]]$parts), 2L)
})

test_that("GEOMETRYCOLLECTION dispatches per member; empties skipped; Z/M dropped", {
  skip_if_not_installed("sf")
  outer <- matrix(c(0, 0, 10, 0, 10, 10, 0, 0), ncol = 2, byrow = TRUE)
  gc <- sf::st_geometrycollection(list(
    sf::st_point(c(1, 2)),
    sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
    sf::st_polygon(list(outer))
  ))
  d <- .sf_decompose(gc)
  expect_identical(vapply(d, `[[`, "", "kind"), c("point", "line", "poly"))

  expect_length(.sf_decompose(sf::st_polygon()), 0L) # EMPTY -> nothing

  # a POINT with Z keeps only the planar XY columns
  d <- .sf_decompose(sf::st_point(c(1, 2, 3)))
  expect_identical(ncol(d[[1]]$parts[[1]]), 2L)
})

# --- spec structure ---------------------------------------------------------

test_that("mark_sf() builds an sf layer and coord_sf() an sf coord", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p <- vplot(nc) |> mark_sf(fill = BIR74) |> coord_sf(crs = 4326)
  layer <- p@layers[[length(p@layers)]]
  expect_identical(layer@mark, "sf")
  expect_identical(layer@stat_params$na_value, "grey80")
  expect_identical(p@coord@kind, "sf")
  expect_equal(p@coord@crs, 4326)
})

# --- scale training + aspect lock -------------------------------------------

test_that("position domain trains from the projected bbox", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  spec <- vplot(nc) |> mark_sf() |> coord_sf()
  proj <- .project_sf_data(spec)
  built <- .build_panels(proj$spec)
  bb <- sf::st_bbox(nc)
  # trained domain is the bbox (with 5% expansion), so it contains the bbox
  expect_lte(built$scales$x$domain[1], bb[["xmin"]])
  expect_gte(built$scales$x$domain[2], bb[["xmax"]])
})

test_that("map aspect: geographic uses 1/cos(lat), projected uses 1", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  rt <- .resolve_theme(.theme_default())

  # geographic (lon/lat) -> respect = TRUE, flagged geographic
  spec <- vplot(nc) |> mark_sf() |> coord_sf()
  proj <- .project_sf_data(spec)
  expect_true(proj$geographic)
  built <- .build_panels(proj$spec)
  built$sf_geographic <- proj$geographic
  lay <- .build_layout(built, list(), list(), rt, FALSE, proj$spec@coord)
  expect_true(lay$respect)

  # projected CRS -> not geographic (aspect 1)
  spec2 <- vplot(nc) |> mark_sf() |> coord_sf(crs = 32119)
  expect_false(.project_sf_data(spec2)$geographic)
})

# --- binned choropleth scale ------------------------------------------------

test_that("binned breaks: base quantile fallback and classInt styles", {
  v <- c(1, 2, 3, 4, 5, 10, 20, 50, 100)
  b <- .binned_breaks(v, 4, "quantile")
  expect_length(b, 5L)
  expect_true(!is.unsorted(b))

  skip_if_not_installed("classInt")
  bf <- .binned_breaks(v, 4, "fisher")
  expect_length(bf, 5L)
})

test_that("interval labels close the last class", {
  labs <- .interval_labels(c(0, 10, 20, 30))
  expect_length(labs, 3L)
  expect_match(labs[3], "\\]$") # last class right-closed
  expect_match(labs[1], "^\\[")
})

test_that("a binned colour scale maps to classes and flags NA", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  nc$SID74[1] <- NA
  spec <- vplot(nc) |> mark_sf(fill = SID74) |> scale_fill_binned(n = 4)
  built <- .build_panels(.project_sf_data(spec)$spec)
  cl <- built$scales$color
  expect_identical(cl$kind, "binned")
  expect_length(cl$colors, 4L)
  expect_true(cl$na)
  # NA input maps to the na_value colour, not NA
  expect_identical(cl$map(NA_real_), cl$na_value)
})

# --- end to end -------------------------------------------------------------

test_that("a choropleth compiles and renders without error", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p <- vplot(nc) |> mark_sf(fill = BIR74) |> coord_sf(crs = 32119)
  scene <- vellum::as_vellum_scene(p)
  expect_true(inherits(scene, "vellum::vellum_scene"))
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
  expect_true(file.exists(f))
})

test_that(".aes_fill_colour prefers the fill channel; .aes_colour prefers color", {
  scales <- list(color = NULL)
  # both set as constants: the polygon fill is `fill`, the border colour is
  # `color` -- a constant `color` must not leak into the fill.
  both <- list(
    params = list(fill = "steelblue", color = "red"),
    values = list()
  )
  expect_identical(.aes_fill_colour(both, scales, NA_character_), "steelblue")
  expect_identical(.aes_colour(both, scales, NA_character_), "red")
  # fill alone -> fill
  fonly <- list(params = list(fill = "steelblue"), values = list())
  expect_identical(.aes_fill_colour(fonly, scales, "grey80"), "steelblue")
  # color alone -> fill falls to the default, never `color`
  conly <- list(params = list(color = "red"), values = list())
  expect_identical(.aes_fill_colour(conly, scales, "grey80"), "grey80")
})

test_that("mark_sf() with a constant fill + colour fills and strokes distinctly", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  # A steelblue fill with a red border must not render like an all-red map (the
  # regression: a constant `color` used to be painted as the fill too).
  fill_and_border <- vellum::scene_raster(
    vplot(nc) |> mark_sf(fill = "steelblue", color = "red") |> coord_sf()
  )
  all_red <- vellum::scene_raster(
    vplot(nc) |> mark_sf(fill = "red", color = "red") |> coord_sf()
  )
  expect_false(identical(fill_and_border, all_red))
})

test_that("mark_sf() errors on non-sf data", {
  skip_if_not_installed("sf")
  expect_error(
    vellumplot:::.resolve_layer(
      (vplot(data.frame(a = 1)) |> mark_sf())@layers[[1]],
      data.frame(a = 1)
    ),
    "sf"
  )
})

# --- bounding box (linear, no vector growth) --------------------------------

test_that(".sf_bbox spans all parts and skips empty / non-finite coords", {
  # two features: a triangle and a segment, plus an empty part that must be
  # ignored -- the bbox is the union extent.
  f1 <- list(list(
    kind = "poly",
    parts = list(
      matrix(c(0, 0, 4, 0, 2, 3, 0, 0), ncol = 2, byrow = TRUE)
    )
  ))
  f2 <- list(list(
    kind = "line",
    parts = list(
      matrix(c(-1, 1, 5, 6), ncol = 2, byrow = TRUE),
      matrix(numeric(0), ncol = 2) # empty part -> skipped
    )
  ))
  expect_identical(.sf_bbox(list(f1, f2)), c(-1, 5, 0, 6))

  # non-finite coordinates are dropped, finite ones still counted
  fna <- list(list(
    kind = "point",
    parts = list(
      matrix(c(NA, 2, 3, Inf, 1, 4), ncol = 2, byrow = TRUE)
    )
  ))
  expect_identical(.sf_bbox(list(fna)), c(1, 3, 2, 4))
})

test_that(".sf_bbox aborts when no finite coordinates remain", {
  empty <- list(list(list(
    kind = "poly",
    parts = list(
      matrix(c(NA, NA), ncol = 2)
    )
  )))
  expect_error(.sf_bbox(empty), "no finite coordinates")
  expect_error(.sf_bbox(list()), "no finite coordinates")
})

# --- feature batching (non-interactive) vs per-feature (interactive) --------

# All sf grobs a layer emits, as provenance records (one record per emitted
# grob). `element_table()`/`scene_model()` skip *unkeyed* paths/lines, so
# non-interactive grob counts must be read from provenance, not the element table.
sf_records <- function(p) {
  Filter(function(e) e$mark == "sf" && e$kind == "mark", plot_provenance(p))
}

test_that("non-interactive polygons batch into one grob per style group", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  n <- nrow(nc)

  # a single constant fill -> one style group -> ONE grob for the whole map
  recs <- sf_records(vplot(nc) |> mark_sf())
  expect_length(recs, 1L)
  expect_identical(sort(recs[[1]]$rows), seq_len(n))

  # a choropleth -> several groups (one per quantized fill), far fewer than n,
  # and every feature is drawn exactly once across the groups
  crecs <- sf_records(vplot(nc) |> mark_sf(fill = BIR74))
  expect_gt(length(crecs), 1L)
  expect_lt(length(crecs), n)
  rows <- sort(unlist(lapply(crecs, `[[`, "rows")))
  expect_identical(rows, seq_len(n))
})

test_that("interactive polygons stay one grob per feature (keys preserved)", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  n <- nrow(nc)

  # per-feature provenance: n grobs, each covering exactly one feature
  recs <- sf_records(vplot(nc) |> mark_sf(data_id = CNTY_ID))
  expect_length(recs, n)
  expect_true(all(vapply(recs, function(e) length(e$rows) == 1L, logical(1))))

  # and one keyed `path` element per feature in the scene model
  sc <- vellum::as_vellum_scene(vplot(nc) |> mark_sf(data_id = CNTY_ID))
  el <- vellum::scene_model(sc)$elements
  keyed <- el[!is.na(el$key), ]
  expect_equal(nrow(keyed), n)
  expect_true(all(keyed$mark == "path"))
  expect_setequal(keyed$key, as.character(nc$CNTY_ID))
})

test_that("batched polygons render identically to per-feature (disjoint)", {
  skip_if_not_installed("sf")
  # three separate, non-adjacent squares sharing one fill -> batched into one
  # grob; with a data_id they fall back to one grob per feature. For genuinely
  # disjoint geometry the two paths are pixel-identical (the batched evenodd path
  # only differs from per-feature drawing where features share an edge).
  sq <- function(x0, y0) {
    sf::st_polygon(list(matrix(
      c(x0, y0, x0 + 1, y0, x0 + 1, y0 + 1, x0, y0 + 1, x0, y0),
      ncol = 2,
      byrow = TRUE
    )))
  }
  d <- sf::st_sf(
    id = c("a", "b", "c"),
    geometry = sf::st_sfc(sq(0, 0), sq(3, 0), sq(0, 3))
  )
  batched <- vellum::scene_raster(vplot(d) |> mark_sf(fill = "steelblue"))
  perfeat <- vellum::scene_raster(
    vplot(d) |> mark_sf(fill = "steelblue", data_id = id)
  )
  expect_identical(batched, perfeat)
})

test_that("lines batch when non-interactive, split per feature when keyed", {
  skip_if_not_installed("sf")
  ln <- function(x0) {
    sf::st_linestring(matrix(
      c(x0, 0, x0 + 1, 2, x0 + 2, 0),
      ncol = 2,
      byrow = TRUE
    ))
  }
  d <- sf::st_sf(
    id = c("l1", "l2", "l3"),
    geometry = sf::st_sfc(ln(0), ln(4), ln(8))
  )

  # one style group (default colour) -> one batched lines_grob
  expect_length(sf_records(vplot(d) |> mark_sf()), 1L)

  # keyed -> one line grob per feature, each a keyed `line` element
  recs <- sf_records(vplot(d) |> mark_sf(data_id = id))
  expect_length(recs, 3L)
  el <- vellum::scene_model(vellum::as_vellum_scene(
    vplot(d) |> mark_sf(data_id = id)
  ))$elements
  keyed <- el[!is.na(el$key), ]
  expect_equal(nrow(keyed), 3L)
  expect_true(all(keyed$mark == "line"))
  expect_setequal(keyed$key, c("l1", "l2", "l3"))
})

# --- map decorations: scale bar, compass, graticule ------------------------

test_that(".nice_scalebar_len snaps to a 1/2/5 x 10^k value", {
  expect_equal(.nice_scalebar_len(37), 20)
  expect_equal(.nice_scalebar_len(6), 5)
  expect_equal(.nice_scalebar_len(0.7), 0.5)
  expect_equal(.nice_scalebar_len(1000), 1000)
  expect_equal(.nice_scalebar_len(0), 1) # degenerate guard
})

test_that(".corner_anchor resolves the four corners and abbreviations", {
  bl <- .corner_anchor("bottomleft")
  expect_equal(c(bl$x0, bl$y0, bl$hdir, bl$vdir), c(0, 0, 1, 1))
  tr <- .corner_anchor("tr")
  expect_equal(c(tr$x0, tr$y0, tr$hdir, tr$vdir), c(1, 1, -1, -1))
  expect_error(.corner_anchor("middle"), "position")
})

test_that("mark_scalebar / mark_compass compile and render on a map", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p <- vplot(nc) |>
    mark_sf(fill = BIR74) |>
    coord_sf(crs = 3857) |>
    mark_scalebar() |>
    mark_compass()
  expect_true(inherits(vellum::as_vellum_scene(p), "vellum::vellum_scene"))
  prov <- plot_provenance(p)
  expect_gt(length(Filter(function(e) e$mark == "scalebar", prov)), 0L)
  expect_gt(length(Filter(function(e) e$mark == "compass", prov)), 0L)
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(p, f))
  expect_true(file.exists(f))
})

test_that("scale bar labels read 0-to-distance left-to-right in every corner", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  # The "0" label sits at the low-x end and the distance at the high-x end no
  # matter which corner the bar occupies (a right-anchored corner grows the bar
  # leftward, which used to mirror the labels -- distance on the left).
  label_x <- function(pos) {
    p <- vplot(nc) |>
      mark_sf() |>
      coord_sf(crs = 3857) |>
      mark_scalebar(position = pos, unit = "km")
    svg <- vellum::scene_svg(vellum::as_vellum_scene(p))
    tags <- regmatches(svg, gregexpr("<text[^>]*>[^<]*</text>", svg))[[1]]
    x_of <- function(pat) {
      tag <- grep(pat, tags, value = TRUE)[1]
      as.numeric(regmatches(tag, regexec('x="([-0-9.]+)"', tag))[[1]][2])
    }
    c(zero = x_of(">0</text>$"), dist = x_of("km</text>$"))
  }
  for (pos in c("bottomleft", "bottomright", "topleft", "topright")) {
    xs <- label_x(pos)
    expect_lt(xs[["zero"]], xs[["dist"]])
  }
})

test_that("mark_scalebar() errors without a map coordinate system", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> mark_scalebar()
  expect_error(vellum::as_vellum_scene(p), "map|coord_sf")
})

test_that("mark_scalebar() accepts a custom unit and explicit distance", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p <- vplot(nc) |>
    mark_sf() |>
    coord_sf(crs = 3857) |>
    mark_scalebar(unit = "mi", distance = 100, position = "bottomright")
  expect_no_error(vellum::as_vellum_scene(p))
})

test_that("graticule reprojection curves meridians for a conic CRS", {
  skip_if_not_installed("sf")
  # Web Mercator: a meridian stays vertical (x constant).
  merc <- .grat_project_line(rep(-80, 30), seq(30, 45, length.out = 30), 3857)
  expect_equal(diff(range(merc$x)), 0, tolerance = 1e-6)
  # Albers (5070) is conic: meridians converge, so x varies along a meridian.
  alb <- .grat_project_line(rep(-100, 30), seq(25, 49, length.out = 30), 5070)
  expect_gt(diff(range(alb$x)), 0)
})

test_that(".grat_lonlat_range recovers a plausible lon/lat window", {
  skip_if_not_installed("sf")
  r <- .grat_lonlat_range(c(-9.2e6, -8.5e6), c(4.1e6, 4.4e6), 3857)
  expect_lt(r$lon[1], r$lon[2])
  expect_lt(r$lat[1], r$lat[2])
  expect_true(r$lon[2] < -70 && r$lat[1] > 30) # eastern-US-ish window
})

test_that("coord_sf(graticule=) renders projected and geographic maps", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  proj <- vplot(nc) |>
    mark_sf(fill = BIR74) |>
    coord_sf(crs = 5070, graticule = TRUE)
  geo <- vplot(nc) |>
    mark_sf(fill = BIR74) |>
    coord_sf(crs = "OGC:CRS84", graticule = list(lon = c(-82, -80, -78)))
  f1 <- local_tempfile(fileext = ".png")
  f2 <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(proj, f1))
  expect_no_error(render_plot(geo, f2))
})

# --- mark_sf_label() --------------------------------------------------------

test_that("mark_sf_label() labels each feature at an interior point", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p <- vplot(nc) |> mark_sf(fill = AREA) |> mark_sf_label(label = NAME)
  layer <- p@layers[[length(p@layers)]]
  expect_identical(layer@mark, "sf_label")
  # repel avoids only the other labels, never the (huge-bbox) polygons
  expect_identical(layer@stat_params$repel$avoid, "labels")
  expect_no_error(plot_svg(p))

  # one interior point per feature, each inside its own polygon
  proj <- .project_sf_data(p)
  L <- .build_panels(proj$spec)$panels[[1]]$resolved[[2]]
  expect_equal(L$n, nrow(nc))
  expect_setequal(as.character(L$values$label), as.character(nc$NAME))
  pts <- sf::st_as_sf(
    data.frame(x = L$values$x, y = L$values$y),
    coords = c("x", "y"),
    crs = sf::st_crs(nc)
  )
  inside <- vapply(
    seq_len(nrow(nc)),
    function(i) {
      as.logical(sf::st_intersects(pts[i, ], nc[i, ], sparse = FALSE))
    },
    logical(1)
  )
  expect_equal(sum(inside), nrow(nc))
})

test_that("mark_sf_label() reprojects labels through coord_sf(crs=)", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p <- vplot(nc) |>
    mark_sf() |>
    mark_sf_label(label = NAME) |>
    coord_sf(crs = 3857)
  proj <- .project_sf_data(p)
  L <- .build_panels(proj$spec)$panels[[1]]$resolved[[2]]
  # the label points come out in the target CRS and still fall inside the
  # projected features (the whole point of routing through the projection)
  nc3857 <- sf::st_transform(nc, 3857)
  pts <- sf::st_as_sf(
    data.frame(x = L$values$x, y = L$values$y),
    coords = c("x", "y"),
    crs = 3857
  )
  inside <- vapply(
    seq_len(nrow(nc)),
    function(i) {
      as.logical(sf::st_intersects(pts[i, ], nc3857[i, ], sparse = FALSE))
    },
    logical(1)
  )
  expect_equal(sum(inside), nrow(nc))
})

test_that("mark_sf_label() requires sf data", {
  skip_if_not_installed("sf")
  expect_error(
    plot_svg(vplot(mtcars) |> mark_sf_label(label = cyl)),
    "requires an .*sf"
  )
})
