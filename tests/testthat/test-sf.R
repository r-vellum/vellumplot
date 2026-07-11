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
