# Phase 2 interactivity declarations: `tooltip=` / `data_id=` / `hover_group=` on
# any mark flow per-row into the vellum scene as element keys/metadata (SVG
# `data-key` + `scene_model()`), and are inert on a static render.

df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, model = rownames(mtcars))
svg_of <- function(p) vellum::scene_svg(as_vellum_scene(p))
model_of <- function(p) vellum::scene_model(as_vellum_scene(p))
points_of <- function(p) {
  el <- model_of(p)$elements
  el[el$mark == "point", ]
}
# Data-mark points only. A colour scale also draws legend key glyphs (point
# grobs too): non-interactive by default (NA key), or — when the plot is
# interactive — tagged with a "legend:<aes>:<level>" key. Exclude both.
data_points_of <- function(p) {
  pts <- points_of(p)
  pts <- pts[!is.na(pts$key), , drop = FALSE]
  pts[!grepl("^legend:", pts$key), , drop = FALSE]
}

test_that("data_id becomes a per-element data-key in the SVG", {
  p <- vplot(df) |> mark_point(x = wt, y = mpg, data_id = model)
  svg <- svg_of(p)
  expect_match(svg, "data-key=")
  expect_match(svg, 'data-key="Mazda RX4"', fixed = TRUE)
  expect_match(svg, 'data-key="Valiant"', fixed = TRUE)
})

test_that("tooltip + data_id surface per element in scene_model()", {
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, tooltip = model, data_id = model)
  el <- data_points_of(p)
  expect_equal(nrow(el), nrow(df))
  expect_setequal(el$key, df$model)
  # meta carries the tooltip aligned to its element
  i <- match("Mazda RX4", el$key)
  expect_equal(el$meta[[i]]$tooltip, "Mazda RX4")
})

test_that("a plot with no interactivity declarations is unchanged (no data-key)", {
  p <- vplot(df) |> mark_point(x = wt, y = mpg)
  expect_no_match(svg_of(p), "data-key")
  # every element carries an NA key (geometry only)
  expect_true(all(is.na(points_of(p)$key)))
})

test_that("tooltip without data_id defaults the key to row identity", {
  p <- vplot(df) |> mark_point(x = wt, y = mpg, tooltip = model)
  el <- data_points_of(p)
  expect_setequal(el$key, as.character(seq_len(nrow(df))))
  expect_match(svg_of(p), 'data-key="1"', fixed = TRUE)
})

test_that("a constant tooltip recycles to every element", {
  p <- vplot(df) |> mark_point(x = wt, y = mpg, data_id = model, tooltip = "hi")
  el <- data_points_of(p)
  expect_true(all(vapply(
    el$meta,
    function(m) identical(m$tooltip, "hi"),
    logical(1)
  )))
})

test_that("data_id keys survive style grouping (colour splits into groups)", {
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, color = factor(mtcars$cyl), data_id = model)
  el <- data_points_of(p)
  # one element per row, each with its own key, despite multiple style groups
  expect_equal(nrow(el), nrow(df))
  expect_setequal(el$key, df$model)
})

test_that("bars carry per-bar keys", {
  db <- data.frame(cat = c("a", "b", "c"), val = c(3, 1, 2))
  p <- vplot(db) |> mark_bar(x = cat, y = val, data_id = cat, tooltip = val)
  el <- model_of(p)$elements
  bars <- el[el$mark == "rect" & !is.na(el$key), ]
  expect_setequal(bars$key, c("a", "b", "c"))
})

test_that("segments carry per-element keys", {
  ds <- data.frame(
    x = c(0, 1),
    y = c(0, 1),
    xe = c(1, 2),
    ye = c(1, 0),
    id = c("s1", "s2")
  )
  p <- vplot(ds) |>
    mark_segment(x = x, y = y, x2 = xe, y2 = ye, data_id = id)
  el <- model_of(p)$elements
  segs <- el[el$mark == "segment" & !is.na(el$key), ]
  expect_setequal(segs$key, c("s1", "s2"))
})

test_that("mark_sf carries per-feature keys (polygons) into SVG + scene_model", {
  skip_if_not_installed("sf")
  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
  p <- vplot(nc) |> mark_sf(tooltip = CNTY_ID, data_id = CNTY_ID)
  sc <- as_vellum_scene(p)
  expect_match(vellum::scene_svg(sc), "data-key=")
  el <- vellum::scene_model(sc)$elements
  keyed <- el[!is.na(el$key), ]
  # one keyed path element per county
  expect_equal(nrow(keyed), nrow(nc))
  expect_true(all(keyed$mark == "path"))
  expect_setequal(keyed$key, as.character(nc$CNTY_ID))
  # tooltip aligned to the feature
  i <- match(as.character(nc$CNTY_ID[1]), keyed$key)
  expect_equal(keyed$meta[[i]]$tooltip, as.character(nc$CNTY_ID[1]))
})

test_that("per-element hover_color / selected_color resolve into element meta", {
  df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, cyl = factor(mtcars$cyl))
  p <- vplot(df) |>
    mark_point(
      x = wt,
      y = mpg,
      data_id = seq_len(nrow(df)),
      hover_color = ifelse(mtcars$cyl == 8, "red", "blue"),
      selected_color = "black"
    )
  el <- data_points_of(p)
  expect_equal(nrow(el), nrow(df))
  # hover_color is data-driven (per row); selected_color is constant (recycled)
  hc <- vapply(
    el$meta,
    function(m) m$hover_color %||% NA_character_,
    character(1)
  )
  expect_setequal(unique(hc), c("red", "blue"))
  expect_true(all(vapply(
    el$meta,
    function(m) identical(m$selected_color, "black"),
    logical(1)
  )))
})

test_that("interactivity declarations do not perturb the rendered pixels", {
  base <- vplot(df) |> mark_point(x = wt, y = mpg)
  keyed <- vplot(df) |>
    mark_point(x = wt, y = mpg, tooltip = model, data_id = model)
  expect_identical(vellum::scene_raster(base), vellum::scene_raster(keyed))
})

# --- Legend interaction: discrete legend swatches drive their data series. -----
# When a plot is interactive, discrete colour/shape swatches are tagged with a
# `legend_for = "<aes>:<level>"` key so vellumwidget can project a swatch back onto the
# whole series, and every data mark carries its series membership in `meta$legend`.

legend_df <- data.frame(
  wt = mtcars$wt,
  mpg = mtcars$mpg,
  model = rownames(mtcars),
  cyl = factor(mtcars$cyl)
)
swatches_of <- function(p) {
  el <- model_of(p)$elements
  el[!is.na(el$key) & grepl("^legend:", el$key), ]
}

test_that("discrete colour legend swatches carry legend_for + tooltip", {
  p <- vplot(legend_df) |>
    mark_point(x = wt, y = mpg, color = cyl, data_id = model)
  sw <- swatches_of(p)
  expect_equal(nrow(sw), nlevels(legend_df$cyl))
  lf <- vapply(
    sw$meta,
    function(m) m[["legend_for"]] %||% NA_character_,
    character(1)
  )
  expect_setequal(lf, paste0("color:", levels(legend_df$cyl)))
  # tooltip mirrors the level label; swatches must NOT carry series membership
  tt <- vapply(
    sw$meta,
    function(m) m[["tooltip"]] %||% NA_character_,
    character(1)
  )
  expect_setequal(tt, levels(legend_df$cyl))
  expect_true(all(vapply(
    sw$meta,
    function(m) is.null(m[["legend"]]),
    logical(1)
  )))
})

test_that("data marks carry their colour-series membership in meta$legend", {
  p <- vplot(legend_df) |>
    mark_point(x = wt, y = mpg, color = cyl, data_id = model)
  el <- data_points_of(p)
  expect_equal(nrow(el), nrow(legend_df))
  i <- match("Mazda RX4", el$key) # cyl == 6
  expect_identical(el$meta[[i]][["legend"]], "color:6")
  j <- match("Cadillac Fleetwood", el$key) # cyl == 8
  expect_identical(el$meta[[j]][["legend"]], "color:8")
})

test_that("legend tagging is inert without interactivity declarations", {
  p <- vplot(legend_df) |> mark_point(x = wt, y = mpg, color = cyl)
  expect_no_match(svg_of(p), "legend:")
  expect_equal(nrow(swatches_of(p)), 0L)
  el <- points_of(p)
  expect_true(all(vapply(
    el$meta,
    function(m) is.null(m[["legend"]]),
    logical(1)
  )))
})
