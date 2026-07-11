# Cross-layer contract: vellumplot compiles to vellum's scene, and depends on the
# shape of `scene_model()` and the SVG `data-vellum-id` attribute. These tests
# assert that dependency explicitly, so a schema change in vellum (caught by the
# nightly run against vellum's `main`) fails loudly here rather than as an
# incidental interactivity-test failure. The contract is specified in vellum's
# `vignette("scene-contract")`.

df <- data.frame(wt = mtcars$wt, mpg = mtcars$mpg, model = rownames(mtcars))

test_that("scene_model() exposes the element columns vellumplot reads", {
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, tooltip = model, data_id = model)
  m <- vellum::scene_model(vellum::as_vellum_scene(p))

  expect_named(
    m$elements,
    c(
      "key",
      "mark",
      "id",
      "name",
      "panel",
      "x0",
      "y0",
      "x1",
      "y1",
      "x",
      "y",
      "w",
      "h",
      "meta"
    )
  )
  # the specific columns vellumplot's interactivity path reads
  expect_true(all(c("mark", "key", "meta") %in% names(m$elements)))
  expect_type(m$elements$meta, "list")
})

test_that("vellumplot emits marks in vellum's documented `mark` vocabulary", {
  vocab <- c(
    "rect",
    "point",
    "circle",
    "hexagon",
    "sector",
    "segment",
    "path",
    "line",
    "polygon"
  )

  # a scatter -> point elements
  mp <- vellum::scene_model(vellum::as_vellum_scene(
    vplot(df) |> mark_point(x = wt, y = mpg, data_id = model)
  ))$elements
  expect_true(all(mp$mark %in% vocab))
  expect_true("point" %in% mp$mark)

  # bars -> rect elements
  mb <- vellum::scene_model(vellum::as_vellum_scene(
    vplot(data.frame(g = c("a", "b", "c"), y = c(1, 2, 3))) |>
      mark_bar(x = g, y = y, data_id = g)
  ))$elements
  expect_true(all(mb$mark %in% vocab))
  expect_true("rect" %in% mb$mark)
})

test_that("provenance id joins to the SVG data-vellum-id (the documented join key)", {
  p <- vplot(df) |>
    mark_point(x = wt, y = mpg, tooltip = model, data_id = model)
  sc <- vellum::as_vellum_scene(p)

  prov <- attr(sc, "vellumplot_provenance")
  expect_gt(length(prov), 0L)
  ids <- vapply(prov, function(r) r$id, character(1))

  # every provenance id surfaces as a data-vellum-id in the SVG ...
  svg <- vellum::scene_svg(sc)
  for (id in ids) {
    expect_match(svg, paste0('data-vellum-id="', id, '"'), fixed = TRUE)
  }
  # ... and as an `id` in the scene_model element table.
  model_ids <- unique(stats::na.omit(vellum::scene_model(sc)$elements$id))
  expect_true(all(ids %in% model_ids))
})
