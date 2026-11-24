# VP-127: inset(align_to =) was ignored; every inset took fractions of the
# whole base cell. "panel" (the default) now measures them in the base plot's
# panel area, "plot" in the plot without its title / subtitle / caption, and
# "full" in the whole cell.

align_base <- function() {
  vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    labs(title = "Title", subtitle = "Subtitle", caption = "Caption")
}
align_guest <- function() {
  vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
}

# the inset's box in device px (x0, x1, y0, y1; y down): the guest's
# plot.background rect, which fills its viewport (drawn after the base plot's,
# or before it for an inset behind the plot)
inset_box_px <- function(s, behind = FALSE) {
  m <- vellum::scene_model(s)$elements
  r <- m[m$mark == "rect" & m$panel == "plot-background", ]
  r <- r[if (behind) 1L else nrow(r), ]
  c(x0 = r$x0, x1 = r$x1, y0 = r$y0, y1 = r$y1)
}
model_box <- function(s, name) {
  m <- vellum::scene_model(s)$panels
  r <- m[m$name == name, ]
  c(x0 = r$px0, x1 = r$px1, y0 = r$py0, y1 = r$py1)
}

test_that("align_to = 'panel' (the default) spans the base panel", {
  base <- align_base()
  pan <- model_box(vellum::as_vellum_scene(base), "panel-1-1")
  s <- vellum::as_vellum_scene(
    inset(base, align_guest(), left = 0, bottom = 0, right = 1, top = 1)
  )
  expect_equal(inset_box_px(s), pan, tolerance = 1e-3)
  # fractions are of the panel: the right half, top quarter
  s2 <- vellum::as_vellum_scene(
    inset(base, align_guest(), left = 0.5, bottom = 0.75, right = 1, top = 1)
  )
  b <- inset_box_px(s2)
  w <- pan[["x1"]] - pan[["x0"]]
  h <- pan[["y1"]] - pan[["y0"]]
  expect_equal(b[["x0"]], pan[["x0"]] + 0.5 * w, tolerance = 1e-3)
  expect_equal(b[["x1"]], pan[["x1"]], tolerance = 1e-3)
  expect_equal(b[["y0"]], pan[["y0"]], tolerance = 1e-3)
  expect_equal(b[["y1"]], pan[["y0"]] + 0.25 * h, tolerance = 1e-3)
})

test_that("align_to = 'plot' leaves out the title, subtitle and caption", {
  base <- align_base()
  s0 <- vellum::as_vellum_scene(base)
  s <- vellum::as_vellum_scene(
    inset(base, align_guest(), 0, 0, 1, 1, align_to = "plot")
  )
  b <- inset_box_px(s)
  px <- s@dpi / 25.4
  m <- 5.5 * px # theme_grey plot.margin
  # full width inside the margin, legend included
  expect_equal(b[["x0"]], m, tolerance = 1e-3)
  expect_equal(b[["x1"]], 6 * s@dpi - m, tolerance = 1e-3)
  expect_gte(b[["x1"]], model_box(s0, "legend")[["x1"]] - 0.01)
  # below the subtitle and above the caption, axes included
  # below the subtitle band and above the caption band
  lay <- .plan_plot(base)$lay
  band <- function(i) vellum::vl_convert(lay$heights[i], "mm") * px
  expect_equal(
    b[["y0"]],
    m + band(lay$title_row) + band(lay$subtitle_row),
    tolerance = 1e-3
  )
  expect_equal(
    b[["y1"]],
    4 * s@dpi - m - band(lay$caption_row),
    tolerance = 1e-3
  )
  pan <- model_box(s0, "panel-1-1")
  expect_lt(b[["x0"]], pan[["x0"]])
  expect_gt(b[["y1"]], pan[["y1"]])
})

test_that("align_to = 'full' spans the whole base cell", {
  s <- vellum::as_vellum_scene(
    inset(align_base(), align_guest(), 0, 0, 1, 1, align_to = "full")
  )
  expect_equal(
    unname(inset_box_px(s)),
    c(0, 6 * s@dpi, 0, 4 * s@dpi),
    tolerance = 1e-3
  )
})

test_that("align_to = 'panel' spans every panel of a faceted plot", {
  base <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~cyl, ncol = 2)
  m <- vellum::scene_model(vellum::as_vellum_scene(base))$panels
  pn <- m[grepl("^panel-", m$name), ]
  s <- vellum::as_vellum_scene(inset(base, align_guest(), 0, 0, 1, 1))
  expect_equal(
    unname(inset_box_px(s)),
    c(min(pn$px0), max(pn$px1), min(pn$py0), max(pn$py1)),
    tolerance = 1e-3
  )
})

test_that("an inset behind the base plot is aligned too", {
  base <- align_base()
  pan <- model_box(vellum::as_vellum_scene(base), "panel-1-1")
  s <- vellum::as_vellum_scene(
    inset(base, align_guest(), 0, 0, 1, 1, on_top = FALSE)
  )
  expect_equal(inset_box_px(s, behind = TRUE), pan, tolerance = 1e-3)
})

test_that("a fixed-aspect inset fits inside the panel area", {
  tall <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    coord_fixed(ratio = 0.5) |>
    theme_void()
  base <- align_base()
  pan <- model_box(vellum::as_vellum_scene(base), "panel-1-1")
  s <- vellum::as_vellum_scene(inset(base, tall, 0.5, 0, 1, 1))
  b <- inset_box_px(s)
  # tall content in a wide box: the full panel height, narrowed from the left
  expect_equal(b[["y0"]], pan[["y0"]], tolerance = 1e-3)
  expect_equal(b[["y1"]], pan[["y1"]], tolerance = 1e-3)
  expect_equal(b[["x1"]], pan[["x1"]], tolerance = 1e-3)
  expect_lt(b[["x1"]] - b[["x0"]], 0.5 * (pan[["x1"]] - pan[["x0"]]))
})

test_that("a composition of several plots places insets in the whole figure", {
  comp <- hconcat(align_base(), align_guest())
  s <- vellum::as_vellum_scene(inset(comp, align_guest(), 0, 0, 0.5, 0.5))
  b <- inset_box_px(s)
  expect_equal(
    unname(b),
    c(
      0,
      0.5 * comp@width * s@dpi,
      0.5 * comp@height * s@dpi,
      comp@height * s@dpi
    ),
    tolerance = 1e-3
  )
})
