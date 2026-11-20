# VP-66: mark_density() stays filled (ggplot2's geom_density() is unfilled), so
# its fill takes a default alpha of 0.8: one curve reads lighter, and
# overlapping groups show through. A set or mapped alpha wins; the outline
# stays opaque.

# The polygons (fills) and lines (outlines) inside the layer groups, with
# their gpar alpha (NA when unset, i.e. opaque).
da_parts <- function(p) {
  s <- vellum::as_scene_spec(vellum::as_vellum_scene(p))
  out <- list(fill = numeric(), line = numeric())
  val <- function(x) if (is.list(x)) x$v else x
  grab <- function(node, inside) {
    id <- val(node$id)
    if (is.character(id) && length(id) == 1L && grepl("^layer-", id)) {
      inside <- TRUE
    }
    cls <- val(node$class)
    if (inside && is.character(cls) && is.list(node$gp)) {
      a <- val(node$gp$alpha)
      a <- if (is.null(a)) NA_real_ else as.numeric(a)
      if (identical(cls, "grob_polygon")) {
        out$fill <<- c(out$fill, a)
      } else if (identical(cls, "grob_lines")) {
        out$line <<- c(out$line, a)
      }
    }
    for (ch in node) {
      if (is.list(ch)) grab(ch, inside)
    }
  }
  grab(s$root, FALSE)
  out
}

test_that("a density's fill defaults to alpha 0.8", {
  one <- vplot(mtcars) |> mark_density(x = mpg)
  grp <- vplot(mtcars) |> mark_density(x = mpg, fill = factor(cyl))
  for (th in list(identity, theme_dark, theme_cyberpunk)) {
    expect_identical(da_parts(th(one))$fill, 0.8)
    expect_identical(da_parts(th(grp))$fill, rep(0.8, 3))
  }
})

test_that("the density outline stays opaque", {
  p <- vplot(mtcars) |>
    mark_density(x = mpg, fill = factor(cyl), color = "black")
  parts <- da_parts(p)
  expect_identical(parts$fill, rep(0.8, 3))
  expect_length(parts$line, 3L)
  expect_true(all(is.na(parts$line)))
})

test_that("a set or mapped alpha wins over the density default", {
  expect_identical(
    da_parts(vplot(mtcars) |> mark_density(x = mpg, alpha = 1))$fill,
    1
  )
  expect_identical(
    da_parts(vplot(mtcars) |> mark_density(x = mpg, alpha = 0.3))$fill,
    0.3
  )
  m <- suppressWarnings(da_parts(
    vplot(mtcars) |>
      mark_density(x = mpg, fill = factor(cyl), alpha = factor(cyl))
  ))
  expect_false(any(m$fill == 0.8))
  expect_length(unique(m$fill), 3L)
  # an explicit alpha still reaches the outline, as before
  o <- da_parts(
    vplot(mtcars) |>
      mark_density(x = mpg, fill = factor(cyl), color = "black", alpha = 0.5)
  )
  expect_identical(o$line, rep(0.5, 3))
})

test_that("other filled bands keep their opaque default", {
  d <- data.frame(x = 1:5, y = c(2, 3, 1, 4, 2))
  expect_true(all(is.na(da_parts(vplot(d) |> mark_area(x = x, y = y))$fill)))
  expect_true(all(is.na(
    da_parts(vplot(d) |> mark_ribbon(x = x, ymin = y - 1, ymax = y + 1))$fill
  )))
})
