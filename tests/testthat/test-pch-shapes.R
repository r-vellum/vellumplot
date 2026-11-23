# VP-103: ggplot2's numeric shape codes draw as ggplot2 draws them -- the open
# codes 0-14 as an outline in the point colour, the solid 15-20 filled with
# it, and the bordered 21-25 with `fill` inside a `color` outline -- in the
# plot and in the legend keys.

# Every grob_points of a plot: its shapes, fill and outline, and whether it
# sits inside a legend.
pch_points <- function(p) {
  s <- vellum::as_scene_spec(vellum::as_vellum_scene(p))
  val <- function(x) if (is.list(x)) x$v else x
  out <- list()
  walk <- function(node, inside) {
    nm <- val(node$name)
    if (is.character(nm) && length(nm) == 1L && grepl("legend", nm)) {
      inside <- TRUE
    }
    if (identical(node$class, "grob_points")) {
      sh <- unique(as.character(val(node$shape)))
      out[[length(out) + 1L]] <<- data.frame(
        legend = inside,
        shape = paste(sh, collapse = ","),
        fill = format(val(node$gp$fill) %||% NA),
        col = format(val(node$gp$col) %||% NA)
      )
    }
    for (ch in node) {
      if (is.list(ch)) walk(ch, inside)
    }
  }
  walk(s$root, FALSE)
  do.call(rbind, out)
}

mt3 <- transform(mtcars, fcyl = factor(cyl), fam = factor(am))

test_that("pch codes map to open, solid and bordered markers", {
  expect_identical(
    .pch_to_shape(c(0, 1, 2, 5, 6)),
    c(
      "square open",
      "circle open",
      "triangle open",
      "diamond open",
      "triangle_down open"
    )
  )
  # R draws 15-18 without an outline and 20 at 2/3 the size of 19 (VP-106)
  expect_identical(
    .pch_to_shape(c(15, 16, 17, 18, 19, 20)),
    c(
      paste(c("square", "circle", "triangle", "diamond"), "bare"),
      "circle",
      "circle small"
    )
  )
  expect_identical(
    .pch_to_shape(21:25),
    paste(c("circle", "square", "diamond", "triangle", "triangle_down"), "filled")
  )
  expect_identical(.pch_to_shape(c(3, 4)), c("plus", "cross"))
  expect_identical(.shape_parts(c("circle open", "square", "star filled", "M0 0"))$style, c("open", "solid", "filled", "solid"))
  expect_identical(.shape_parts(c("circle open", "M0 0"))$marker, c("circle", "M0 0"))
})

test_that("open shapes draw stroke-only, in the plot and the shape legend", {
  p <- vplot(mt3) |>
    mark_point(x = wt, y = mpg, shape = fam) |>
    scale_shape_manual(values = c(1, 0))
  pts <- pch_points(p)
  marks <- pts[!pts$legend, ]
  expect_setequal(strsplit(marks$shape, ",")[[1]], c("circle", "square"))
  expect_true(all(marks$fill == "NA"))
  keys <- pts[pts$legend, ]
  expect_setequal(keys$shape, c("circle", "square"))
  expect_true(all(keys$fill == "NA"))
  expect_true(all(keys$col != "NA"))
})

test_that("a constant pch shape draws (it drew nothing) and keys the colour legend", {
  p <- vplot(mt3) |> mark_point(x = wt, y = mpg, color = fcyl, shape = 1)
  pts <- pch_points(p)
  marks <- pts[!pts$legend, ]
  expect_identical(nrow(marks), 3L) # one group per colour
  expect_true(all(marks$shape == "circle" & marks$fill == "NA"))
  keys <- pts[pts$legend, ]
  expect_identical(nrow(keys), 3L)
  expect_true(all(keys$fill == "NA"))
  # each key's outline is its level's colour, as the marks are
  expect_setequal(keys$col, marks$col)
})

test_that("bordered shapes fill with `fill` inside a `color` outline", {
  p <- vplot(mt3) |> mark_point(x = wt, y = mpg, fill = fcyl, shape = 21)
  pts <- pch_points(p)
  marks <- pts[!pts$legend, ]
  expect_true(all(marks$col == "black"))
  expect_identical(length(unique(marks$fill)), 3L)
  keys <- pts[pts$legend, ]
  expect_true(all(keys$col == "black"))
  expect_setequal(keys$fill, marks$fill)
  # with no fill a bordered shape is unfilled, as in ggplot2
  q <- pch_points(vplot(mt3) |> mark_point(x = wt, y = mpg, shape = 21))
  expect_identical(q$fill, "NA")
  # a constant fill and colour
  r <- pch_points(
    vplot(mt3) |>
      mark_point(x = wt, y = mpg, shape = 24, fill = "orange", color = "navy")
  )
  expect_identical(r$shape, "triangle")
  expect_identical(c(r$fill, r$col), c("orange", "navy"))
})

test_that("solid codes and named shapes draw as before", {
  # (16 has no outline since VP-106; 19 fills and outlines in one colour)
  p <- vplot(mt3) |> mark_point(x = wt, y = mpg, color = fcyl, shape = 19)
  pts <- pch_points(p)
  marks <- pts[!pts$legend, ]
  expect_true(all(marks$fill == marks$col))
  expect_true(all(pts$shape == "circle"))
  q <- pch_points(
    vplot(mt3) |>
      mark_point(x = wt, y = mpg, shape = fcyl) |>
      scale_shape_manual(values = c("circle open", "square", "triangle filled"))
  )
  marks <- q[!q$legend, ]
  # the solid square fills; the open circle and the unfilled bordered
  # triangle share the no-fill group
  expect_identical(marks$fill[marks$shape == "square"], "black")
  expect_identical(marks$fill[marks$shape == "circle,triangle"], "NA")
})

test_that("an identity shape scale reads pch codes", {
  d <- data.frame(x = 1:3, y = 1:3, s = c(1, 16, 21))
  pts <- pch_points(
    vplot(d) |> mark_point(x = x, y = y, shape = s) |> scale_shape_identity()
  )
  expect_true(all(pts$shape == "circle"))
  # 1 and 21 (no fill) unfilled, 16 solid
  expect_setequal(pts$fill, c("NA", "black"))
})

test_that("override.aes shape codes draw as the plot does", {
  p <- vplot(mt3) |>
    mark_point(x = wt, y = mpg, color = fcyl) |>
    guides(color = guide_legend(override.aes = list(shape = 0)))
  keys <- pch_points(p)
  keys <- keys[keys$legend, ]
  expect_true(all(keys$shape == "square" & keys$fill == "NA"))
})
