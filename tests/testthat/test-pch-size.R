# VP-106: ggplot2's solid pch codes differ in outline and size -- R draws 15-18
# without a border (so 16 is slightly smaller than 19) and 20, the bullet, at
# 2/3 the size of 19 -- in the plot and in the legend keys.
# VP-107: colour and size legend keys draw a layer's constant shape, whatever
# it is (VP-103 did this only for open and bordered shapes).

# Every grob_points of a plot: shape, outline, size (mm), and whether it sits
# inside a legend.
pch_pts <- function(p) {
  s <- vellum::as_scene_spec(vellum::as_vellum_scene(p))
  val <- function(x) if (is.list(x)) x$v else x
  out <- list()
  walk <- function(node, inside) {
    nm <- val(node$name)
    if (is.character(nm) && length(nm) == 1L && grepl("legend", nm)) {
      inside <- TRUE
    }
    if (identical(node$class, "grob_points")) {
      out[[length(out) + 1L]] <<- data.frame(
        legend = inside,
        shape = as.character(val(node$shape)),
        col = format(val(node$gp$col) %||% NA),
        size = val(node$size$value)
      )
    }
    for (ch in node) {
      if (is.list(ch)) walk(ch, inside)
    }
  }
  walk(s$root, FALSE)
  do.call(rbind, out)
}

pd <- data.frame(x = 1:6, y = 1, s = c(15, 16, 17, 18, 19, 20))

test_that("pch 15-18 draw without an outline, 20 at 2/3 the size of 19", {
  p <- vplot(pd) |>
    mark_point(x = x, y = y, shape = s, size = 3) |>
    scale_shape_identity()
  pts <- pch_pts(p)
  pts <- pts[!pts$legend, ]
  pts <- pts[order(pts$shape, pts$size), ]
  bare <- pts[pts$col == "NA", ]
  expect_setequal(bare$shape, c("square", "circle", "triangle", "diamond"))
  # the triangle (17) at R's size for it (VP-111)
  expect_equal(
    bare$size[order(bare$shape)],
    3 * c(1, 1, 1, sqrt(4 * pi / (3 * sqrt(3))))
  )
  solid <- pts[pts$col != "NA", ]
  expect_identical(solid$shape, c("circle", "circle"))
  expect_equal(sort(solid$size), c(2, 3))
})

test_that("a constant pch 16 / 20 draws as in ggplot2", {
  p16 <- pch_pts(vplot(pd) |> mark_point(x = x, y = y, shape = 16))
  expect_true(all(p16$col == "NA"))
  p20 <- pch_pts(vplot(pd) |> mark_point(x = x, y = y, shape = 20, size = 1.5))
  expect_equal(unique(p20$size), 1)
  expect_true(all(p20$col != "NA"))
})

test_that("shape legend keys draw pch 16 and 20 as the plot does", {
  d <- data.frame(x = 1:4, y = 1:4, g = c("a", "a", "b", "b"))
  p <- vplot(d) |>
    mark_point(x = x, y = y, shape = g) |>
    scale_shape_manual(values = c(a = 19, b = 20))
  keys <- pch_pts(p)
  keys <- keys[keys$legend, ]
  expect_identical(nrow(keys), 2L)
  expect_equal(keys$size[2] / keys$size[1], 2 / 3)
  p <- vplot(d) |>
    mark_point(x = x, y = y, shape = g) |>
    scale_shape_manual(values = c(a = 16, b = 15))
  keys <- pch_pts(p)
  keys <- keys[keys$legend, ]
  expect_true(all(keys$col == "NA"))
})

test_that("colour and size keys draw a constant solid shape (VP-107)", {
  mt <- transform(mtcars, fcyl = factor(cyl))
  p <- vplot(mt) |> mark_point(x = wt, y = mpg, color = fcyl, shape = "square")
  keys <- pch_pts(p)
  keys <- keys[keys$legend, ]
  expect_identical(nrow(keys), 3L)
  expect_true(all(keys$shape == "square"))
  p <- vplot(mt) |> mark_point(x = wt, y = mpg, size = hp, shape = "triangle")
  keys <- pch_pts(p)
  keys <- keys[keys$legend, ]
  expect_gt(nrow(keys), 1L)
  expect_true(all(keys$shape == "triangle"))
  # a pch code keys as it draws: 17 is a triangle without an outline, 20 a
  # small circle
  p <- vplot(mt) |> mark_point(x = wt, y = mpg, color = fcyl, shape = 17)
  keys <- pch_pts(p)
  keys <- keys[keys$legend, ]
  expect_true(all(keys$shape == "triangle" & keys$col == "NA"))
  k19 <- pch_pts(
    vplot(mt) |> mark_point(x = wt, y = mpg, color = fcyl, shape = 19)
  )
  k20 <- pch_pts(
    vplot(mt) |> mark_point(x = wt, y = mpg, color = fcyl, shape = 20)
  )
  expect_equal(k20$size[k20$legend] / k19$size[k19$legend], rep(2 / 3, 3))
})

test_that("a constant SVG icon shape leaves the colour keys as circles", {
  mt <- transform(mtcars, fcyl = factor(cyl))
  p <- vplot(mt) |>
    mark_point(
      x = wt,
      y = mpg,
      color = fcyl,
      shape = "M12 2l3 7h7l-5.5 4.5 2 7-6.5-4.5-6.5 4.5 2-7L2 9h7z"
    )
  keys <- pch_pts(p)
  keys <- keys[keys$legend, ]
  expect_identical(nrow(keys), 3L)
  expect_true(all(keys$shape == "circle"))
})

test_that("size and shape keys take a layer's constant colour", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, size = hp, color = "red")
  keys <- pch_pts(p)
  keys <- keys[keys$legend, ]
  expect_gt(nrow(keys), 1L)
  expect_true(all(keys$col == "red"))
  mt <- transform(mtcars, fcyl = factor(cyl))
  p <- vplot(mt) |> mark_point(x = wt, y = mpg, shape = fcyl, color = "red")
  keys <- pch_pts(p)
  keys <- keys[keys$legend, ]
  expect_identical(nrow(keys), 3L)
  expect_true(all(keys$col == "red"))
  # no constant colour: the keys keep the default
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg, size = hp)
  keys <- pch_pts(p)
  expect_false(any(keys$col[keys$legend] == "red"))
})

# VP-109: point keys default to the points' colour (black), as ggplot2 keys
# them, in every key path -- size, shape and merged size + shape legends.
test_that("size and shape keys default to the points' black (VP-109)", {
  mt <- transform(mtcars, fcyl = factor(cyl))
  key_cols <- function(p) {
    k <- pch_pts(p)
    k$col[k$legend]
  }
  expect_true(all(
    key_cols(vplot(mt) |> mark_point(x = wt, y = mpg, size = hp)) == "black"
  ))
  expect_true(all(
    key_cols(vplot(mt) |> mark_point(x = wt, y = mpg, shape = fcyl)) == "black"
  ))
  merged <- function(...) {
    suppressWarnings(pch_pts(
      vplot(mt) |> mark_point(x = wt, y = mpg, size = fcyl, shape = fcyl, ...)
    ))
  }
  k <- merged()
  k <- k[k$legend, ]
  expect_identical(nrow(k), 3L)
  expect_true(all(k$col %in% c("black", "NA")))
  # a merged size + shape legend takes the layer's constant colour too
  k <- merged(color = "tomato")
  k <- k[k$legend, ]
  expect_identical(nrow(k), 3L)
  expect_true(all(k$col %in% c("tomato", "NA")))
  # a constant fill colours the points (`.aes_colour()`), and so the keys
  k <- pch_pts(
    vplot(mt) |> mark_point(x = wt, y = mpg, size = hp, fill = "steelblue")
  )
  expect_true(all(k$col == "steelblue"))
})

test_that("point keys follow the theme's ink (VP-109)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = hp) |>
    theme(geom = element_geom(ink = "navy"))
  k <- pch_pts(p)
  expect_true(all(k$col == "navy"))
})

# VP-111: R's graphics engine sizes its symbols from one radius r -- the
# circle's radius and the squares' (0, 15) half side, as vellum's markers --
# but draws triangles with circumradius TRC0 r, the open diamond with
# half-diagonal sqrt(2) r, the bordered diamond DMDC r, the bordered square
# with half side SQRC r, and the plus and cross sqrt(2) times vellum's.
test_that("pch codes draw at R's per-symbol sizes (VP-111)", {
  trc0 <- sqrt(4 * pi / (3 * sqrt(3)))
  want <- c(
    "0" = 1,
    "1" = 1,
    "2" = trc0,
    "3" = sqrt(2),
    "4" = sqrt(2),
    "5" = sqrt(2),
    "6" = trc0,
    "15" = 1,
    "16" = 1,
    "17" = trc0,
    "18" = 1,
    "19" = 1,
    "20" = 2 / 3,
    "21" = 1,
    "22" = sqrt(pi / 4),
    "23" = sqrt(pi / 4) * sqrt(2),
    "24" = trc0,
    "25" = trc0
  )
  for (code in names(want)) {
    pts <- pch_pts(
      vplot(data.frame(x = 1, y = 1)) |>
        mark_point(x = x, y = y, shape = !!as.numeric(code), size = 2)
    )
    expect_equal(unique(pts$size), 2 * want[[code]], info = code)
  }
  # the plus and cross are line glyphs drawn by name
  expect_identical(
    unique(
      pch_pts(
        vplot(data.frame(x = 1, y = 1)) |> mark_point(x = x, y = y, shape = 3)
      )$shape
    ),
    "plus"
  )
})

test_that("named markers keep their size; pch keys scale as the points (VP-111)", {
  one <- data.frame(x = 1, y = 1)
  for (nm in c("triangle", "diamond", "square", "plus", "cross")) {
    pts <- pch_pts(
      vplot(one) |> mark_point(x = x, y = y, shape = !!nm, size = 2)
    )
    expect_equal(unique(pts$size), 2, info = nm)
  }
  d <- data.frame(x = 1:2, y = 1:2, g = c("a", "b"))
  keys <- pch_pts(
    vplot(d) |>
      mark_point(x = x, y = y, shape = g) |>
      scale_shape_manual(values = c(a = 21, b = 24))
  )
  keys <- keys[keys$legend, ]
  expect_equal(
    keys$size[keys$shape == "triangle"] / keys$size[keys$shape == "circle"],
    sqrt(4 * pi / (3 * sqrt(3)))
  )
})
