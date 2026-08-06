# Text marks: mark_text / mark_label.

d <- head(mtcars, 8)
d$nm <- rownames(d)

test_that("constructors set the right mark", {
  expect_identical(
    (vplot(d) |> mark_text(x = wt, y = mpg, label = nm))@layers[[1]]@mark,
    "text"
  )
  expect_identical(
    (vplot(d) |> mark_label(x = wt, y = mpg, label = nm))@layers[[1]]@mark,
    "label"
  )
})

test_that("label is resolved as a mapped channel", {
  res <- vellumplot:::.resolve_layers(
    vplot(d) |> mark_text(x = wt, y = mpg, label = nm)
  )
  expect_identical(res[[1]]$values$label, d$nm)
})

test_that("size/family/hjust become params; angle can map", {
  L <- (vplot(d) |>
    mark_text(x = wt, y = mpg, label = nm, size = 10, hjust = "left"))@layers[[
    1
  ]]
  expect_identical(L@params$size, 10)
  expect_identical(L@params$hjust, "left")
})

test_that("mark_text / mark_label render (incl. flipped)", {
  for (b in list(
    function(p) mark_text(p, x = wt, y = mpg, label = nm),
    function(p) mark_label(p, x = wt, y = mpg, label = nm)
  )) {
    f <- local_tempfile(fileext = ".png")
    render_plot(b(vplot(d)), f)
    expect_gt(file.info(f)$size, 0)
    f2 <- local_tempfile(fileext = ".png")
    render_plot(b(vplot(d)) |> coord_flip(), f2)
    expect_gt(file.info(f2)$size, 0)
  }
})

test_that("text renders ink (the labels)", {
  img <- render_px(vplot(d) |> mark_text(x = wt, y = mpg, label = nm))
  expect_gt(count_near(img, c(0, 0, 0), 0.3), 50) # dark text pixels
})

test_that("mark_label draws a light background behind text", {
  img <- render_px(
    vplot(d) |> mark_label(x = wt, y = mpg, label = nm, fill = "white")
  )
  # white label boxes sit over the grey panel -> many near-white pixels inside
  expect_gt(count_near(img, c(1, 1, 1), 0.04), 200)
})

test_that("a mapped fill colours the label background per group (H11)", {
  d2 <- d
  d2$grp <- factor(rep(c("a", "b"), length.out = nrow(d2)))
  # fill is a mapped channel, not a constant param
  L <- (vplot(d2) |>
    mark_label(x = wt, y = mpg, label = nm, fill = grp))@layers[[1]]
  expect_true("fill" %in% names(L@encoding))
  expect_null(L@params$fill)
  # ...and the two groups render as two distinct, saturated backgrounds.
  img <- render_px(
    vplot(d2) |>
      mark_label(x = wt, y = mpg, label = nm, fill = grp) |>
      scale_fill_manual(values = c(a = "red", b = "blue"))
  )
  expect_gt(count_near(img, c(1, 0, 0), 0.15), 50) # red boxes
  expect_gt(count_near(img, c(0, 0, 1), 0.15), 50) # blue boxes
})

test_that("mark_text() errors clearly when no label is mapped", {
  expect_error(
    plot_svg(vplot(mtcars) |> mark_text(x = wt, y = mpg)),
    "needs a .*label"
  )
})
