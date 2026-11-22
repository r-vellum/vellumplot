# coord_flip() and reversed position scales: nudges follow the data axes (VP-88)
# and boxes on a reversed axis are drawn (VP-89).

.text_offsets <- function(p) {
  sc <- vellum::as_vellum_scene(p)
  walk <- function(node, acc) {
    if (S7::S7_inherits(node, vellum:::gtree)) {
      for (ch in node@children) acc <- walk(ch, acc)
    } else if (S7::S7_inherits(node, vellum:::grob_text)) {
      acc <- c(acc, list(node))
    }
    acc
  }
  g <- walk(vellum:::.materialize(sc), list())
  # the data labels are the single-character ones (axis labels are numbers)
  g <- Filter(function(t) all(t@label %in% letters), g)[[1]]
  c(x = vctrs_field(g@x, "offset")[1], y = vctrs_field(g@y, "offset")[1])
}

test_that("nudge_x / nudge_y follow the data axes under coord_flip() (VP-88)", {
  d <- data.frame(k = c("a", "b"), x = 1:2, y = c(3, 5))
  base <- vplot(d) |> mark_point(x = x, y = y)
  # unflipped: nudge_y moves up
  expect_equal(
    .text_offsets(base |> mark_text(x = x, y = y, label = k, nudge_y = 3)),
    c(x = 0, y = 3)
  )
  # flipped: data y runs horizontally, so nudge_y moves right ...
  expect_equal(
    .text_offsets(
      base |> mark_text(x = x, y = y, label = k, nudge_y = 3) |> coord_flip()
    ),
    c(x = 3, y = 0)
  )
  # ... and nudge_x moves up
  expect_equal(
    .text_offsets(
      base |> mark_text(x = x, y = y, label = k, nudge_x = 4) |> coord_flip()
    ),
    c(x = 0, y = 4)
  )
})

test_that("a positive nudge points toward larger values on a reversed axis", {
  d <- data.frame(k = c("a", "b"), x = 1:2, y = c(3, 5))
  p <- vplot(d) |>
    mark_text(x = x, y = y, label = k, nudge_y = 4, nudge_x = 2) |>
    scale_y_reverse()
  expect_equal(.text_offsets(p), c(x = 2, y = -4))
  # flipped and reversed: data x runs vertically and is reversed
  p <- vplot(d) |>
    mark_text(x = x, y = y, label = k, nudge_x = 2) |>
    coord_flip() |>
    scale_x_reverse()
  expect_equal(.text_offsets(p), c(x = 0, y = -2))
})

# How many boxes painted in the marker fill reach the drawn SVG. vellum's
# element table records a box's geometry even when the renderer drops it, so
# count what is painted: a box with a negative device size used to be skipped.
.MARK <- "#123456"
.n_painted <- function(p) {
  s <- paste(plot_svg(p), collapse = "")
  lengths(regmatches(s, gregexpr(paste0('fill="', .MARK, '"'), s, fixed = TRUE)))
}

test_that("bars draw on a reversed axis, flipped or not (VP-89)", {
  d <- data.frame(x = 1:4, y = c(3, 5, 2, 4))
  dd <- data.frame(x = c("a", "b", "c", "d"), y = c(3, 5, 2, 4))
  b <- vplot(d) |> mark_bar(x = x, y = y, fill = "#123456")
  # one painted path per bar group; the unreversed chart is the reference
  ref <- .n_painted(b)
  expect_gt(ref, 0L)
  cases <- list(
    b |> scale_x_reverse(),
    b |> scale_y_reverse(),
    b |> coord_flip() |> scale_x_reverse(),
    b |> coord_flip() |> scale_y_reverse(),
    b |> xlim(5, 0),
    vplot(dd) |>
      mark_bar(x = x, y = y, fill = "#123456") |>
      coord_flip() |>
      scale_y_reverse()
  )
  for (p in cases) {
    expect_equal(.n_painted(p), ref)
  }
})

test_that("histograms, tiles and boxplots draw on a reversed axis (VP-89)", {
  d <- data.frame(g = rep(1:3, each = 10), v = c(1:10, 3:12, 5:14))
  pairs <- list(
    list(
      vplot(d) |> mark_histogram(x = v, bins = 5, fill = "#123456"),
      scale_x_reverse
    ),
    list(vplot(d) |> mark_tile(x = g, y = v, fill = "#123456"), scale_y_reverse),
    list(
      vplot(d) |> mark_boxplot(x = factor(g), y = v, fill = "#123456"),
      scale_y_reverse
    )
  )
  for (pr in pairs) {
    n <- .n_painted(pr[[1]])
    expect_gt(n, 0L)
    expect_equal(.n_painted(pr[[2]](pr[[1]])), n)
  }
})
