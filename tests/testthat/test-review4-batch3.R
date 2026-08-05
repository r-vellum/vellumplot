# Regression tests for the REVIEW4 Batch 3 validation & robustness fixes.

d <- data.frame(x = 1:10, y = 1:10, g = rep(c("a", "b"), 5))
svg <- function(p) paste(plot_svg(p), collapse = "")

# SC2 / SC3 / SC4 -----------------------------------------------------------
test_that("binned scales validate n and style up front", {
  base <- vplot(d) |> mark_point(x = x, y = y)
  expect_error(base |> scale_x_binned(n = -2), "positive integer")
  expect_error(base |> scale_x_binned(n = c(3, 4)), "positive integer")
  expect_error(
    base |> scale_x_binned(style = c("pretty", "equal")),
    "single string"
  )
  expect_no_error(base |> scale_x_binned(n = 5))
})

test_that("a numeric output range rejects a non-finite bound clearly", {
  expect_error(
    vplot(d) |>
      mark_point(x = x, y = y, size = x) |>
      scale_size(range = c(NA, 4)),
    "finite"
  )
})

# GC2 -----------------------------------------------------------------------
test_that("coord scalars reject non-scalar / non-finite values", {
  expect_error(coord_fixed(vplot(d), ratio = c(1, 2)), "single positive finite")
  expect_error(coord_fixed(vplot(d), ratio = -1), "single positive finite")
  expect_error(coord_polar(vplot(d), start = c(0, 1)), "single finite")
  expect_error(coord_radial(vplot(d), end = c(0, 1)), "single finite")
  expect_no_error(coord_radial(vplot(d), start = pi / 2, end = pi))
})

# GC1 / GC5 (behavioural: still render) -------------------------------------
test_that("a discrete axis and a single-value domain still render", {
  expect_no_error(svg(
    vplot(data.frame(g = c("a", "b", "c"), y = c(1, 2, 3))) |>
      mark_bar(x = g, y = y)
  ))
})

# MK2 / MK3 / MK5 / MK6 -----------------------------------------------------
test_that("set_mask rejects a region", {
  expect_error(
    set_mask(vplot(d), region = data.frame(x = 1:3, y = 1:3)),
    "not supported"
  )
})

test_that("pattern constructors reject non-positive dimensions", {
  expect_error(pattern_stripe(spacing = 0), "positive number")
  expect_error(pattern_crosshatch(linewidth = -1), "positive number")
  expect_error(pattern_checker(size = 0), "positive number")
  expect_error(pattern_dot(spacing = -2), "positive number")
  expect_no_error(pattern_stripe(spacing = 4, linewidth = 1))
})

test_that("clip_to rejects an NA-group region vertex", {
  expect_error(
    clip_to(
      vplot(d),
      region = data.frame(x = 1:3, y = 1:3, group = c(1, NA, 1))
    ),
    "missing values"
  )
})

test_that("mark_rug validates sides and a rolling window validates k", {
  expect_error(vplot(d) |> mark_rug(x = x, sides = "z"), "must be a string")
  expect_no_error(vplot(d) |> mark_rug(x = x, sides = "tblr"))
  expect_error(
    vplot(d) |> mark_line(x = x, y = y, window = list(op = "mean", k = -3)),
    "positive integer"
  )
})

# CP4 / CP6 -----------------------------------------------------------------
test_that("an NA facet value drops the row rather than making an NA panel", {
  df <- data.frame(x = 1:6, y = 1:6, f = c("a", "b", "a", "b", NA, "a"))
  fa <- vellumplot:::.facet_assign(
    vplot(df) |> mark_point(x = x, y = y) |> facet_wrap(~f)
  )
  levs <- unlist(lapply(fa$panels, function(p) p$lvl))
  expect_setequal(levs, c("a", "b"))
})

test_that("position stack tolerates an NA height", {
  df <- data.frame(
    x = c("a", "a", "b", "b"),
    y = c(1, NA, 2, 3),
    g = c("p", "q", "p", "q")
  )
  expect_no_error(svg(vplot(df) |> mark_bar(x = x, y = y, fill = g)))
})

# SR2 -----------------------------------------------------------------------
test_that("an element override on a preset warns; a pristine preset does not", {
  over <- vplot(d) |>
    mark_point(x = x, y = y) |>
    theme_minimal() |>
    theme(axis.text = element_text(color = "red"))
  expect_warning(as_spec(over), "Dropped custom theme elements")

  expect_no_warning(as_spec(
    vplot(d) |> mark_point(x = x, y = y) |> theme_minimal()
  ))
})

# SR5 -----------------------------------------------------------------------
test_that("an MCP tools/call with no tool name returns a clean error", {
  res <- vellumplot:::.mcp_call_tool(NULL, list())
  expect_true(isTRUE(res$isError))
})
