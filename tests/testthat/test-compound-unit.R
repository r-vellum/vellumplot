# Consumers of vellum's compound `native + mm` / `npc + mm` unit (B1):
# device-exact drop shadows and mm label nudges.

# Pull the drawable grobs out of a compiled plot's scene.
.grobs_of <- function(p) {
  sc <- vellum::as_vellum_scene(p)
  root <- vellum:::.materialize(sc)
  walk <- function(node, acc) {
    if (S7::S7_inherits(node, vellum:::gtree)) {
      for (ch in node@children) acc <- walk(ch, acc)
    } else {
      acc <- c(acc, node)
    }
    acc
  }
  walk(root, list())
}

.text_grobs <- function(p) {
  Filter(function(g) S7::S7_inherits(g, vellum:::grob_text), .grobs_of(p))
}

test_that("mark_text(nudge_x/nudge_y) offsets the label by an exact mm amount", {
  df <- data.frame(x = 1, y = 1, lab = "hi")
  g <- .text_grobs(vplot(df) |> mark_text(label = lab, x = x, y = y, nudge_x = 10, nudge_y = -3))[[1]]
  expect_equal(vctrs_field(g@x, "offset"), 10)
  expect_equal(vctrs_field(g@y, "offset"), -3)
  # the base stays the data anchor (native), only the offset carries the nudge
  expect_equal(vctrs_field(g@x, "unit"), 1L) # native
})

test_that("no nudge leaves the label offset at zero (byte-identical)", {
  df <- data.frame(x = 1, y = 1, lab = "hi")
  g <- .text_grobs(vplot(df) |> mark_text(label = lab, x = x, y = y))[[1]]
  expect_equal(vctrs_field(g@x, "offset"), 0)
})

test_that("mark_label(nudge_*) and mark_text(nudge_*) render", {
  df <- data.frame(x = 1:3, y = 1:3, lab = letters[1:3])
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vplot(df) |> mark_text(label = lab, x = x, y = y, nudge_y = 2), f))
  expect_no_error(render_plot(vplot(df) |> mark_label(label = lab, x = x, y = y, nudge_x = 2), f))
})

test_that("shadow() offset is now an absolute mm distance (device-exact)", {
  s <- shadow(x = 2, y = -1)
  expect_equal(s@x, 2)
  expect_equal(s@y, -1)
  # renders through the compound-unit copy viewport
  df <- data.frame(x = 1:20, y = cumsum(rnorm(20)))
  f <- local_tempfile(fileext = ".png")
  expect_no_error(render_plot(vplot(df) |> mark_line(x = x, y = y, effects = list(shadow())), f))
})
