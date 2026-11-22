# Mid-transition behaviour of animate() + anim_save(), end to end (VP-80).
# The keyframe tests in test-animate.R only look at the states themselves; these
# render the in-between frames and compare them with the keyframes, which is
# where structural mismatches (VR-26..28) used to hide.

skip_if_not_installed("png")

# Render every frame of `a`, returned as arrays in order.
anim_frames <- function(a) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  vellumplot:::.anim_render(a, dir, "frames")
  lapply(sort(list.files(dir, full.names = TRUE)), png::readPNG)
}

# Keyframe `i` of `a`, rasterised the same way the frames are.
keyframe_png <- function(a, i) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  s <- a@scenes[[i]]
  vellum::vl_render_animation(list(s, s), 1L, 0, dir, format = "frames")
  png::readPNG(list.files(dir, full.names = TRUE)[[1L]])
}

same_png <- function(x, y) isTRUE(all.equal(x, y))

quiet_animate <- function(p, ...) suppressWarnings(animate(p, ...))

test_that("equal states tween: a mid frame is neither keyframe", {
  d <- data.frame(
    s = rep(c("A", "B"), each = 3),
    x = c(1, 2, 3, 3, 2, 1),
    y = 1:3
  )
  a <- vplot(d, width = 3, height = 2, dpi = 50) |>
    mark_point(x = x, y = y) |>
    transition_states(s, wrap = FALSE, state_length = 0)
  expect_no_warning(a <- animate(a, nframes = 5))
  fr <- anim_frames(a)
  expect_true(same_png(fr[[1]], keyframe_png(a, 1)))
  expect_false(same_png(fr[[3]], keyframe_png(a, 1)))
  expect_false(same_png(fr[[3]], keyframe_png(a, 2)))
  expect_true(same_png(fr[[5]], keyframe_png(a, 2)))
})

test_that("a colour group missing from a state cuts at the midpoint and warns", {
  d <- data.frame(
    s = rep(c("A", "B"), each = 6),
    g = c("p", "p", "q", "q", "r", "r", "p", "p", "p", "q", "q", "q"),
    x = c(1:6, 6:1),
    y = c(1:6, 1:6)
  )
  p <- vplot(d, width = 3, height = 2, dpi = 50) |>
    mark_point(x = x, y = y, color = g) |>
    transition_states(s, wrap = FALSE, state_length = 0)
  expect_warning(a <- animate(p, nframes = 9), class = "vellumplot_anim_cut")
  expect_warning(animate(p, nframes = 9), "A -> B")
  fr <- anim_frames(a)
  # Before VR-26 every frame showed state A; state B never appeared.
  expect_true(same_png(fr[[4]], keyframe_png(a, 1)))
  expect_true(same_png(fr[[6]], keyframe_png(a, 2)))
  expect_true(same_png(fr[[9]], keyframe_png(a, 2)))
})

test_that("a colour group appearing later is shown in the final frame", {
  d <- data.frame(
    s = c("A", "A", "B", "B", "B"),
    g = c("p", "p", "p", "p", "q"),
    x = c(1, 2, 1, 2, 3),
    y = c(1, 2, 2, 1, 3)
  )
  a <- vplot(d, width = 3, height = 2, dpi = 50) |>
    mark_point(x = x, y = y, color = g) |>
    transition_states(s, wrap = FALSE, state_length = 0) |>
    quiet_animate(nframes = 6)
  fr <- anim_frames(a)
  expect_true(same_png(fr[[6]], keyframe_png(a, 2)))
})

test_that("a line whose vertex count changes ends on the later line", {
  l <- rbind(
    data.frame(s = "A", x = 1:5, y = c(1, 3, 2, 4, 3)),
    data.frame(s = "B", x = 1:6, y = c(5, 4, 5, 4, 5, 4))
  )
  p <- vplot(l, width = 3, height = 2, dpi = 50) |>
    mark_line(x = x, y = y) |>
    transition_states(s, wrap = FALSE, state_length = 0)
  expect_warning(a <- animate(p, nframes = 9), "change in number")
  fr <- anim_frames(a)
  expect_true(same_png(fr[[4]], keyframe_png(a, 1)))
  # Before VR-27 the last frame drew B's first five vertices only.
  expect_true(same_png(fr[[9]], keyframe_png(a, 2)))
})

test_that("a changing text label reaches the last state's string", {
  tx <- data.frame(
    s = c("first", "second", "third"),
    x = c(1, 2, 3),
    y = c(1, 2, 3)
  )
  a <- vplot(tx, width = 3, height = 2, dpi = 50) |>
    mark_text(x = x, y = y, label = s, size = 8) |>
    transition_states(s, wrap = FALSE) |>
    animate(nframes = 12)
  fr <- anim_frames(a)
  # Before VR-28 the label stayed "second" through the final hold.
  expect_true(same_png(fr[[12]], keyframe_png(a, 3)))
})

test_that("two animated SVGs on one page do not share CSS names", {
  d <- data.frame(
    s = rep(c("A", "B"), each = 3),
    x = c(1, 2, 3, 3, 2, 1),
    y = 1:3
  )
  mk <- function(n, fps) {
    vplot(d, width = 3, height = 2, dpi = 50) |>
      mark_point(x = x, y = y) |>
      transition_states(s, state_length = 0) |>
      animate(nframes = n, fps = fps)
  }
  f1 <- withr::local_tempfile(fileext = ".svg")
  f2 <- withr::local_tempfile(fileext = ".svg")
  anim_save(f1, mk(4, 1))
  anim_save(f2, mk(40, 10))
  kf <- function(f) {
    svg <- paste(readLines(f, warn = FALSE), collapse = "")
    regmatches(svg, regexpr("@keyframes [^{]+", svg))
  }
  expect_false(identical(kf(f1), kf(f2)))
  expect_false(grepl(
    "vfcycle",
    paste(readLines(f1, warn = FALSE), collapse = "")
  ))
})

test_that("fps is written exactly, not rounded to a whole number", {
  d <- data.frame(s = rep(c("A", "B"), each = 2), x = c(1, 2, 2, 1), y = 1:2)
  a <- vplot(d, width = 2, height = 2, dpi = 40) |>
    mark_point(x = x, y = y) |>
    transition_states(s) |>
    animate(nframes = 4, fps = 12.5)
  f <- withr::local_tempfile(fileext = ".png")
  anim_save(f, a)
  raw <- readBin(f, "raw", n = file.size(f))
  pos <- grepRaw("fcTL", raw)
  u16 <- function(i) sum(as.integer(raw[i:(i + 1L)]) * c(256, 1))
  # 12.5 fps = 2/25 s per frame (it used to be written as 1/12).
  expect_identical(c(u16(pos + 24L), u16(pos + 26L)), c(2, 25))
  # A 0.5 fps slideshow: 2 s per frame, 8 s for 4 frames.
  a@fps <- 0.5
  s <- withr::local_tempfile(fileext = ".svg")
  anim_save(s, a)
  expect_match(paste(readLines(s, warn = FALSE), collapse = ""), "vc 8s steps")
})

test_that("nframes must be a positive whole number", {
  p <- vplot(data.frame(s = c(1, 2), x = 1:2, y = 1:2)) |>
    mark_point(x = x, y = y) |>
    transition_states(s)
  expect_error(animate(p, nframes = 2.5), "whole number")
  expect_error(animate(p, nframes = 0), "whole number")
  expect_error(animate(p, nframes = NA), "whole number")
  expect_identical(animate(p, nframes = 3)@nframes, 3)
})

test_that("integer times keep every keyframe's rows (VP-77)", {
  tt <- data.frame(t = rep(c(50000L, 100000L), each = 3), x = 1:6, y = 1:6)
  en <- vellumplot:::.enumerate_states(
    (vplot(tt) |> transition_time(t))@transition,
    tt
  )
  # 100000L used to become "100000", which missed the level "1e+05".
  expect_identical(as.vector(table(en$key)), c(3L, 3L))
  expect_false(anyNA(en$key))
})

test_that("reordered rows tween by identity, not by position (VR-32)", {
  skip(
    "Identity matching across reordered rows is VR-32 (sprint 37): rows are paired by position today."
  )
  # Two points swap row order between states but keep their places: with
  # identity matching nothing should move, so a mid frame equals both keyframes.
  d <- data.frame(
    s = c("A", "A", "B", "B"),
    id = c("a", "b", "b", "a"),
    x = c(1, 2, 2, 1),
    y = c(1, 2, 2, 1)
  )
  a <- vplot(d, width = 3, height = 2, dpi = 50) |>
    mark_point(x = x, y = y) |>
    transition_states(s, wrap = FALSE, state_length = 0) |>
    animate(nframes = 3)
  fr <- anim_frames(a)
  expect_true(same_png(fr[[2]], keyframe_png(a, 1)))
})
