# Keyframe animation grammar: transition_states() / ease_aes() / animate() /
# anim_save(), plus the frozen-scale (non-reactive) guardrail.

# Sorted unique text drawn in a scene's SVG (axis tick labels, titles, ...).
axis_text <- function(scene) {
  svg <- vellum::scene_svg(vellum::as_vellum_scene(scene))
  m <- regmatches(svg, gregexpr(">([^<>]+)</text>", svg))[[1L]]
  sort(unique(gsub(">|</text>", "", m)))
}

test_that("transition_states() records a TransitionSpec", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    transition_states(
      cyl,
      transition_length = 2,
      state_length = 0.5,
      wrap = FALSE
    )
  expect_true(S7::S7_inherits(p@transition, vellumplot:::TransitionSpec))
  expect_equal(p@transition@transition_length, 2)
  expect_equal(p@transition@state_length, 0.5)
  expect_false(p@transition@wrap)
  expect_true(rlang::is_quosure(p@transition@var))
})

test_that("transition_states() validates its arguments", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(transition_states(p, cyl, transition_length = 0), "positive")
  expect_error(transition_states(p, cyl, state_length = -1), "non-negative")
})

test_that("ease_aes() sets and validates the easing", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> ease_aes("cubic-in-out")
  expect_equal(p@ease@default, "cubic-in-out")
  expect_error(ease_aes(p, "wobble"), "Unknown easing")
  expect_error(ease_aes(p, "cubic-sideways"), "Unknown easing")
})

test_that("easing functions hit their endpoints and stay monotone", {
  for (e in c(
    "linear",
    "cubic-in-out",
    "sine-out",
    "elastic-in",
    "bounce-out",
    "back-in-out"
  )) {
    expect_equal(vellumplot:::.ease_apply(0, e), 0, tolerance = 1e-9, info = e)
    expect_equal(vellumplot:::.ease_apply(1, e), 1, tolerance = 1e-9, info = e)
  }
  # linear is the identity; cubic-in-out is symmetric about (0.5, 0.5).
  tt <- seq(0, 1, by = 0.1)
  expect_equal(vellumplot:::.ease_apply(tt, "linear"), tt)
  expect_equal(
    vellumplot:::.ease_apply(0.5, "cubic-in-out"),
    0.5,
    tolerance = 1e-9
  )
})

test_that(".anim_schedule spans the keyframes within [0, 1]", {
  s <- vellumplot:::.anim_schedule(3, 30, "linear", c(1, 1), 1, wrap = FALSE)
  expect_equal(length(s$seg), 30L)
  expect_equal(length(s$seg), length(s$frac))
  expect_true(all(s$seg %in% 1:2)) # K-1 segments, no wrap
  expect_true(all(s$frac >= 0 & s$frac <= 1))
  # wrap adds a segment K -> K+1
  sw <- vellumplot:::.anim_schedule(3, 30, "linear", c(1, 1, 1), 1, wrap = TRUE)
  expect_true(max(sw$seg) == 3L)
  # per-segment weights allocate frames proportionally (no holds)
  wt <- vellumplot:::.anim_schedule(3, 40, "linear", c(1, 3), 0, wrap = FALSE)
  expect_equal(sum(wt$seg == 1L), 10L)
  expect_equal(sum(wt$seg == 2L), 30L)
})

test_that("transition_time weights frames by the time gap", {
  # Integer times (e.g. years) are the common case; the gap weights must still be
  # doubles for the animation object.
  d <- data.frame(
    t = rep(c(0L, 1L, 10L), each = 2),
    x = c(1, 2, 2, 3, 5, 6),
    y = 1:6
  )
  p <- vplot(d) |> mark_point(x = x, y = y) |> transition_time(t)
  expect_equal(p@transition@kind, "time")
  a <- animate(p, nframes = 30)
  expect_equal(a@states, c("0", "1", "10"))
  expect_equal(a@seg_weights, c(1, 9)) # gaps between 0, 1, 10
  expect_type(a@seg_weights, "double")
  expect_false(a@wrap)
})

test_that("transition_time needs a numeric column", {
  d <- data.frame(g = rep(letters[1:2], each = 2), x = 1:4, y = 1:4)
  p <- vplot(d) |> mark_point(x = x, y = y) |> transition_time(g)
  expect_error(animate(p), "numeric time column")
})

test_that("animate() compiles one keyframe per state", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    transition_states(cyl)
  a <- animate(p, nframes = 12, fps = 10)
  expect_true(S7::S7_inherits(a, vellumplot:::vellum_animation))
  expect_equal(a@states, c("4", "6", "8"))
  expect_length(a@scenes, 3L)
})

test_that("animate() rejects specs it cannot animate", {
  no_tr <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(animate(no_tr), "transition_states")

  faceted <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    facet_wrap(~gear) |>
    transition_states(cyl)
  expect_error(animate(faceted), "facet")

  # a genuinely single-state column
  d1 <- transform(mtcars, only = 1)
  single <- vplot(d1) |> mark_point(x = wt, y = mpg) |> transition_states(only)
  expect_error(animate(single), "at least 2 states")
})

test_that("scales are frozen across keyframes (the non-reactive guardrail)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    transition_states(cyl)
  a <- animate(p, nframes = 6)

  # Every keyframe shares one frozen set of axis breaks, even though the per-state
  # data ranges differ wildly (cyl 4 is light, cyl 8 is heavy).
  ref <- axis_text(a@scenes[[1]])
  expect_equal(axis_text(a@scenes[[2]]), ref)
  expect_equal(axis_text(a@scenes[[3]]), ref)

  # Contrast: compiling those states independently would move the breaks.
  i4 <- vplot(mtcars[mtcars$cyl == 4, ]) |> mark_point(x = wt, y = mpg)
  i8 <- vplot(mtcars[mtcars$cyl == 8, ]) |> mark_point(x = wt, y = mpg)
  expect_false(identical(axis_text(i4), axis_text(i8)))
})

test_that("data_id carries element identity through to the keyframes (enter/exit)", {
  # Element "z" exists only in state 2, so it enters; "a"/"b" persist. The Rust
  # tween keys off these to fade z in (per-element enter/exit tested in vellum);
  # here we assert the identity plumbing: keys reach each keyframe scene.
  d <- data.frame(
    st = c(1, 1, 2, 2, 2),
    id = c("a", "b", "a", "b", "z"),
    x = c(0.2, 0.5, 0.2, 0.5, 0.8),
    y = 0.5
  )
  a <- vplot(d) |>
    mark_point(x = x, y = y, data_id = id, size = 8) |>
    transition_states(st, wrap = FALSE) |>
    animate(nframes = 4)

  keys <- function(sc) {
    sort(stats::na.omit(vellum::scene_model(sc)$elements$key))
  }
  expect_setequal(keys(a@scenes[[1]]), c("a", "b"))
  expect_setequal(keys(a@scenes[[2]]), c("a", "b", "z")) # z enters

  skip_if_not_installed("magick")
  gif <- withr::local_tempfile(fileext = ".gif")
  anim_save(gif, a)
  expect_true(file.exists(gif))
})

test_that("transition_reveal() wipes the panel left to right", {
  skip_if_not_installed("png")
  d <- data.frame(t = 1:30, y = sin(seq(0, 6, length.out = 30)))
  p <- vplot(d) |> mark_line(x = t, y = y) |> transition_reveal(t)
  expect_equal(p@transition@kind, "reveal")
  a <- animate(p, nframes = 6)
  expect_length(a@scenes, 2L) # one compile, revealed at 0% and 100%

  dir <- withr::local_tempdir()
  sch <- vellumplot:::.anim_schedule(
    2,
    6,
    "linear",
    a@seg_weights,
    a@state_length,
    a@wrap
  )
  vellum::vl_render_animation(
    a@scenes,
    sch$seg,
    sch$frac,
    dir,
    format = "frames",
    fps = 10
  )
  files <- sort(list.files(dir, pattern = "\\.png$", full.names = TRUE))

  # Ink in the right half of the panel appears only once the wipe reaches it, so
  # it is (near) empty early and fuller late; the left half fills first.
  right_ink <- function(f) {
    arr <- png::readPNG(f)
    w <- dim(arr)[2]
    sum(arr[, (w / 2 + 1):w, 1] < 0.5)
  }
  ri <- vapply(files, right_ink, numeric(1))
  expect_lt(ri[1], ri[length(ri)]) # right side reveals later
  expect_true(ri[length(ri)] > ri[2]) # and keeps growing
})

test_that("anim_save() writes a GIF and an APNG", {
  skip_if_not_installed("magick")
  a <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    transition_states(cyl) |>
    animate(nframes = 15, fps = 10)

  gif <- withr::local_tempfile(fileext = ".gif")
  expect_equal(anim_save(gif, a), gif)
  expect_equal(nrow(magick::image_info(magick::image_read(gif))), 15L)

  apng <- withr::local_tempfile(fileext = ".png")
  anim_save(apng, a)
  expect_true(file.exists(apng))
})

test_that("anim_save() writes an animated SVG", {
  a <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    transition_states(cyl) |>
    animate(nframes = 12, fps = 12)

  svg <- withr::local_tempfile(fileext = ".svg")
  expect_equal(anim_save(svg, a), svg)
  txt <- paste(readLines(svg, warn = FALSE), collapse = "\n")
  # a real, self-contained animated SVG: vector markup, a CSS frame cycle, and a
  # reduced-motion fallback (the accessibility contract WI-11 promises)
  expect_match(txt, "<svg")
  expect_match(txt, "@keyframes")
  expect_match(txt, "prefers-reduced-motion")
})

test_that("anim_save() advises a raster format for a dense .svg scene", {
  # each state (keyframe) carries ~1000 marks, past the advisory threshold
  dense <- data.frame(
    x = runif(2000),
    y = runif(2000),
    g = rep(c("a", "b"), 1000)
  )
  a <- vplot(dense) |>
    mark_point(x = x, y = y) |>
    transition_states(g) |>
    animate(nframes = 4)
  svg <- withr::local_tempfile(fileext = ".svg")
  expect_gt(vellumplot:::.anim_element_count(a), 800L)
  expect_message(anim_save(svg, a), "raster format")
})

test_that("anim_save() validates its inputs", {
  a <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    transition_states(cyl) |>
    animate(nframes = 6)
  expect_error(
    anim_save(withr::local_tempfile(fileext = ".mp4"), a),
    "\\.gif.*\\.png.*\\.svg"
  )
  expect_error(anim_save("x.gif", mtcars), "vellum_animation")
})
