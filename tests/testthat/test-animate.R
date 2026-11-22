# Keyframe animation grammar: transition_states() / ease_aes() / animate() /
# anim_save(), plus the frozen-scale (non-reactive) guardrail.

# mtcars by cyl draws a different number of points per state, so animate()
# warns that the states cut rather than tween; these tests are about other things.
quiet_animate <- function(...) {
  withCallingHandlers(
    animate(...),
    vellumplot_anim_cut = function(w) invokeRestart("muffleWarning")
  )
}

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

test_that("ease_aes() takes per-class easings and validates each", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  e <- ease_aes(p, "cubic-in-out", color = "linear", size = "sine-out")@ease
  expect_equal(e@default, "cubic-in-out")
  expect_equal(e@color, "linear")
  expect_equal(e@size, "sine-out")
  # Unset classes stay NA and resolve to the default.
  expect_true(is.na(e@position))
  expect_true(is.na(e@alpha))
  cls <- vellumplot:::.ease_classes(e)
  expect_equal(cls$position, "cubic-in-out")
  expect_equal(cls$alpha, "cubic-in-out")
  expect_equal(cls$color, "linear")
  expect_error(ease_aes(p, "linear", size = "wobble"), "Unknown easing")
})

test_that("ease_aes() accepts colour as an alias but not both spellings", {
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_equal(ease_aes(p, colour = "sine-in")@ease@color, "sine-in")
  expect_error(ease_aes(p, color = "linear", colour = "linear"), "not both")
})

test_that(".ease_classes() falls back to the default for every class", {
  cls <- vellumplot:::.ease_classes(EaseSpec(default = "bounce-out"))
  expect_equal(unname(unlist(cls)), rep("bounce-out", 4))
  # A NULL spec is the un-eased plot: linear everywhere.
  expect_equal(
    unname(unlist(vellumplot:::.ease_classes(NULL))),
    rep("linear", 4)
  )
})

test_that(".anim_schedule emits one fraction vector per class", {
  # A single easing name (the pre-per-aesthetic call form) must give four
  # identical schedules -- that is what keeps existing animations unchanged.
  one <- vellumplot:::.anim_schedule(
    3,
    30,
    "cubic-in-out",
    c(1, 1),
    1,
    wrap = FALSE
  )
  expect_equal(one$frac_col, one$frac)
  expect_equal(one$frac_size, one$frac)
  expect_equal(one$frac_alpha, one$frac)

  # Split curves: position eased, everything else linear. Same segments and same
  # endpoints, different interiors.
  split <- vellumplot:::.anim_schedule(
    3,
    30,
    list(
      position = "cubic-in-out",
      color = "linear",
      size = "linear",
      alpha = "linear"
    ),
    c(1, 1),
    1,
    wrap = FALSE
  )
  expect_equal(split$seg, one$seg)
  expect_equal(split$frac, one$frac) # position keeps the cubic curve
  expect_false(isTRUE(all.equal(split$frac_col, split$frac)))
  expect_equal(range(split$frac_col), c(0, 1))
  # A cubic-in-out lags a linear curve over the first half of a transition, so
  # the difference has a direction, not just a magnitude.
  mid <- split$frac_col > 0 & split$frac_col < 0.5
  expect_true(all(split$frac[mid] <= split$frac_col[mid] + 1e-9))
})

test_that("animate() carries per-class easings onto the animation", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    transition_states(cyl) |>
    ease_aes("cubic-in-out", alpha = "linear")
  a <- quiet_animate(p, nframes = 6, fps = 10)
  expect_equal(vellumplot:::.anim_easings(a)$position, "cubic-in-out")
  expect_equal(vellumplot:::.anim_easings(a)$alpha, "linear")
})

test_that("an animation with no easing_aes resolves to a single curve", {
  # `easing_aes` is empty on a hand-built animation (and on any object made
  # before per-class easing existed); every class falls back to `easing`.
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    transition_states(cyl)
  a <- quiet_animate(p, nframes = 4, fps = 10)
  a@easing <- "sine-in"
  a@easing_aes <- list()
  expect_equal(unname(unlist(vellumplot:::.anim_easings(a))), rep("sine-in", 4))
})

test_that("a per-class easing changes the rendered frames", {
  # End to end, through the engine. The fixture matters: every state holds the
  # same four bars, so the marks *match* across keyframes and genuinely tween.
  # (With a differing row set per state nothing matches, the tween only snaps at
  # the halfway point, and every monotone curve crosses that at the same frame --
  # so the output would be identical no matter how it was eased.)
  d <- data.frame(
    g = rep(letters[1:4], 3),
    t = rep(1:3, each = 4),
    v = c(1, 3, 2, 4, 4, 1, 3, 2, 2, 4, 1, 3)
  )
  base <- vplot(d) |>
    mark_bar(x = g, y = v, fill = v) |>
    transition_states(t, state_length = 0)
  write_gif <- function(p) {
    f <- withr::local_tempfile(fileext = ".gif", .local_envir = parent.frame())
    anim_save(f, animate(p, nframes = 20, fps = 10))
    readBin(f, "raw", 5e6)
  }
  uniform <- write_gif(ease_aes(base, "cubic-in-out"))
  # Bar heights are positional and the fill is a continuous colour, so each of
  # these two classes moves independently of the other.
  expect_false(identical(
    uniform,
    write_gif(ease_aes(base, "cubic-in-out", position = "linear"))
  ))
  expect_false(identical(
    uniform,
    write_gif(ease_aes(base, "cubic-in-out", color = "linear"))
  ))
  # Back-compat: spelling out the same curve for all four classes must render
  # byte-identically to the single-curve form.
  expect_identical(
    uniform,
    write_gif(ease_aes(
      base,
      "cubic-in-out",
      position = "cubic-in-out",
      color = "cubic-in-out",
      size = "cubic-in-out",
      alpha = "cubic-in-out"
    ))
  )
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

test_that("transition_time needs a numeric, Date or POSIXct column", {
  d <- data.frame(g = rep(letters[1:2], each = 2), x = 1:4, y = 1:4)
  p <- vplot(d) |> mark_point(x = x, y = y) |> transition_time(g)
  expect_error(animate(p), "numeric, Date or POSIXct time column")
})

test_that("animate() compiles one keyframe per state", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl)) |>
    transition_states(cyl)
  a <- quiet_animate(p, nframes = 12, fps = 10)
  expect_true(S7::S7_inherits(a, vellumplot:::vellum_animation))
  expect_equal(a@states, c("4", "6", "8"))
  expect_length(a@scenes, 3L)
})

test_that("animate() rejects specs it cannot animate", {
  no_tr <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  expect_error(animate(no_tr), "transition_states")

  # (a faceted plot animates since VP-84; per-panel scales are refused, below)

  # a genuinely single-state column
  d1 <- transform(mtcars, only = 1)
  single <- vplot(d1) |> mark_point(x = wt, y = mpg) |> transition_states(only)
  expect_error(animate(single), "at least 2 states")
})

test_that("scales are frozen across keyframes (the non-reactive guardrail)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    transition_states(cyl)
  a <- quiet_animate(p, nframes = 6)

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
  # keyed per layer ("l<layer>|"), so two layers never match each other's marks
  expect_setequal(keys(a@scenes[[1]]), c("l1|a", "l1|b"))
  expect_setequal(keys(a@scenes[[2]]), c("l1|a", "l1|b", "l1|z")) # z enters

  skip_if_not_installed("magick")
  gif <- withr::local_tempfile(fileext = ".gif")
  anim_save(gif, a)
  expect_true(file.exists(gif))
})

# Elements of a keyframe scene, by mark kind (scene model).
reveal_elems <- function(scene) {
  vellum::scene_model(vellum::as_vellum_scene(scene))$elements
}

test_that("transition_reveal() reveals the rows along `along`, frame by frame (VP-81)", {
  d <- data.frame(t = 1:10, y = (1:10)^2)
  p <- vplot(d) |>
    mark_line(x = t, y = y) |>
    mark_point(x = t, y = y) |>
    transition_reveal(t)
  expect_equal(p@transition@kind, "reveal")
  a <- animate(p, nframes = 7)
  # One keyframe per frame, and the schedule lands exactly on each of them.
  expect_length(a@scenes, 7L)
  sch <- vellumplot:::.anim_schedule(
    7,
    a@nframes,
    vellumplot:::.anim_easings(a),
    a@seg_weights,
    a@state_length,
    a@wrap
  )
  expect_equal(sch$seg, c(1:6, 6))
  expect_equal(sch$frac, c(rep(0, 6), 1))
  # Cutoffs 1, 2.5, 4, ..., 10: a point appears once it is reached.
  npts <- vapply(a@scenes, function(s) sum(reveal_elems(s)$mark == "point"), integer(1))
  expect_equal(npts, as.integer(floor(seq(1, 10, length.out = 7))))
  # The background and gridlines are drawn whole on every frame.
  grid <- function(s) {
    e <- reveal_elems(s)
    sum(e$mark %in% c("rect", "segment") & !is.na(e$panel))
  }
  g <- vapply(a@scenes, grid, integer(1))
  expect_true(all(g == g[7]))
})

test_that("a revealed line ends on an interpolated leading point (VP-81)", {
  d <- data.frame(t = c(0, 10), x = c(0, 100), y = c(0, 50))
  r <- vellumplot:::.reveal_rows(d, d$t, 2.5, lead = c("", ""))
  expect_equal(nrow(r), 2L)
  expect_equal(r$t, c(0, 2.5))
  expect_equal(r$x, c(0, 25))
  expect_equal(r$y, c(0, 12.5))
  # Without `lead` (points), only the rows reached.
  expect_equal(nrow(vellumplot:::.reveal_rows(d, d$t, 2.5)), 1L)
  # Groups interpolate separately and each lead sits after its group's rows.
  d2 <- data.frame(t = c(1, 2, 3, 1, 2, 3), y = c(1, 2, 3, 10, 20, 30), g = rep(c("a", "b"), each = 3))
  r2 <- vellumplot:::.reveal_rows(d2, d2$t, 1.5, lead = d2$g)
  expect_equal(r2$g, c("a", "a", "b", "b"))
  expect_equal(r2$y, c(1, 1.5, 10, 15))
  # A Date column keeps its class.
  d3 <- data.frame(t = as.Date("2020-01-01") + c(0, 10), y = c(0, 1))
  r3 <- vellumplot:::.reveal_rows(d3, as.double(d3$t), as.double(as.Date("2020-01-06")), lead = c("", ""))
  expect_s3_class(r3$t, "Date")
  expect_equal(r3$t[2], as.Date("2020-01-06"))
})

test_that("transition_reveal() follows `along`, not the x axis (VP-81)", {
  # A spiral revealed along its parameter: the drawn ink grows outwards from
  # the centre instead of wiping in from the left edge.
  skip_if_not_installed("png")
  tt <- seq(0, 4 * pi, length.out = 100)
  sp <- data.frame(t = tt, x = tt * cos(tt), y = tt * sin(tt))
  a <- vplot(sp) |> mark_path(x = x, y = y, linewidth = 2) |> transition_reveal(t) |> animate(nframes = 5)
  ink_cols <- function(s) {
    f <- withr::local_tempfile(fileext = ".png")
    vellum::render(vellum::as_vellum_scene(s), f)
    arr <- png::readPNG(f)
    e <- reveal_elems(s)
    pr <- e[e$mark == "rect" & e$panel == "panel-1-1", ][1, ]
    cols <- seq(ceiling(pr$x0) + 2, floor(pr$x1) - 2)
    rows <- seq(ceiling(pr$y0) + 2, floor(pr$y1) - 2)
    cols[range(which(colSums(arr[rows, cols, 1] < 0.3) > 0))]
  }
  ext <- lapply(a@scenes[2:5], ink_cols)
  width <- vapply(ext, diff, numeric(1))
  expect_true(all(diff(width) > 0))
  left <- vapply(ext, `[`, numeric(1), 1)
  # The left edge never moves inwards and ends further out than it started.
  # Not strictly per frame: between t = pi and 2 * pi the spiral's leftmost
  # point stays at x = -pi, so two frames can share their left edge.
  expect_true(all(diff(left) <= 0))
  expect_lt(left[length(left)], left[1])
  # The middle frame holds only the rows with t <= cutoff: drawn by the path
  # data, so its extent is that of the inner turns.
  spec_mid <- vellumplot:::.reveal_rows(sp, sp$t, 2 * pi, lead = rep("", 100))
  expect_true(all(spec_mid$t <= 2 * pi + 1e-12))
  expect_equal(max(spec_mid$t), 2 * pi)
})

test_that("transition_reveal() works under coord_flip() and a reversed scale (VP-81)", {
  d <- data.frame(t = 1:20, v = sin(1:20))
  for (p in list(
    vplot(d) |> mark_line(x = t, y = v) |> mark_point(x = t, y = v) |> coord_flip(),
    vplot(d) |> mark_line(x = t, y = v) |> mark_point(x = t, y = v) |> scale_x_reverse()
  )) {
    a <- p |> transition_reveal(t) |> animate(nframes = 5)
    npts <- vapply(a@scenes, function(s) sum(reveal_elems(s)$mark == "point"), integer(1))
    expect_equal(npts, as.integer(floor(seq(1, 20, length.out = 5))))
  }
})

test_that("transition_reveal() keeps a layer without `along` whole and rejects a discrete along", {
  d <- data.frame(t = 1:10, y = 1:10)
  ref <- data.frame(h = 5)
  a <- vplot(d) |>
    mark_point(x = t, y = y) |>
    mark_rule(yintercept = h, data = ref) |>
    transition_reveal(t) |>
    animate(nframes = 3)
  expect_length(a@scenes, 3L)
  d$s <- letters[1:10]
  expect_error(
    vplot(d) |> mark_point(x = t, y = y) |> transition_reveal(s) |> animate(nframes = 3),
    "numeric, Date or POSIXct"
  )
})

test_that("a revealed animation renders its frames", {
  skip_if_not_installed("png")
  d <- data.frame(t = 1:30, y = sin(seq(0, 6, length.out = 30)))
  a <- vplot(d) |> mark_line(x = t, y = y) |> transition_reveal(t) |> animate(nframes = 6)
  dir <- withr::local_tempdir()
  vellumplot:::.anim_render(a, dir, "frames")
  files <- sort(list.files(dir, pattern = "\\.png$", full.names = TRUE))
  expect_length(files, 6L)
  right_ink <- function(f) {
    arr <- png::readPNG(f)
    w <- dim(arr)[2]
    sum(arr[, (w / 2 + 1):w, 1] < 0.5)
  }
  ri <- vapply(files, right_ink, numeric(1))
  expect_lt(ri[1], ri[6])
})

test_that("anim_save() writes a GIF and an APNG", {
  skip_if_not_installed("magick")
  a <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    transition_states(cyl) |>
    quiet_animate(nframes = 15, fps = 10)

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
    quiet_animate(nframes = 12, fps = 12)

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
    quiet_animate(nframes = 6)
  expect_error(
    anim_save(withr::local_tempfile(fileext = ".mp4"), a),
    "\\.gif.*\\.png.*\\.svg"
  )
  expect_error(anim_save("x.gif", mtcars), "vellum_animation")
})

test_that("anim_save() writes .apng and explains video/WebP (VP-85)", {
  a <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    transition_states(cyl) |>
    quiet_animate(nframes = 4)
  f <- withr::local_tempfile(fileext = ".apng")
  anim_save(f, a)
  raw <- readBin(f, "raw", file.size(f))
  expect_identical(raw[1:8], as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
  expect_true(length(grepRaw("acTL", raw)) > 0) # an animated PNG
  # Upper-case extensions work too.
  f2 <- withr::local_tempfile(fileext = ".APNG")
  expect_no_error(anim_save(f2, a))

  err <- function(ext) {
    tryCatch(
      anim_save(withr::local_tempfile(fileext = ext), a),
      error = function(e) conditionMessage(e)
    )
  }
  expect_match(err(".mp4"), "no video encoder")
  expect_match(err(".mp4"), "ffmpeg")
  expect_match(err(".webm"), "no video encoder")
  expect_match(err(".webp"), "no WebP encoder")
  expect_match(err(".webp"), "magick")
  expect_match(err(".tiff"), "cannot write")
})

test_that("render_plot() on an animation points at anim_save() (VP-85)", {
  a <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    transition_states(cyl) |>
    quiet_animate(nframes = 4)
  expect_error(
    render_plot(a, withr::local_tempfile(fileext = ".png")),
    "anim_save"
  )
})

test_that("a layer's own data is filtered to each keyframe's state", {
  d <- data.frame(
    x = rep(1:4, 3),
    y = rep(1:4, 3),
    s = rep(c("A", "B", "C"), each = 4)
  )
  ann <- data.frame(x = 2, y = 2, s = c("A", "B", "C"), lab = c("a", "b", "c"))
  p <- vplot(d) |>
    mark_point(x = x, y = y) |>
    mark_text(x = x, y = y, label = lab, data = ann) |>
    transition_states(s)
  a <- animate(p, nframes = 6, fps = 10)
  expect_length(a@scenes, 3L)
  # the annotation layer keeps one row per keyframe, not all three at once
  tr <- p@transition
  for (lev in c("A", "B", "C")) {
    expect_equal(sum(vellumplot:::.layer_state_key(tr, ann) == lev), 1L)
  }
  # a layer whose data lacks the state column persists whole (NULL key)
  expect_null(vellumplot:::.layer_state_key(tr, data.frame(x = 1, y = 1)))
})

# ---- frame variables (VP-82) and Date/POSIXct times (VP-83) -----------------

# The title text of every frame of an animated SVG, in frame order.
svg_frame_titles <- function(a, pattern) {
  f <- withr::local_tempfile(fileext = ".svg")
  suppressMessages(anim_save(f, a))
  doc <- xml2::read_xml(f)
  frames <- xml2::xml_find_all(doc, "/*[local-name()='svg']/*[local-name()='g']")
  vapply(frames, function(g) {
    tx <- xml2::xml_text(xml2::xml_find_all(g, ".//*[local-name()='text']"))
    hit <- grep(pattern, tx, value = TRUE)
    if (length(hit)) hit[[1]] else NA_character_
  }, character(1))
}

test_that("frame variables are substituted glue-style (VP-82)", {
  g <- vellumplot:::.glue_one
  e <- new.env()
  e$k <- 10
  expect_identical(g("Year {round(frame_time)}", list(frame_time = 1952.6), e), "Year 1953")
  expect_identical(g("{{literal}} {frame} of {nframes}", list(frame = 2L, nframes = 9), e), "{literal} 2 of 9")
  expect_identical(g("{frame * k}", list(frame = 3L), e), "30") # caller env behind the vars
  expect_identical(g("no braces", list(), e), "no braces")
  expect_error(g("{nope}", list(), e), "animation label")
})

test_that("a {frame_time} title updates on every frame, not only at keyframes (VP-82)", {
  skip_if_not_installed("xml2")
  d <- data.frame(t = rep(c(0, 10, 20), each = 2), x = rep(1:2, 3), y = c(1, 2, 2, 3, 3, 1))
  a <- vplot(d) |>
    mark_point(x = x, y = y) |>
    labs(title = "t = {round(frame_time, 1)}") |>
    transition_time(t) |>
    animate(nframes = 9)
  expect_named(a@frame_text$bands, "title")
  got <- svg_frame_titles(a, "^t = ")
  # 9 frames evenly over 0..20: every frame shows its own time.
  expect_identical(got, paste("t =", round(seq(0, 20, length.out = 9), 1)))
})

test_that("state frame variables follow the schedule (VP-82)", {
  d <- data.frame(s = rep(c("a", "b", "c"), each = 2), x = rep(1:2, 3), y = 1:6)
  a <- vplot(d) |>
    mark_point(x = x, y = y) |>
    labs(title = "{closest_state}", subtitle = "{previous_state}>{next_state}") |>
    transition_states(s, wrap = TRUE) |>
    animate(nframes = 12)
  sch <- vellumplot:::.anim_schedule(3, 12, "linear", a@seg_weights, a@state_length, a@wrap)
  v <- vellumplot:::.schedule_vars(a@frame_text, sch)
  closest <- vapply(v, `[[`, "", "closest_state")
  prev <- vapply(v, `[[`, "", "previous_state")
  nxt <- vapply(v, `[[`, "", "next_state")
  moving <- vapply(v, `[[`, NA, "transitioning")
  # Held frames: all three equal; moving: from -> to, closest flips at 0.5.
  expect_true(all(prev[!moving] == nxt[!moving]))
  expect_true(all(ifelse(sch$frac[moving] < 0.5, closest[moving] == prev[moving], closest[moving] == nxt[moving])))
  # The wrap's last move goes back to the first state.
  last_move <- max(which(moving))
  expect_identical(c(prev[last_move], nxt[last_move]), c("c", "a"))
  expect_identical(vapply(v, `[[`, 0, "progress"), (1:12) / 12)

  skip_if_not_installed("xml2")
  got <- svg_frame_titles(a, "^[abc]$")
  expect_identical(got, closest)
})

test_that("transition_time() accepts Date and POSIXct times (VP-83)", {
  d <- data.frame(t = rep(as.Date("2020-01-01") + c(0, 31, 59), each = 3), x = rep(1:3, 3), y = c(1, 2, 3, 2, 3, 4, 3, 4, 5))
  a <- vplot(d) |>
    mark_point(x = x, y = y) |>
    labs(title = "{format(frame_time, '%Y-%m-%d')}") |>
    transition_time(t) |>
    animate(nframes = 10)
  expect_length(a@scenes, 3L)
  expect_identical(a@states, as.character(as.double(as.Date("2020-01-01") + c(0, 31, 59))))
  # Segments weighted by the day gaps, as for a numeric time.
  expect_equal(a@seg_weights, c(31, 28))
  sch <- vellumplot:::.anim_schedule(3, 10, "linear", a@seg_weights, 0, FALSE)
  v <- vellumplot:::.schedule_vars(a@frame_text, sch)
  ft <- lapply(v, `[[`, "frame_time")
  expect_s3_class(ft[[1]], "Date")
  expect_equal(ft[[1]], as.Date("2020-01-01"))
  expect_equal(ft[[10]], as.Date("2020-02-29"))

  d$p <- as.POSIXct(d$t, tz = "UTC") + 3600
  a2 <- vplot(d) |>
    mark_point(x = x, y = y) |>
    labs(title = "{frame_time}") |>
    transition_time(p) |>
    animate(nframes = 5)
  v2 <- vellumplot:::.schedule_vars(
    a2@frame_text,
    vellumplot:::.anim_schedule(3, 5, "linear", a2@seg_weights, 0, FALSE)
  )
  expect_s3_class(v2[[1]]$frame_time, "POSIXct")
  expect_identical(attr(v2[[1]]$frame_time, "tzone"), "UTC")
  expect_equal(v2[[5]]$frame_time, as.POSIXct("2020-02-29 01:00:00", tz = "UTC"))

  d$f <- factor(d$t)
  expect_error(
    vplot(d) |> mark_point(x = x, y = y) |> transition_time(f) |> animate(),
    "numeric, Date or POSIXct"
  )
})

test_that("a reveal's {frame_along} label is compiled into each frame (VP-82)", {
  d <- data.frame(t = 1:9, y = 1:9)
  a <- vplot(d) |>
    mark_line(x = t, y = y) |>
    labs(title = "along {frame_along} ({frame}/{nframes})") |>
    transition_reveal(t) |>
    animate(nframes = 5)
  f <- withr::local_tempfile(fileext = ".svg")
  vellum::render(vellum::as_vellum_scene(a@scenes[[3]]), f)
  expect_true(any(grepl("along 5 (3/5)", readLines(f, warn = FALSE), fixed = TRUE)))
})

test_that("a plot without frame variables keeps no frame text", {
  d <- data.frame(s = rep(1:2, each = 2), x = 1:4, y = 1:4)
  a <- vplot(d) |> mark_point(x = x, y = y) |> labs(title = "Plain") |> transition_states(s) |> animate(nframes = 4)
  expect_length(a@frame_text, 0L)
  sch <- vellumplot:::.anim_schedule(2, 4, "linear", a@seg_weights, a@state_length, a@wrap)
  expect_null(vellumplot:::.frame_text(a, sch))
})

# ---- facets (VP-84) ---------------------------------------------------------

test_that("a faceted animation keeps every panel in every frame (VP-84)", {
  d <- data.frame(
    s = rep(c("A", "B", "C"), each = 4),
    p = c("x", "x", "y", "y", "x", "x", "x", "x", "y", "y", "y", "x"),
    id = rep(1:4, 3),
    v = c(1, 2, 3, 4, 2, 3, 4, 1, 4, 3, 2, 1)
  )
  a <- expect_no_warning(
    vplot(d) |>
      mark_point(x = id, y = v, data_id = id) |>
      facet_wrap(~p) |>
      transition_states(s, wrap = FALSE) |>
      animate(nframes = 12)
  )
  # State B has no "y" rows, but its keyframe still lays out both panels.
  panels <- lapply(a@scenes, function(sc) sort(unique(reveal_elems(sc)$panel)))
  expect_identical(panels[[2]], panels[[1]])
  expect_identical(panels[[3]], panels[[1]])
  # Every adjacent pair tweens (same layout), and the identity includes the
  # panel: point 3 leaves panel y and enters panel x between A and B.
  pr <- vellum::vl_tween_pairing(a@scenes)
  expect_true(all(pr$paired))
  expect_equal(pr$exited[1], 2L) # points 3 and 4 leave panel y ...
  expect_equal(pr$entered[1], 2L) # ... and enter panel x
  keys <- vellum::scene_display_list(vellum::as_vellum_scene(a@scenes[[1]]))$keys
  keys <- keys[!is.na(keys) & nzchar(keys)]
  expect_true(all(grepl("^panel-1-[12]\\|l1\\|", keys)))
  # Mid-transition both copies of point 3 are drawn, fading.
  fr <- vellum:::.tween_frame(a@scenes[[1]], a@scenes[[2]], 0.5)$element_table()
  expect_equal(sum(grepl("\\|l1\\|3$", fr$key)), 2L)
})

test_that("a faceted gapminder-style animation tweens without cuts (VP-84)", {
  skip_if_not_installed("gapminder")
  g <- as.data.frame(gapminder::gapminder)
  g <- g[g$year %in% c(1952, 1957, 1962), ]
  a <- expect_no_warning(
    vplot(g) |>
      mark_point(x = gdpPercap, y = lifeExp, color = continent, data_id = country) |>
      facet_wrap(~continent) |>
      transition_time(year) |>
      animate(nframes = 6)
  )
  pr <- vellum::vl_tween_pairing(a@scenes)
  expect_true(all(pr$paired))
  expect_true(all(pr$exited == 0L & pr$entered == 0L))
})

test_that("animate() refuses per-panel scales with a clear message (VP-84)", {
  d <- data.frame(s = rep(1:2, each = 4), p = rep(c("x", "y"), 4), x = 1:8, y = 8:1)
  for (sc in c("free", "free_x", "free_y")) {
    expect_error(
      vplot(d) |> mark_point(x = x, y = y) |> facet_wrap(~p, scales = sc) |> transition_states(s) |> animate(),
      "per-panel scales"
    )
  }
})

test_that("a faceted reveal reveals every panel along `along` (VP-84)", {
  d <- data.frame(t = rep(1:10, 2), y = c(1:10, 10:1), g = rep(c("a", "b"), each = 10))
  a <- vplot(d) |> mark_point(x = t, y = y) |> facet_wrap(~g) |> transition_reveal(t) |> animate(nframes = 4)
  npts <- vapply(a@scenes, function(s) sum(reveal_elems(s)$mark == "point"), integer(1))
  expect_equal(npts, 2L * c(1L, 4L, 7L, 10L))
})
