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

# The keyed elements of the frame at `frac` between keyframes `i` and `i + 1`
# (a mid-transition scene, not a keyframe): one row per key, with the device-px
# centre `cx`, `cy` of its box.
mid_elements <- function(a, i = 1L, frac = 0.5) {
  et <- as.data.frame(
    vellum:::.tween_frame(a@scenes[[i]], a@scenes[[i + 1L]], frac)$element_table()
  )
  et$cx <- (et$x0 + et$x1) / 2
  et$cy <- (et$y0 + et$y1) / 2
  et <- et[et$key != "", , drop = FALSE]
  rownames(et) <- et$key
  et
}

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

test_that("a colour group missing from a state fades out instead of cutting (VR-32)", {
  d <- data.frame(
    s = rep(c("A", "B"), each = 6),
    g = c("p", "p", "q", "q", "r", "r", "p", "p", "p", "q", "q", "q"),
    x = c(1:6, 6:1),
    y = c(1:6, 1:6)
  )
  p <- vplot(d, width = 3, height = 2, dpi = 50) |>
    mark_point(x = x, y = y, color = g) |>
    transition_states(s, wrap = FALSE, state_length = 0)
  expect_no_warning(a <- animate(p, nframes = 9))
  pr <- vellum::vl_tween_pairing(a@scenes)
  # group r (2 points) leaves; p and q each gain a third point
  expect_identical(c(pr$paired, pr$snapped, pr$exited, pr$entered), c(1L, 0L, 2L, 2L))
  fr <- anim_frames(a)
  expect_false(same_png(fr[[5]], keyframe_png(a, 1)))
  expect_false(same_png(fr[[5]], keyframe_png(a, 2)))
  expect_true(same_png(fr[[9]], keyframe_png(a, 2)))
  # mid-transition: the leaving r points are still drawn (fading) where they were
  m <- mid_elements(a)
  k1 <- mid_elements(a, frac = 0)
  r <- grep("^l1\\|r", m$key, value = TRUE)
  expect_length(r, 2L)
  expect_equal(m[r, "cx"], k1[r, "cx"])
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
  expect_warning(a <- animate(p, nframes = 9), "number of points")
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
  # Two points swap row order between states but keep their places: matched by
  # their group, nothing moves, so a mid frame equals both keyframes.
  d <- data.frame(
    s = c("A", "A", "B", "B"),
    id = c("a", "b", "b", "a"),
    x = c(1, 2, 2, 1),
    y = c(1, 2, 2, 1)
  )
  for (p in list(
    vplot(d, width = 3, height = 2, dpi = 50) |> mark_point(x = x, y = y, group = id),
    vplot(d, width = 3, height = 2, dpi = 50) |> mark_point(x = x, y = y, data_id = id)
  )) {
    a <- p |>
      transition_states(s, wrap = FALSE, state_length = 0) |>
      animate(nframes = 3)
    fr <- anim_frames(a)
    expect_true(same_png(fr[[2]], keyframe_png(a, 1)))
  }
})

# A small gapminder: 3 continents x 4 countries x 3 years, each country on its
# own path, one colour per continent.
mini_gap <- function() {
  g <- expand.grid(country = sprintf("c%02d", 1:12), year = c(1, 2, 3))
  g$continent <- c("Africa", "Asia", "Europe")[(as.integer(substr(g$country, 2, 3)) - 1) %/% 4 + 1]
  k <- as.integer(substr(g$country, 2, 3))
  g$gdp <- k + g$year * 0.5
  g$life <- (k * 7) %% 13 + g$year
  g
}
gap_anim <- function(g, ..., dpi = 50) {
  vplot(g, width = 4, height = 3, dpi = dpi) |>
    mark_point(x = gdp, y = life, color = continent, ...) |>
    transition_time(year) |>
    animate(nframes = 10)
}
# Each matched key's mid-transition centre is halfway between its keyframe
# centres (segment `i`).
expect_midway <- function(a, keys, i = 1L) {
  m <- mid_elements(a, i)
  k0 <- mid_elements(a, i, 0)
  k1 <- mid_elements(a, i, 1)
  expect_equal(m[keys, "cx"], (k0[keys, "cx"] + k1[keys, "cx"]) / 2, tolerance = 1e-6)
  expect_equal(m[keys, "cy"], (k0[keys, "cy"] + k1[keys, "cy"]) / 2, tolerance = 1e-6)
}
# A point's colour in a scene: the pixel at its centre.
colour_at <- function(a, key, i = 1L, frac = 0.5) {
  s <- vellum:::.tween_frame(a@scenes[[i]], a@scenes[[i + 1L]], frac)
  m <- mid_elements(a, i, frac)
  s$pixel(round(m[key, "cx"]), round(m[key, "cy"]))[1:3]
}

test_that("gapminder with one row dropped: every other country still tweens (VR-32)", {
  g <- mini_gap()
  g2 <- g[!(g$country == "c05" & g$year == 2), ] # an Asian country-year
  expect_no_warning(a <- gap_anim(g2, data_id = country))
  m <- mid_elements(a, 1)
  keys <- setdiff(paste0("l1|", sprintf("c%02d", 1:12)), "l1|c05")
  expect_midway(a, keys, 1)
  expect_midway(a, keys, 2)
  # c05 leaves in segment 1 (fading where it was) and comes back in segment 2.
  expect_equal(m["l1|c05", "cx"], mid_elements(a, 1, 0)["l1|c05", "cx"])
  pr <- vellum::vl_tween_pairing(a@scenes)
  expect_identical(pr$exited, c(1L, 0L))
  expect_identical(pr$entered, c(0L, 1L))
  # colours are the country's own, not a blend with a neighbour's. Sampled at
  # dpi 200 so the centre pixel sits well inside the point on every platform
  # (at dpi 50 a point is ~4 px and its centre can land on an anti-aliased edge).
  hi <- gap_anim(g2, data_id = country, dpi = 200)
  expect_equal(colour_at(hi, "l1|c06"), colour_at(hi, "l1|c06", frac = 0))
  # Without a data_id the other continents are untouched: their rows pair by
  # order within the continent, which a dropped Asian row does not shift.
  b <- gap_anim(g2)
  africa <- paste0("l1|Africa#", 1:4)
  expect_midway(b, africa, 1)
})

test_that("gapminder with a year's rows shuffled tweens as if sorted (VR-32)", {
  g <- mini_gap()
  set.seed(3)
  i2 <- which(g$year == 2)
  g3 <- g
  g3[i2, ] <- g[sample(i2), ]
  a <- gap_anim(g, data_id = country)
  b <- gap_anim(g3, data_id = country)
  for (i in 1:2) {
    ma <- mid_elements(a, i)
    mb <- mid_elements(b, i)
    expect_equal(mb[rownames(ma), c("cx", "cy")], ma[, c("cx", "cy")])
  }
  expect_identical(vellum::vl_tween_pairing(b@scenes)$exited, c(0L, 0L))
})

test_that("a bar race sorted by rank slides its bars and keeps their colours (VR-32)", {
  d <- data.frame(
    year = rep(1:2, each = 4),
    country = c("A", "B", "C", "D", "C", "A", "D", "B"),
    gdp = c(1, 2, 3, 4, 1.5, 2.5, 3.5, 4.5)
  )
  d$rank <- ave(d$gdp, d$year, FUN = rank)
  d <- d[order(d$year, d$rank), ] # rows sorted by rank within year
  expect_no_warning(
    a <- vplot(d, width = 4, height = 3, dpi = 50) |>
      mark_bar(x = rank, y = gdp, fill = country) |>
      mark_text(x = rank, y = gdp, label = country, hjust = 0) |>
      coord_flip() |>
      transition_states(year, wrap = FALSE, state_length = 0) |>
      animate(nframes = 9)
  )
  pr <- vellum::vl_tween_pairing(a@scenes)
  expect_identical(c(pr$snapped, pr$exited, pr$entered), c(0L, 0L, 0L))
  bars <- paste0("l1|", c("A", "B", "C", "D"), "#1")
  expect_midway(a, bars)
  m <- mid_elements(a)
  k0 <- mid_elements(a, frac = 0)
  k1 <- mid_elements(a, frac = 1)
  # C climbs from rank 3 to rank 1: halfway, it sits at rank 2's height ...
  expect_equal(m["l1|C#1", "cy"], k0["l1|B#1", "cy"], tolerance = 1e-6)
  # ... in its own colour all the way.
  expect_equal(colour_at(a, "l1|C#1"), colour_at(a, "l1|C#1", frac = 0))
  expect_equal(colour_at(a, "l1|C#1", frac = 1), colour_at(a, "l1|C#1", frac = 0))
})

test_that("a colour group appearing fades in, and a point changing group slides (VR-32)", {
  d <- data.frame(
    s = c("A", "A", "B", "B", "B"),
    id = c("u", "v", "u", "v", "w"),
    g = c("p", "p", "p", "q", "q"),
    x = c(1, 2, 1, 3, 4),
    y = c(1, 2, 2, 1, 3)
  )
  expect_no_warning(
    a <- vplot(d, width = 3, height = 2, dpi = 50) |>
      mark_point(x = x, y = y, color = g, data_id = id) |>
      transition_states(s, wrap = FALSE, state_length = 0) |>
      animate(nframes = 5)
  )
  pr <- vellum::vl_tween_pairing(a@scenes)
  expect_identical(c(pr$exited, pr$entered), c(0L, 1L))
  # v moves from group p to the new group q: it slides, it does not fade.
  expect_midway(a, c("l1|u", "l1|v"))
  v0 <- colour_at(a, "l1|v", frac = 0)
  v1 <- colour_at(a, "l1|v", frac = 1)
  vm <- colour_at(a, "l1|v")
  expect_false(isTRUE(all.equal(vm, v0)))
  expect_false(isTRUE(all.equal(vm, v1)))
})
