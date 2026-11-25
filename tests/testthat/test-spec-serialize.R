# Phase A: the spec serializer (as_spec / from_spec / JSON bridge).

# The encoding-level part of a spec (everything but the data payload, whose hash
# is sensitive to trivial type/rowname differences on reconstruction).
enc_part <- function(spec) spec[setdiff(names(spec), "data")]

test_that("a basic plot round-trips structurally (idempotent IR)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = cyl, size = 3) |>
    labs(title = "T", x = "Weight")
  s1 <- as_spec(p)
  p2 <- from_spec(s1)
  s2 <- as_spec(p2)
  expect_identical(enc_part(s1), enc_part(s2))
})

test_that("reconstructed plot compiles to a scene", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    mark_smooth(x = wt, y = mpg)
  p2 <- from_spec(as_spec(p))
  expect_no_error(vellum::as_vellum_scene(p2))
})

test_that("layers preserve mark, stat, params, position, encoding", {
  p <- vplot(mtcars) |>
    mark_histogram(x = mpg, bins = 20L) |>
    mark_bar(x = gear, position = "dodge")
  s <- as_spec(p)
  expect_identical(s$layers[[1]]$mark, "bar")
  expect_identical(s$layers[[1]]$stat, "bin")
  # `bins` is tagged in the IR so its integer type survives JSON (see
  # test-review4-batch4.R); the guarantee is that the round-trip restores it.
  expect_identical(from_spec(s)@layers[[1]]@stat_params$bins, 20L)
  expect_identical(s$layers[[2]]$position, "dodge")
  expect_identical(names(s$layers[[1]]$encoding), "x")
  expect_identical(s$layers[[1]]$encoding$x$field, "mpg")
})

test_that("expression channels and after_stat round-trip", {
  p <- vplot(mtcars) |> mark_point(x = log(wt), y = mpg, color = factor(cyl))
  s <- as_spec(p)
  expect_identical(s$layers[[1]]$encoding$x$expr, "log(wt)")
  expect_identical(s$layers[[1]]$encoding$color$expr, "factor(cyl)")
  expect_null(s$layers[[1]]$encoding$x$field)
  p2 <- from_spec(s)
  expect_identical(as_spec(p2)$layers[[1]]$encoding$color$expr, "factor(cyl)")
})

test_that("scales round-trip (continuous, manual, gradient2)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = hp) |>
    scale_x_continuous(trans = "log10", limits = c(1, 6)) |>
    scale_color_gradient2(midpoint = 100)
  s1 <- as_spec(p)
  s2 <- as_spec(from_spec(s1))
  expect_identical(enc_part(s1)$scales, enc_part(s2)$scales)
  xsc <- Filter(function(sc) sc$aesthetic == "x", s1$scales)[[1]]
  expect_identical(xsc$trans, "log10")
})

test_that("coord and facet round-trip", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    coord_flip() |>
    facet_wrap(~cyl, ncol = 2L)
  s1 <- as_spec(p)
  s2 <- as_spec(from_spec(s1))
  expect_identical(s1$coord$kind, "flip")
  expect_identical(s1$facet$type, "wrap")
  expect_identical(unlist(s1$facet$cols), "cyl")
  expect_identical(s1$facet, s2$facet)
})

test_that("theme round-trips by preset name plus settings", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme_minimal() |>
    theme(legend.position = "bottom")
  s <- as_spec(p)
  expect_identical(s$theme$preset, "minimal")
  expect_identical(s$theme$settings$legend.position, "bottom")
  p2 <- from_spec(s)
  expect_identical(as_spec(p2)$theme$preset, "minimal")
})

test_that("JSON round-trip is idempotent", {
  skip_if_not_installed("jsonlite")
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = cyl) |>
    facet_wrap(~gear)
  json <- spec_to_json(p)
  expect_type(json, "character")
  p2 <- spec_from_json(json)
  expect_identical(enc_part(as_spec(p)), enc_part(as_spec(p2)))
})

test_that("small data inlines and rebuilds; typed columns survive", {
  df <- data.frame(
    d = as.Date("2020-01-01") + 0:4,
    g = factor(c("a", "b", "a", "b", "a"), levels = c("b", "a")),
    v = 1:5
  )
  p <- vplot(df) |> mark_point(x = d, y = v, color = g)
  s <- as_spec(p)
  expect_false(is.null(s$data$values))
  p2 <- from_spec(s)
  expect_s3_class(p2@data$d, "Date")
  expect_identical(levels(p2@data$g), c("b", "a"))
  expect_identical(p2@data$v, 1:5)
})

test_that("large data stores by reference and needs from_spec(data=)", {
  df <- data.frame(x = 1:3000, y = rnorm(3000))
  p <- vplot(df) |> mark_point(x = x, y = y)
  s <- as_spec(p)
  expect_null(s$data[["values"]])
  expect_true(nzchar(s$data$hash))
  expect_error(from_spec(s), class = "vellumplot_missing_data")
  p2 <- from_spec(s, data = df)
  expect_identical(nrow(p2@data), 3000L)
})

test_that("unserializable pieces are refused with a classed error, not dropped", {
  # a secondary axis carries a transform closure
  p_sec <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    scale_y_continuous(sec.axis = sec_axis(~ . * 2))
  expect_error(as_spec(p_sec), class = "vellumplot_unserializable")

  # per-layer data is not serializable
  p_dat <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, data = head(mtcars))
  expect_error(as_spec(p_dat), class = "vellumplot_unserializable")
})

test_that("effects round-trip", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, effects = list(glow(size = 4)))
  s <- as_spec(p)
  expect_identical(s$layers[[1]]$effects[[1]]$effect, "glow")
  expect_identical(s$layers[[1]]$effects[[1]]$size, 4)
  p2 <- from_spec(s)
  expect_s7_class(p2@layers[[1]]@effects[[1]], GlowSpec)
})

test_that("spec_schema is valid JSON and self-describes", {
  skip_if_not_installed("jsonlite")
  sch <- spec_schema(as = "list")
  expect_identical(sch$title, "vellumplot spec (v1)")
  expect_true("layers" %in% names(sch$properties))
})

# --- REVIEW3 D1: slot-list constants drive both round-trip directions --------

test_that("a polar coord round-trips every non-default slot", {
  p <- vplot(mtcars) |>
    mark_bar(x = factor(cyl)) |>
    coord_radial(
      theta = "y",
      start = 0.5,
      end = 3,
      direction = -1,
      inner_radius = 0.3
    )
  co <- from_spec(as_spec(p))@coord
  expect_identical(co@kind, "polar")
  expect_identical(co@theta, "y")
  expect_equal(co@start, 0.5)
  expect_equal(co@end, 3)
  expect_equal(co@direction, -1)
  expect_equal(co@rmin, 0.3)
})

test_that("a coord_trans round-trips its per-axis transforms", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    coord_trans(x = "log10", y = "sqrt")
  co <- from_spec(as_spec(p))@coord
  expect_identical(co@kind, "trans")
  expect_identical(co@xtrans, "log10")
  expect_identical(co@ytrans, "sqrt")
})

test_that("marginal distributions round-trip with their field types intact", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    add_marginal(
      type = "histogram",
      sides = "t",
      size = 0.2,
      bins = 12L,
      group = TRUE
    )
  m <- from_spec(as_spec(p))@marginal
  expect_identical(m@type, "histogram")
  expect_identical(m@sides, "t")
  expect_equal(m@size, 0.2)
  expect_type(m@bins, "integer")
  expect_identical(m@bins, 12L)
  expect_true(m@group)
  # an unspecified field falls back to the MarginalSpec default, not a NULL
  expect_equal(m@adjust, 1)
})

test_that("interactivity/animation are refused (not silently dropped) by as_spec()", {
  d <- data.frame(x = 1:6, y = 1:6, g = rep(c("a", "b"), 3))
  base <- vplot(d) |> mark_point(x = x, y = y)
  expect_error(
    as_spec(base |> select_point("s")),
    class = "vellumplot_unserializable"
  )
  expect_error(
    as_spec(base |> select_point("s") |> filter_by("s")),
    class = "vellumplot_unserializable"
  )
  expect_error(
    as_spec(base |> transition_states(g)),
    class = "vellumplot_unserializable"
  )
  expect_error(
    as_spec(base |> inspect_source()),
    class = "vellumplot_unserializable"
  )
  # a plain plot still round-trips
  expect_no_error(from_spec(as_spec(base)))
})

test_that("ordered factors and POSIXct time zones survive a round-trip", {
  d <- data.frame(
    v = ordered(c("lo", "hi", "lo"), levels = c("lo", "hi")),
    t = as.POSIXct("2020-06-01 12:00:00", tz = "America/New_York"),
    y = 1:3
  )
  q <- from_spec(as_spec(vplot(d) |> mark_point(x = v, y = y)))
  expect_true(is.ordered(q@data$v)) # ordering preserved (ordinal, not nominal)
  expect_identical(levels(q@data$v), c("lo", "hi"))
  expect_identical(attr(q@data$t, "tzone"), "America/New_York") # display zone kept
  expect_equal(as.numeric(q@data$t), as.numeric(d$t)) # same instant
})

# --- JSON round-trip fidelity ------------------------------------------------
# JSON has no integer type: `jsonlite` reads every whole number back as an
# integer. A decoder that hands that straight to an S7 <double> prop fails (every
# coord's `start = 0` / `direction = 1` / `rmin = 0` did), so sweep a broad set
# of constructors through the real wire format and require the round-tripped
# spec to render the identical SVG.

# The SVG of a plot, with the per-render accessible-description counter ids
# (`vl<n>-d`) normalised so two renders of the same plot compare equal.
rt_svg <- function(p) {
  gsub("vl[0-9]+", "vl", vellum::scene_svg(vellum::as_vellum_scene(p)))
}

test_that("a JSON round-trip renders identically across constructors", {
  skip_if_not_installed("jsonlite")
  pt <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
  cont <- vplot(mtcars) |> mark_point(x = wt, y = mpg, color = hp)
  disc <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, color = factor(cyl), size = hp)
  cases <- list(
    # every coord (whole-number defaults are the regression)
    coord_cartesian = pt |> coord_cartesian(),
    coord_cartesian_lims = pt |>
      coord_cartesian(xlim = c(2, 5), ylim = c(10, 30)),
    coord_flip = pt |> coord_flip(),
    coord_fixed = pt |> coord_fixed(ratio = 1),
    coord_equal = pt |> coord_equal(),
    coord_polar = pt |> coord_polar(),
    coord_polar_y = pt |> coord_polar(theta = "y", start = 1, direction = -1),
    coord_radial = pt |> coord_radial(inner_radius = 0.2),
    coord_radial_arc = pt |> coord_radial(start = 0, end = 3),
    coord_trans = pt |> coord_trans(x = "log10"),
    # position scales, with whole-number limits / breaks / expand
    scale_x = pt |> scale_x_continuous(limits = c(1, 6), breaks = c(2, 4, 6)),
    scale_y = pt |>
      scale_y_continuous(
        limits = c(10, 40),
        breaks = c(10, 20, 30),
        labels = c("a", "b", "c")
      ),
    scale_expand = pt |> scale_x_continuous(expand = c(0, 1)),
    scale_log = pt |> scale_y_continuous(trans = "log10"),
    lims = pt |> lims(x = c(1, 6), y = c(10, 40)),
    scale_binned = pt |> scale_x_binned(n = 4),
    # colour / size / alpha / shape scales
    colour_cont = cont |> scale_color_continuous(limits = c(0, 400)),
    colour_gradient2 = cont |> scale_color_gradient2(midpoint = 150),
    colour_binned = cont |>
      scale_color_binned(n = 4, breaks = c(100, 200, 300)),
    colour_manual = disc |>
      scale_color_manual(values = c("red", "green", "blue")),
    size = disc |> scale_size(range = c(1, 6), limits = c(0, 400)),
    size_area = disc |> scale_size_area(max_size = 8),
    alpha = vplot(mtcars) |>
      mark_point(x = wt, y = mpg, alpha = hp) |>
      scale_alpha(range = c(0, 1)),
    # guides
    guide_none = disc |> guides(color = "none"),
    guide_legend = disc |> guides(color = guide_legend(reverse = TRUE)),
    guide_colourbar = cont |> guides(color = guide_colourbar(n.breaks = 4)),
    # facets
    facet_wrap = pt |> facet_wrap(~cyl, ncol = 2),
    facet_grid = pt |> facet_grid(am ~ gear),
    # theme and labels
    theme_bw = pt |> theme_bw(),
    theme_setting = disc |> theme(legend.position = "bottom"),
    labs = pt |>
      labs(title = "T", subtitle = "S", x = "X", y = "Y", caption = "C"),
    # stats / marks with whole-number params
    histogram = vplot(mtcars) |> mark_histogram(x = mpg, bins = 10),
    histogram_bw = vplot(mtcars) |> mark_histogram(x = mpg, binwidth = 2),
    bin2d = vplot(mtcars) |> mark_bin2d(x = wt, y = mpg, bins = 5),
    density = vplot(mtcars) |> mark_density(x = mpg, adjust = 1),
    boxplot = vplot(mtcars) |> mark_boxplot(x = factor(cyl), y = mpg),
    dodge = vplot(mtcars) |>
      mark_bar(
        x = factor(cyl),
        fill = factor(am),
        position = position_dodge(width = 1)
      ),
    text = vplot(mtcars) |> mark_text(x = wt, y = mpg, label = cyl, size = 3),
    # effects with whole-number <double> slots
    outline = vplot(mtcars) |>
      mark_point(x = wt, y = mpg, effects = list(outline())),
    shadow = vplot(mtcars) |>
      mark_point(x = wt, y = mpg, effects = list(shadow(x = 1, y = -1))),
    motion = vplot(mtcars) |>
      mark_point(x = wt, y = mpg, effects = list(motion())),
    marginal = pt |> add_marginal(type = "density")
  )
  for (nm in names(cases)) {
    p <- cases[[nm]]
    p2 <- expect_no_error(spec_from_json(spec_to_json(p)), message = nm)
    expect_identical(rt_svg(p2), rt_svg(p), label = nm)
  }
})

test_that("a JSON round-trip restores double coord slots", {
  skip_if_not_installed("jsonlite")
  p <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> coord_flip()
  co <- spec_from_json(spec_to_json(p))@coord
  expect_identical(co@kind, "flip")
  expect_type(co@start, "double")
  expect_type(co@direction, "double")
  expect_type(co@rmin, "double")
})

test_that("an untagged whole-number param decodes as double, a tagged one as integer", {
  skip_if_not_installed("jsonlite")
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg, size = 3) |>
    mark_histogram(x = mpg, bins = 20L)
  p2 <- spec_from_json(spec_to_json(p))
  expect_identical(p2@layers[[1]]@params$size, 3)
  expect_identical(p2@layers[[2]]@stat_params$bins, 20L)
})

test_that("deferred theme() tweaks on an unthemed plot serialize (VP-133)", {
  p <- vplot(mtcars) |>
    mark_point(x = wt, y = mpg) |>
    theme(legend.position = "bottom", panel.spacing = 3)
  expect_no_warning(s <- as_spec(p))
  expect_identical(s$theme$preset, "gray")
  expect_identical(s$theme$settings$legend.position, "bottom")
  expect_identical(s$theme$settings$panel.spacing, 3)
  # an element tweak is reported as dropped, as when theme() merged at once
  q <- theme(p, axis.text = element_text(colour = "red"))
  expect_warning(as_spec(q), "axis.text")
  # a no-op tweak of a gray default element is not
  r <- theme(p, axis.text = element_text(colour = "grey20"))
  expect_no_warning(as_spec(r))
})
