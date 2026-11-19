# ggplot2 looks default scales and guides named by a string up by name along
# the search path, where vellumplot's constructors of the same name mask its
# own. Those constructors hand a call from ggplot2 back to ggplot2 (VP-49).

test_that("ggplot2 plots build with vellumplot's constructors in scope", {
  skip_if_not_installed("ggplot2")
  # The test environment sees vellumplot's exports, as a session with both
  # packages attached does, so ggplot2's lookup finds them here too.
  build <- function(p) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off())
    expect_no_error(ggplot2::ggplot_build(p))
    expect_no_error(ggplot2::ggplotGrob(p))
  }
  gg <- ggplot2::ggplot
  aes <- ggplot2::aes
  build(
    gg(mtcars, aes(wt, mpg, colour = factor(cyl))) +
      ggplot2::geom_point() +
      ggplot2::guides(colour = "legend")
  )
  build(
    gg(mtcars, aes(wt, mpg, colour = hp)) +
      ggplot2::geom_point() +
      ggplot2::scale_colour_continuous(guide = "colourbar")
  )
  build(
    gg(mtcars, aes(wt, mpg, size = qsec, alpha = disp, shape = factor(am))) +
      ggplot2::geom_point()
  )
  build(
    gg(mtcars, aes(factor(cyl), fill = factor(gear))) +
      ggplot2::geom_bar() +
      ggplot2::guides(x = "axis", fill = "none")
  )
  d <- data.frame(
    t = as.Date("2020-01-01") + 0:9,
    v = 1:10,
    o = factor(letters[1:10], ordered = TRUE)
  )
  build(gg(d, aes(t, v, colour = o, linewidth = v, group = 1)) + ggplot2::geom_line())
  # the default scale is ggplot2's own
  b <- ggplot2::ggplot_build(gg(mtcars, aes(wt, mpg)) + ggplot2::geom_point())
  expect_s3_class(b$layout$panel_scales_x[[1]], "ScaleContinuousPosition")
})

test_that("every constructor ggplot2 can look up by name defers to ggplot2", {
  skip_if_not_installed("ggplot2")
  ns <- getNamespaceExports("vellumplot")
  clash <- intersect(ns, getNamespaceExports("ggplot2"))
  reachable <- c(
    grep(
      "^scale_(x|y|colou?r|fill|alpha|size|shape|linetype|linewidth)_(continuous|discrete|ordinal|date|datetime|time|identity|binned)$",
      clash,
      value = TRUE
    ),
    grep("^guide_", clash, value = TRUE)
  )
  expect_gt(length(reachable), 40L)
  # ggplot2's time scales need hms
  if (!rlang::is_installed("hms")) {
    reachable <- grep("_time$", reachable, value = TRUE, invert = TRUE)
  }
  # call each one from a frame inside ggplot2's namespace, as ggplot2 does
  from_ggplot2 <- new.env(parent = asNamespace("ggplot2"))
  for (nm in reachable) {
    f <- getExportedValue("vellumplot", nm)
    # (ggplot2's discrete size / alpha scales warn about themselves)
    res <- suppressWarnings(eval(quote(f()), list(f = f), from_ggplot2))
    expect_true(inherits(res, "ggproto"), info = nm)
  }
  # called by anyone else, a vellumplot constructor is unchanged
  expect_error(scale_x_continuous(), "plot")
  expect_identical(guide_none(), "none")
  expect_identical(guide_legend()$kind, "legend")
})
