#' @include classes.R seam.R
NULL

# A composition of independent plots arranged on a grid. Each sub-plot keeps its
# own scales, axes, and legend; the composition only arranges them.
PlotComposition <- S7::new_class(
  "PlotComposition",
  package = "vellumplot",
  properties = list(
    plots = S7::class_list, # list<PlotSpec>
    nrow = S7::new_property(S7::class_double, default = 1),
    ncol = S7::new_property(S7::class_double, default = 1),
    width = S7::new_property(S7::class_double, default = 6),
    height = S7::new_property(S7::class_double, default = 4)
  )
)

.collect_plots <- function(dots) {
  if (!length(dots)) cli::cli_abort("Provide at least one plot to arrange.")
  for (p in dots) {
    if (!S7::S7_inherits(p, PlotSpec)) {
      cli::cli_abort("Composition expects {.cls PlotSpec} objects from {.fn vplot}.")
    }
  }
  dots
}

#' Arrange plots side by side
#'
#' Compose several independent plots into one image. `hconcat()` lays them in a
#' row, `vconcat()` in a column, and `concat()` on a grid of `ncol`/`nrow` cells.
#' Each sub-plot keeps its own scales, axes, and legend (this is view
#' composition, not faceting).
#'
#' @param ... [PlotSpec]s to arrange.
#' @param ncol,nrow Grid dimensions for `concat()` (defaults to roughly square).
#' @param width,height Output size in inches (defaults scale with the grid).
#' @return A `PlotComposition` (renders via [render_plot()] / [vellum::render()]).
#' @examples
#' a <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' b <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 10)
#' hconcat(a, b)
#' @export
concat <- function(..., ncol = NULL, nrow = NULL, width = NULL, height = NULL) {
  plots <- .collect_plots(list(...))
  n <- length(plots)
  if (is.null(ncol) && is.null(nrow)) ncol <- ceiling(sqrt(n))
  if (is.null(ncol)) ncol <- ceiling(n / nrow)
  if (is.null(nrow)) nrow <- ceiling(n / ncol)
  ncol <- as.double(ncol)
  nrow <- as.double(nrow)
  w0 <- plots[[1]]@width
  h0 <- plots[[1]]@height
  PlotComposition(
    plots = plots, nrow = nrow, ncol = ncol,
    width = width %||% (ncol * w0), height = height %||% (nrow * h0)
  )
}

#' @rdname concat
#' @export
hconcat <- function(..., height = NULL) {
  concat(..., ncol = ...length(), nrow = 1, height = height)
}

#' @rdname concat
#' @export
vconcat <- function(..., width = NULL) {
  concat(..., ncol = 1, nrow = ...length(), width = width)
}

# Compile a composition: an outer grid of equal null cells, each holding one
# sub-plot rendered with .draw_plot() (which pushes its own grid_layout inside
# the cell).
.compile_composition <- function(comp) {
  ncol <- comp@ncol
  nrow <- comp@nrow
  nulls <- function(k) do.call(c, replicate(k, vellum::unit(1, "null"), simplify = FALSE))
  scene <- vellum::vl_scene(width = comp@width, height = comp@height, bg = "white")
  scene <- vellum::push(scene, vellum::viewport(layout = vellum::grid_layout(nulls(ncol), nulls(nrow))))
  for (i in seq_along(comp@plots)) {
    r <- (i - 1L) %/% ncol + 1L
    cc <- (i - 1L) %% ncol + 1L
    scene <- vellum::push(scene, vellum::viewport(row = r, col = cc))
    scene <- .draw_plot(scene, comp@plots[[i]])
    scene <- vellum::pop(scene)
  }
  vellum::pop(scene)
}

S7::method(.as_vellum_scene, PlotComposition) <- function(x, ...) .compile_composition(x)

S7::method(print, PlotComposition) <- function(x, ...) {
  cli::cli_text("{.cls PlotComposition} {length(x@plots)} plot{?s} in a {x@nrow}x{x@ncol} grid")
  invisible(x)
}
