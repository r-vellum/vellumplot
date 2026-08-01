#' @include seam.R compile-facet.R
NULL

# A plot the document functions accept: a compiled-to-scene grammar object.
.is_renderable <- function(x) {
  S7::S7_inherits(x, PlotSpec) ||
    S7::S7_inherits(x, PlotComposition) ||
    S7::S7_inherits(x, VTable)
}

.check_renderable_list <- function(
  plots,
  arg = "plots",
  call = rlang::caller_env()
) {
  if (!is.list(plots) || S7::S7_inherits(plots, PlotSpec)) {
    cli::cli_abort(
      "{.arg {arg}} must be a list of plots.",
      call = call
    )
  }
  if (!length(plots)) {
    cli::cli_abort("{.arg {arg}} is empty.", call = call)
  }
  ok <- vapply(plots, .is_renderable, logical(1))
  if (!all(ok)) {
    cli::cli_abort(
      c(
        "Every entry of {.arg {arg}} must be a plot.",
        i = "A {.cls PlotSpec}, {.cls PlotComposition}, or {.cls VTable}; entry {which(!ok)[1]} is not."
      ),
      call = call
    )
  }
  invisible(plots)
}

# Split a faceted PlotSpec into one plot per facet cell: the facet is removed,
# the plot data filtered to that cell, and the cell's label used as the title
# (unless one is already set). Reuses the compiler's own facet-key logic, so wrap
# and grid facets split the same way. Each page trains its own scales.
.facet_split <- function(spec) {
  facet <- spec@facet
  data <- spec@data
  quos <- c(facet@rows, facet@cols)
  key <- .facet_key(quos, data)
  levs <- levels(key)
  lapply(levs, function(lev) {
    sub <- spec
    sub@data <- data[!is.na(key) & key == lev, , drop = FALSE]
    sub@facet <- NULL
    if (is.null(sub@labels$title) && is.null(sub@labels$subtitle)) {
      sub@labels <- c(sub@labels, list(title = as.character(lev)))
    }
    sub
  })
}

#' Write a multi-page PDF
#'
#' `pdf_pages()` writes several plots into one PDF, one plot per page — a report,
#' a slide deck, or one page per facet. It is the multi-page companion of
#' [render_plot()] (which writes a single page). Pages may differ in size (each
#' plot keeps its own `width`/`height`), and the per-page accessibility tags
#' (structure tree + `Alt`, see the *Accessibility* article) are written for
#' every page.
#'
#' @param x Either a **list** of plots (each a [PlotSpec], composition, or table)
#'   — one page each — or a **single faceted** `PlotSpec`, in which case it is
#'   split into one page per facet cell (the facet is dropped and the data
#'   filtered per page; each page trains its own scales).
#' @param path Output `.pdf` path.
#' @return `path`, invisibly.
#' @seealso [render_plot()], [render_all()]
#' @examples
#' p1 <- vplot(mtcars) |> mark_point(x = wt, y = mpg)
#' p2 <- vplot(mtcars) |> mark_histogram(x = mpg, bins = 8)
#' f <- tempfile(fileext = ".pdf")
#' pdf_pages(list(p1, p2), f)
#'
#' # one page per facet cell:
#' faceted <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> facet_wrap(~cyl)
#' pdf_pages(faceted, tempfile(fileext = ".pdf"))
#' @export
pdf_pages <- function(x, path) {
  pages <- if (S7::S7_inherits(x, PlotSpec) && !is.null(x@facet)) {
    .facet_split(x)
  } else if (.is_renderable(x)) {
    cli::cli_abort(c(
      "A single, unfaceted plot is one page -- use {.fn render_plot} for it.",
      i = "Pass a {.emph list} of plots, or a faceted plot to split by facet."
    ))
  } else {
    .check_renderable_list(x)
    x
  }
  vellum::pdf_pages(pages, path)
}

#' Render many plots to separate files, in parallel
#'
#' `render_all()` renders a list of independent plots to separate files across
#' CPU cores — small multiples, a batch export, one file per group. The work is
#' embarrassingly parallel (one whole plot per worker), and the result is
#' byte-identical to rendering them one by one. Parallelism uses process forks,
#' so it speeds things up on macOS/Linux and falls back to sequential on Windows;
#' it is only worth it for several substantial plots.
#'
#' @param plots A named or unnamed list of plots.
#' @param paths Output paths, one per plot (the format of each comes from its
#'   extension, as in [render_plot()]). As a shortcut, when `plots` is **named**
#'   `paths` may be a single existing **directory**, and each plot is written to
#'   `<name>.png` inside it.
#' @param workers Number of parallel workers; `NULL` (default) uses the available
#'   cores. `1` forces sequential.
#' @param ... Passed to each [vellum::render()] (e.g. `text`, `cvd`).
#' @return `paths` (the resolved file paths), invisibly.
#' @seealso [render_plot()], [pdf_pages()], [repeat_()]
#' @examples
#' plots <- list(
#'   wt = vplot(mtcars) |> mark_point(x = wt, y = mpg),
#'   hp = vplot(mtcars) |> mark_point(x = hp, y = mpg)
#' )
#' render_all(plots, tempdir())
#' @export
render_all <- function(plots, paths, workers = NULL, ...) {
  .check_renderable_list(plots)
  vellum::render_all(plots, paths, workers = workers, ...)
}
