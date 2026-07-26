#' @include classes.R sparkline.R
NULL

#' Render a plot to a self-contained SVG string
#'
#' `plot_svg()` compiles any vellumplot object (a [PlotSpec], [vtable()], or a
#' composition) to a stand-alone `<svg>` **string** rather than a file — the way
#' to embed a chart *inline* in HTML: a [vsparkline()] in a `gt` / `reactable` /
#' `DT` cell, a chart in a Quarto callout, or an inline glyph in a report. The
#' string carries a `viewBox`, so it scales.
#'
#' @param plot A [PlotSpec], [vtable()], or composition.
#' @param width,height Optional page-size override in **inches** (defaults to the
#'   object's own size).
#' @param scaling `"fixed"` (default) keeps the pixel size; `"fit"` sets the root
#'   `<svg>` `width`/`height` to `100%` so it fills its container (the `viewBox`
#'   preserves the aspect).
#' @param text How glyphs are emitted: `"native"` (default) or `"outline"` (paths,
#'   for maximum portability).
#' @return A length-1 character SVG string.
#' @seealso [vsparkline()], [gt_vsparkline()], [render_plot()]
#' @examples
#' svg <- plot_svg(vsparkline(cumsum(rnorm(20))))
#' substr(svg, 1, 40)
#' @export
plot_svg <- function(
  plot,
  width = NULL,
  height = NULL,
  scaling = c("fixed", "fit"),
  text = "native"
) {
  scaling <- match.arg(scaling)
  # Size override is set on the plot object (PlotSpec / VTable / composition all
  # carry a double `width`/`height` in inches) before it compiles.
  if (!is.null(width)) {
    .check_dim(width, "width")
    plot <- S7::set_props(plot, width = as.double(width))
  }
  if (!is.null(height)) {
    .check_dim(height, "height")
    plot <- S7::set_props(plot, height = as.double(height))
  }
  svg <- vellum::scene_svg(vellum::as_vellum_scene(plot), text = text)
  if (identical(scaling, "fit")) {
    # rewrite only the root <svg ...> element's fixed width/height to 100%,
    # keeping the viewBox so the aspect is preserved.
    svg <- sub(
      '(<svg[^>]*?)width="[0-9.]+" height="[0-9.]+"',
      '\\1width="100%" height="100%"',
      svg
    )
  }
  svg
}

#' Add a vellumplot sparkline column to a `gt` table
#'
#' `gt_vsparkline()` renders a **list-column of numeric vectors** as a per-row
#' [vsparkline()], embedded as inline SVG in a [gt::gt()] table — so a gt table
#' (with all of gt's formatting and theming) gains a vellum sparkline column.
#' Needs the \pkg{gt} package, and HTML output (inline SVG is not embedded by gt's
#' LaTeX / Word backends).
#'
#' The vectors are recovered from the gt object's data in its current row order;
#' apply `gt_vsparkline()` **before** any gt row reordering / grouping.
#'
#' @param gt_object A [gt::gt()] object.
#' @param column The list-column to render (bare name or string).
#' @param type Sparkline type: `"line"` (default), `"bar"`, or `"winloss"`.
#' @param ... Further [vsparkline()] arguments (e.g. `color`, `points`).
#' @param width,height,units Sparkline size (default `30 x 8` mm).
#' @return The modified `gt` object.
#' @seealso [vsparkline()], [vtable()], [plot_svg()]
#' @examples
#' \dontrun{
#' df <- data.frame(metric = c("A", "B"))
#' df$trend <- list(cumsum(rnorm(20)), cumsum(rnorm(20)))
#' gt::gt(df) |> gt_vsparkline(trend, type = "line")
#' }
#' @export
gt_vsparkline <- function(
  gt_object,
  column,
  type = "line",
  ...,
  width = 30,
  height = 8,
  units = "mm"
) {
  .need_pkg("gt", "gt_vsparkline()")
  col <- rlang::as_name(rlang::ensym(column))
  data <- gt_object[["_data"]]
  if (is.null(data) || !col %in% names(data)) {
    cli::cli_abort("{.arg gt_object} has no column {.field {col}}.")
  }
  raw <- data[[col]]
  if (!is.list(raw)) {
    cli::cli_abort(
      "Column {.field {col}} must be a list-column of numeric vectors."
    )
  }
  svgs <- lapply(raw, function(v) {
    gt::html(plot_svg(
      vsparkline(
        as.numeric(v),
        type = type,
        width = width,
        height = height,
        units = units,
        ...
      ),
      scaling = "fit"
    ))
  })
  gt::text_transform(
    gt_object,
    locations = gt::cells_body(columns = {{ column }}),
    fn = function(x) svgs
  )
}
