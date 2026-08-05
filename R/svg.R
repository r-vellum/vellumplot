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
#' @param inline `TRUE` strips the leading `<?xml …?>` prolog so the `<svg>` is a
#'   valid fragment to drop straight into an HTML paragraph / cell / Quarto inline
#'   expression. `FALSE` (default) keeps the stand-alone-file prolog.
#' @param recolor Optional named character vector mapping a **source colour** to a
#'   replacement written verbatim into the SVG — e.g.
#'   `c(grey30 = "currentColor")` makes a `grey30` sparkline follow the surrounding
#'   text colour (dark-mode-adaptive inline). Each name is resolved to its hex and
#'   swapped for the value; use for CSS keywords (`"currentColor"`) or `var(--x)`
#'   that R's colour engine can't emit itself.
#' @param text How glyphs are emitted: `"native"` (default) or `"outline"` (paths,
#'   for maximum portability).
#' @param manifest If `TRUE`, embed a reproducibility manifest (see
#'   [plot_manifest()]) as an XML comment in the SVG, so the figure carries its
#'   own data fingerprint; [plot_verify()] reads it back. Default `FALSE`.
#' @return A length-1 character SVG string.
#' @seealso [vsparkline()], [gt_vsparkline()], [render_plot()], [plot_manifest()]
#' @examples
#' svg <- plot_svg(vsparkline(cumsum(rnorm(20))))
#' substr(svg, 1, 40)
#' # a transparent, prolog-free, dark-mode-adaptive inline sparkline:
#' plot_svg(vsparkline(1:9, color = "grey30"),
#'   inline = TRUE, recolor = c(grey30 = "currentColor")
#' )
#' @export
plot_svg <- function(
  plot,
  width = NULL,
  height = NULL,
  scaling = c("fixed", "fit"),
  inline = FALSE,
  recolor = NULL,
  text = "native",
  manifest = FALSE
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
  if (length(recolor)) {
    from <- names(recolor)
    if (is.null(from) || any(!nzchar(from))) {
      cli::cli_abort(
        "{.arg recolor} must be a named vector: {.code c(from = to)}."
      )
    }
    for (i in seq_along(recolor)) {
      # resolve the source colour to the hex the backend emits (lowercase), then
      # swap it for the replacement token verbatim.
      hex <- tolower(.as_hex(from[i]))
      svg <- gsub(hex, recolor[[i]], svg, ignore.case = TRUE)
    }
  }
  if (isTRUE(inline)) {
    svg <- sub("^\\s*<\\?xml[^>]*\\?>\\s*", "", svg)
  }
  if (isTRUE(manifest) && S7::S7_inherits(plot, PlotSpec)) {
    # inject the manifest comment just before the root <svg> element.
    svg <- sub("(<svg)", paste0(.manifest_comment(plot), "\n\\1"), svg)
  }
  svg
}

#' Encode a plot as a data URI
#'
#' `plot_data_uri()` renders a plot to a self-contained **`data:` URI** string —
#' the bytes of the image inlined as base64 — ready to drop into an HTML `<img
#' src>`, a Markdown image, an email, or anywhere a URL is expected without a
#' separate file. `"svg"` (default) inlines the crisp vector SVG; `"png"` inlines
#' a raster render.
#'
#' @param plot A [PlotSpec] (or any object [plot_svg()] / [render_plot()]
#'   accepts).
#' @param format `"svg"` (vector, default) or `"png"` (raster).
#' @param width,height Optional size override in inches.
#' @param dpi For `format = "png"`, the render resolution.
#' @param ... Passed to [plot_svg()] (svg) or [render_plot()] (png).
#' @return A length-1 character `data:` URI.
#' @seealso [plot_svg()], [render_plot()]
#' @examples
#' uri <- vplot(mtcars) |> mark_point(x = wt, y = mpg) |> plot_data_uri()
#' substr(uri, 1, 30)
#' @export
plot_data_uri <- function(
  plot,
  format = c("svg", "png"),
  width = NULL,
  height = NULL,
  dpi = NULL,
  ...
) {
  format <- match.arg(format)
  if (identical(format, "svg")) {
    svg <- plot_svg(plot, width = width, height = height, ...)
    paste0("data:image/svg+xml;base64,", jsonlite::base64_enc(svg))
  } else {
    f <- tempfile(fileext = ".png")
    on.exit(unlink(f), add = TRUE)
    if (!is.null(width)) {
      plot <- S7::set_props(plot, width = as.double(width))
    }
    if (!is.null(height)) {
      plot <- S7::set_props(plot, height = as.double(height))
    }
    render_plot(plot, f, dpi = dpi, ...)
    bytes <- readBin(f, "raw", n = file.info(f)$size)
    paste0("data:image/png;base64,", jsonlite::base64_enc(bytes))
  }
}

# A colour spec -> the `#rrggbb` string the SVG backend emits. A hex is returned
# as-is; a name / spec is resolved via grDevices (no alpha -- SVG uses fill-opacity).
.as_hex <- function(color) {
  if (is.character(color) && grepl("^#[0-9A-Fa-f]{6,8}$", color)) {
    return(substr(color, 1L, 7L))
  }
  rgb <- grDevices::col2rgb(color)
  grDevices::rgb(rgb[1L], rgb[2L], rgb[3L], maxColorValue = 255)
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
