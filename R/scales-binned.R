#' @include classes.R scales.R
NULL

# --- binned (classed) colour scales -----------------------------------------
#
# A binned scale cuts a continuous variable into classes and maps each class to
# one colour of a sequential palette -- the cartographic norm for choropleths
# (classed maps read back to a value range far more reliably than an unclassed
# ramp). Breaks come from `classInt::classIntervals()` when available (it offers
# jenks/fisher/etc.); a base-R fallback covers the common styles without the
# dependency. `classInt` lives in Suggests.

# Compute `n`-class breaks for `v` under a classification `style`. `quantile`,
# `equal`, and `pretty` are handled in base R so no dependency is needed; every
# other style (jenks, fisher, kmeans, headtails, sd, ...) routes through
# classInt::classIntervals(). Returns a sorted numeric vector of length n+1.
.binned_breaks <- function(v, n, style) {
  v <- v[is.finite(v)]
  if (!length(v)) {
    cli::cli_abort("A binned scale needs at least one finite value.")
  }
  base_styles <- c("quantile", "equal", "pretty")
  if (style %in% base_styles) {
    brks <- switch(
      style,
      quantile = stats::quantile(
        v,
        probs = seq(0, 1, length.out = n + 1),
        names = FALSE,
        type = 7
      ),
      equal = seq(min(v), max(v), length.out = n + 1),
      pretty = {
        p <- pretty(v, n = n)
        # keep the pretty cuts strictly covering the data range
        p[p >= min(v) - 1e-9 & p <= max(v) + 1e-9]
      }
    )
    brks <- sort(unique(as.numeric(brks)))
    if (length(brks) < 2L) {
      brks <- range(v) + c(-0.5, 0.5)
    }
    return(brks)
  }
  .need_pkg("classInt", sprintf("scale_*_binned(style = \"%s\")", style))
  ci <- classInt::classIntervals(v, n = n, style = style)
  sort(unique(as.numeric(ci$brks)))
}

# Human-readable interval labels "[a, b)" from a break vector; the last class is
# right-closed "[y, z]". Numbers are formatted compactly.
.interval_labels <- function(brks) {
  fmt <- function(x) trimws(formatC(x, format = "g", digits = 4))
  k <- length(brks) - 1L
  if (k < 1L) {
    return(character(0))
  }
  lo <- fmt(brks[-length(brks)])
  hi <- fmt(brks[-1])
  close <- c(rep(")", k - 1L), "]")
  paste0("[", lo, ", ", hi, close)
}

#' Binned (classed) colour scales
#'
#' `scale_fill_binned()` / `scale_color_binned()` cut a continuous aesthetic into
#' classes and colour each class with one step of a sequential palette — the
#' standard choropleth scale. Class breaks are chosen by a classification
#' `style`. `quantile`, `equal`, and `pretty` need no extra packages; other
#' styles (e.g. `"jenks"`, `"fisher"`, `"kmeans"`, `"headtails"`) delegate to the
#' optional `classInt` package (in `Suggests`) and error if it is absent.
#'
#' @param plot A [PlotSpec].
#' @param style Classification style: one of `"quantile"`, `"equal"`, `"pretty"`
#'   (base R), or any `classInt::classIntervals()` style. Default `"quantile"`.
#' @param n Number of classes (default `5`). Ignored when `breaks` is supplied.
#' @param breaks Explicit class boundaries (length = number of classes + 1),
#'   overriding `style`/`n`.
#' @param palette A sequential palette: `NULL` (viridis), an `grDevices::hcl.pals()`
#'   name, or a vector of colours interpolated across the classes.
#' @param labels Optional class labels (one per class); defaults to interval
#'   ranges like `"[0, 10)"`.
#' @param na_value Fill colour for `NA` values (shown as a distinct legend
#'   swatch). Default `"grey80"`.
#' @param name Legend title, or `NULL` to derive from the encoding.
#' @return The modified [PlotSpec].
#' @seealso [mark_sf()], [scale_fill_continuous()]
#' @examples
#' \dontrun{
#' nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)
#' vplot(nc) |>
#'   mark_sf(fill = SID74) |>
#'   scale_fill_binned(style = "quantile", n = 5) |>
#'   coord_sf()
#' }
#' @export
scale_fill_binned <- function(
  plot,
  style = "quantile",
  n = 5,
  breaks = NULL,
  palette = NULL,
  labels = NULL,
  na_value = "grey80",
  name = NULL
) {
  .binned_scale(plot, "fill", style, n, breaks, palette, labels, na_value, name)
}

#' @rdname scale_fill_binned
#' @export
scale_color_binned <- function(
  plot,
  style = "quantile",
  n = 5,
  breaks = NULL,
  palette = NULL,
  labels = NULL,
  na_value = "grey80",
  name = NULL
) {
  .binned_scale(
    plot,
    "color",
    style,
    n,
    breaks,
    palette,
    labels,
    na_value,
    name
  )
}

.binned_scale <- function(
  plot,
  aesthetic,
  style,
  n,
  breaks,
  palette,
  labels,
  na_value,
  name
) {
  .check_plot(plot)
  .add_scale(
    plot,
    ScaleSpec(
      aesthetic = aesthetic,
      type = "binned",
      palette = palette,
      breaks = breaks,
      labels = labels,
      style = style,
      n = n,
      na_value = na_value,
      name = name
    )
  )
}
